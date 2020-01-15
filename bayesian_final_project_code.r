rm(list=ls())
library(dplyr)
library(tidyr)
library(ggplot2)
library(zoo)

setwd('C:\\Users\\Evan Generoli\\Documents\\Graduate School\\Spring Semester 2019\\Bayesian\\final_project')
df = read.csv('loan.csv', stringsAsFactors = F)
#save(df, file = 'df_raw.rds')
load('df_raw.rds')

## want to filter observations by settled loans (ie. have been fully paid or charged off)
# can't use issue date b/c there are two terms of loans (3yr & 5yr)
# >>> create term completion date = issue date + term length

# issue_d is a character -- recode to usable format
class(df$issue_d)
df$issue_date = as.yearmon(df$issue_d, "%b-%Y")
class(df$issue_date)
df$issue_date %>% head()
summary(df$issue_date)

## create numeric term from character
# term is either " 36 Months" or " 60 Months" (notice initial spaces)
# also replace raw term variable with values of '3 year' & '5 year'
table(df$term)
df$term_numeric = df$term %>% substr(2,3) %>% as.numeric() / 12
df$term = paste(df$term_numeric, 'year', sep = ' ')
table(df$term_numeric)

# create term completion date = issue date + term length
# save whole dataframe to df_whole.rds, then df can be truncated 
df$term_completion_date = df$issue_date + df$term_numeric

# levels 4 & 5 need to be recoded -- meeting the credit policy
df$loan_status = factor(df$loan_status)
levels(df$loan_status)
# [1] "Charged Off"   "Current"   "Default"
# [4] "Does not meet the credit policy. Status:Charged Off"
# [5] "Does not meet the credit policy. Status:Fully Paid"
# [6] "Fully Paid"    "In Grace Period"   "Late (16-30 days)"   "Late (31-120 days)"

# recode 4 & 5 to 1 & 6 respectively, then drop now empty factor levels (4 & 5)
df$loan_status[df$loan_status == levels(df$loan_status)[4]] <- levels(df$loan_status)[1]
df$loan_status[df$loan_status == levels(df$loan_status)[5]] <- levels(df$loan_status)[6]
df$loan_status = droplevels(df$loan_status) # drop empty levels
levels(df$loan_status)

# create new dataframe with only loans whose terms have finished (feb '19 is the latest month in dataset)
df = df %>% filter(term_completion_date <= 'feb 2019' %>% as.yearmon('%b %Y'))
table(df$loan_status)
# cutting term completion date at feb 2019 leaves obs. with loan_status:
#   current, grace period, late (16-30 days) & late (31-120 days)

### look at cros-tab of term completion date and loan status 
temp = table(df$term_completion_date, df$loan_status)
temp = temp[ ,c(1,4,2:3,5:7)]   # reorder columns 
# find earliest term completion date with nonzero entries for anything except 'fully paid' & 'charged off'
i = min(which(!(temp[,3] == 0 & temp[,4] == 0 & temp[,5] == 0 & temp[,6] == 0 & temp[,7] == 0)))
levels(as.factor(df$term_completion_date))[i]
# > 'jan 2018' >>> all loans w/ term completion date < 'jan 2018' are settled
rm(i,temp)

# cut completion date at dec 2017 (inclusive) so observations are all settled loans (fully paid/ charged off)
df = df %>% filter(term_completion_date <= 'dec 2017' %>% as.yearmon('%b %Y'))
# dataset is now 358,894 observations 
save(df, file = 'df_truncated.rds')

rm(list = ls())
load('df_truncated.rds')
library(zoo)

df$loan_status = droplevels(df$loan_status) # remove empty levels
table(df$loan_status)
as.numeric(table(df$loan_status)[1]) + as.numeric(table(df$loan_status)[2]) == nrow(df)

# create binary variable for default
df$default = as.numeric(df$loan_status == 'Charged Off')

# recode earliest credit line from string to date
df$earliest_cr_line =  as.yearmon(df$earliest_cr_line, '%b-%Y')
# create credit length = issue_date - earliest credit line date
df$credit_length = df$issue_date - df$earliest_cr_line

# create indicator variable for home owner
df$home_owner = as.numeric(df$home_ownership=='MORTGAGE' | df$home_ownership=='OWN')

# recode loan amount, funded amount, & annual income in terms of $1000s 
df$loan_amnt = df$loan_amnt / 1000
df$funded_amnt = df$funded_amnt / 1000
df$annual_inc = df$ annual_inc / 1000
plot(densit(df$annual_inc))
summary(df$annual_inc)

#create logged variables of annual income, credit length, and loan amount
df$log_annual_inc = log(df$annual_inc)
df$log_credit_length = log(df$credit_length)
df$log_loan_amnt = log(df$loan_amnt)

# emp length is categorical -- create numeric version
df$emp_length_raw = df$emp_length
# replace all with format: '1 year','2 years',etc.
index = which(!(df$emp_length == '< 1 year' | df$emp_length == '10+ years' | df$emp_length == 'n/a'))
df$emp_length[index] = df$emp_length[index] %>% substr(1,1) %>% as.numeric()
df$emp_length[df$emp_length == '< 1 year'] = 0.5
df$emp_length[df$emp_length == '10+ years'] = 10
df$emp_length[df$emp_length == 'n/a'] = 0 ############## maybe recode as zeros?
df$emp_length = df$emp_length %>% as.numeric()
rm(index)


save(df, file = 'df_ans.rds')
##########################################################################################
#########    Analysis / Final Cleaning    #########
###################################################

rm(list = ls())
library(dplyr)
setwd('C:\\Users\\Evan Generoli\\Documents\\Graduate School\\Spring Semester 2019\\Bayesian\\final_project')
load('df_ans.rds')

# pare down dataset to only a few interesting variables for analysis
keep = c('annual_inc','log_annual_inc','dti','home_owner','credit_length','int_rate',
         'log_credit_length','loan_amnt','log_loan_amnt','emp_length','default') 

df = df[ ,(names(df) %in% keep)]
names(df)
rm(keep)

## take random sample to create smaller dataset to work with
sum(is.na(df)) # 66 rows have NA values out of n = 358,894 (0.018% of sample) -- drop them
set.seed(100)
df = na.omit(df) %>% sample_n(1000)

#############################################
############################     EDA

str(df)
summary(df)
library(psych)
library(purrr)
library(tidyr)
library(ggplot2)

### numerical summaries
df_sum = as.matrix(na.omit(df[ ,!(names(df) %in% c('log_annual_inc','log_credit_length','log_loan_amnt'))]))
sum_stats = matrix(0, nrow = ncol(df_sum), ncol = 5)
row.names(sum_stats) = colnames(df_sum)
colnames(sum_stats) = c('Min','Median','Mean','Max','Std. Dev.')
for (i in 1:ncol(df_sum)){
  sum_stats[i,] = c(min(df_sum[,i]), median(df_sum[,i]), mean(df_sum[,i]), 
                    max(df_sum[,i]), sd(df_sum[,i]))
}
sum_stats = sum_stats %>% round(2)
sum_stats
rm(df_sum)


######### histograms
df %>%
  gather() %>% 
  ggplot(aes(value)) +
  facet_wrap(~ key, scales = "free") +
  geom_histogram()

#### annual income can clearly benefit from a log transformation
#### credit length appears somewhat improved by the log transformation as well,
#### loan amount does appear marginally improved by a log transformation
drop = c('annual_inc','loan_amnt','credit_length') # drop originals
df = df[ ,!(names(df) %in% drop)]
x_vars = names(df)[!(names(df) %in% 'default')]
rm(drop)

######  SPLOM, correlations, etc.
pairs.panels(df, 
             method = "pearson", # correlation method
             hist.col = "#00AFBB",
             density = FALSE,  # show density plots
             ellipses = FALSE, # show correlation ellipses
             smooth = FALSE,
             lm = TRUE)

# fit frequentist model to get MLEs of betas for g-prior
library(stats)
mod_freq = glm(as.formula(paste('default ~', paste(x_vars, collapse = ' + '))), 
                     family = binomial(link = 'logit'), data = df)

#### code / fit models w/ stan
library(rstan)

stan_model_code = "
data {
  int<lower=1> n;  // number of observations
  int<lower=1> k;  // number of parameters to estimate
  int y[n];        // indicator of default
  matrix[n, k] X;  // covariates
  vector[k] mu0;   // prior mean for beta
  cov_matrix[k] V; // covariance matrix for beta g-prior
}
parameters {
  vector[k] beta;
}
transformed parameters {
  vector[n] eta; // eta (log odds) is a linear combination of X's
  eta = X * beta;
}
model {
  // prior distributions
    beta ~ multi_normal(mu0, V);
  // data distribution
    y ~ bernoulli_logit(eta);
}
generated quantities {
  vector[k] exp_beta;
  real log_lik[n];
  exp_beta = exp(beta);
  for(i in 1:n) {
    log_lik[i] = bernoulli_logit_lpmf(y[i] | eta[i]);
  }
}
"

################### create g-prior covariance matrix
X = cbind(1, as.matrix(df[ ,x_vars]))

# logit model
n = nrow(X)
H_logit = diag(nrow = n)
pi_logit = numeric(n)
for (i in 1:n){
  pi_logit[i] = 1 / (1 + exp(-X[i, ] %*% mod_freq$coefficients))
  H_logit[i,i] = pi_logit[i] * (1 - pi_logit[i])
}

H_beta = t(X) %*% H_logit %*% X
sigma_beta_logit = n * solve(H_beta)
sigma_beta_logit_no_const = sigma_beta_logit[-1,-1]
rm(H_beta,H_logit,pi_logit)


########################### forward stepwise selection algorithm w/ WAIC to determine good model
stan_mod_list = list()
retained_vars = list()
waic = list()
seq = c(1:8)
waic_best = 100000000
vars_kept = c(1)
for (i in seq[1:7]){
  
  k = i + 1
  stan_mod_list[[i]] = list()
  waic[[i]] = numeric()
  
  for(j in 1:rev(seq)[i+1]){
    vars_temp = c(vars_kept, seq[-vars_kept][j])
    stan_data = list(n = n, k = k, y = df$default, X = X[ ,vars_temp], 
                     mu0 = numeric(k), V = sigma_beta_logit[vars_temp,vars_temp])
    mod = stan(model_code = stan_model_code, data = stan_data, 
               iter = 10000, chains = 3)
    stan_mod_list[[i]][[j]] = mod
    
    ll = extract_log_lik(mod, merge_chains = FALSE)
    waic[[i]][j] = (waic(ll))$waic
  }
  
  if (min(waic[[i]]) < waic_best){
    index = which.min(waic[[i]])
    waic_best = waic[[i]][index]
    vars_kept = c(vars_kept, seq[-vars_kept][index])
    retained_vars[[i]] = vars_kept
    stan_mod_list[[i]] = stan_mod_list[[i]][[index]]
  } else {
    break
  }
  
}
rm(i,j,k,index,ll,mod,seq,vars_kept,vars_temp)

# > waic
# [[1]]
# [1] 812.0740 851.8296 849.2093 855.4523 840.1220 851.7088 856.2157
# [[2]]
# [1] 807.7218 810.8380 813.9563 802.3916 812.5074 813.5095
# [[3]]
# [1] 801.8712 803.0758 804.2344 804.4707 803.5515
# [[4]]
# [1] 801.9076 803.3039 803.7277 802.6592
# 
# > colnames(X)[retained_vars[[1]]]
# [1] ""        "int_rate"
# > colnames(X)[retained_vars[[2]]]
# [1] ""        "int_rate"       "log_annual_inc"
# > colnames(X)[retained_vars[[3]]]
# [1] ""        "int_rate"       "log_annual_inc"     "emp_length" 
# > colnames(X)
# [1] ""                  "int_rate"          "emp_length"        "dti"              
# [5] "home_owner"        "log_annual_inc"    "log_credit_length" "log_loan_amnt"
##########################################################################
#######    model settled on is int_rate + log_annual_inc + emp_length

mod_final = stan_mod_list[[3]]
X_final_mod = X[ ,retained_vars[[3]]]

# check convergence with gelman-rubin statistics
r_hat = matrix(nrow = 3, ncol = 2)
for (i in 1:3){
  r_hat[i, ] = c(min(summary(stan_mod_list[[i]])$summary[,"Rhat"]),
                 max(summary(stan_mod_list[[i]])$summary[,"Rhat"]))
}

# check trace plots
stan_trace(mod_final, c('beta'))

params = c('beta[1]','beta[2]','beta[3]','beta[4]',
           'exp_beta[1]','exp_beta[2]','exp_beta[3]','exp_beta[4]')
# summary of fitted values
summary(mod_final)$summary[params,]

# posterior means, medians, and 95% central posterior intervals
summary(mod_final)$summary[params, c("mean", "50%", "2.5%", "97.5%")]

# plot of densities for betas
stan_dens(mod_final, par = params[1:4],
          separate_chains = TRUE)

#############################################
######### posterior predictive checks
############### create 1000 replicated datasets
# extract beta samples
chains = extract(mod_final, pars = 'beta') %>% as.data.frame()
### randomly sample 1000 rows from beta chains dataframe
n_yrep = 1000
set.seed(123)
beta_sample = chains[sample(1:nrow(chains), size = n_yrep), ]
### create eta (log odds) matrix
eta = matrix(nrow = n_yrep, ncol = n)
for (i in 1:n_yrep){
  eta[i, ] = X_final_mod %*% as.numeric(beta_sample[i, ]) %>% as.numeric()
}
### transform into probabilities
y_rep2 = 1 / (1 + exp(-eta))
# round to 0 or 1
y_rep = round(y_rep2)

#### plot densities/histograms of replicated datasets overlayed with observed data density
library(bayesplot)
ppc_dens_overlay(df$default, y_rep[1:50, ])
ppc_dens_overlay(df$default, y_rep2[1:50, ])

ppc_bars(df$default, y_rep)
ppc_hist(df$default, y_rep[1:8, ])

#### test quantity -- let T(y) = # defaults ie. sum(y)
t_y = sum(df$default)
t_yrep = rowSums(y_rep)
p_b = (sum(t_yrep < t_y) + 1)/(n_yrep + 1)

# plot histogram of test stat
hist(t_yrep, breaks = 20)
# with vertical red line at observed value
hist(t_yrep, breaks = 20, xlim = c(0,160))
abline(v = t_y, col = 'red')

