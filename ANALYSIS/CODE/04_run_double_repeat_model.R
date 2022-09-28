# >>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>
# Project: parrot repeat
# Date started: 17-12-2021
# Date last modified: 17-12-2021
# Author: Simeon Q. Smeele
# Description: Running the model for the double repeats.
# source('ANALYSIS/CODE/04_run_double_repeat_model.R')
# >>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>

# Including behaviour
m_double <- ulam(
  alist(
    response ~ dbinom(1, p),
    logit(p) <- a_bar + 
      z_id[id] * sigma_id + 
      z_beh[beh] * sigma_beh,
    a_bar ~  normal(-1, 1),
    z_id[id] ~ normal(0, 0.5),
    z_beh[beh] ~ normal(0, 0.5),
    sigma_id ~ dexp(1),
    sigma_beh ~ dexp(2),
    gq> vector[id]: a_ind <<- a_bar + z_id * sigma_id,
    gq> vector[beh]: a_beh <<- a_bar + z_beh * sigma_beh
  ), data = dat_double, chains = 4, cores = 4, iter = 8000, warmup = 500, cmdstan = TRUE)

save(m_double, file = path_model_double)

message('Succesfully ran model and saved results. ')