# >>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>
# Project: parrot repeat
# Date started: 02-05-2021
# Date last modified: 18-12-2021
# Author: Simeon Q. Smeele
# Description: Analysis of the delay data.
# source('ANALYSIS/CODE/06_run_delay_model.R')
# >>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>

m_delay = ulam(
  alist(
    
    response ~ dbinom(1, p),
    logit(p) <- a_bar + 
      z_id[id] * sigma_id + 
      z_beh[beh] * sigma_beh +
      (b_bar + zb_id[id] * sigma_id_b) * log_time,
    a_bar ~  normal(-1, 2),
    z_id[id] ~ normal(0, 1),
    z_beh[beh] ~ normal(0, 1),
    b_bar ~ normal(0, 1),
    zb_id[id] ~ normal(0, 1),
    sigma_id ~ dexp(2),
    sigma_id_b ~ dexp(2),
    sigma_beh ~ dexp(2),
    gq> vector[id]: a_ind <<- a_bar + z_id * sigma_id,
    gq> vector[beh]: a_beh <<- a_bar + z_beh * sigma_beh
    
  ), data = dat_delay, chains = 4, cores = 4, iter = 4000, warmup = 500, cmdstan = TRUE)

save(m_delay, file = path_model_delay)

message('Succesfully ran model and saved results. ')