# Loading libraries
libraries = c('tidyverse', 'readxl', 'rethinking')
for(i in libraries){
  if(! i %in% installed.packages()) lapply(i, install.packages)
  lapply(libraries, require, character.only = TRUE)
}

# Clean R
rm(list = ls()) 

# Load data
dat_orig = read.csv2('DATA/Parrots_repeat.csv')
dat_orig$Animal = dat_orig$Animal %>% str_remove(' ')
delay = read.csv2('DATA/Delay-responses.csv')

# Translate id's
trans_id  = c(MrHuang = 1, Charlie = 2, Gargamel = 3)

# Clean data
trans_behaviour = 1:length(unique(delay$b1[delay$b1 != 'repeat']))
names(trans_behaviour) = unique(delay$b1[delay$b1 != 'repeat'])
delay_rep = delay[delay$b2 == 'repeat',]
clean_dat = data.frame(id = trans_id[delay_rep$Animal],
                       beh = trans_behaviour[delay_rep$b1],
                       response = ifelse(delay_rep$correct.or.not.2 == 'c', 1, 0),
                       log_time = log(delay_rep$delay))

m_delay = ulam(
  alist(
    response ~ dbinom(1, p),
    logit(p) <- a_bar + 
      z_id[id] * sigma_id + 
      z_beh[beh] * sigma_beh +
      b * log_time,
    a_bar ~  normal(-1, 1),
    z_id[id] ~ normal(0, 0.5),
    z_beh[beh] ~ normal(0, 0.5),
    b ~ normal(0, 0.5),
    sigma_id ~ dexp(1),
    sigma_beh ~ dexp(2),
    gq> vector[id]: a_ind <<- a_bar + z_id * sigma_id,
    gq> vector[beh]: a_beh <<- a_bar + z_beh * sigma_beh
  ), data = clean_dat, chains = 4, cores = 1, iter = 8000, warmup = 500)

precis(m_delay, depth = 2)
save(m_delay, file = 'RESULTS/m_delay.R')