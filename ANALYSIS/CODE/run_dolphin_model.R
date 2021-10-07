# >>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>
# Project: parrot repeat
# Date started: 27-09-2021
# Date last modified: 27-09-2021
# Author: Simeon Q. Smeele
# Description: Analysis of the non-delay trials with the dolphins. 
# source('ANALYSIS/CODE/run_dolphin_model.R')
# >>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>

# >>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>
# DATA ----
# >>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>

# Loading libraries
libraries = c('data.table', 'tidyverse', 'readxl', 'rethinking')
for(i in libraries){
  if(! i %in% installed.packages()) lapply(i, install.packages)
  lapply(libraries, require, character.only = TRUE)
}

# Clean R
rm(list = ls()) 

set_ulam_cmdstan(F)

# Load data
dat_orig = read.csv2('ANALYSIS/DATA/dolphins/Data_dolphins.csv')
dat_orig$Animal = dat_orig$Animal %>% str_remove(' ')

# Colours
colours_animals = c(Clara = '#B71C1C', Charlie = '#9B59B6', Gargamel = '#004D40', 
                    Nouba = '#FBC02D', Eva = '#303F9F')

# Translate id's
trans_id  = c(Clara = 1)

# >>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>
# DATA WRANLING ----
# >>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>

# Fix the two columns
dat = data.frame()
for(i in 1:nrow(dat_orig)){
  for(j in 1:2){
    new = data.frame(animal = dat_orig$Animal[i],
                     session = dat_orig$Session[i],
                     behaviour = dat_orig[i,sprintf('Behaviour_%s', j)],
                     offered = dat_orig[i,sprintf('Offered_%s', j)])
    dat = rbind(dat, new)
  }
}

# Translation behaviours
trans_behaviour = 1:length(unique(dat$behaviour[dat$behaviour != 'repeat']))
names(trans_behaviour) = unique(dat$behaviour[dat$behaviour != 'repeat'])

# Retrieve the single repeat performance
dat_single = data.frame()
for(i in 2:nrow(dat)){
  if(dat$behaviour[i] == 'repeat' & dat$behaviour[i-1] != 'repeat'){
    new = data.frame(response = ifelse(dat$offered[i] == dat$behaviour[i-1], 1L, 0L),
                     id = trans_id[dat$animal[i]],
                     beh = trans_behaviour[dat$behaviour[i-1]])
    dat_single = rbind(dat_single, new)
  }
}

# Retrieve the double repeat performance
dat_double = data.frame()
for(i in 3:nrow(dat)){
  if(dat$behaviour[i] == 'repeat' & dat$behaviour[i-1] == 'repeat'){
    new = data.frame(response = ifelse(dat$offered[i-1] == dat$behaviour[i-2] &
                                         dat$offered[i] == dat$behaviour[i-2], 1L, 0L),
                     id = trans_id[dat$animal[i]],
                     beh = trans_behaviour[dat$behaviour[i-2]])
    dat_double = rbind(dat_double, new)
  }
}

# >>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>
# ANALYSIS: single repeats ----
# >>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>

# Including behaviour
m_single <- ulam(
  alist(
    response ~ dbinom(1, p),
    logit(p) <- a_bar + 
      # z_id[id] * sigma_id + 
      z_beh[beh] * sigma_beh,
    a_bar ~  normal(-0.5, 1),
    # z_id[id] ~ normal(0, 0.5),
    z_beh[beh] ~ normal(0, 0.5),
    # sigma_id ~ dexp(1),
    sigma_beh ~ dexp(2),
    # gq> vector[id]: a_ind <<- a_bar + z_id * sigma_id,
    gq> vector[beh]: a_beh <<- a_bar + z_beh * sigma_beh
  ), data = dat_single, chains = 4, cores = 1, iter = 8000, warmup = 500)

save(m_single, file = 'ANALYSIS/RESULTS/dolphins/m_single.RData')
precis(m_single, depth = 2)

# Plotting results
post = extract.samples(m_single)

# Overall
prior = rnorm(1e6, -0.5, 1) %>% inv_logit %>% density
plot(prior, xlim = c(-0.1, 1.1), ylim = c(0, 20), main = 'overall', 
     xlab = 'probability of success', ylab = 'density')
polygon(prior, col = alpha('grey', 0.5))
post$a_bar %>% inv_logit %>% density(bw = 0.02) %>% polygon(col = alpha('black', 0.8))
lines(c(0.25, 0.25), c(0, 20), lty = 2, lwd = 3, col = alpha('black', 0.5))
points(dat_single$response + runif(nrow(dat_single), 0, 0.1), runif(nrow(dat_single), 5, 15),
       pch = 16, col = alpha('black', 0.8))
pc = sum(dat_single$response)/nrow(dat_single)
lines(c(pc, pc), c(0, 20), lty = 2, lwd = 3, col = alpha('black', 0.8))
