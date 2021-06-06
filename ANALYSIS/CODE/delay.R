# >>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>
# Project: parrot repeat
# Date started: 02-05-2021
# Date last modified: 05-05-2021
# Author: Simeon Q. Smeele
# Description: Analysis of the delay data.
# source('delay.R', chdir = T)
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
# dev.off()
# cat('\014')  

# Set working directory to mother-folder
# setwd(str_remove(dirname(rstudioapi::getActiveDocumentContext()$path), '/CODE'))

# Load functions
.functions = sapply(list.files('functions', pattern = '*.R', full.names = T), source)

# Load data
dat_orig = read.csv2('../DATA/Parrots_repeat.csv')
dat_orig$Animal = dat_orig$Animal %>% str_remove(' ')
delay = read.csv2('../DATA/Delay-responses.csv')

# Colours
colours_animals = c(MrHuang = '#B71C1C', Charlie = '#9B59B6', Gargamel = '#004D40', 
                    Nouba = '#FBC02D', Eva = '#303F9F')

# Translate id's
trans_id  = c(MrHuang = 1, Charlie = 2, Gargamel = 3)

# >>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>
# ANALYSIS: delay ----
# >>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>

# Clean data
trans_behaviour = 1:length(unique(delay$b1[delay$b1 != 'repeat']))
names(trans_behaviour) = unique(delay$b1[delay$b1 != 'repeat'])
delay_rep = delay[delay$b2 == 'repeat',]
clean_dat = data.frame(id = trans_id[delay_rep$Animal],
                       beh = trans_behaviour[delay_rep$b1],
                       response = ifelse(delay_rep$correct.or.not.2 == 'c', 1, 0),
                       log_time = log(delay_rep$delay))

# Make simple plot
plot(clean_dat$log_time, 
     clean_dat$response + runif(nrow(clean_dat), 0, 0.1), 
     col = alpha(colours_animals[trans_id[clean_dat$id]], 0.8), pch = 16)

# Include averages
times = unique(clean_dat$log_time)
perfs = sapply(times, function(x) 
  sum(clean_dat$response[clean_dat$log_time == x])/nrow(clean_dat[clean_dat$log_time == x,]))
points(times, perfs, pch = 16, col = alpha(1, 0.8), cex = 2)

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

# Plot results
post = extract.samples(m_delay)
pred_perfs = sapply(times, function(time){
  sapply(1:length(post$a_bar), function(i) post$a_bar[i] + post$b[i] * time) %>% mean
})
lines(times, pred_perfs, lty = 2, lwd = 3, )


