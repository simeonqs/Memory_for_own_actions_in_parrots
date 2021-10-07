# >>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>
# Project: parrot repeat
# Date started: 27-09-2021
# Date last modified: 27-09-2021
# Author: Simeon Q. Smeele
# Description: Analysis of the non-delay trials with the dolphins. 
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
dev.off()
cat('\014')  

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
pdf('RESULTS/single - densities.pdf', 7, 7)
par(mfrow = c(2, 2))
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
# Each individual
prior = (rnorm(1e6, -0.5, 1) + rnorm(1e6, 0, 0.5)) %>% inv_logit %>% density
for(animal in trans_id){
  plot(prior, xlim = c(-0.1, 1.1), ylim = c(0, 20), main = names(trans_id[animal]), 
       xlab = 'probability of success', ylab = 'density')
  polygon(prior, col = alpha('grey', 0.5))
  post$a_ind[, animal] %>% inv_logit %>% density(bw = 0.02) %>% 
    polygon(col = alpha(colours_animals[trans_id[animal]], 0.8))
  lines(c(0.25, 0.25), c(0, 20), lty = 2, lwd = 3, col = alpha('black', 0.5))
  # Add raw data
  sub = dat_single[dat_single$id == animal,]
  points(sub$response + runif(nrow(sub), 0, 0.1), runif(nrow(sub), 5, 15),
         pch = 16, col = alpha(colours_animals[trans_id[animal]], 0.8))
  pc = sum(sub$response)/nrow(sub)
  lines(c(pc, pc), c(0, 20), lty = 2, lwd = 3, col = alpha(colours_animals[trans_id[animal]], 0.8))
}
dev.off()

# >>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>
# ANALYSIS: double repeats ----
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
  ), data = dat_double, chains = 4, cores = 4, iter = 8000, warmup = 500)

save(m_double, file = 'ANALYSIS/RESULTS/dolphins/m_double.RData')
precis(m_double, depth = 2) %>% plot

# Plotting results
post = extract.samples(m_double)
pdf('RESULTS/double - densities.pdf', 7, 7)
par(mfrow = c(2, 2))
# Overall
prior = rnorm(1e6, -1, 1) %>% inv_logit %>% density
plot(prior, xlim = c(-0.1, 1.1), ylim = c(0, 20), main = 'overall', 
     xlab = 'probability of success', ylab = 'density')
polygon(prior, col = alpha('grey', 0.5))
post$a_bar %>% inv_logit %>% density(bw = 0.02) %>% polygon(col = alpha('black', 0.8))
lines(c(1/8, 1/8), c(0, 20), lty = 2, lwd = 3, col = alpha('black', 0.5))
points(dat_double$response + runif(nrow(dat_double), 0, 0.1), runif(nrow(dat_double), 5, 15),
       pch = 16, col = alpha('black', 0.8))
pc = sum(dat_double$response)/nrow(dat_double)
lines(c(pc, pc), c(0, 20), lty = 2, lwd = 3, col = alpha('black', 0.8))
# Each individual
prior = (rnorm(1e6, -1, 1) + rnorm(1e6, 0, 0.5)) %>% inv_logit %>% density
for(animal in trans_id){
  plot(prior, xlim = c(-0.1, 1.1), ylim = c(0, 20), main = names(trans_id[animal]), 
       xlab = 'probability of success', ylab = 'density')
  polygon(prior, col = alpha('grey', 0.5))
  post$a_ind[, animal] %>% inv_logit %>% density(bw = 0.02) %>% 
    polygon(col = alpha(colours_animals[trans_id[animal]], 0.8))
  lines(c(1/8, 1/8), c(0, 20), lty = 2, lwd = 3, col = alpha('black', 0.5))
  # Add raw data
  sub = dat_double[dat_double$id == animal,]
  points(sub$response + runif(nrow(sub), 0, 0.1), runif(nrow(sub), 5, 15),
         pch = 16, col = alpha(colours_animals[trans_id[animal]], 0.8))
  pc = sum(sub$response)/nrow(sub)
  lines(c(pc, pc), c(0, 20), lty = 2, lwd = 3, col = alpha(colours_animals[trans_id[animal]], 0.8))
}
dev.off()

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
  ), data = clean_dat, chains = 4, cores = 4, iter = 8000, warmup = 500)

precis(m_delay, depth = 2)

# Plot results
post = extract.samples(m_delay)
pred_perfs = sapply(times, function(time){
  sapply(1:length(post$a_bar), function(i) post$a_bar[i] + post$b[i] * time) %>% mean
})
lines(times, pred_perfs, lty = 2, lwd = 3, )


