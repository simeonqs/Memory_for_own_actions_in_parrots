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
dev.off()
cat('\014')

# Set working directory to mother-folder
setwd(str_remove(dirname(rstudioapi::getActiveDocumentContext()$path), '/CODE'))

# Load data
dat_orig = read.csv2('DATA/Parrots_repeat.csv')
dat_orig$Animal = dat_orig$Animal %>% str_remove(' ')
delay = read.csv2('DATA/Delay-responses.csv')

# Colours
colours_animals = c(MrHuang = '#B71C1C', Charlie = '#9B59B6', Gargamel = '#004D40', 
                    Nouba = '#FBC02D', Eva = '#303F9F')

# Translate id's
trans_id  = c(MrHuang = 1, Charlie = 2, Gargamel = 3)

# >>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>
# ANALYSIS ----
# >>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>

# Clean data
trans_behaviour = 1:length(unique(delay$b1[delay$b1 != 'repeat']))
names(trans_behaviour) = unique(delay$b1[delay$b1 != 'repeat'])
delay_rep = delay[delay$b2 == 'repeat',]
clean_dat = data.frame(id = trans_id[delay_rep$Animal],
                       beh = trans_behaviour[delay_rep$b1],
                       response = ifelse(delay_rep$correct.or.not.2 == 'c', 1, 0),
                       log_time = log(delay_rep$delay))

# M1: Varying intercept ind and beh ----
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

precis(m_delay, depth = 2) %>% plot

# M2: Varying slope and intercept ind and beh ----
m_delay_2 = ulam(
  alist(
    
    response ~ dbinom(1, p),
    logit(p) <- a_bar + 
      z_id[id] * sigma_id + 
      z_beh[beh] * sigma_beh +
      (b_bar + zb_id[id] * sigma_id_b) * log_time,
    a_bar ~  normal(-1, 1),
    z_id[id] ~ normal(0, 0.5),
    z_beh[beh] ~ normal(0, 0.5),
    b_bar ~ normal(0, 0.5),
    zb_id[id] ~ normal(0, 1),
    sigma_id ~ dexp(1),
    sigma_id_b ~ dexp(1),
    sigma_beh ~ dexp(1),
    gq> vector[id]: a_ind <<- a_bar + z_id * sigma_id,
    gq> vector[beh]: a_beh <<- a_bar + z_beh * sigma_beh
    
  ), data = clean_dat, chains = 4, cores = 4, iter = 4000, warmup = 500)

precis(m_delay_2, depth = 3) %>% plot

# M2a: Varying slope and intercept ind and beh ----
m_delay_2a = ulam(
  alist(
    
    # Main model
    response ~ dbinom(1, p),
    logit(p) <- a_id[id] + 
      z_beh[beh] * sigma_beh +
      b_id[id] * log_time,
    
    # ID
    c(a_id, b_id)[id] ~ multi_normal( c(a_bar, b_bar) , Rho , sigma_id), 
    a_bar ~  normal(0, 1),
    b_bar ~ normal(0, 0.5),
    sigma_id ~ exponential(1),
    Rho ~ lkj_corr(2),
    
    # Behaviour
    z_beh[beh] ~ normal(0, 0.5),
    sigma_beh ~ exponential(2)
    
  ), data = clean_dat, chains = 4, cores = 4, iter = 4000, warmup = 500)

precis(m_delay_2a, depth = 3) %>% plot

# M3: Varying slope and intercept ind and beh - uncentered ----
m_delay_3 = ulam(
  alist(
    
    # Main model
    response ~ dbinom(1, p),
    logit(p) <- a_bar + par_id[id, 1] + par_beh[beh, 1] +
      (b_bar + par_id[id, 2] + par_beh[beh, 2]) * log_time,
    
    # Adaptive priors - non-centered 
    transpars> matrix[id, 2]:par_id <- compose_noncentered( sigma_id , L_Rho_id , z_id ), 
    transpars> matrix[beh, 2]:par_beh <- compose_noncentered( sigma_beh , L_Rho_beh , z_beh ), 
    matrix[2, id]:z_id ~ normal( 0 , 1 ), 
    matrix[2, beh]:z_beh ~ normal( 0 , 1 ),
    
    # Fixed priors
    a_bar ~  normal(-1, 1),
    b_bar ~ normal(-0.5, 0.5),
    vector[2]:sigma_id ~ dexp(1), 
    cholesky_factor_corr[2]:L_Rho_id ~ lkj_corr_cholesky( 2 ), 
    vector[2]:sigma_beh ~ dexp(1), 
    cholesky_factor_corr[2]:L_Rho_beh ~ lkj_corr_cholesky( 2 ),
    
    # Compute ordinary correlation matrixes from Cholesky fids 
    gq> matrix[2, 2]:Rho_id <<- Chol_to_Corr(L_Rho_id),
    gq> matrix[2, 2]:Rho_beh <<- Chol_to_Corr(L_Rho_beh)

    
  ), data = clean_dat, chains = 4, cores = 4, iter = 4000, warmup = 500)

precis(m_delay_3, depth = 3) %>% plot

# >>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>
# PLOTTING ----
# >>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>

# Make simple plot
pdf('RESULTS/delay.pdf')
plot(clean_dat$log_time + runif(nrow(clean_dat), 0, 0.1), 
     clean_dat$response + runif(nrow(clean_dat), 0, 0.1), 
     col = alpha(colours_animals[trans_id[clean_dat$id]], 0.8), pch = 16,
     xlab = 'time [s]', ylab = 'probability correct response', xaxt = 'n', xlim = c(1, 3))
axis(1, c(1, 1.5, 2, 2.5, 3), round(exp(c(1, 1.5, 2, 2.5, 3))))

# Include averages
times = unique(clean_dat$log_time)
perfs = sapply(times, function(x) 
  sum(clean_dat$response[clean_dat$log_time == x])/nrow(clean_dat[clean_dat$log_time == x,]))
points(times, perfs, pch = 3, col = alpha(1, 0.7), cex = 2)

# Include results
post = extract.samples(m_delay)
pred_perfs_means = sapply(times, function(time){
  sapply(1:length(post$a_bar), function(i) post$a_bar[i] + post$b[i] * time) %>% mean %>% inv_logit
})
pred_perfs_PI = sapply(times, function(time){
  sapply(1:length(post$a_bar), function(i) post$a_bar[i] + post$b[i] * time) %>% PI %>% inv_logit
})
lines(times, pred_perfs_means, lty = 2, lwd = 3)
shade(pred_perfs_PI, times, col = alpha('black', 0.2))

# Include chance level
lines(c(1, 3), c(0.25, 0.25), lty = 3, lwd = 3, col = alpha(1, 0.3))
dev.off()

# Plot varying effects
plot(clean_dat$log_time + runif(nrow(clean_dat), 0, 0.1), 
     clean_dat$response + runif(nrow(clean_dat), 0, 0.1), 
     col = alpha(colours_animals[trans_id[clean_dat$id]], 0.8), pch = 16,
     xlab = 'time [s]', ylab = 'probability correct response', xaxt = 'n', xlim = c(1, 3))
axis(1, c(1, 1.5, 2, 2.5, 3), round(exp(c(1, 1.5, 2, 2.5, 3))))
times = unique(clean_dat$log_time)
post = extract.samples(m_delay_3)
pred_perfs_means = sapply(times, function(time){
  sapply(1:length(post$a_bar), function(i){
    with(post, a_bar[i] + (b_bar[i]) * time)
  }) %>% mean %>% inv_logit
})
pred_perfs_PI = sapply(times, function(time){
  sapply(1:length(post$a_bar), function(i){
    with(post, a_bar[i] + (b_bar[i]) * time)
  }) %>% PI %>% inv_logit
})
pred_perfs_id_means = sapply(times, function(time){
  sapply(1:2, function(id){
    sapply(1:length(post$a_bar), function(i){
      with(post, a_bar + par_id[i, id, 1]+
             (b_bar + par_id[i, id, 2]) * time)
    }) %>% mean %>% inv_logit
  })
})
lines(times, pred_perfs_means, lty = 2, lwd = 3)
shade(pred_perfs_PI, times, col = alpha('black', 0.2))
for(i in 1:2) lines(times, pred_perfs_id_means[i,], lty = 2, lwd = 3,
                    col = alpha(colours_animals[trans_id[i]], 0.8))


