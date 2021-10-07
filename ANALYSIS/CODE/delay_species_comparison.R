# >>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>
# Project: parrot repeat
# Date started: 02-05-2021
# Date last modified: 16-07-2021
# Author: Simeon Q. Smeele
# Description: Analysis of the delay data, comparing species. 
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

# Translations
trans_id  = c(MrHuang = 1, Charlie = 2, Gargamel = 3, # parrots
              Cassandra = 4, Mara = 5, Nano = 6, Sayula = 7, # sea lions
              Giotto = 8, Naja = 9, # harbour seals
              Nino = 10) # grey seal
trans_beh = c(fluff = 1, wings = 2, spin = 3, leg = 4, # parrots
              wave = 5, giro = 6, growl = 7, # sea lions
              growl_hs = 8, wave_hs = 9, shy_hs = 10, chin_hs = 11, snort_hs = 12, # harbour seals
              wave_gs = 13, shy_gs = 14, snort_gs = 15) # grey seal

# Load data parrots
parrot_data = read.csv2('DATA/parrots/Delay-responses.csv')
delay_rep = parrot_data[parrot_data$b2 == 'repeat',]
clean_dat = data.frame(id = trans_id[delay_rep$Animal],
                       beh = trans_beh[delay_rep$b1],
                       response = ifelse(delay_rep$correct.or.not.2 == 'c', 1, 0),
                       log_time = log(delay_rep$delay + 0.5), # adding 0.5 for handling time
                       species = 1) # 1 is parrots

# Load data sea lions
files = list.files('DATA/sea lions', '*csv', full.names = T)
sea_lion_data = files %>% lapply(function(file){
  dat = read.csv2(file)
  dat$animal = file %>% str_remove('DATA/sea lions/Delay-responses-') %>% strsplit('-') %>% sapply(`[`,1)
  return(dat)
}) %>% bind_rows
delay_rep = sea_lion_data[sea_lion_data$b2 == 'repeat',]
clean_dat = rbind(clean_dat, 
                  data.frame(id = trans_id[delay_rep$animal],
                             beh = trans_beh[delay_rep$b1],
                             response = ifelse(delay_rep$correct.or.not.2 == 'c', 1, 0),
                             log_time = log(delay_rep$delay + 0.5),
                             species = 2)) # 2 sea lions

# Load data harbour seals
files = list.files('DATA/harbour seals', '*csv', full.names = T)
harbour_seal_data = files %>% lapply(function(file){
  dat = read.csv2(file)
  dat$animal = file %>% str_remove('DATA/harbour seals/Delay responses ') %>% strsplit(' ') %>% sapply(`[`,1)
  return(dat)
}) %>% bind_rows
delay_rep = harbour_seal_data[harbour_seal_data$b2 == 'repeat',]
delay_rep$b1 = paste0(delay_rep$b1 , '_', 'hs') # to make sure behaviours are not the same as sea lions
clean_dat = rbind(clean_dat, 
                  data.frame(id = trans_id[delay_rep$animal],
                             beh = trans_beh[delay_rep$b1],
                             response = ifelse(delay_rep$correct.or.not.2 == 'c', 1, 0),
                             log_time = log(delay_rep$delay + 0.5),
                             species = 3)) # 3 harbour seals

# Load data grey seal
files = list.files('DATA/grey seal', '*csv', full.names = T)
grey_seal_data = files %>% lapply(function(file){
  dat = read.csv2(file)
  dat$animal = file %>% str_remove('DATA/grey seal/Delay responses ') %>% strsplit(' ') %>% sapply(`[`,1)
  return(dat)
}) %>% bind_rows
delay_rep = grey_seal_data[grey_seal_data$b2 == 'repeat',]
delay_rep$b1 = paste0(delay_rep$b1 , '_', 'gs') # to make sure behaviours are not the same as others
clean_dat = rbind(clean_dat, 
                  data.frame(id = trans_id[delay_rep$animal],
                             beh = trans_beh[delay_rep$b1],
                             response = ifelse(delay_rep$correct.or.not.2 == 'c', 1, 0),
                             log_time = log(delay_rep$delay + 0.5),
                             species = 4)) # 4 grey seal

# Load previous model results
for(file in list.files('RESULTS', '*RData', full.names = T)) load(file)

# >>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>
# ANALYSIS ----
# >>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>

# M1: Varying all - no covariance - uncentered ----
m_delay_species_1 = ulam(
  alist(
    
    response ~ dbinom(1, p),
    logit(p) <- 
      (a_bar + 
         z_sp[species] * sigma_sp +
         z_id[id] * sigma_id + 
         z_beh[beh] * sigma_beh) +
      (b_bar + 
         zb_sp[species] * sigma_sp_b +
         zb_id[id] * sigma_id_b +
         zb_beh[beh] * sigma_beh_b) * log_time,
    a_bar ~  normal(-1, 2),
    z_sp[species] ~ normal(0, 1),
    z_id[id] ~ normal(0, 1),
    z_beh[beh] ~ normal(0, 1),
    b_bar ~ normal(0, 1),
    zb_sp[species] ~ normal(0, 1),
    zb_id[id] ~ normal(0, 1),
    zb_beh[beh] ~ normal(0, 1),
    sigma_sp ~ dexp(2),
    sigma_sp_b ~ dexp(2),
    sigma_id ~ dexp(2),
    sigma_id_b ~ dexp(2),
    sigma_beh ~ dexp(2),
    sigma_beh_b ~ dexp(2),
    gq> vector[species]: a_sp <<- a_bar + z_sp * sigma_sp,
    gq> vector[id]: a_ind <<- a_bar + z_id * sigma_id,
    gq> vector[beh]: a_beh <<- a_bar + z_beh * sigma_beh
    
  ), data = clean_dat, chains = 4, cores = 4, iter = 4000, warmup = 500)

precis(m_delay_species_1, depth = 3) %>% plot
save(m_delay_species_1, file = 'RESULTS/m_delay_species_1.RData')

# >>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>
# PLOTTING ----
# >>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>

# M1 ----
load('RESULTS/m_delay_species_1.RData')
post = extract.samples(m_delay_species_1)

# Intercept
sp_1 = density((post$a_sp[,1]))
sp_2 = density((post$a_sp[,2]))
sp_3 = density((post$a_sp[,3]))
sp_4 = density((post$a_sp[,4]))
plot(sp_1, main = 'nope no species differences')
polygon(sp_1, col = alpha('grey', 0.5))
polygon(sp_2, col = alpha('blue', 0.5))
polygon(sp_3, col = alpha('red', 0.5))
polygon(sp_4, col = alpha('green', 0.5))

# Raw averages
plot_dat = clean_dat
plot_dat$time_rounded = round(plot_dat$log_time)
sum_dat = plot_dat %>% group_by(time_rounded, species) %>% summarise(mean_perf = mean(response))
plot(sum_dat$time_rounded, sum_dat$mean_perf, 
     pch = 16, cex = 2, col = alpha(sum_dat$species, 0.5),
     ylim = c(0, 1), xaxt = 'n',
     xlab = 'time [s]', ylab = 'performance')
axis(1, at = -1:3, labels = round(exp(-1:3)))

# Lines
times = round(sort(unique(clean_dat$log_time)))
pred_perfs_means = lapply(1:max(clean_dat$species), function(species){
  sapply(times, function(time){
    sapply(1:length(post$a_bar), function(i){
      with(post, a_sp[i, species] + (b_bar[i] + zb_sp[i, species] * sigma_sp_b[i]) * time)
    }) %>% mean %>% inv_logit
  })
})
for(species in 1:max(clean_dat$species)) lines(times, pred_perfs_means[[species]], col = alpha(species, 0.5),
                                               lwd = 3, lty = 2)

# Add legend
text(x = rep(-1, max(clean_dat$species)),
     y = seq(0.1, 0.4, length.out = max(clean_dat$species)),
     labels = c('parrots', 'sea lions', 'harbour seals', 'grey seal'),
     col = alpha(1:max(clean_dat$species), 0.8),
     adj = 0)

