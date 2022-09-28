# >>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>
# Project: parrot repeat
# Date started: 17-12-2021
# Date last modified: 01-07-2022
# Author: Simeon Q. Smeele
# Description: This script loads and cleans the data.
# source('ANALYSIS/CODE/01_load_clean_data.R')
# >>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>

# >>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>
# DATA LOADING ----
# >>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>

# Loading libraries
libraries = c('data.table', 'tidyverse', 'readxl', 'rethinking')
for(lib in libraries){
  if(! lib %in% installed.packages()) lapply(lib, install.packages)
  lapply(libraries, require, character.only = TRUE)
}

# Paths
path_functions = 'ANALYSIS/CODE/functions'
path_data = 'ANALYSIS/DATA/parrots/Parrots_repeat.csv'
path_data_delay = 'ANALYSIS/DATA/parrots/Delay-responses.csv'
path_model_single = 'ANALYSIS/RESULTS/m_single.RData'
path_pdf_single = 'ANALYSIS/RESULTS/single - densities.pdf'
path_model_double = 'ANALYSIS/RESULTS/m_double.RData'
path_pdf_double = 'ANALYSIS/RESULTS/double - densities.pdf'
path_model_delay = 'ANALYSIS/RESULTS/m_delay.RData'
path_pdf_delay = 'ANALYSIS/RESULTS/delay - scatter.pdf'

# Load functions
.functions = sapply(list.files(path_functions, pattern = '*.R', full.names = T), source)

# Colours
colours_animals = c(MrHuang = '#44AA99',  # green
                    Charlie = '#DDCC77',  # yellow
                    Gargamel = '#CC6677') # red

# Translate id's
trans_id  = c(MrHuang = 1, Charlie = 2, Gargamel = 3)

# Load data
dat_orig = read.csv2(path_data)
dat_orig$Animal = dat_orig$Animal %>% str_remove(' ')
delay = read.csv2(path_data_delay)

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

# Clean delay data
delay_rep = delay[delay$b2 == 'repeat',]
dat_delay = data.frame(id = trans_id[delay_rep$Animal],
                       beh = trans_behaviour[delay_rep$b1],
                       response = ifelse(delay_rep$correct.or.not.2 == 'c', 1, 0),
                       log_time = log(delay_rep$delay))
dat_delay$log_time = dat_delay$log_time

message('Loaded and cleaned data.')