# >>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>
# Project: parrot repeat
# Date started: 18-12-2021
# Date last modified: 01-07-2022
# Author: Simeon Q. Smeele
# Description: Plotting the results for the delays. 
# source('delay.R', chdir = T)
# >>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>

# Load results
source('ANALYSIS/CODE/01_load_clean_data.R')
load(path_model_delay)

# Plot
pdf(path_pdf_delay, 5, 5)
plot(NULL,
     col = alpha('black', 0.2), pch = 16,xlab = 'time [s]', ylab = 'probability correct response', 
     xaxt = 'n', xlim = c(1, 3), ylim = c(-0.1, 1.1))
axis(1, c(1, 1.5, 2, 2.5, 3), round(exp(c(1, 1.5, 2, 2.5, 3))))

# Include averages
times = unique(dat_delay$log_time)
perfs = lapply(trans_id, function(id){
  sapply(times, function(x) 
    sum(dat_delay$response[dat_delay$log_time == x & dat_delay$id == id])/
      nrow(dat_delay[dat_delay$log_time == x & dat_delay$id == id,]))
})
lapply(trans_id, function(id){
  points(times, perfs[[id]], pch = 3, col = alpha(colours_animals[id], 0.5), cex = 2)
  points(times, perfs[[id]], pch = 16, col = alpha(colours_animals[id], 0.5), cex = 2)
})

# Add chance-level
lines(c(min(times), max(times)), rep(0.25, 2), lty = 2, lwd = 3, col = 'grey')

# Include model results
post = extract.samples(m_delay)
pred_perfs_means = sapply(times, function(time){
  sapply(1:length(post$a_bar), function(i){
    with(post, a_bar[i] + (b_bar[i]) * time)
  }) %>% mean %>% inv_logit
})
pred_perfs_PI = sapply(times, function(time){
  sapply(1:length(post$a_bar), function(i){
    with(post, a_bar[i] + b_bar[i] * time)
  }) %>% PI %>% inv_logit
})
shade(pred_perfs_PI, times, col = alpha('black', 0.2))
lines(times, pred_perfs_means, lty = 2, lwd = 3)

dev.off()

# Print results for paper
message('Sigma id')
print(mean(post$sigma_id)) 
print(PI(post$sigma_id))

message('Sigma behaviour')
print(mean(post$sigma_beh)) 
print(PI(post$sigma_beh))

message('10 sec performance')
print(inv_logit(mean(with(post, a_bar + b_bar * log(10)))))
print(inv_logit(PI(with(post, a_bar + b_bar * log(10)))))

