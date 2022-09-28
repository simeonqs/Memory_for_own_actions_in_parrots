# >>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>
# Project: parrot repeat
# Date started: 17-12-2021
# Date last modified: 15-06-2022
# Author: Simeon Q. Smeele
# Description: Plotting results double repeat. 
# >>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>

# Load model
load(path_model_double)

# Plotting results
post = extract.samples(m_double)
pdf(path_pdf_double, 6, 6)
par(mfrow = c(2, 2))
# Overall
prior = rnorm(1e6, -1, 1) %>% inv_logit %>% density
plot(prior, xlim = c(-0.1, 1.1), ylim = c(0, 20), main = 'overall', 
     xlab = 'probability of success', ylab = 'density')
polygon(prior, col = alpha('grey', 0.5))
post$a_bar %>% inv_logit %>% density(bw = 0.02) %>% polygon(col = alpha('black', 0.8))
lines(c(1/16, 1/16), c(0, 20), lty = 2, lwd = 3, col = alpha('black', 0.5))
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
  lines(c(1/16, 1/16), c(0, 20), lty = 2, lwd = 3, col = alpha('black', 0.5))
  # Add raw data
  sub = dat_double[dat_double$id == animal,]
  points(sub$response + runif(nrow(sub), 0, 0.1), runif(nrow(sub), 5, 15),
         pch = 16, col = alpha(colours_animals[trans_id[animal]], 0.8))
  pc = sum(sub$response)/nrow(sub)
  lines(c(pc, pc), c(0, 20), lty = 2, lwd = 3, col = alpha(colours_animals[trans_id[animal]], 0.8))
}
dev.off()

# Print results for paper
message('Sigma id')
print(mean(post$sigma_id)) 
print(PI(post$sigma_id))

message('Sigma behaviour')
print(mean(post$sigma_beh)) 
print(PI(post$sigma_beh))
