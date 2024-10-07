library(tidyverse)
dt <- tribble(
  ~x, ~y, ~w, ~z,
  210, 300, 220, 180,
  102, 100, 119, 187,
  176, 175, 188, 173,
  87, 95, 91, 94,
  202, 210, 234, 218,
  110, 122, 131, 128
)
dt

#1_A
mean_dt <- map_dbl(dt, mean, na.rm = TRUE)
mean_dt


#1_B:
sd_dt <- map_dbl(dt, sd, na.rm = TRUE)
sd_dt


#1_C:
sqrt_dt <- map_df(dt, sqrt)
sqrt_dt



#1_D:
  # Define a function to calculate the desired statistics for each column
  calc_stats <- function(col) {
    c(
      mean = mean(col, na.rm = TRUE),
      max = max(col, na.rm = TRUE),
      q1 = quantile(col, 0.25, na.rm = TRUE),
      q3 = quantile(col, 0.75, na.rm = TRUE),
      median = median(col, na.rm = TRUE),
      min = min(col, na.rm = TRUE)
    )
  }

summary_stats <- sapply(dt, calc_stats)

summary_stats <- as.data.frame(summary_stats)
summary_stats



#Question 2:
exp_means <- function() {
  means <- numeric(10000)  
  for (i in 1:10000) {
    sample <- rexp(5, rate = 1)
    means[i] <- mean(sample)
  }
  return(means)
}

exp_mean_dist <- exp_means()

hist(exp_mean_dist, breaks = 50, main = "Distribution of Means")


#2_a
exp_means_map <- map_dbl(1:10000, ~mean(rexp(5, rate = 1)))

hist(exp_means_map, breaks = 50, main = "Distribution of Means using map")

#2_b
exp_means_replicate <- replicate(10000, mean(rexp(5, rate = 1)))

hist(exp_means_replicate, breaks = 50, main = "Distribution of Means using replicate()")

#2_c
exp_means_for_diff_sizes <- function() {
  sample_sizes <- c(5, 10, 20)
  for (n in sample_sizes) {
    means <- replicate(10000, mean(rexp(n, rate = 1)))
    hist(means, breaks = 50, main = paste("Sample size:", n), xlab = "Mean")
  }
}

exp_means_for_diff_sizes()

#Question 3
data(mtcars)

output <- numeric(ncol(mtcars)) 

for (i in seq_along(mtcars)) {  
  output[[i]] <- sd(mtcars[[i]], na.rm = TRUE)  
}
names(output) <- colnames(mtcars)
output





















