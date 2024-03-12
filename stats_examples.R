
library(stats)
numerator <- 10
denominator <- 100

rate <- numerator/denominator

confidence_interval <- binom.test(numerator,denominator)
confidence_interval <- binom.test(numerator, denominator, conf.level = 0.95)$conf.int

confidence_interval


# Example data
data <- c(0.1, 0.25, 0.08, 0.14)  # Replace with your actual data

# Function to compute index of disparity
compute_id <- function(data) {
  # Compute index of disparity (replace this with your own formula)
  id <- sum(data) / length(data)  # Example formula, replace with your own
  return(id)
}

compute_id(data)

# Function to perform bootstrapping
bootstrap <- function(data, num_iter = 1000) {
  ids <- numeric(num_iter)
  for (i in 1:num_iter) {
    # Resample data with replacement
    resampled_data <- sample(data, replace = TRUE)
    # Compute index of disparity for resampled data
    ids[i] <- compute_id(resampled_data)
  }
  return(ids)
}

# Perform bootstrapping
bootstrap_ids <- bootstrap(data)

# Calculate standard error
se <- sd(bootstrap_ids)

# Print standard error
print(se)
