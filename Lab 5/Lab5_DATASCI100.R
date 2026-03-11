# ==============================================================================
# Lab 5: Sampling Distributions - R Code
# ==============================================================================

# ------------------------------------------------------------------------------
# Setup and Package Loading
# ------------------------------------------------------------------------------

# Install tidyverse if needed (uncomment to run)
# install.packages("tidyverse")

# Load tidyverse
library(tidyverse)

# ------------------------------------------------------------------------------
# Importing the Data
# ------------------------------------------------------------------------------

# Set working directory (change to your folder)
# setwd("~/Your/Working/Directory/")

# Import data using tidyverse
yrbss <- read_csv("yrbss2013.csv")

# Base R alternative
# yrbss <- read.csv("yrbss2013.csv", header = TRUE)

# Examine the data
glimpse(yrbss)
summary(yrbss)

# ------------------------------------------------------------------------------
# Exploring the Population
# ------------------------------------------------------------------------------

# Create a factor variable
bullied <- factor(yrbss$bullied)

# Frequency table
table(bullied)

# Proportion table (TRUE POPULATION PROPORTIONS)
prop.table(table(bullied))

# Base R bar plot
barplot(prop.table(table(bullied)), beside = TRUE)

# ggplot2 bar plot
bullied_df <- tibble(bullied = bullied)

ggplot(bullied_df, aes(x = bullied)) +
  geom_bar()

# ------------------------------------------------------------------------------
# Taking a Single Sample
# ------------------------------------------------------------------------------

# Take a simple random sample of size 10
samp1 <- sample(x = bullied, size = 10)
samp1

# Calculate sample proportion
prop.table(table(samp1))

# Alternative: calculate proportion "yes" manually
sum(samp1 == "yes") / length(samp1)

# ------------------------------------------------------------------------------
# Effect of Sample Size
# ------------------------------------------------------------------------------

# Take a larger sample of size 200
samp2 <- sample(x = bullied, size = 200)
prop.table(table(samp2))

# Take another sample of size 200
samp3 <- sample(x = bullied, size = 200)
prop.table(table(samp3))

# Compare - they're different but both close to truth!

# ------------------------------------------------------------------------------
# Understanding For Loops - Step by Step
# ------------------------------------------------------------------------------

# Example 1: The simplest loop
for(i in 1:5) {
  print(i)
}

# Example 2: Using the loop counter
for(i in 1:5) {
  print(i * 10)
}

# Example 3: Storing results in a vector
results <- rep(NA, 5)

for(i in 1:5) {
  results[i] <- i * 10
}

results

# Example 4: Sampling in a loop
sample_props <- rep(NA, 5)

for(i in 1:5) {
  # Take a sample of size 10
  samp <- sample(bullied, size = 10)
  
  # Calculate proportion who said "yes" (bullied)
  prop_yes <- sum(samp == "yes") / length(samp)
  
  # Store the proportion
  sample_props[i] <- prop_yes
  
  # Print what we're doing (optional, for learning)
  print(paste("Sample", i, "proportion bullied:", prop_yes))
}

sample_props

# ------------------------------------------------------------------------------
# Full Sampling Distribution Simulation
# ------------------------------------------------------------------------------

sample_prop10 <- matrix(rep(NA, 500), nrow= 500, ncol = 2)
for(i in 1:500){
  samp <- sample(bullied, 10)
  sample_prop10[i,] <- prop.table(table(samp))
}

# Examine the results
head(sample_prop10)

# Calculate average proportions across all 500 samples
colMeans(sample_prop10)

# Create bar plot of average proportions
barplot(colMeans(sample_prop10), 
        names.arg = c('no', 'yes'), 
        ylim = c(0, 1),
        main = "Average Proportions Across 500 Samples",
        ylab = "Proportion")

# ------------------------------------------------------------------------------
# Visualizing the Sampling Distribution
# ------------------------------------------------------------------------------

sampling_dist <- tibble(
  sample_num = 1:500,
  prop_no = sample_prop10[, 1],
  prop_yes = sample_prop10[, 2]
)

ggplot(sampling_dist, aes(x = prop_yes)) +
  geom_histogram(bins = 10) +
  geom_vline(xintercept = 0.1937)

# ------------------------------------------------------------------------------
# Comparing Different Sample Sizes
# ------------------------------------------------------------------------------

# Function to generate sampling distribution for a given sample size
generate_sampling_dist <- function(sample_size, num_samples = 500) {
  props <- rep(NA, num_samples)
  for(i in 1:num_samples) {
    samp <- sample(bullied, size = sample_size)
    props[i] <- sum(samp == "yes") / length(samp)
  }
  return(props)
}

# Generate for three different sample sizes
props_n10 <- generate_sampling_dist(10)
props_n50 <- generate_sampling_dist(50)
props_n200 <- generate_sampling_dist(200)

# Combine into a data frame for plotting
all_props <- tibble(
  proportion = c(props_n10, props_n50, props_n200),
  sample_size = rep(c("n = 10", "n = 50", "n = 200"), 
                    each = 500)
)

# Create comparison histogram
ggplot(all_props, aes(x = proportion)) +
  geom_histogram(bins = 20) +
  geom_vline(xintercept = 0.19) +
  facet_wrap(~sample_size, ncol = 1)

# Calculate standard deviations (standard errors) for each
tibble(
  sample_size = c(10, 50, 200),
  std_error = c(sd(props_n10), sd(props_n50), sd(props_n200))
)

# ------------------------------------------------------------------------------
# Exercise Solutions (Examples)
# ------------------------------------------------------------------------------


# Easy Exercise 3: Store 10 sample proportions
sample_props_10 <- rep(NA, 10)

for(i in 1:10) {
  samp <- sample(bullied, size = 20)
  sample_props_10[i] <- sum(samp == "yes") / length(samp)
}

sample_props_10


# Easy Exercise 6: Compare different sample sizes
samp_10 <- sample(bullied, size = 10)
samp_100 <- sample(bullied, size = 100)
samp_1000 <- sample(bullied, size = 1000)

sum(samp_10 == "yes") / 10
sum(samp_100 == "yes") / 100
sum(samp_1000 == "yes") / 1000

# Largest sample usually closest to 0.19

# ------------------------------------------------------------------------------
# End of Lab 5 Code
# ------------------------------------------------------------------------------
