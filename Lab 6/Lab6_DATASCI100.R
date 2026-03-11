# ==============================================================================
# Lab 6: Inference for a Single Proportion - R Code
# A Modern Approach with Tidyverse
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
setwd("~/Your/Working/Directory/")

# Read in the data - Base R approach
gardasil <- read.table("gardasil.txt", header = TRUE, 
                       stringsAsFactors = TRUE)

# Tidyverse approach
# gardasil <- read_table("gardasil.txt")

# Explore the data
str(gardasil)
summary(gardasil)
glimpse(gardasil)

# ------------------------------------------------------------------------------
# Exploring Completion Rates
# ------------------------------------------------------------------------------

# View contingency table with frequencies
table(gardasil$Completed)

# Convert frequency table into a proportion table
prop.table(table(gardasil$Completed))

# Using tidyverse
gardasil %>%
  count(Completed) %>%
  mutate(proportion = n / sum(n))

# ------------------------------------------------------------------------------
# One Sample Proportion Test - Manual Input
# ------------------------------------------------------------------------------

# Run one sample z test on a proportion
prop.test(469, 469 + 944, p = 0.4, correct = FALSE)

# Or equivalently
prop.test(469, 1413, p = 0.4, correct = FALSE)

# ------------------------------------------------------------------------------
# Understanding the Test Statistic
# ------------------------------------------------------------------------------

# Calculate z-statistic from chi-squared
sqrt(27.3)

# Use pnorm to calculate probability
pnorm(5.2)  # This gives lower tail (wrong for our purposes)

# Calculate upper tail (Z > 5.2)
pnorm(5.2, lower.tail = FALSE)

# The value above is the same as:
1 - pnorm(5.2)

# OR
pnorm(-5.2)

# Get two-tailed probability
2 * pnorm(-5.2)

# To put it all together and correct for rounding errors:
2 * (1 - pnorm(sqrt(27.29)))

# ------------------------------------------------------------------------------
# Using Table as Input - Factor Level Issues
# ------------------------------------------------------------------------------

# Proportion test of the Gardasil table (INCORRECT - tests "no" instead of "yes")
prop.test(table(gardasil$Completed), p = 0.4, correct = FALSE)

# Fix by reordering factor levels
gardasil$Completed2 <- factor(gardasil$Completed, 
                              levels = c("yes", "no"))

# Proportion test with corrected factor levels (CORRECT)
prop.test(table(gardasil$Completed2), p = 0.4, correct = FALSE)


# ------------------------------------------------------------------------------
# Visualizing the Confidence Interval
# ------------------------------------------------------------------------------

# Extract confidence interval from prop.test
test_result <- prop.test(469, 1413, p = 0.4, correct = FALSE)

# View components
test_result$estimate      # Sample proportion
test_result$conf.int      # Confidence interval
test_result$p.value       # P-value
test_result$statistic     # Chi-squared statistic

# Create data for CI plot
ci_data <- tibble(
  point_estimate = test_result$estimate,
  lower = test_result$conf.int[1],
  upper = test_result$conf.int[2],
  hypothesized = 0.40
)

# Plot the confidence interval
ggplot(ci_data, aes(x = 1, y = point_estimate)) +
  geom_point(size = 3) +
  geom_segment(aes(x = 1, xend = 1, y = lower, yend = upper)) +
  geom_hline(yintercept = 0.40, color = "red") +
  ylim(0.25, 0.45) +
  labs(y = "Proportion")

# ------------------------------------------------------------------------------
# Visualizing the Sampling Distribution
# ------------------------------------------------------------------------------

# Create sampling distribution under H0
p0 <- 0.40
n <- 1413
se <- sqrt(p0 * (1 - p0) / n)

# Generate values for the normal curve
x_values <- seq(0.30, 0.50, length.out = 100)
y_values <- dnorm(x_values, mean = p0, sd = se)

sampling_dist <- tibble(
  proportion = x_values,
  density = y_values
)

# Plot the sampling distribution
ggplot(sampling_dist, aes(x = proportion, y = density)) +
  geom_line() +
  geom_vline(xintercept = 0.332, color = "blue") +
  geom_vline(xintercept = 0.40, color = "red")


# ------------------------------------------------------------------------------
# End of Lab 6 Code
# ------------------------------------------------------------------------------
