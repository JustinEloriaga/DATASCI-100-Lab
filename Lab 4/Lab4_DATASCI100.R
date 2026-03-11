# ==============================================================================
# Lab 4: Normal & Binomial Distributions - R Code
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

# Check working directory
getwd()

# Set working directory if needed (change to your folder)
# setwd("~/Your/Working/Directory/")

# Import data using tidyverse
fruitfly <- read_csv("fruitfly.csv")

# Base R alternative
# fruitfly <- read.csv("fruitfly.csv")

# ------------------------------------------------------------------------------
# Exploring the Data - Summary Statistics
# ------------------------------------------------------------------------------

# Base R approach
summary(fruitfly$lifespan)

# View the data
glimpse(fruitfly)

# ------------------------------------------------------------------------------
# Visualizing the Distribution
# ------------------------------------------------------------------------------

# ggplot2 histogram
ggplot(fruitfly, aes(x = lifespan)) +
  geom_histogram()

# Specify number of bins
ggplot(fruitfly, aes(x = lifespan)) +
  geom_histogram(bins = 15)

# ------------------------------------------------------------------------------
# Normal Distribution - Lower Tail Probabilities
# ------------------------------------------------------------------------------

# Get help on pnorm
?pnorm

# Calculate probability that lifespan <= 50 days
pnorm(q = 50, mean = 57.4, sd = 17.6)

# Compare to empirical proportion in our data
# Base R approach
sum(fruitfly$lifespan <= 50) / length(fruitfly$lifespan)

# Tidyverse approach
fruitfly %>%
  summarize(prop_under_50 = mean(lifespan <= 50))

# ------------------------------------------------------------------------------
# Normal Distribution - Upper Tail Probabilities
# ------------------------------------------------------------------------------

# Probability that lifespan > 50 days - Method 1
1 - pnorm(q = 50, mean = 57.4, sd = 17.6)

# Method 2 - using lower.tail argument
pnorm(q = 50, mean = 57.4, sd = 17.6, lower.tail = FALSE)

# ------------------------------------------------------------------------------
# Normal Distribution - Interval Probabilities
# ------------------------------------------------------------------------------

# Probability that lifespan is between 50 and 70 days
# Method 1: Subtract two probabilities
pnorm(q = 70, mean = 57.4, sd = 17.6) - pnorm(q = 50, mean = 57.4, sd = 17.6)

# Method 2: Use diff function
diff(pnorm(q = c(50, 70), mean = 57.4, sd = 17.6))

# ------------------------------------------------------------------------------
# Normal Distribution - Finding Percentiles
# ------------------------------------------------------------------------------

# Find the 90th percentile
qnorm(p = 0.90, mean = 57.4, sd = 17.6)

# Verify that this is indeed the 90th percentile
pnorm(q = 79.96, mean = 57.4, sd = 17.6)

# Find other percentiles
qnorm(p = 0.25, mean = 57.4, sd = 17.6)  # 1st quartile
qnorm(p = 0.50, mean = 57.4, sd = 17.6)  # Median
qnorm(p = 0.75, mean = 57.4, sd = 17.6)  # 3rd quartile

# ------------------------------------------------------------------------------
# Visualizing the Normal Distribution
# ------------------------------------------------------------------------------

# Create data for the normal curve
x_values <- seq(10, 100, length.out = 100)
normal_data <- tibble(
  x = x_values,
  density = dnorm(x_values, mean = 57.4, sd = 17.6)
)

# Plot histogram with normal curve overlay
ggplot(fruitfly, aes(x = lifespan)) +
  geom_histogram(aes(y = after_stat(density)), bins = 15) +
  geom_line(data = normal_data, aes(x = x, y = density))

# ------------------------------------------------------------------------------
# Binomial Distribution - Single Probability
# ------------------------------------------------------------------------------

# Get help on dbinom
?dbinom

# Calculate probability of exactly 5 successes out of 8 trials
dbinom(x = 5, size = 8, prob = 0.663)

# Calculate probabilities for all possible outcomes (0 to 8)
dbinom(x = 0:8, size = 8, prob = 0.663)

# ------------------------------------------------------------------------------
# Creating a Probability Distribution Table
# ------------------------------------------------------------------------------

# Tidyverse approach
Fruitfly8 <- tibble(
  x = 0:8,
  prob = dbinom(x = 0:8, size = 8, prob = 0.663)
)

# View the tibble
Fruitfly8

# ------------------------------------------------------------------------------
# Visualizing the Binomial Distribution
# ------------------------------------------------------------------------------

# ggplot2 barplot
ggplot(Fruitfly8, aes(x = x, y = prob)) +
  geom_bar(stat = "identity")

# ------------------------------------------------------------------------------
# Binomial Distribution - Cumulative Probabilities
# ------------------------------------------------------------------------------

# Probability that X <= 5 (using dbinom with sum)
sum(dbinom(x = 0:5, size = 8, prob = 0.663))

# Using the tibble
sum(Fruitfly8$prob[Fruitfly8$x <= 5])

# Probability that X >= 3
sum(dbinom(x = 3:8, size = 8, prob = 0.663))

# Using base R subsetting
sum(Fruitfly8$prob[Fruitfly8$x >= 3])

# ------------------------------------------------------------------------------
# Using pbinom for Cumulative Probabilities
# ------------------------------------------------------------------------------

# Probability that X <= 5
pbinom(q = 5, size = 8, prob = 0.663)

# Probability that X >= 3
# Method 1: 1 - P(X <= 2)
1 - pbinom(q = 2, size = 8, prob = 0.663)

# Method 2: using lower.tail argument
pbinom(q = 2, size = 8, prob = 0.663, lower.tail = FALSE)


# ------------------------------------------------------------------------------
# End of Lab 4 Code
# ------------------------------------------------------------------------------
