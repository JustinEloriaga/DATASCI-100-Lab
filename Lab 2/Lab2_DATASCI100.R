# ==============================================================================
# Lab 2: Summarizing and Visualizing Data - R Code
# ==============================================================================

# ------------------------------------------------------------------------------
# Setup and Package Loading
# ------------------------------------------------------------------------------

# Load tidyverse
library(tidyverse)

# ------------------------------------------------------------------------------
# Importing the Data
# ------------------------------------------------------------------------------

# Set working directory (change to your folder)
#setwd("~/Desktop/DATASCI_100/Lab_Datasets/")

# Import data using tidyverse
babies <- read_table("babies.txt")

# Alternative: Base R approach
# babies <- read.table("babies.txt", header = TRUE)

# ------------------------------------------------------------------------------
# Exploring Data Structure
# ------------------------------------------------------------------------------

# View first few rows
head(babies)

# Get a glimpse of the data structure
glimpse(babies)

# View the data
View(babies)

# Get dimensions
dim(babies)

# Check structure
str(babies)

# Basic summary
summary(babies)

# ------------------------------------------------------------------------------
# Creating Factor Variables
# ------------------------------------------------------------------------------

# Tidyverse approach - create both factor variables at once
babies <- babies %>%
  mutate(
    parityf = factor(parity, labels = c("first born", "otherwise")),
    smokef = factor(smoke, labels = c("not now", "yes now"))
  )

# Base R approach
# babies$parityf <- factor(babies$parity, labels = c("first born", "otherwise"))
# babies$smokef <- factor(babies$smoke, labels = c("not now", "yes now"))

# Verify the changes
glimpse(babies)
summary(babies)

# ------------------------------------------------------------------------------
# Summarizing Numeric Variables
# ------------------------------------------------------------------------------

# Single variable summaries
summary(babies$bwt)
mean(babies$gestation)
mean(babies$bwt, na.rm = TRUE)
sd(babies$bwt, na.rm = TRUE)
median(babies$bwt, na.rm = TRUE)
IQR(babies$bwt, na.rm = TRUE)
min(babies$bwt, na.rm = TRUE)
max(babies$bwt, na.rm = TRUE)

# Multiple statistics with tidyverse
babies %>%
  summarize(
    mean_bwt = mean(bwt, na.rm = TRUE),
    sd_bwt = sd(bwt, na.rm = TRUE),
    median_bwt = median(bwt, na.rm = TRUE),
    iqr_bwt = IQR(bwt, na.rm = TRUE),
    min_bwt = min(bwt, na.rm = TRUE),
    max_bwt = max(bwt, na.rm = TRUE),
    n = n()
  )

# ------------------------------------------------------------------------------
# Grouped Summaries
# ------------------------------------------------------------------------------

# Base R approach using tapply()
tapply(X = babies$bwt, INDEX = babies$smokef, FUN = mean, na.rm = TRUE)
tapply(X = babies$bwt, INDEX = babies$smokef, FUN = sd, na.rm = TRUE)

babies %>%
  group_by(smokef) %>%
  summarize(
    mean_bwt = mean(bwt, na.rm = TRUE),
    sd_bwt = sd(bwt, na.rm = TRUE)
  )
# ------------------------------------------------------------------------------
# Base R Visualizations - Numeric Variables
# ------------------------------------------------------------------------------

# Histogram
hist(babies$bwt)

# Boxplot
boxplot(babies$bwt)

# Side-by-side boxplots
boxplot(babies$bwt ~ babies$smokef)

# Scatterplot
plot(babies$gestation, babies$bwt)

# ------------------------------------------------------------------------------
# ggplot2 Visualizations - Histograms
# ------------------------------------------------------------------------------

# Basic histogram
babies %>%
  ggplot(aes(x = bwt)) +
  geom_histogram(bins = 30) +
  labs(
    title = "Distribution of Baby Birth Weight",
    x = "Birth Weight (ounces)",
    y = "Count"
  )

# Histogram with mean line
babies %>%
  ggplot(aes(x = bwt)) +
  geom_histogram(bins = 30, fill = "steelblue", color = "white") +
  geom_vline(aes(xintercept = mean(bwt, na.rm = TRUE)), 
             color = "red", linetype = "dashed", size = 1) +
  labs(
    title = "Distribution of Baby Birth Weight",
    subtitle = "Red line indicates mean weight",
    x = "Birth Weight (ounces)",
    y = "Count"
  ) +
  theme_minimal()

# ------------------------------------------------------------------------------
# ggplot2 Visualizations - Density Plots
# ------------------------------------------------------------------------------

# Basic density plot
babies %>%
  ggplot(aes(x = bwt)) +
  geom_density() +
  labs(
    title = "Density Plot of Baby Birth Weight",
    x = "Birth Weight (ounces)",
    y = "Density"
  )

# Overlapping density plots
babies %>%
  ggplot(aes(x = bwt, fill = smokef)) +
  geom_density() +
  labs(
    title = "Birth Weight Distribution by Smoking Status",
    x = "Birth Weight (ounces)",
    y = "Density",
    fill = "Smoking Status"
  )

# ------------------------------------------------------------------------------
# ggplot2 Visualizations - Boxplots
# ------------------------------------------------------------------------------

# Single boxplot
babies %>%
  ggplot(aes(y = bwt)) +
  geom_boxplot() +
  labs(
    title = "Distribution of Baby Birth Weight",
    y = "Birth Weight (ounces)"
  ) 

# Side-by-side boxplots
babies %>%
  ggplot(aes(x = smokef, y = bwt, fill = smokef)) +
  geom_boxplot() +
  labs(
    title = "Birth Weight by Smoking Status",
    x = "Smoking Status",
    y = "Birth Weight (ounces)"
  )

# Boxplots by two factors
babies %>%
  ggplot(aes(x = parityf, y = bwt, fill = smokef)) +
  geom_boxplot() +
  labs(
    title = "Birth Weight by Parity and Smoking Status",
    x = "Parity",
    y = "Birth Weight (ounces)",
    fill = "Smoking Status"
  )

# ------------------------------------------------------------------------------
# Summarizing Categorical Variables
# ------------------------------------------------------------------------------

# Base R approach
table(babies$smokef)

# Store the table
smk.tab <- table(babies$smokef)

# Add margins
addmargins(smk.tab)

# Calculate proportions
prop.table(smk.tab)

# Tidyverse approach - count
babies %>%
  count(smokef)

# Count with proportions
babies %>%
  count(smokef) %>%
  mutate(proportion = n / sum(n))

# Detailed summary with percentages
babies %>%
  count(smokef) %>%
  mutate(
    proportion = n / sum(n),
    percentage = proportion * 100
  )

# ------------------------------------------------------------------------------
# Two-Way Contingency Tables
# ------------------------------------------------------------------------------

# Base R approach
table(babies$smokef, babies$parityf)

# Store the table
smk.par.tab <- table(babies$smokef, babies$parityf)

# Add margins
addmargins(smk.par.tab)

# Overall proportions
prop.table(smk.par.tab)

# Row proportions
prop.table(smk.par.tab, margin = 1)

# Column proportions
prop.table(smk.par.tab, margin = 2)

# Tidyverse approach
babies %>%
  count(smokef, parityf)

# Add overall proportions
babies %>%
  count(smokef, parityf) %>%
  mutate(proportion = n / sum(n))

# Group-wise proportions (by smoking status - row proportions)
babies %>%
  group_by(smokef) %>%
  count(parityf) %>%
  mutate(proportion = n / sum(n))

# Group-wise proportions (by parity - column proportions)
babies %>%
  group_by(parityf) %>%
  count(smokef) %>%
  mutate(proportion = n / sum(n))

# Create wider format table
babies %>%
  count(smokef, parityf) %>%
  pivot_wider(names_from = parityf, values_from = n)

# ------------------------------------------------------------------------------
# ggplot2 Bar Charts
# ------------------------------------------------------------------------------

# Single variable bar chart
babies %>%
  ggplot(aes(x = smokef, fill = smokef)) +
  geom_bar() +
  labs(
    title = "Distribution of Smoking Status",
    x = "Smoking Status",
    y = "Count"
  )

# Grouped bar chart (side-by-side)
babies %>%
  ggplot(aes(x = parityf, fill = smokef)) +
  geom_bar(position = "dodge") +
  labs(
    title = "Smoking Status by Parity",
    x = "Parity",
    y = "Count",
    fill = "Smoking Status"
  )

# Stacked bar chart with counts
babies %>%
  ggplot(aes(x = parityf, fill = smokef)) +
  geom_bar(position = "stack") +
  labs(
    title = "Smoking Status by Parity (Stacked)",
    x = "Parity",
    y = "Count",
    fill = "Smoking Status"
  )

# Stacked proportional bar chart
babies %>%
  count(parityf, smokef) %>%
  group_by(parityf) %>%
  mutate(proportion = n / sum(n)) %>%
  ggplot(aes(x = parityf, y = proportion, fill = smokef)) +
  geom_col() +
  labs(
    title = "Proportion of Smoking Status by Parity",
    x = "Parity",
    y = "Proportion",
    fill = "Smoking Status"
  )

# ------------------------------------------------------------------------------
# End of Lab 2 Code
# ------------------------------------------------------------------------------
