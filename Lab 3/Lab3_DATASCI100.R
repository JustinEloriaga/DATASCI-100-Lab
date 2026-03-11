# ==============================================================================
# Lab 3: Data Cleaning and Manipulation - R Code
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
# use getwd() to see what your current working directory is
#setwd("~/Your/Working/Directory/")

# Read in data using tidyverse
acs <- read_csv("acs.csv")

# Base R approach
# acs <- read.csv("acs.csv", header = TRUE)

# ------------------------------------------------------------------------------
# Exploring the Data
# ------------------------------------------------------------------------------

# Examine structure of ACS using tidyverse
glimpse(acs)

# Base R approach
str(acs)

# Examine ACS variables
summary(acs)

# ------------------------------------------------------------------------------
# Examining Age Distribution
# ------------------------------------------------------------------------------

# Table summarizing age
table(acs$Age)

# Using tidyverse count
acs %>% count(Age)


# Histogram using ggplot2
ggplot(acs, aes(x = Age)) +
  geom_histogram()

# ------------------------------------------------------------------------------
# Identifying Problematic Values
# ------------------------------------------------------------------------------

# Identify implausible observations in age
acs$Age == 130
acs$Age > 100

# View position 157 in age
acs$Age[157]

# Print entry in 157th row and second column
acs[157, 2]

# Print all columns in 157th row
acs[157, ]

# Print all columns where age is greater than 100
acs[acs$Age > 100, ]

# ------------------------------------------------------------------------------
# Re-coding to Missing - Base R Approach
# ------------------------------------------------------------------------------

# Create new variable which will be the new cleaned version of age
acs$Age2 <- acs$Age

# Re-code 157th entry of Age2 to NA
acs$Age2[157] <- NA

# More efficient: re-code all entries of Age2 where Age is greater than 100
acs$Age2[acs$Age > 100] <- NA

# Verify re-coding of Age2
summary(acs$Age2)

# ------------------------------------------------------------------------------
# Handling Missing Values in Calculations
# ------------------------------------------------------------------------------

# This returns NA because of missing value
acs %>%
  summarize(mean_age = mean(Age2))

# Calculate mean with tidyverse
acs %>%
  summarize(mean_age = mean(Age2, na.rm = TRUE))

# ------------------------------------------------------------------------------
# Numerical to Categorical - Base R Approach
# ------------------------------------------------------------------------------

# Create new variable for age category
acs$AgeCategory <- factor(NA, levels = c("child", "adult", "senior citizen"))

# Assign values of age category
acs$AgeCategory[acs$Age2 <= 18] <- "child"
acs$AgeCategory[acs$Age2 > 18 & acs$Age2 <= 55] <- "adult"
acs$AgeCategory[acs$Age2 > 55] <- "senior citizen"

# Check coding of age category
table(acs$Age2, acs$AgeCategory)

# ------------------------------------------------------------------------------
# Numerical to Categorical - Tidyverse Approach
# ------------------------------------------------------------------------------

# Create and factor in one step
acs <- acs %>%
  mutate(
    AgeCategory = factor(
      case_when(
        Age2 <= 18 ~ "child",
        Age2 > 18 & Age2 <= 55 ~ "adult",
        Age2 > 55 ~ "senior citizen"
      ),
      levels = c("child", "adult", "senior citizen")
    )
  )

# ------------------------------------------------------------------------------
# Verifying Age Category Coding
# ------------------------------------------------------------------------------


# Check coding with tidyverse
acs %>%
  count(Age2, AgeCategory)

# Get summary statistics by category
acs %>%
  group_by(AgeCategory) %>%
  summarize(
    min_age = min(Age2, na.rm = TRUE),
    max_age = max(Age2, na.rm = TRUE),
    n = n()
  )

# ------------------------------------------------------------------------------
# Categorical to Categorical - Base R Approach
# ------------------------------------------------------------------------------

# Create new variable for race category
acs$RaceNew <- factor(NA, levels = c("white", "non-white"))

# Re-assign values of the new race variable
acs$RaceNew[acs$Race == "white"] <- "white"
acs$RaceNew[acs$Race == "asian" | acs$Race == "black" | 
            acs$Race == "other"] <- "non-white"

# Check re-coding of RaceNew
table(acs$Race, acs$RaceNew)

# ------------------------------------------------------------------------------
# Additional Data Manipulation - Income Categories
# ------------------------------------------------------------------------------

# Create income category using tidyverse
acs <- acs %>%
  mutate(
    IncomeCategory = case_when(
      Income < 30 ~ "low",
      Income >= 30 & Income < 80 ~ "middle",
      Income >= 80 ~ "high"
    ),
    IncomeCategory = factor(IncomeCategory, 
                           levels = c("low", "middle", "high"))
  )

# ------------------------------------------------------------------------------
# Visualizing Cleaned Data
# ------------------------------------------------------------------------------

# Histogram of cleaned age
ggplot(acs, aes(x = Age2)) +
  geom_histogram()

# Bar chart of age categories
ggplot(acs, aes(x = AgeCategory)) +
  geom_bar()

# Bar chart of race categories
ggplot(acs, aes(x = RaceNew)) +
  geom_bar()

# Income by age category
ggplot(acs, aes(x = AgeCategory, y = Income)) +
  geom_boxplot()

# Income by race
ggplot(acs, aes(x = RaceNew, y = Income)) +
  geom_boxplot()

# Age vs Income
ggplot(acs, aes(x = Age2, y = Income)) +
  geom_point()


# ------------------------------------------------------------------------------
# End of Lab 3 Code
# ------------------------------------------------------------------------------
