# ==============================================================================
# Lab 1: Introduction to Data - R Code
# DATASCI 100
# ==============================================================================

# ------------------------------------------------------------------------------
# Setup and Package Loading
# ------------------------------------------------------------------------------
# Load the tidyverse package
library(tidyverse)

# ------------------------------------------------------------------------------
# Importing the Data
# ------------------------------------------------------------------------------

# Set working directory (change this to your folder)
# setwd("~/Desktop/DATASCI_100/Lab_Datasets/")

# Import data using tidyverse
arbuthnot <- read_csv("arbuthnot.csv")

# ------------------------------------------------------------------------------
# Examining the Data
# ------------------------------------------------------------------------------

# View the entire dataset
arbuthnot

# View first six rows
head(arbuthnot)

# View last six rows
tail(arbuthnot)

# View in RStudio's data viewer
View(arbuthnot)

# Get a glimpse of the data structure (tidyverse function)
glimpse(arbuthnot)

# Get dimensions of the data frame
dim(arbuthnot)

# Get column names
names(arbuthnot)

# Get summary statistics
summary(arbuthnot)

# ------------------------------------------------------------------------------
# Accessing Variables
# ------------------------------------------------------------------------------

# Traditional base R approach with dollar sign
arbuthnot$boys

# ------------------------------------------------------------------------------
# Introduction to the Pipe Operator
# ------------------------------------------------------------------------------

# Without pipe
summary(arbuthnot)

# With pipe (same result)
arbuthnot %>% summary()

# ------------------------------------------------------------------------------
# Creating New Variables
# ------------------------------------------------------------------------------

# Base R approach - adding vectors
arbuthnot$boys + arbuthnot$girls

# Tidyverse approach using mutate() - create total variable
arbuthnot <- arbuthnot %>%
  mutate(total = boys + girls)

# Create multiple variables at once
arbuthnot <- arbuthnot %>%
  mutate(
    total = boys + girls,
    prop_boys = boys / total,
    prop_girls = girls / total
  )

# View the updated data
arbuthnot

# ------------------------------------------------------------------------------
# Logical Comparisons
# ------------------------------------------------------------------------------

# Check if boys outnumber girls each year (returns TRUE/FALSE)
arbuthnot$boys > arbuthnot$girls

# Count how many years boys outnumbered girls
sum(arbuthnot$boys > arbuthnot$girls)

# Using tidyverse to summarize
arbuthnot %>%
  summarize(years_more_boys = sum(boys > girls))

# Create a logical variable and summarize
arbuthnot <- arbuthnot %>%
  mutate(more_boys = boys > girls)

# Count TRUE values
arbuthnot %>%
  summarize(count = sum(more_boys))

# ------------------------------------------------------------------------------
# Base R Plotting
# ------------------------------------------------------------------------------

# Simple scatter plot
plot(x = arbuthnot$year, y = arbuthnot$prop_boys)

# Line plot with color
plot(x = arbuthnot$year, y = arbuthnot$prop_boys, 
     type = "l", col = "blue")

# ------------------------------------------------------------------------------
# ggplot2: Basic Plots
# ------------------------------------------------------------------------------

# Basic scatter plot with ggplot2
ggplot(data = arbuthnot) +
  geom_point(mapping = aes(x = year, y = prop_girls))

# Using pipe and simplified syntax
arbuthnot %>%
  ggplot(aes(x = year, y = prop_boys)) +
  geom_point()

# Add line connecting points
arbuthnot %>%
  ggplot(aes(x = year, y = prop_boys)) +
  geom_point() +
  geom_line()

# ------------------------------------------------------------------------------
# ggplot2: Enhanced Visualization
# ------------------------------------------------------------------------------

# Sophisticated visualization with multiple layers
arbuthnot %>%
  ggplot(aes(x = year, y = prop_boys)) +
  geom_line(color = "steelblue", size = 1) +
  geom_point(color = "steelblue", size = 2) +
  labs(
    title = "Proportion of Male Baptisms in London",
    subtitle = "Data from 1629 to 1710",
    x = "Year",
    y = "Proportion of Boys",
    caption = "Source: Dr. John Arbuthnot's baptism records"
  ) +
  theme_minimal() +
  theme(
    plot.title = element_text(face = "bold", size = 14),
    plot.subtitle = element_text(face = "italic", size = 11),
    axis.title = element_text(face = "bold")
  )


# ------------------------------------------------------------------------------
# End of Lab 1 Code
# ------------------------------------------------------------------------------
