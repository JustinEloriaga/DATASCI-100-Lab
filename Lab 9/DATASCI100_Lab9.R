# ============================================================
# Lab 9: Inference for Difference of Two Means and Paired Data
# DATASCI 100 -- Spring 2025
# ============================================================

library(tidyverse)

# Set working directory if needed
# setwd("~/Your/Working/Directory/")


# ------------------------------------------------------------
# PART 1: Paired t-Test
# ------------------------------------------------------------

# --- Load course evaluations data ---
evals <- read_csv("CourseEvals.csv")

# --- Compute the within-pair difference ---
evals <- evals %>%
  mutate(diff = course_eval - prof_eval)

# --- Summary statistics for the difference ---
evals %>%
  summarize(
    mean_diff = mean(diff),
    sd_diff   = sd(diff)
  )

# --- Visualize the distribution of differences ---
ggplot(evals, aes(x = diff)) +
  geom_histogram(bins = 20) +
  labs(x = "course_eval - prof_eval",
       title = "Distribution of Paired Differences")

# --- Conduct the paired t-test ---

# Method 1: one-sample t-test on the difference variable
t.test(evals$diff)

# Method 2: supply both variables with paired = TRUE
t.test(evals$course_eval, evals$prof_eval, paired = TRUE)


# ------------------------------------------------------------
# PART 2: Two-Sample t-Test
# ------------------------------------------------------------

# --- Load Mario Kart data ---
mariokart <- read_csv("mariokart.csv")

# Quick overview
glimpse(mariokart)
summary(mariokart$total_pr)

# --- Explore total_pr ---
ggplot(mariokart, aes(x = total_pr)) +
  geom_histogram(bins = 20) +
  labs(x = "Total Price (USD)", title = "Distribution of Total Price")

# --- Identify and inspect high-price outliers ---
mariokart %>%
  filter(total_pr > 100)

# --- Remove outliers ---
mkClean <- mariokart %>%
  filter(total_pr < 100)

ggplot(mkClean, aes(x = total_pr)) +
  geom_histogram(bins = 20) +
  labs(x = "Total Price (USD)", title = "Total Price After Removing Outliers")

# --- Recode shipping speed into four groups ---
mkClean <- mkClean %>%
  mutate(newship = case_when(
    ship_sp %in% c("firstClass", "priority") ~ "FirstClass/Priority",
    ship_sp %in% c("ups3Day", "upsGround")   ~ "UPS",
    ship_sp == "standard"                    ~ "Standard",
    TRUE                                     ~ "other"
  ))

# Verify recoding
table(mkClean$newship, mkClean$ship_sp)

# --- Explore the two groups of interest ---
mk_two <- mkClean %>%
  filter(newship %in% c("UPS", "Standard"))

# Side-by-side boxplots
ggplot(mk_two, aes(x = newship, y = total_pr)) +
  geom_boxplot() +
  labs(x = "Shipping Method", y = "Total Price (USD)",
       title = "Total Price by Shipping Method")

# Histograms for each group
ggplot(mk_two, aes(x = total_pr)) +
  geom_histogram(bins = 15) +
  facet_wrap(~ newship) +
  labs(x = "Total Price (USD)")

# Group summary statistics
mk_two %>%
  group_by(newship) %>%
  summarize(n    = n(),
            mean = mean(total_pr),
            sd   = sd(total_pr))

# --- Conduct the two-sample t-test ---
ups      <- mkClean$total_pr[mkClean$newship == "UPS"]
standard <- mkClean$total_pr[mkClean$newship == "Standard"]

# Equal variances assumed
t.test(ups, standard, var.equal = TRUE)

# Welch approximation (unequal variances, also the default)
t.test(ups, standard, var.equal = FALSE)

