# ============================================================
# Lab 8: Inference for a Single Mean and Errors in Inference
# DATASCI 100 -- Spring 2025
# ============================================================

library(tidyverse)

# Set working directory if needed
# setwd("~/Your/Working/Directory/")


# ------------------------------------------------------------
# PART 1: Inference for a Single Mean
# ------------------------------------------------------------

# --- Load course evaluations data ---
evals <- read_csv("CourseEvals.csv")

# --- Explore cls_perc_eval ---
summary(evals$cls_perc_eval)
sd(evals$cls_perc_eval)

ggplot(evals, aes(x = cls_perc_eval)) +
  geom_histogram(bins = 20)

# --- One-sample t-test: H0: mu = 80 vs Ha: mu != 80 ---
t.test(evals$cls_perc_eval, mu = 80)

# --- 90% confidence interval ---
t.test(evals$cls_perc_eval, mu = 80, conf.level = 0.90)

# --- One-sided alternative: Ha: mu < 80 ---
t.test(evals$cls_perc_eval, mu = 80, alternative = "less")

# --- Using the t distribution directly ---

# Two-sided p-value for t = -2, df = 50
2 * pt(-2, df = 50)

# Two-sided p-value for t = 2, df = 50
2 * (1 - pt(2, df = 50))
2 * pt(2, df = 50, lower.tail = FALSE)

# Critical t-value for 95% CI with 50 df
qt(0.025, df = 50)
qt(0.975, df = 50)


# ------------------------------------------------------------
# PART 2: Errors in Inference
# ------------------------------------------------------------

# --- Load helper functions ---
source("TestingFunctions.R")

# --- Population distribution ---
ggplot(evals, aes(x = cls_perc_eval)) +
  geom_histogram(bins = 20)

mean(evals$cls_perc_eval)
sd(evals$cls_perc_eval)

# --- Run simulation: 100 samples of size 50 ---
sim1 <- inference.means(variable    = evals$cls_perc_eval,
                        sample.size = 50,
                        alpha       = 0.05,
                        num.reps    = 100)

View(sim1)

# --- Examine hypothesis test performance ---
ggplot(sim1, aes(x = samp.est)) +
  geom_histogram(bins = 20) +
  labs(title = "Sample Means")

ggplot(sim1, aes(x = test.stat)) +
  geom_histogram(bins = 20) +
  labs(title = "t Test Statistics")

ggplot(sim1, aes(x = p.val)) +
  geom_histogram(bins = 20) +
  labs(title = "p-values")

table(sim1$decision)

# --- Examine confidence interval performance ---
plot.ci(results = sim1, true.val = 74.4)

table(sim1$capture)

# --- Agreement between CI and hypothesis test ---
table(sim1$capture, sim1$decision)

# --- Long-run performance: 10,000 simulations ---
sim1long <- inference.means(variable    = evals$cls_perc_eval,
                            sample.size = 50,
                            alpha       = 0.05,
                            num.reps    = 10000)

table(sim1long$decision)
table(sim1long$capture)
