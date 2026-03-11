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
