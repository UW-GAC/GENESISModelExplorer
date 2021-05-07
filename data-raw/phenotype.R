## code to prepare `phenotype` dataset goes here
library(dplyr)
library(charlatan)
library(Biobase)

# Set a seed for reproducibility.
set.seed(123)

# Number of samples.
n <- 1000

# HapMap populations
pops = c("ASW", "CEU", "CHB", "CHD", "GIH", "JPT", "LWK", "MXL", "MKK", "TSI", "YRI")

# Create a data frame of fake phenotype data.
dat <- data.frame(
  sample.id = sprintf("samp%s", 1:n),
  population = sample(pops, replace = TRUE, n),
  sex = sample(c("M", "F"), replace = TRUE, n)
) %>%
  mutate(
    height_cm = 171 - 12 * (sex == "F") + rnorm(sd = 7, n = n),
    bmi = rnorm(26, sd = 4, n = n),
    weight_kg = (height_cm / 100)^2 * bmi,
    obese = bmi > 30
  )

phenotype <- AnnotatedDataFrame(dat)

save(phenotype, file = "inst/extdata/phenotype.RData")
