## code to prepare `genotype` dataset goes here
library(tibble)

# Set a seed for reproducibility.
set.seed(123)

# Rest of the code.

nm <- get(load("inst/extdata/null_model.RData"))

# Include only a subset of the samples.
sample_include <- nm$fit$sample.id
n <- length(sample_include)


afreqs <- c(0.05, 0.1, 0.2, 0.3)
geno <- tibble(
  sample.id = sample_include,
  `chr1:1000_A_T` = sample(c(0, 1, 2), n, replace = TRUE, prob = c(1 - afreqs[1], 2 * afreqs[1] * (1 - afreqs[1]), afreqs[1])),
  `chr2:2000_C_CT` = sample(c(0, 1, 2), n, replace = TRUE, prob = c(1 - afreqs[2], 2 * afreqs[2] * (1 - afreqs[2]), afreqs[2])),
  `chr3:3000_AAT_T` = sample(c(0, 1, 2), n, replace = TRUE, prob = c(1 - afreqs[3], 2 * afreqs[3] * (1 - afreqs[3]), afreqs[3])),
  `chr4:4000_G_T` = sample(c(0, 1, 2), n, replace = TRUE, prob = c(1 - afreqs[4], 2 * afreqs[4] * (1 - afreqs[4]), afreqs[4]))
)
head(geno)

saveRDS(geno, "inst/extdata/genotypes.rds")
