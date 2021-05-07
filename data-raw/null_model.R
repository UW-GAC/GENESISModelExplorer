## code to prepare `null_model` dataset goes here
library(Biobase)
library(GENESIS)

# Set a seed for reproducibility.
set.seed(123)

# Rest of the code.
phen <- get(load("inst/extdata/phenotype.RData"))

# Include only a subset of the samples.
n <- 900
sample_include <- sample(phen$sample.id, n)
phen <- phen[phen$sample.id %in% sample_include, ]

# Create a covariance matrix to use as a random effect.
cov_mat <- matrix(NA, nrow = n, ncol = n)
diag(cov_mat) <- rnorm(n, mean = 1, sd = 0.01)
v <- abs(rnorm(n * (n - 1) / 2, sd = 0.01, mean = 0.01))
cov_mat[upper.tri(cov_mat)] <- cov_mat[lower.tri(cov_mat)] <- v
rownames(cov_mat) <- colnames(cov_mat) <- sample_include

outcome <- c("bmi")
covars <- c("population", "height")

null_model <- fitNullModel(
  phen,
#  sample.id = sample_include,
  outcome = c("bmi"),
  covars = c("population", "sex", "height_cm"),
  family = "gaussian",
  cov.mat = cov_mat
)
save(null_model, file = "inst/extdata/null_model.RData", version = 2)
