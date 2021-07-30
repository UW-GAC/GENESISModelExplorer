test_that("load null model works as expected", {
  nullmod_file <- system.file("extdata", "null_model.RData", package="shinyNullModel")
  nm <- get(load(nullmod_file))
  out <- .load_null_model(nullmod_file)
  expect_s3_class(out, "tbl_df")
  expect_equal(out$sample.id, nm$fit$sample.id)
  vars <- setdiff(names(out), "sample.id")
  expect_true(all(stringr::str_starts(vars, "Model: "))) # Appends model: to the names
})

test_that("load null model data updates old null models", {
  nullmod_file <- system.file("extdata", "null_model_old.RData", package="shinyNullModel")
  nm <- get(load(nullmod_file))
  expect_warning(out <- .load_null_model(nullmod_file), "updated")
  expect_s3_class(out, "tbl_df")
  expect_equal(out$sample.id, nm$sample.id)
  vars <- setdiff(names(out), "sample.id")
  expect_true(all(stringr::str_starts(vars, "Model: "))) # Appends model: to the names
})

test_that("load null model fails iwth incorrect data type", {
  nm <- c(1, 2, 3)
  tmpfile <- tempfile()
  save(nm, file = tmpfile)
  expect_error(.load_null_model(tmpfile), "valid GENESIS null model")
  unlink(tmpfile)
})

test_that("load phenotype works as expected with AnnotatedDataFrame", {
  phenotype_file <- system.file("extdata", "phenotype.RData", package="shinyNullModel")
  phen <- get(load(phenotype_file))
  out <- .load_phenotype(phenotype_file)
  expect_s3_class(out, "tbl_df")
  expect_equal(out$sample.id, phen$sample.id)
  vars <- setdiff(names(out), "sample.id")
  expect_true(all(stringr::str_starts(vars, "Phenotype: "))) # Appends model: to the names
})

test_that("load phenotype works as expected with data frame", {
  # Save as a temporary file
  phen <- get(load(system.file("extdata", "phenotype.RData", package="shinyNullModel"))) %>%
    Biobase::pData()
  tmpfile <- tempfile()
  save(phen, file = tmpfile)
  out <- .load_phenotype(tmpfile)
  expect_s3_class(out, "tbl_df")
  expect_equal(out$sample.id, phen$sample.id)
  vars <- setdiff(names(out), "sample.id")
  expect_true(all(stringr::str_starts(vars, "Phenotype: "))) # Appends model: to the names
  unlink(tmpfile)
})

test_that("load phenotype fails if sample.id is missing", {
  # Remove sample id to make sure the function returns an error.
  tmp <- get(load(system.file("extdata", "phenotype.RData", package="shinyNullModel")))
  tmp <- tmp[, Biobase::varLabels(tmp) != "sample.id"]
  tmpfile <- tempfile()
  save(tmp, file = tmpfile)
  expect_error(.load_phenotype(tmpfile), "must include sample.id")
  unlink(tmpfile)
})

test_that("load phenotype fails with incorrect data type", {
  # Save a file with a incorrect data type.
  tmp <- 1:3
  tmpfile <- tempfile()
  save(tmp, file = tmpfile)
  expect_error(.load_phenotype(tmpfile), "must be a data frame or AnnotatedDataFrame")
  unlink(tmpfile)
})

test_that("load genotype works as expected with one variant", {
  geno <- tibble::tibble(
    sample.id = sprintf("sample%s", 1:5),
    `chr1:1000_A_G` = c(0, 1, 2, 0, 1)
  )
  tmpfile <- withr::local_file("geno.RData")
  saveRDS(geno, tmpfile)

  out <- .load_genotype(tmpfile)
  expect_s3_class(out, "tbl_df")
  # Check names and format.
  expect_equal(nrow(out), 5) # five samples
  expect_equal(ncol(out), 2) # sample id and one variants
  variant_name <- c("Genotype: chr1:1000_A_G")
  expect_equal(names(out), c("sample.id", variant_name))
  expect_equal(out$sample.id, c("sample1", "sample2", "sample3", "sample4", "sample5"))
  # Check values.
  expect_equal(out[["Genotype: chr1:1000_A_G"]], geno[[2]])
})

test_that("load genotype works as expected with multiple variants", {
  geno <- tibble::tibble(
    sample.id = sprintf("sample%s", 1:5),
    `chr1:1000_A_G` = c(0, 1, 2, 0, 1),
    `chr2:2000_A_AT` = c(0, 0, 1, NA, 0)
  )
  tmpfile <- withr::local_file("geno.RData")
  saveRDS(geno, tmpfile)

  out <- .load_genotype(tmpfile)
  expect_s3_class(out, "tbl_df")
  # Check names and format.
  expect_equal(nrow(out), 5) # four samples
  expect_equal(ncol(out), 3) # sample id and two variants
  variant_names <- c(
    "Genotype: chr1:1000_A_G",
    "Genotype: chr2:2000_A_AT"
  )
  expect_equal(names(out), c("sample.id", variant_names))
  expect_equal(out$sample.id, c("sample1", "sample2", "sample3", "sample4", "sample5"))
  # Check values.
  expect_equal(out[[variant_names[1]]], geno[[2]])
  expect_equal(out[[variant_names[2]]], geno[[3]])
})

test_that("load genotype fails if missing sample columns", {
  geno <- tibble::tibble(
    `chr1:1000_A_G` = c(0, 1, 2, 0, 1),
    `chr2:2000_A_AT` = c(0, 0, 1, NA, 0)
  )
  tmpfile <- withr::local_file("geno.RData")
  saveRDS(geno, tmpfile)
  expect_error(.load_genotype(tmpfile), "must have a sample.id column")
})

test_that('load genotype fails if missing any variant columns',{
  geno <- tibble::tibble(
    sample.id = sprintf("sample%s", 1:5),
  )
  tmpfile <- withr::local_file("geno.RData")
  saveRDS(geno, tmpfile)
  expect_error(.load_genotype(tmpfile), "contain variant columns")
})


test_that("load data works as expected", {
  nullmod_file <- system.file("extdata", "null_model.RData", package="shinyNullModel")
  nm <- .load_null_model(nullmod_file)
  phenotype_file <- system.file("extdata", "phenotype.RData", package="shinyNullModel")
  phen <- .load_phenotype(phenotype_file)
  out <- .load_data(nullmod_file, phenotype_file)
  expect_s3_class(out, "tbl_df")
  expect_equal(nrow(out), nrow(nm))
  expect_equal(out$sample.id, nm$sample.id)
  expect_equal(names(out), unique(c("sample.id", names(nm), names(phen))))
})

test_that("load data works when null model has fewer samples than phenotype file", {
  # Hacky, for now.
  tmp <- get(load(system.file("extdata", "null_model.RData", package="shinyNullModel")))
  tmp$fit <- tmp$fit[1:10, ]
  tmpfile <- tempfile()
  save(tmp, file = tmpfile)
  nm <- .load_null_model(tmpfile)
  phenotype_file <- system.file("extdata", "phenotype.RData", package="shinyNullModel")
  phen <- .load_phenotype(phenotype_file)
  out <- .load_data(tmpfile, phenotype_file)
  expect_s3_class(out, "tbl_df")
  expect_equal(nrow(out), 10)
  expect_equal(out$sample.id, nm$sample.id)
  expect_equal(names(out), unique(c("sample.id", names(nm), names(phen))))
  unlink(tmpfile)
})

test_that("load data fails when null model has more samples than phenotype file", {
  nullmod_file <- system.file("extdata", "null_model.RData", package="shinyNullModel")
  nm <- .load_null_model(nullmod_file)
  tmp <- get(load(system.file("extdata", "phenotype.RData", package="shinyNullModel")))
  tmp <- tmp[1:10, ]
  tmpfile <- tempfile()
  save(tmp, file = tmpfile)
  expect_error(.load_data(nullmod_file, tmpfile), "must contain all sample.ids")
  unlink(tmpfile)
})

test_that("load data fails when phenotype file has duplicated sample ids not in null model file", {
  nullmod_file <- system.file("extdata", "null_model.RData", package="shinyNullModel")
  nm <- .load_null_model(nullmod_file)
  phen <- get(load(system.file("extdata", "phenotype.RData", package="shinyNullModel")))
  tmp <- rbind(
    Biobase::pData(phen),
    Biobase::pData(phen) %>% dplyr::filter(!(sample.id %in% nm$sample.id))
  ) %>%
    Biobase::AnnotatedDataFrame()
  tmpfile <- withr::local_file("pheno.RData")
  save(tmp, file = tmpfile)
  expect_error(.load_data(nullmod_file, tmpfile), "duplicated sample.ids")
  unlink(tmpfile)
})

test_that("load data fails when phenotype file has duplicated sample ids in null model file", {
  nullmod_file <- system.file("extdata", "null_model.RData", package="shinyNullModel")
  nm <- .load_null_model(nullmod_file)
  phen <- get(load(system.file("extdata", "phenotype.RData", package="shinyNullModel")))
  tmp <- rbind(
    Biobase::pData(phen),
    Biobase::pData(phen) %>% dplyr::filter(sample.id %in% nm$sample.id[1])
  ) %>%
    Biobase::AnnotatedDataFrame()
  tmpfile <- withr::local_file("pheno.RData")
  save(tmp, file = tmpfile)
  expect_error(.load_data(nullmod_file, tmpfile), "duplicated sample.ids")
  unlink(tmpfile)
})

test_that("load data works as expected with genotype file", {
  nullmod_file <- system.file("extdata", "null_model.RData", package="shinyNullModel")
  nm <- .load_null_model(nullmod_file)
  phenotype_file <- system.file("extdata", "phenotype.RData", package="shinyNullModel")
  phen <- .load_phenotype(phenotype_file)
  genotype_file <- system.file("extdata", "genotypes.rds", package="shinyNullModel")
  geno <- .load_genotype(genotype_file)
  out <- .load_data(nullmod_file, phenotype_file, genotype_filename = genotype_file)
  expect_s3_class(out, "tbl_df")
  expect_equal(nrow(out), nrow(nm))
  expect_equal(out$sample.id, nm$sample.id)
  expect_equal(names(out), unique(c("sample.id", names(nm), names(phen), names(geno))))
})

test_that("load data works as expected if genotype file has additional samples", {
  # Hacky, for now.
  tmp <- get(load(system.file("extdata", "null_model.RData", package="shinyNullModel")))
  tmp$fit <- tmp$fit[1:10, ]
  tmpfile <- tempfile()
  save(tmp, file = tmpfile)
  nm <- .load_null_model(tmpfile)
  phenotype_file <- system.file("extdata", "phenotype.RData", package="shinyNullModel")
  phen <- .load_phenotype(phenotype_file)
  genotype_file <- system.file("extdata", "genotypes.rds", package="shinyNullModel")
  geno <- .load_genotype(genotype_file)
  out <- .load_data(tmpfile, phenotype_file, genotype_filename = genotype_file)
  expect_s3_class(out, "tbl_df")
  expect_equal(nrow(out), 10)
  expect_equal(out$sample.id, nm$sample.id)
  expect_equal(names(out), unique(c("sample.id", names(nm), names(phen), names(geno))))
})

test_that("load data works as expected if genotype file has fewer samples", {
  nullmod_file <- system.file("extdata", "null_model.RData", package="shinyNullModel")
  nm <- .load_null_model(nullmod_file)
  phenotype_file <- system.file("extdata", "phenotype.RData", package="shinyNullModel")
  phen <- .load_phenotype(phenotype_file)
  tmp <- readRDS(system.file("extdata", "genotypes.rds", package="shinyNullModel"))
  tmp <- tmp[1:10, ]
  tmpfile <- withr::local_file("geno.rds")
  saveRDS(tmp, tmpfile)
  expect_error(.load_data(nullmod_file, phenotype_file, tmpfile), "Genotype file must contain all sample.ids")
  unlink(tmpfile)


})
