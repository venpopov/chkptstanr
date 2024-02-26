# TODO: maybe add a if (interactive()) to local options about recompiling, that
# way I cover both cases?
withr::local_options(
  test_recompile = FALSE,
  test_checkpoint_path = "local/chkpt_test_auto_brms"
)


test_that("chkpt_brms picks up after stopping and returns intermediatery results", {
  skip_on_cran()
  path <- setup_model_testing(dir = "context1")

  # simplified example from vignete for faster execution
  bf_m1 <- brms::bf(
    formula = count ~ zAge + zBase,
    family = poisson()
  )

  cat("\n\nRunning for 2 checkpoints then stop programatically\n\n")

  # run for 1 checkpoints then stop
  expect_message(chkpt_brms(
    formula = bf_m1,
    data = brms::epilepsy,
    iter_warmup = 400,
    iter_sampling = 1200,
    iter_per_chkpt = 200,
    stop_after = 300,
    path = path,
  ), "Interupted during warmup. No samples available.")

  cat("\n\nGet 1 more checkpoints - brmsfit returned even if sampling interupt\n\n")

  fit2 <- chkpt_brms(
    formula = bf_m1,
    data = brms::epilepsy,
    path = path,
    iter_warmup = 400,
    iter_sampling = 1200,
    iter_per_chkpt = 200,
    stop_after = 600
  )

  expect_true(is(fit2, "brmsfit"))

  cat("\n\nFinish sampling\n\n")

  fit3 <- chkpt_brms(
    formula = bf_m1,
    data = brms::epilepsy,
    path = path,
    iter_warmup = 400,
    iter_sampling = 1200,
    iter_per_chkpt = 200,
  )

  expect_true(is(fit3, "brmsfit"))
})


test_that("chkpt_brms works with data2", {
  skip_on_cran()
  path <- setup_model_testing(dir = "context2")
  # generate data
  # from: https://cran.r-project.org/web/packages/brms/vignettes/brms_phylogenetics.html
  phylo <- ape::read.nexus("https://paul-buerkner.github.io/data/phylo.nex")
  data_simple <- read.table(
    "https://paul-buerkner.github.io/data/data_simple.txt",
    header = TRUE
  )
  A <- ape::vcv.phylo(phylo)

  path <- file.path(tempdir(), "chkpt_brms_test")
  # clean up
  on.exit(unlink(path, recursive = TRUE))

  prior <- c(
    brms::prior(normal(0, 10), "b"),
    brms::prior(normal(0, 50), "Intercept"),
    brms::prior(student_t(3, 0, 20), "sd"),
    brms::prior(student_t(3, 0, 20), "sigma")
  )

  checkpoint_model <- try(chkpt_brms(
    phen ~ cofactor + (1 | gr(phylo, cov = A)),
    data = data_simple,
    family = gaussian(),
    data2 = list(A = A),
    prior = prior,
    path = path,
    iter_warmup = 100,
    iter_sampling = 200,
    iter_per_chkpt = 100
  ), silent = T)

  expect_false(is(checkpoint_model, "try-error"))
  expect_true(is(checkpoint_model, "brmsfit"))
})


test_that("chkpt_brms refuses to continue sampling if we change key arguments", {
  skip_on_cran()
  path <- setup_model_testing(dir = "context3")

  # simplified example from vignete for faster execution
  bf_m1 <- brms::bf(
    formula = count ~ zAge + zBase,
    family = poisson()
  )

  cat("\n\nRunning for 1 checkpoint then stopping\n\n")

  # run for 1 checkpoints then stop
  fit_m1 <- try(chkpt_brms(
    formula = bf_m1,
    data = brms::epilepsy,
    iter_warmup = 100,
    iter_sampling = 200,
    iter_per_chkpt = 100,
    stop_after = 100,
    path = path
  ), silent = T)

  cat("\n\nTrying to pick up where we stopped\n\n")

  expect_error(chkpt_brms(
    formula = bf(count ~ 1),
    data = brms::epilepsy,
    path = path,
    iter_warmup = 100,
    iter_sampling = 200,
    iter_per_chkpt = 100
  ), "arguments have been changed")
})


test_that("the family can be specified separately from the formula in a
           base model with no stopping", {
  path <- setup_model_testing(dir = "context4")
  formula <- brms::bf(formula = count ~ zAge + zBase)
  family <- poisson()
  fit <- chkpt_brms(
    formula = formula,
    family = family,
    data = brms::epilepsy,
    iter_warmup = 400,
    iter_sampling = 1200,
    iter_per_chkpt = 200,
    path = path
  )
  expect_true(is(fit, "brmsfit"))
  print(fit)
})

test_that("the family can be specified separately from the formula in a
           base model with stopping before sampling", {
  path <- setup_model_testing(dir = "context5")
  formula <- brms::bf(formula = count ~ zAge + zBase)
  family <- poisson()
  fit <- chkpt_brms(
    formula = formula,
    family = family,
    data = brms::epilepsy,
    iter_warmup = 400,
    iter_sampling = 1200,
    iter_per_chkpt = 200,
    path = path,
    stop_after = 300
  )

  fit <- chkpt_brms(
    formula = formula,
    family = family,
    data = brms::epilepsy,
    iter_warmup = 400,
    iter_sampling = 1200,
    iter_per_chkpt = 200,
    path = path,
    stop_after = 800
  )
  expect_true(is(fit, "brmsfit"))
})

withr::deferred_run()
