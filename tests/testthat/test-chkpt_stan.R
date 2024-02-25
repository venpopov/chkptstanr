withr::local_options(test_recompile = FALSE,
                     test_checkpoint_path = "local/chkpt_test_auto_stan")

test_that("chkpt_stan picks up after stopping", {
  skip_on_cran()
  path <- setup_model_testing(dir = 'context2')

  # simplified example from vignete for faster execution
  bf_m1 <- brms::bf(
    formula = count ~ zAge + zBase,
    family = poisson()
  )

  cat("\n\nRunning for 2 checkpoints then stop programatically\n\n")

  # run for 1 checkpoints then stop
  stancode <- brms::make_stancode(bf_m1, data = brms::epilepsy)
  standata <- brms::make_standata(bf_m1, data = brms::epilepsy)
  chkpt_stan(
    model_code = stancode,
    data = standata,
    iter_warmup = 400,
    iter_sampling = 1200,
    iter_per_chkpt = 200,
    stop_after = 300,
    path = path
  )

  cat("\n\nGet 1 more checkpoints - brmsfit returned even if sampling interupt\n\n")

  res <- chkpt_stan(
    model_code = stancode,
    data = standata,
    iter_warmup = 400,
    iter_sampling = 1200,
    iter_per_chkpt = 200,
    stop_after = 600,
    path = path
  )

  expect_false(is(res, "try-error"))
})

test_that("refuses to continue sampling if we change key arguments", {
  path <- file.path(tempdir(), "chkpt_stan_test1")
  # clean up
  on.exit(unlink(path, recursive = TRUE))

  # simplified example from vignete for faster execution
  bf_m1 <- brms::bf(
    formula = count ~ zAge + zBase,
    family = poisson()
  )

  cat("\n\nRunning for 2 checkpoints then stop programatically\n\n")

  # run for 1 checkpoints then stop
  stancode <- brms::make_stancode(bf_m1, data = brms::epilepsy)
  standata <- brms::make_standata(bf_m1, data = brms::epilepsy)
  chkpt_stan(
    model_code = stancode,
    data = standata,
    iter_warmup = 400,
    iter_sampling = 1200,
    iter_per_chkpt = 200,
    stop_after = 300,
    path = path)
  
  cat("\n\nTrying to pick up where we stopped\n\n")
  
  standata <- brms::make_standata(count ~ 1, data = brms::epilepsy)
  expect_error(chkpt_stan(
    model_code = stancode,
    data = standata,
    iter_warmup = 400,
    iter_sampling = 1200,
    iter_per_chkpt = 200,
    stop_after = 600,
    path = path
  ), "arguments have been changed")
})
