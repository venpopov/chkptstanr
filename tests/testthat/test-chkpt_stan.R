test_that("chkpt_stan picks up after stopping", {
  path <- file.path(tempdir(), "chkpt_stan_test1")
  # clean up
  on.exit(unlink(path, recursive = TRUE))

  # simplified example from vignete for faster execution
  bf_m1 <- brms::bf(
    formula = count ~ zAge + zBase,
    family = poisson()
  )

  cat("\n\nRunning for 1 checkpoint then stopping\n\n")

  # run for 1 checkpoints then stop
  stancode <- brms::make_stancode(bf_m1, data = brms::epilepsy)
  standata <- brms::make_standata(bf_m1, data = brms::epilepsy)
  fit_m1 <- try(chkpt_stan(
    model_code = stancode,
    data = standata,
    iter_warmup = 100,
    iter_sampling = 200,
    iter_per_chkpt = 100,
    stop_after = 1,
    path = path
  ), silent = T)

  cat("\n\nTrying to pick up where we stopped\n\n")

  res <- try(chkpt_stan(
    model_code = stancode,
    data = standata,
    iter_warmup = 100,
    iter_sampling = 200,
    iter_per_chkpt = 100,
    path = path
  ), silent = T)
  
  print(res)

  expect_false(is(res, "try-error"))
})
