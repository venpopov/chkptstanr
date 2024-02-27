test_that('combine_samples works', {
  # note, the test checkpoint files were generate via 
  # create_testing_samples('tests/testthat/combine_test_checkpoints')
  path <- test_path('combine_test_checkpoints')
  sample_files <- list.files(paste0(path, "/cmd_output/"), full.names = TRUE)
  ind_fits <- read_stan_csv_multiple(sample_files, idpattern = 'output_[0-9]+_')
  
  res <- combine_samples(sample_files, warmup_chkpts = 1, save_warmup = FALSE)
  chains <- res@sim$chains
  sim <- res@sim
  samples <- sim$samples
  stan_args <- res@stan_args
  expect_true(inherits(res, "stanfit"))
  expect_equal(res@model_name, "model")
  expect_equal(res@stanmodel@model_name, "model")
  for (i in seq_len(chains)) {
    # info in samples
    expect_equal(nrow(samples[[i]]), 400)
    expect_equal(nrow(attr(samples[[i]], "sampler_params")), 400)
    expect_true(all(attr(samples[[i]], "elapsed_time") > c(warmup = 0.004, sample = 0.01)))
    expect_match(attr(samples[[i]], "adaptation_info"), 
                 attr(ind_fits[[5]]@sim$samples[[i]], "adaptation_info"))
    expect_equal(names(attr(samples[[i]], "mean_pars")), 
                 c('b.1','b.2','Intercept','lprior','b_Intercept'))
    expect_lt(attr(samples[[i]], "mean_lp"), -860)
    # info in permutation
    expect_equal(length(sim$permutation[[i]]), 400)
    expect_equal(length(unique(sim$permutation[[i]])), 400)
    # info in stan_args
    expect_equal(stan_args[[i]]$model, "model")
    expect_equal(stan_args[[i]]$start_datetime, ind_fits[[1]]@stan_args[[i]]$start_datetime)
    expect_equal(stan_args[[i]]$iter, 500)
    expect_equal(stan_args[[i]]$warmup, 100)
    expect_equal(stan_args[[i]]$save_warmup, 0)
  }
  expect_equal(sim$iter, 500)
  expect_equal(sim$warmup, 100)
  expect_equal(sim$n_save, c(400,400))
  expect_equal(sim$warmup2, c(0,0))
})
