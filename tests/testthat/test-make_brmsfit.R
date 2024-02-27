test_that('combine_samples works', {
  # note, the test checkpoint files were generate via 
  # create_testing_samples('tests/testthat/combine_test_checkpoints')
  path <- test_path('combine_test_checkpoints')
  sample_files <- list.files(paste0(path, "/cp_output/"))
  res <- combine_samples(sample_files)
  chains <- res@sim$chains
  sim <- res@sim
  samples <- sim$samples
  stan_args <- res@stan_args
  expect_type(res, "stanfit")
  expect_equal(res$model_name, "model")
  expect_equal(res$model_name, res)
  expect_equal(res@sim)
  for (i in seq_len(chains)) {
    # info in samples
    expect_equal(nrow(samples[[i]]), 400)
    expect_equal(nrow(attr(samples[[i]], "sampler_params")), 400)
    expect_gt(attr(samples[[i]], "elapsed_time"), 0.01)
    expect_equal(attr(samples[[i]], "adaptation_info"), "Check what this should be")
    expect_equal(attr(samples[[i]], "mean_pars"), "Check what this should be")
    expect_equal(attr(samples[[i]], "mean_lp__"), "Check what this should be")
    # info in permutation
    expect_equal(length(sim$permutation[[i]]), 400)
    expect_equal(length(unique(sim$permutation[[i]])), 400)
    # info in stan_args
    expect_equal(stan_args[[i]]$model, "model")
    expect_equal(stan_args[[i]]$start_date, "Check what this should be")
    expect_equal(stan_args[[i]]$iter, 400)
    expect_equal(stan_args[[i]]$warmup, 100)
    expect_equal(stan_args[[i]]$save_warmup, 0)
    expect_equal(stan_args[[i]]$stepsize, "Check what this should be")
    expect_equal(stan_args[[i]]$adaptation_info, "Check what this should be")
    expect_equal(stan_args[[i]]$time_info, "Check what this should be")
  }
  expect_equal(sim$iter, 400)
  expect_equal(sim$warmup, 100)
  expect_equal(sim$n_save, c(400,400))
  expect_equal(sim$warmup2, "Check what this should be")
})


# path <- test_path('combine_test_checkpoints')
# sample_files <- list.files(paste0(path, "/cmd_output/"), full.names = TRUE)
# sample_files <- sample_files[grepl("output_5", sample_files)]
# res <- rstan::read_stan_csv(sample_files)


# the result should be an ofbject of class stanfit

# [] rename model_name
# [] combine sim
#    - $samples (including attributes)
#    - $iter
#    - $warmup
#    - $n_save
#    - $warmup2
#    - $permutation
# @inits - from initial checkpoint
# stan_args - list of nchains
#     $model - same model name as above
#     $start_date time - from first checkpoint
#     $iter, warmup, 
# recognize save_warmup