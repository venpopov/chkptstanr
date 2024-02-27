withr::local_options(test_recompile = FALSE,
                     test_checkpoint_path = "local/chkpt_test_helpers")

test_that("rstring works with seed", {
  seed <- sample.int(.Machine$integer.max, 1L)
  expect_equal(rstring(seed = seed), rstring(seed = seed))
  expect_false(rstring(seed = seed) == rstring(seed = seed + 1))
  expect_false(rstring(seed = seed) == rstring())
})


test_that("reset_checkpoints works", {
  path <- tempfile()
  .use_checkpoint_folder(path)
  expect_equal(dir(path), c("cmd_output", "cp_info", "cp_samples", "stan_model"))
  reset_checkpoints(path)
  expect_equal(dir(path), "stan_model")
  unlink(path, recursive = TRUE)
})

test_that("setup_model_testing works", {
  context <- rstring()
  path <- setup_model_testing(context)
  .use_checkpoint_folder(path)
  expect_equal(dir(path), c("cmd_output", "cp_info", "cp_samples", "stan_model"))
  setup_model_testing(dir = context, recompile = FALSE)
  expect_equal(dir(path), c("stan_model"))
  setup_model_testing(dir = context, recompile = TRUE)
  expect_equal(dir(path), character(0))
})
