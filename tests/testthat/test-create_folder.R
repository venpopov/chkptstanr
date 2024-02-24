test_that("create_folder works and is deprecated", {
  path <- rstring()
  expect_warning(create_folder(path))
  
  suppressWarnings({
    expect_equal(path, create_folder(path))
    expect_equal(dir(path), c("cmd_fit", "cp_info", "cp_samples", "stan_model"))
  })
  
  unlink(path, recursive = TRUE)
})


