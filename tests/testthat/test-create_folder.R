test_that("create_folder: new folder is created with null path, relative return, ", {
  folder_name <- rstring()
  path <- create_folder(folder_name = folder_name)
  expect_true(dir.exists(path))
  expect_equal(path, folder_name)
  expect_equal(dir(path), c("cmd_fit", "cp_info", "cp_samples", "stan_model"))
  unlink(path, recursive = TRUE)
})


test_that("create_folder: new folder is created with null path, not relative return", {
  folder_name <- rstring()
  path <- create_folder(folder_name = folder_name, return_relative = FALSE)
  expect_true(dir.exists(path))
  expect_equal(path, file.path(getwd(), folder_name))
  expect_equal(dir(path), c("cmd_fit", "cp_info", "cp_samples", "stan_model"))
  unlink(path, recursive = TRUE)
})
  

test_that("create_folder: new folder is created with specified path", {
  folder_name <- rstring()
  path <- create_folder(folder_name = folder_name, path = getwd())
  expect_true(dir.exists(path))
  expect_equal(path, file.path(getwd(), folder_name))
  expect_equal(dir(path), c("cmd_fit", "cp_info", "cp_samples", "stan_model"))
  unlink(path, recursive = TRUE)
})

test_that("create_folder: new folder is created with nested folder_name", {
  folder_name <- rstring()
  nested_folder <- rstring()
  path <- create_folder(folder_name = file.path(nested_folder, folder_name))
  expect_true(dir.exists(path))
  expect_equal(path, file.path(nested_folder, folder_name))
  expect_equal(dir(path), c("cmd_fit", "cp_info", "cp_samples", "stan_model"))
  unlink(file.path(nested_folder), recursive = TRUE)
})
