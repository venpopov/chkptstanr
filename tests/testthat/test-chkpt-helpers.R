test_that("rstring works with seed", {
  seed <- sample.int(.Machine$integer.max, 1L)
  expect_equal(rstring(seed = seed), rstring(seed = seed))
  expect_false(rstring(seed = seed) == rstring(seed = seed + 1))
  expect_false(rstring(seed = seed) == rstring())
})
