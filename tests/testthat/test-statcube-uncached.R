# statcube AP tests that are currently not cacheable and therefore slow

test_that("other endpoints work", {
  expect_no_error(sc_info())
  expect_no_error(sc_rate_limit_table())
  expect_no_error(sc_rate_limit_schema())
  rl <- expect_no_error(sc_rate_limits(sc_table(sc_example("acc"))))
  expect_output(print(rl))
})

test_that("key", {
  wrong_key <- "wrong_key"
  key <- sc_key()
  expect_error(sc_key_set(wrong_key))
  suppressMessages(expect_message(sc_key_set(key, test = FALSE)))
})
