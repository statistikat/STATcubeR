test_that("od_table() works", {
  x <- expect_no_error(od_table("OGD_krebs_ext_KREBS_1"))
  expect_s3_class(x, "od_table")
  tabulated <- expect_no_error(x$tabulate())
  expect_s3_class(tabulated, "data.frame")
  expect_output(print(x$resources))
})

test_that("OGD datasets can be listed", {
  dataset_index <- expect_no_error(od_list())
  expect_s3_class(dataset_index, "data.frame")
})

test_that("OGD revisions work", {
  rev <- expect_no_error(od_revisions())
  expect_s3_class(rev, "character")
  expect_output(print(rev))
})

test_that("catalogue", {
  expect_no_error(od_catalogue())
})

test_that("OGD save and reload works", {
  x <- expect_no_error(od_table("OGD_krebs_ext_KREBS_1"))
  tmp <- tempfile()
  expect_no_error(od_table_save(x, tmp))
  expect_no_error({ y <- od_table_local(tmp) })
  expect_s3_class(y, "od_table")
  expect_equal(x$meta, y$meta)
})
