test_that("sc_schema works", {
  catalogue <- expect_no_error(sc_schema_catalogue())
  expect_s3_class(catalogue, "sc_schema")
  expect_s3_class(catalogue[[1]], "sc_schema")
  expect_output(print(catalogue))
  flattened <- expect_no_error(sc_schema_flatten(catalogue, "DATABASE"))
  expect_s3_class(flattened, "data.frame")
  expect_output(print(flattened))
  expect_no_error(sc_schema_db("deake005"))
})

test_that("table endpoint works", {
  x <- expect_no_error(sc_table(sc_example("acc"), language = "both"))
  expect_s3_class(x, "sc_table")
  expect_no_error(sc_check_response(x$response))
  expect_no_error(x$tabulate())
  expect_output(print(x))
  expect_output(print(x$meta))
  expect_output(print(x$field()))
})

test_that("saved tables work", {
  st <- expect_no_error(sc_table_saved_list())
  expect_s3_class(st, "data.frame")
  x <- expect_no_error(sc_table_saved(st$id[1]))
  expect_s3_class(x, "sc_table")
})

test_that("sc_table_custom", {
  expect_no_error(sc_table_custom("str:database:detouextregsai"))
  expect_no_error({
    schema <- sc_schema_db("detouextregsai")
    region <- schema$`Other Classifications`$`Tourism commune [ABO]`$
      `Regionale Gliederung (Ebene +1)`
    month <- schema$`Mandatory fields`$`Season/Tourism Month`
    x <- sc_table_custom(
      schema,
      schema$Facts$Arrivals,
      list(month, region),
      recodes = c(
        sc_recode(region, total = FALSE, map = list(
          region$Achensee,
          list(region$Arlberg, region$`Ausseerland-Salzkammergut`)
        )),
        sc_recode(month, total = FALSE)
      )
    )
  })
})

test_that("cache", {
  dir <- sc_cache_dir()
  sc_cache_dir(tempfile())
  r1 <- sc_with_cache("test", function() { rnorm(1) })
  r2 <- sc_with_cache("test", function() { rnorm(1) })
  expect_identical(r1, r2)
  sc_cache_disable()
  r3 <- sc_with_cache("test", function() { rnorm(1) })
  expect_false(identical(r2, r3))
  expect_message(sc_cache_enable())
  r4 <- sc_with_cache("test", function() { rnorm(1) })
  expect_identical(r2, r4)
  if (startsWith(sc_cache_dir(), tempdir()))
    expect_message(sc_cache_clear())
})
