# statcube AP tests that are currently not cacheable and therefore slow

httptest::with_mock_api({
  test_that("other endpoints work", {
    expect_no_error(sc_info())
    expect_no_error(sc_rate_limit_table())
    expect_no_error(sc_rate_limit_schema())
    rl <- expect_no_error(sc_rate_limits(
      sc_table_custom("str:database:detouextregsai")))
    expect_output(print(rl))
  })
})

httptest::with_mock_api({
  test_that("key", {
    key <- sc_key()
    suppressMessages(expect_message(sc_key_set(key)))
  })
})

httptest::with_mock_api({
  test_that("invalid json causes error", {
    expect_error(sc_table_custom("str:database:de_wrong_uri"))
    error <- sc_last_error()
    expect_equal(error$status_code, 200)
    expect_equal(httr::http_type(error), "text/html")
  })
})

httptest::with_mock_api({
  test_that("cell limit throws error", {
    expect_error(sc_table_custom(
      "str:database:debevstand",
      "str:measure:debevstand:F-BEVSTAND:F-ISIS-1",
      c("str:field:debevstand:F-BEVSTAND:C-A10-0",
        "str:valueset:debevstand:F-BEVSTAND:C-GNU-2:C-GNU-2",
        "str:valueset:debevstand:F-BEVSTAND:C-BESC51-0:C-BESC51-0",
        "str:valueset:debevstand:F-BEVSTAND:C-BESC11-0:C-BESC11-0")
    ))
    error <- sc_last_error_parsed()
    expect_equal(error$status$message, "Client error: (400) Bad Request")
    expect_equal(error$content$errorType, "CELL_COUNT")
  })
})

httptest::with_mock_api({
  test_that("invalid uri in saved table throws error", {
    expect_error(sc_table_saved("invalid_uri"))
    error <- sc_last_error_parsed()
    expect_equal(error$status$message, "Client error: (400) Bad Request")
    expect_equal(error$content$errorType, "TXD_NOT_FOUND")
  })
})

httptest::with_mock_api({
  test_that("invalid uri in schema throws error", {
    expect_error(sc_schema("invalid_uri"))
    error <- sc_last_error_parsed()
    expect_equal(error$status$message, "Client error: (400) Bad Request")
    expect_equal(error$content$errorType, "SCHEMA_COMPONENT_NOT_FOUND")
  })
})

httptest::with_mock_api({
  test_that("invalid uri in /table body throws error", {
    expect_error(sc_table_custom("str:database:detouextregsai",
                                 "str:measure:detouextregsai:F-DATA1:INVALID"))
    error <- sc_last_error()
    expect_equal(error$status_code, 200)
    expect_equal(httr::http_type(error), "text/html")
  })
})
