test_that("od_utils", {
  expect_no_error(od_cache_summary())
  expect_no_error(od_downloads())
})

test_that("browse", {
  expect_no_error(sc_browse())
  expect_no_error(sc_browse_database("deake005"))
})

# $recode is part of the base class (OGD and STATcube)
test_that("recode", {
  x <- od_table("OGD_krebs_ext_KREBS_1")
  expect_no_error(
    x$recode$
      label_field("C-KRE_GESCHLECHT-0", "en", "SEX")$
      label_measure("F-KRE", "en", "NUMBER")$
      level("C-KRE_GESCHLECHT-0", "GESCHLECHT-1", "en", "MALE")
  )

  earnings <- od_table("OGD_veste309_Veste309_1")

  expect_no_error({
    earnings$recode$
      total_codes("C-A11-0", "A11-1")$
      total_codes("C-STAATS-0", "STAATS-9")$
      total_codes("C-VEBDL-0", "VEBDL-10")$
      total_codes("C-BESCHV-0", "BESCHV-1")

    earnings$total_codes("C-A11-0" = "A11-1")
    earnings$total_codes()
  })

  expect_no_error({
    earnings$tabulate("C-STAATS-0")
    earnings$recode$visible("C-STAATS-0", "STAATS-8", FALSE)
    earnings$tabulate("C-STAATS-0")
    earnings$recode$
      order("C-A11-0", c("A11-3", "A11-1", "A11-2"))
  })
})
