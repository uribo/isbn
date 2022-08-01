test_that("works", {
  expect_equal(
    isbn_convert13to10(c("978-4-06-516404-4", "9784863542167")),
    c("4065164044", "486354216X"))
  expect_equal(
    isbn_convert10to13(c("412345674X", "4022518286")),
    c("9784123456746", "9784022518286"))
})
