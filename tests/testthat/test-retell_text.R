test_that("retell_text() works", {
  expect_equal(retell_text(token = my_token), message('You forgot to enter the URL!'))
})
