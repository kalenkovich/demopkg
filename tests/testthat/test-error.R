test_that("multiplication works", {
  expect_equal(2 * 2, 4)
})

test_that("warning", {
  expect_false ( all(1:2 == 1:3) )
})
