test_that("Creating a madlib works", {
  # This is a valid madlib
  text1 = "The predictor <__> was <__>, with a p-value of <__>, after adjusting for <__>"
  expect_no_error(madlib(text1))

  # This is not a valid madlib
  text2 = "The predictor age was significant, with a p-value of 0.04, after adjust for gender."
  expect_error(madlib(text2))

  # This is also not a madlib
  text3 = 5
  expect_error(madlib(text3))
})

test_that("We can correctly count the number of blanks to fill", {
  text = "The predictor <__> was <__>, with a p-value of <__>, after adjusting for <__>"
  x = madlib(text)
  expect_equal(count_blanks(x), 4)
})

test_that("A basic madlib with only string components can be filled", {
  text = "The model for <__> was adjusted for <__>."
  x = madlib(text)
  component1 = madlib_string("mortality")
  component2 = madlib_string("age")
  output = fill(x, component1, component2)
  expect_equal(output, "The model for mortality was adjusted for age.")
})
