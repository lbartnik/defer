expect_setequal <- function (object, expected, info = NULL, label = NULL, expected.label = NULL)
{
  lab_act <- testthat:::make_label(object, label)
  lab_exp <- testthat:::make_label(expected, expected.label)
  
  expect_equal(sort(object), sort(expected), info = info, label = lab_act, expected.label = lab_exp)
}