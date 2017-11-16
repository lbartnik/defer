expect_setequal <- function (object, expected, info = NULL, label = NULL, expected.label = NULL)
{
  lab_act <- testthat:::quasi_label(rlang::enquo(object), label)
  lab_exp <- testthat:::quasi_label(rlang::enquo(expected), expected.label)

  expect_equal(sort(object), sort(expected), info = info, label = lab_act, expected.label = lab_exp)
}