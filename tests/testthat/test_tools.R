# ===================================================================== #
#  An R package by Certe:                                               #
#  https://github.com/certe-medical-epidemiology                        #
#                                                                       #
#  Licensed as GPL-v2.0.                                                #
# ===================================================================== #

test_that("plot2 tool arguments match the formals of the installed plot2()", {
  skip_if_not_installed("plot2")
  # ellmer::tool() validates the argument names against the formals of
  # plot2::plot2(). Building the tool against the installed plot2 catches
  # renamed or removed plot2 arguments before installation breaks.
  build <- get("build_tool_plot2", envir = asNamespace("certellm"))
  expect_error(build(), NA)
})
