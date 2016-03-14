if (packageVersion("testthat") >= "0.7.1.99") {
  library("testthat")
  library("dplyr")
  test_check("rrrsa")
}
