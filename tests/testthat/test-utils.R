context("utils")

## rsa.informativity()
## -------------------
test_that("rsa.convertVecType returns valid values", {
  v1 <- seq(1, 5)
  v2 <- LETTERS[v1]
  v3 <- as.factor(v2)

  expect_equal(length(v1), length(rsa.convertVecType(v1, v2))) #! maintian length
  expect_equal(typeof(v2), typeof(rsa.convertVecType(v2, v1))) #! numeric to character
  expect_equal(typeof(v3), typeof(rsa.convertVecType(v3, v1))) #! numeric to factor
  expect_equal(typeof(v2), typeof(rsa.convertVecType(v2, v3))) #! factor to character
  expect_equal(typeof(v3), typeof(rsa.convertVecType(v3, v2))) #! character to factor
})

## rsa.normVec
## -----------
test_that("rsa.normVec returns valid values", {
  vec1 <- seq(1, 5)
  answer_vec1 <- seq(1, 5) / sum(seq(1, 5))
  bad_vec <- letters[v1]
  zero_vec <- rep(0, 100)
  neg_vec <- zero_vec; neg_vec[100] <- -100

  expect_error(rsa.normVec(bad_vec), "rsa.normVec expects positive numerical vector")
  expect_error(rsa.normVec(neg_vec), "rsa.normVec expects positive numerical vector")
  expect_equal(answer_vec1, rsa.normVec(vec1))
  expect_equal(rep(0, length(zero_vec)), rsa.normVec(zero_vec))
})

## rsa.renameCol
## -------------
test_that("rsa.renameCol correctly renames cols", {
  df <- data.frame(letters = letters[1:5], LETTERS = LETTERS[1:5], numbers = 1:5)

  expect_equal(c("letters", "UpperCase", "numbers"), names(rsa.renameCol(df, "LETTERS", "UpperCase")))
  expect_equal(c("lowerCase", "UpperCase", "numbers"), names(rsa.renameCol(df, c("letters", "LETTERS"), c("lowerCase", "UpperCase"))))
  expect_warning(rsa.renameCol(df, c("ben", "peloquin"), c("lowerCase", "UpperCase")), "Please review colnames passsed, no matches found.")
})
# undebug(rsa.renameCol)
# debug(rsa.renameCol)

## rsa.runDf
## ---------
test-that("rsa.runDf passed correct data", {

})

test_that("rsa.runDf returns correct values", {
  df <- data.frame(scales = c(rep("some_all", 10), rep("good_excellent", 10)),
                    stars = as.factor(rep(1:5, 4)),
                    words = c(rep("all", 5), rep("some", 5), c(rep("excellent", 5), rep("good", 5))),
                    speaker.p = c(rep(0.0, 4), 1.0,
                                  0.0, rep(0.25, 4),
                                  rep(0.0, 4), 1.0,
                                  0.0, rep(0.25, 4)),
                    pragmatics = c(0, 0, 0, 0.15, 0.85,
                                   0, 0.1, 0.25, 0.5, 0.15,
                                   0, 0, 0, 0.15, 0.85,
                                   0, 0.1, 0.25, 0.5, 0.15))
  df <- df %>%
    mutate(cost = stringr::str_length(words),
           priors = 0.20)
  badSemantics <- df %>%
    mutate("speaker.p" = as.character(speaker.p))

  expect_error(rsa.runDf(df, quantityVarName = "stars", semanticsVarName = "speaker.p", itemVarName = "wrds"),
               "Cannot find column specification for quantity OR semantics OR items")
  expect_error(rsa.runDf(df, quantityVarName = "stars", semanticsVarName = "speaker", itemVarName = "words"),
               "Cannot find column specification for quantity OR semantics OR items")
  expect_error(rsa.runDf(df, quantityVarName = "books", semanticsVarName = "speaker", itemVarName = "words"),
               "Cannot find column specification for quantity OR semantics OR items")
  expect_error(rsa.runDf(df, quantityVarName = "stars", semanticsVarName = "speaker.p", itemVarName = "words",
                         priorsVarName = "prior"), "Cannot find priors column")
  expect_error(rsa.runDf(df, quantityVarName = "stars", semanticsVarName = "speaker.p", itemVarName = "words",
                         costsVarName = "costs"), "Cannot find costs column")
  expect_error(rsa.runDf(badSemantics, quantityVarName = "stars", semanticsVarName = "speaker.p", itemVarName = "words",
                         costsVarName = "costs"), "runDf expects semantics to be a numeric quantity")
})
debug(rsa.runDf)
undebug(rsa.runDF)
