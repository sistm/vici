context("test serv method")

library(golem)
library(vici)

test_that("load example data", {
  df <- vici::ICS_ex
  cat(str(df))
  expect_true(df["1",1] == 1 && as.character(df["1",4]) == "Placebo")
})