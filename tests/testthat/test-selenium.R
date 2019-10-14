context("Selenium Test")

library(RSelenium)
library(testthat)
library(wdman)

#rD <- rsDriver(port = 4455L, browser = c("chrome", "firefox", "phantomjs",
#                                   "internet explorer"), version = "latest", chromever = "latest",
#        geckover = "latest", iedrver = NULL, phantomver = "2.1.1",
#        verbose = FALSE, check = TRUE)

pht <- phantomjs(port = 4567L)

remDr <- remoteDriver(browserName = "phantomjs",port=4567L)
remDr$open(silent = FALSE)
appURL <- "http://127.0.0.1:8080"
#vici::run_app()
test_that("can connect to app", {  
  remDr$navigate(appURL)
  appTitle <- remDr$getTitle()[[1]]
  expect_equal(appTitle, "VICI")  
})

remDr$close()
pht$stop()
#rD[["server"]]$stop()