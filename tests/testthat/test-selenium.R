context("Selenium Test")

library(RSelenium)
library(testthat)
library(wdman)

rD <- rsDriver(port = 4455L, browser = c("chrome", "firefox", "phantomjs",
                                   "internet explorer"), version = "latest", chromever = "latest",
        geckover = "latest", iedrver = NULL, phantomver = "2.1.1",
        verbose = FALSE, check = TRUE)

#pht <- phantomjs(port = 4567L)

remDr <- remoteDriver(browserName = "firefox",port=4455L)
remDr$open(silent = FALSE)
appURL <- "http://127.0.0.1:8080"
#vici::run_app()
test_that("can connect to app", {  
  remDr$navigate(appURL)
  webElem <- remDr$findElement(using = "xpath", value = "/html/body/div[2]/h2")
  textWebElem <- webElem$getElementText()
  #appTitle <- remDr$getTitle()[[1]]
  #cat("textWebElem => ")
  #cat(as.character(textWebElem),"\n")
  expect_equal(as.character(textWebElem), "VICI: accurate estimation of Vaccine Induced Cellular Immunogenicity with bivariate modeling")  
})

remDr$close()
#pht$stop()
rD[["server"]]$stop()