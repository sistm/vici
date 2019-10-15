context("Selenium Test")

#library(RSelenium)
#library(testthat)
#library(future)


#skip_on_cran()


#rD <- RSelenium::rsDriver(
#  browser = "firefox",
#  extraCapabilities = list(
#    "moz:firefoxOptions" = list(
#      args = list('--headless')
#    )
#  )
#)


#remDr <- rD$client#remoteDriver(browserName = "firefox",port=4455L)
#remDr$open(silent = FALSE)
#appURL <- "http://127.0.0.1:8080"
#app %<-% vici::run_app()


#test_that("can connect to app", {
#  skip_on_cran()
#  remDr$navigate(appURL)
#  webElem <- remDr$findElement(using = "xpath", value = "/html/body/div[2]/h2")
#  textWebElem <- webElem$getElementText()
#  expect_equal(as.character(textWebElem), "VICI: accurate estimation of Vaccine Induced Cellular Immunogenicity with bivariate modeling")  
#})

#remDr$close()
#rD$server$stop()