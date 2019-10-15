context("Selenium Test")

library(RSelenium)
library(testthat)
library(future)


skip_on_cran()


#rD <- rsDriver(port = 4455L, browser = c("chrome", "firefox", "phantomjs",
#                                   "internet explorer"), version = "latest", chromever = "latest",
#        geckover = "latest", iedrver = NULL, phantomver = "2.1.1",
#        verbose = FALSE, check = TRUE)

rD <- RSelenium::rsDriver(
  browser = "firefox",
  extraCapabilities = list(
    "moz:firefoxOptions" = list(
      args = list('--headless')
    )
  )
)

#pht <- phantomjs(port = 4567L)

remDr <- rD$client#remoteDriver(browserName = "firefox",port=4455L)
remDr$open(silent = FALSE)
appURL <- "http://127.0.0.1:8080"
app %<-% vici::run_app()


test_that("can connect to app", {
  skip_on_cran()
  remDr$navigate(appURL)
  webElem <- remDr$findElement(using = "xpath", value = "/html/body/div[2]/h2")
  textWebElem <- webElem$getElementText()
  #appTitle <- remDr$getTitle()[[1]]
  expect_equal(as.character(textWebElem), "VICI: accurate estimation of Vaccine Induced Cellular Immunogenicity with bivariate modeling")  
})

remDr$close()
rD$server$stop()
#pht$stop()
#rD[["server"]]$stop()