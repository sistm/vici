context("Selenium Test")

library(RSelenium)
library(testthat)

waitFor <- function(how,id){
  webElem <- NULL
  while(is.null(webElem)){
    webElem <- tryCatch({remDr$findElement(using = how, value = id)},
                        error = function(e){NULL})
    #loop until element with name <value> is found in <webpage url>
  }
  return(webElem)
}


remDr <- remoteDriver(browserName = "firefox",port=4455L) 
remDr$open(silent = FALSE)
remDr$setTimeout(type = "page load", milliseconds = 5000)
appURL <- "http://google.com"#"http://127.0.0.1:8080"
#app %<-% vici::run_app()


test_that("can connect to app", {
  #skip_on_cran()
  remDr$navigate(appURL)
  #sys.wa
  webElem <- waitFor("xpath","/html/body/div[2]/h2")#remDr$findElement(using = "xpath", value = "/html/body/div[2]/h2")
  textWebElem <- webElem$getElementText()
  expect_equal(as.character(textWebElem), "VICI: accurate estimation of Vaccine Induced Cellular Immunogenicity with bivariate modeling")  
})

test_that("Scénario standard example Data",{
  remDr$navigate(appURL)
  loadButton <- remDr$findElement(using = "id", value = "settings_pan_ui_1-loadExample")
  loadButton$clickElement()
  
  Arm <- waitFor("xpath","/html/body/div[2]/div/div[2]/div/div/div[2]/div[2]/div/table/tbody/tr[1]/td[5]")#remDr$findElement(using = "xpath", value = "/html/body/div[2]/div/div[2]/div/div/div[2]/div[2]/div/table/tbody/tr[1]/td[5]")
  expect_equal(as.character(Arm$getElementText()), "Placebo")
  Response1 <- remDr$findElement(using = "xpath", value = "/html/body/div[2]/div/div[2]/div/div/div[2]/div[2]/div/table/tbody/tr[1]/td[6]")
  expect_equal(as.character(Response1$getElementText()), "0.0801")
  
  fit <- remDr$findElement(using = "id", value = "modelfit_ui_1-fit")
  fit$clickElement()
  
  fRep <- waitFor("xpath","/html/body/div[2]/div/div[2]/div/div/div[1]/div/div[2]/div[2]/div/div/div[1]/div/div[2]/table/tbody/tr[1]/td[1]")#remDr$findElement(using = "xpath", value = "/html/body/div[2]/div/div[2]/div/div/div[1]/div/div[2]/div[2]/div/div/div[1]/div/div[2]/table/tbody/tr[1]/td[1]")
  expect_equal(as.character(fRep$getElementText()),"Average response in reference stimulation NS in reference arm Placebo")
  standardError1 <- remDr$findElement(using = "xpath", value = "/html/body/div[2]/div/div[2]/div/div/div[1]/div/div[2]/div[2]/div/div/div[1]/div/div[2]/table/tbody/tr[1]/td[3]")
  expect_equal(as.character(standardError1$getElementText()),"0.00760")
  pValue1 <- remDr$findElement(using = "xpath", value = "/html/body/div[2]/div/div[2]/div/div/div[1]/div/div[2]/div[2]/div/div/div[1]/div/div[2]/table/tbody/tr[1]/td[4]")
  expect_equal(as.character(pValue1$getElementText()),"0.00000")
  
  lRep <- remDr$findElement(using = "xpath", value = "/html/body/div[2]/div/div[2]/div/div/div[1]/div/div[2]/div[2]/div/div/div[1]/div/div[2]/table/tbody/tr[11]/td[1]")
  expect_equal(as.character(lRep$getElementText()),"Effect of arm A3 on response in stimulation S2")
  standardError2 <- remDr$findElement(using = "xpath", value = "/html/body/div[2]/div/div[2]/div/div/div[1]/div/div[2]/div[2]/div/div/div[1]/div/div[2]/table/tbody/tr[11]/td[3]")
  expect_equal(as.character(standardError2$getElementText()),"0.01200")
  pValue2 <- remDr$findElement(using = "xpath", value = "/html/body/div[2]/div/div[2]/div/div/div[1]/div/div[2]/div[2]/div/div/div[1]/div/div[2]/table/tbody/tr[11]/td[4]")
  expect_equal(as.character(pValue2$getElementText()),"0.72415")
  
})

remDr$close()
#rD$server$stop()