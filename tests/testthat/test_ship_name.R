library(shinytest)
library(testthat)

context("Test shiny app")

# open shiny app
app <- ShinyDriver$new('.')


test_that("Throws an error when incorrect ship names", {
  
  # set ship type
  app$setInputs(ship_type = "High Special")
  
  # get output text
  first_ship_name <- app$getValue(name = "ship_name")
  
  # test
  expect_equal(first_ship_name, "RIVO")
})

# stop shiny app
app$stop()
