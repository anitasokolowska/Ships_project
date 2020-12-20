library(shinytest)
library(testthat)

context("Test shiny app")

# open shiny app
app <- ShinyDriver$new(path = ".")


test_that("Throws an error when incorrect output", {
  
  # set character inputs
  app$setInputs(ship_type = "Cargo")
  app$setInputs(ship_name = "ADELE")
  
  # get output text
  html_output <- app$getValue(name = "meters_box")
  dist_value <- stringr::str_extract(html_output, "[0-9]{1,6}")
  
  # test
  expect_equal(dist_value, "1638")
})

# stop shiny app
app$stop()

