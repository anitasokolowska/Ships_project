library(shinytest)
library(testthat)

context("Test shiny app")

# open shiny app
app <- ShinyDriver$new('.')


test_that("Throws an error when incorrect app title", {

  # get app title
  title_text <- app$getTitle()

  # test
  expect_equal(title_text, "Exploration of the Marine data")
})


test_that("Throws an error when incorrect inputs or outputs", {

  # get app widgets
  widgets_list <- app$listWidgets()

  # test
  expect_equal(widgets_list[["input"]], c("ship_type", "ship_name"))
  expect_equal(widgets_list[["output"]], c("meters_box", "leaflet_map"))
})

# stop shiny app
app$stop()
