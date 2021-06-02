test_that("Weather records", {
  # Read standard apsim format with single file 
  file <- system.file("extdata/WeatherRecordsDemo1.met", package = "weaana")
  records <- readWeatherRecords(file)
  site_info <- siteInfor(records)
  expect_equal(site_info$Number, "0000001")
  
  # Read file with date columns
  file <- system.file("extdata/WeatherRecordsDemo3.met", package = "weaana")
  records <- readWeatherRecords(file)
  # Read multiple files
  
})
