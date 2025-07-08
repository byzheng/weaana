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

test_that("Weather records with SILO standard format", {
  # Read standard apsim format with single file
  file <- system.file("extdata/ppd_72150.txt", package = "weaana")
  records <- readWeatherRecords(file, dataFormat = "STANDARD")
  site_info <- siteInfor(records)
  expect_equal(site_info$Number, "72150")

  file <- system.file("extdata/drill_-35_147.txt", package = "weaana")
  records <- readWeatherRecords(file, dataFormat = "STANDARD")
  site_info <- siteInfor(records)
  expect_equal(site_info$Latitude, -35)
  expect_equal(site_info$Longitude, 147)
})
