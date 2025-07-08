test_that("Weather records", {
  # Read standard apsim format with single file
  file <- system.file("extdata/WeatherRecordsDemo1.met", package = "weaana")
  climates <- getWeatherRecords(readWeatherRecords(file))
  set.seed(1)
  n <- 10
  sowing <- rep(as.Date("1995-05-01"), 10)
  emergence <- rep(10, 10)
  heading <- NULL
  flowering <- runif(10) * 20 + 50
  maturity <- runif(10) * 20 + 100
  latitude <- -27
  res <- climate_by_stages(
    climates = climates,
    sowing = sowing,
    emergence = emergence,
    heading = heading,
    flowering = flowering,
    maturity = maturity,
    latitude = latitude
  )
  expect_error({
    res <- climate_by_stages(
      climates = climates,
      sowing = sowing,
      emergence = emergence,
      # heading = heading,
      # flowering = flowering,
      maturity = maturity,
      latitude = latitude
    )
  })
})
