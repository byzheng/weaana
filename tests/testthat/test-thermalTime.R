test_that("Thermal time", {
    # Read weather records
    met_file <- system.file("extdata/WeatherRecordsDemo1.met", package = "weaana")
    records <- readWeatherRecords(met_file)
    x_temp <- c(0, 26, 34)
    y_temp <- c(0, 26, 0)
    res <- thermalTime(records, x_temp, y_temp)
})
