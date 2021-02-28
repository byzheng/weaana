test_that("Thermal time", {
    options(digits = 20)
    # The daily thermal time
    mint <- c(0, 10)
    maxt <- c(30, 40)
    x_temp <- c(0, 20, 35)
    y_temp <- c(0, 20, 0)
    res <- thermalTimeDaily(mint, maxt, x_temp, y_temp)
    expect_equal(res, c(15, 13.3333333333))
    res <- thermalTimeDaily(mint, maxt, x_temp, y_temp, method = '3hr')
    expect_equal(res, c(9.279687499999, 8.7905625000000))
    
    met_file <- system.file("extdata/WeatherHourly.csv", package = "weaana")
    hourly <- read.csv(met_file, as.is = TRUE) %>%
        tibble() %>%
        mutate(timestamp = as.POSIXct(timestamp, format = "%Y-%m-%dT%H:%M:%SZ"))
    x_temp <- c(0, 20, 35)
    y_temp <- c(0, 20, 0)
    res <- thermalTimeHourly(hourly$timestamp, hourly$temperature, x_temp, y_temp)
    #expect_equal(res$value[1], 16.391951095089993)
    # Read weather records
    met_file <- system.file("extdata/WeatherRecordsDemo1.met", package = "weaana")
    records <- readWeatherRecords(met_file)
    x_temp <- c(0, 26, 34)
    y_temp <- c(0, 26, 0)
    res <- thermalTime(records, x_temp, y_temp)
    #expect_equal(res[1,3], 19.549999999)
    res <- thermalTime(records, x_temp, y_temp, method = "3hr")
    #expect_equal(res[1,3], 19.5499999999)
    
      
})
