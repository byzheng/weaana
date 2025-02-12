library(testthat)

test_that("tt_hourly_pp returns a numeric vector of length 24", {
    tmin <- 10
    tmax <- 20
    tmax_b <- 21
    tmin_a <- 11
    lat <- -27
    doy <- 10
    
    result <- tt_hourly_pp(tmin, tmax, tmax_b, tmin_a, lat, doy)
    
    expect_type(result, "double")   # Ensure result is numeric
    expect_length(result, 24)       # Ensure result has 24 hourly values
})

test_that("tt_hourly_pp handles invalid inputs", {
    expect_error(tt_hourly_pp("ten", 20, 21, 11, -27, 10))
    expect_error(tt_hourly_pp(10, 20, 21, 11, -100, 10))
    expect_error(tt_hourly_pp(10, 20, 21, 11, -27, 400))
})
