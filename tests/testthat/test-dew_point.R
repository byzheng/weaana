test_that("dp_mint returns correct scalar value", {
    result <- dp_mint(10, 80)
    expect_equal(round(result, 2), 6.71)
})

test_that("dp_mint handles vector input", {
    tmin <- c(10, 15)
    rh <- c(80, 90)
    result <- dp_mint(tmin, rh)
    expect_equal(round(result, 2), c(6.71, 13.37))
})

test_that("dp_mint warns for out-of-range RH", {
    expect_warning(dp_mint(10, -10), 
                   "Relative humidity values should be between 0 and 100")
})

