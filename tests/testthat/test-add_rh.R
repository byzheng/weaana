

test_that("Add relative humidity ", {
    
    expect_error(met_new <- add_rh(c(1, 2), c(3, 4)))
    expect_error(met_new <- add_rh(c(1, 2), c(3)))
    apsim <- system.file("extdata/ppd_72150.met", package = "weaana")
    standard <- system.file("extdata/drill_-35_147.txt", package = "weaana")
    
    
    met_new <- add_rh(apsim, standard)
    expect_equal(is.null(met_new$rhmint), FALSE)
    expect_equal(is.null(met_new$rhmaxt), FALSE)
    
    
    tmp_file <- tempfile(fileext = ".met")
    expect_no_error(met_new <- add_rh(apsim, standard, output = tmp_file))
    expect_error(met_new <- add_rh(apsim, standard, output = tmp_file))
    expect_no_error(met_new <- add_rh(apsim, standard, output = tmp_file, overwrite = TRUE))
    
    records2 <- readWeatherRecords(tmp_file) |> 
        getWeatherRecords()
    
    expect_equal(is.null(met_new$rhmint), FALSE)
    expect_equal(is.null(met_new$rhmaxt), FALSE)
    
    
    file.remove(tmp_file)
})
