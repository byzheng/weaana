# * Author:    Bangyou Zheng (Bangyou.Zheng@csiro.au)
# * Created:   21:35 Tuesday, 7 June 2011
# *

#' Calculate thermal time using cardinal temperatures
#' 
#' @param weather WeaAna object
#' @param x_temp The cardinal temperatures 
#' @param y_temp The effective thermal time
#' @param method The method to calculate thermal time. 
#' The default method is ( maxt + mint ) / 2 - base. 
#' The three hour temperature methods will be used if method = '3hr'
#' @return A data.frame with three columns: year, day and thermalTime.
#' @export
#' @examples 
#' met_file <- system.file("extdata/WeatherRecordsDemo1.met", package = "weaana")
#' records <- readWeatherRecords(met_file)
#' x_temp <- c(0, 26, 34)
#' y_temp <- c(0, 26, 0)
#' res <- thermalTime(records, x_temp, y_temp)
#' head(res)
#' res <- thermalTime(records, x_temp, y_temp, method = "3hr")
#' head(res)
thermalTime <- function(weather, x_temp, y_temp, method = NULL)
{
    if (class(weather) != "WeaAna") {
        stop("WeaAna class is required.")
    }
    res <- NULL
    for ( i in 1:weather@num )
    {
        w_data <- getWeatherRecords( weather[i])
        w_data <- w_data %>% 
            dplyr::mutate(thermalTime = thermalTimeDaily(
                .data$mint, 
                .data$maxt, 
                x_temp = x_temp, 
                y_temp = y_temp,
                method = method)
            )
        
        res[[i]] <- w_data %>% 
            dplyr::select(dplyr::all_of(c('year', 'day', 'thermalTime')))
    }
    res <- dplyr::bind_cols(res)
    return( res )
}



#' Calculate thermal time using cardinal temperatures
#'
#' @param maxt The maximum temperature
#' @param mint The minimum temperature
#' @param x_temp The cardinal temperatures 
#' @param y_temp The effective thermal time
#' @param method The method to calculate thermal time. 
#' The default method is ( maxt + mint ) / 2 - base. 
#' The three hour temperature methods will be usesd if method = '3hr'
#' @return The thermal time.
#' @export
#' @examples 
#' mint <- c(0, 10)
#' maxt <- c(30, 40)
#' x_temp <- c(0, 20, 35)
#' y_temp <- c(0, 20, 0)
#' thermalTimeDaily(mint, maxt, x_temp, y_temp)
#' thermalTimeDaily(mint, maxt, x_temp, y_temp, method = '3hr')
thermalTimeDaily <- function(mint, maxt, x_temp, y_temp,
                         method = NULL)
{
    if (!is.numeric(maxt) | !is.numeric(mint)) {
        stop("Numeric vector is required for mint and maxt.")
    }
    if (length(mint) != length(maxt)) {
        stop("mint and maxt require the same length.")
    }
    if (is.null(method)) {
        meant <- (maxt + mint) / 2
        tt <- interpolationFunction(x = x_temp, y = y_temp, values = meant)
        return (tt)
    } else if (method == "3hr") {
        pos <- mint > maxt
        if (sum(pos) > 0) {
            stop("Minimum temperature is more than maximum temperature.")
        }
        # Calculate the 3hour temperature
        hour <- seq(1, 8)
        frac <- 0.92105 + 0.1140 * hour - 0.0703 * 
            hour * hour + 0.0053 * hour * hour * hour
        mint2 <- matrix(rep(mint, times = 8), ncol = 8)
        maxt2 <- matrix(rep(maxt, times = 8), ncol = 8)
        frac2 <- matrix(rep(frac, each = length(mint)), ncol = 8)
        temp <- mint2 + (maxt2 - mint2) * frac2
        tt <- matrix(interpolationFunction(x = x_temp, y = y_temp, temp), ncol = 8)
        res <- apply(tt, 1, mean)
        return(res)
    } else {
        stop("Not implemented for method ", method)
    }
}



#' Calculate thermal time using the hourly temperature (non daily temperature)
#'
#' @param timestamp The timestamp of weather records
#' @param temperature The temperature 
#' @param x_temp The cardinal temperatures 
#' @param y_temp The effective thermal time
#'
#' @return A data frame with daily thermal time
#' @export
#' @examples 
#' met_file <- system.file("extdata/WeatherHourly.csv", package = "weaana")
#' hourly <- read.csv(met_file, as.is = TRUE) 
#' 
#' hourly$timestamp <- as.POSIXct(hourly$timestamp, format = "%Y-%m-%dT%H:%M:%SZ")
#' x_temp <- c(0, 20, 35)
#' y_temp <- c(0, 20, 0)
#' thermalTimeHourly(hourly$timestamp, hourly$temperature, x_temp, y_temp)
thermalTimeHourly <- function(timestamp, temperature, x_temp, y_temp)  {
    if (!("POSIXct" %in% class(timestamp))) {
        stop("POSIXct class is required for timestamp")
    }
    if (length(timestamp) != length(temperature)) {
        stop("Require the same length of timestamp and temperature")
    }
    if (sum(is.na(timestamp)) > 0) {
        stop("Missing values are found in the timestamp")
    }
    if (sum(is.na(temperature)) > 0) {
        stop("Missing values are found in the temperature")
    }
    
    # Calculate the time difference
    diff <- as.numeric(timestamp) - dplyr::lag(as.numeric(timestamp))
    # Assign the second to the first value
    diff[1] <- diff[2]
    res <- tibble::tibble(timestamp = timestamp, 
                          temperature = temperature, 
                          diff = diff) %>% 
        dplyr::mutate(date = as.Date(.data$timestamp)) %>% 
        dplyr::rowwise() %>% 
        dplyr::mutate(tt = interpolationFunction(
            x = x_temp, 
            y = y_temp, 
            values = .data$temperature)) %>% 
        dplyr::group_by(.data$date) %>% 
        dplyr::summarise(value = sum(.data$tt * .data$diff) / sum(.data$diff), 
                         .groups = "drop")
    res    
}

