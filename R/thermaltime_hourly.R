#' @title Hourly Air Temperature Interpolation
#' @description Computes hourly estimates of air temperature (Ta) from daily Tmin, Tmax, and day length
#' using the method of Goudriaan (1994). The function assumes a sinusoidal temperature variation during
#' daylight hours and an exponential decay after sunset.
#'
#' @param tmin Numeric. Minimum temperature of the day (°C).
#' @param tmax Numeric. Maximum temperature of the day (°C).
#' @param tmax_b Numeric. Maximum temperature from the previous day (°C).
#' @param tmin_a Numeric. Minimum temperature of the following day (°C).
#' @param lat Numeric. Latitude of the location (degrees, -90 to 90).
#' @param doy Integer. Day of the year (1 to 366).
#'
#' @details
#' - **Daytime interpolation**: During sunlight hours, Ta is estimated using a sinusoidal curve fitted between Tmin and Tmax.
#' - **Nighttime interpolation**: After sunset, Ta follows an exponential decline from the sunset temperature to the next morning's Tmin.
#' - **Sunrise and Sunset Calculation**: 
#'   - Sunrise hour: `hsrise = 12 - d/2`
#'   - Sunset hour: `hsset = 12 + d/2`
#'   - Tmax occurs at hour = 13.5.
#' 
#' If the Controlled Environment module is used for weather data, the function takes hourly values from that instead of computing them.
#'
#' @return A numeric vector of length 24 containing the estimated hourly air temperatures (°C).
#'
#' @examples
#' tmin <- 10
#' tmax <- 20
#' tmax_b <- 21
#' tmin_a <- 11
#' lat <- -27
#' doy <- 10
#' hourly_temps <- tt_hourly_pp(tmin, tmax, tmax_b, tmin_a, lat, doy)
#'
#' @references Goudriaan, J. (1994). Advanced Methods for Calculating Radiation Models. 
#'             Netherlands Journal of Agricultural Science, 42(4), 315-323.
#'
#' @export
tt_hourly_pp <- function(tmin, tmax, tmax_b, tmin_a, lat, doy) {
    # Validate inputs
    if (!is.numeric(c(tmin, tmax, tmax_b, tmin_a, lat, doy))) {
        stop("All inputs must be numeric.")
    }
    if (lat < -90 || lat > 90) {
        stop("Latitude must be between -90 and 90 degrees.")
    }
    if (doy < 1 || doy > 366) {
        stop("Day of the year (doy) must be between 1 and 366.")
    }
    if (tmax < tmin) {
        stop("tmax must be greater than or equal to tmin.")
    }
    if (tmax_b < tmin_a) {
        stop("tmax_b must be greater than or equal to tmin_a.")
    }
    
    # Constants
    P <- 1.5  # Time offset factor
    TC <- 4.0 # Cooling time constant
    d <- dayLength(doy, lat) # Day length calculation
    hsrise <- 12 - d / 2 # Hour of sunrise
    hsset <- 12 + d / 2 # Hour of sunset
    
    hour <- seq(0, 23, by = 1) # Hourly time sequence
    n <- 24 - d # Night-time duration
    ta <- rep(NA, length(hour)) # Initialize temperature vector
    
    # Period A: Midnight to sunrise
    pos <- hour < hsrise
    tsset <- tmin + (tmax_b - tmin) * sin(pi * (d / (d + 2 * P)))
    ta[pos] <- (tmin - tsset * exp(-n / TC) +
                    (tsset - tmin) * exp(-(hour[pos] + 24 - hsset) / TC)) /
        (1 - exp(-n / TC))
    
    # Period B: Sunrise to Tmax peak
    pos <- hour >= hsrise & hour < (12 + P)
    ta[pos] <- tmin + (tmax - tmin) * sin(pi * (hour[pos] - hsrise) / (d + 2 * P))
    
    # Period C: Tmax peak to sunset
    pos <- hour >= (12 + P) & hour < hsset
    ta[pos] <- tmin_a + (tmax - tmin_a) * sin(pi * (hour[pos] - hsrise) / (d + 2 * P))
    
    # Period D: Sunset to midnight
    pos <- hour >= hsset
    tsset <- tmin_a + (tmax - tmin_a) * sin(pi * (d / (d + 2 * P)))
    ta[pos] <- (tmin_a - tsset * exp(-n / TC) +
                    (tsset - tmin_a) * exp(-(hour[pos] - hsset) / TC)) / 
        (1 - exp(-n / TC))
    
    return(ta)
}
