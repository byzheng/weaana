#' Calculate Dew Point Temperature from Minimum Temperature and Relative Humidity
#'
#' @description 
#' This function estimates the dew point temperature (C) using the Magnus-Tetens approximation,
#' based on the minimum air temperature and relative humidity at that time.
#'
#' @param mint Numeric. Minimum air temperature in degrees Celsius.
#' @param minrh Numeric. Relative humidity (\%) at minimum temperature.
#'
#' @return Numeric. Dew point temperature in degrees Celsius.
#'
#' @details
#' The dew point temperature is calculated using the following equations:
#'
#' \deqn{
#' \gamma = \frac{a \cdot T}{b + T} + \ln\left(\frac{RH}{100}\right)
#' }
#' \deqn{
#' T_{dew} = \frac{b \cdot \gamma}{a - \gamma}
#' }
#'
#' where \eqn{a = 17.62} and \eqn{b = 243.12}.
#'
#' @examples
#' dp_mint(10, 80)  # Expected output: around 6.71
#'
#' @export
dp_mint <- function(mint, minrh) {
    stopifnot(is.numeric(mint), is.numeric(minrh))
    a <- 17.62
    b <- 243.12
    
    if (any(minrh <= 0 | minrh > 100, na.rm = TRUE)) {
        warning("Relative humidity values should be between 0 and 100.")
    }
    
    gamma <- (a * mint) / (b + mint) + log(minrh / 100)
    t_dew <- (b * gamma) / (a - gamma)
    
    return(t_dew)
}
