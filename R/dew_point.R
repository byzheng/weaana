#' @md
#' @title Estimate Dew Point Temperature from Minimum Air Temperature and Relative Humidity
#'
#' @description
#' Computes the dew point temperature (°C) using the **Magnus–Tetens approximation**,
#' a widely used empirical relationship between air temperature, relative humidity,
#' and the saturation vapor pressure of water vapor. The method provides a good
#' approximation for typical atmospheric conditions and is suitable for most
#' meteorological and agricultural applications.
#'
#' @param mint Numeric. Minimum air temperature (°C).
#' @param minrh Numeric. Relative humidity (%) corresponding to the minimum temperature.
#'
#' @return Numeric. Dew point temperature (°C).
#'
#' @details
#' The dew point temperature is estimated using the Magnus–Tetens equation:
#'
#' \deqn{
#' \gamma = \frac{a \cdot T}{b + T} + \ln\left(\frac{RH}{100}\right)
#' }
#' \deqn{
#' T_{dew} = \frac{b \cdot \gamma}{a - \gamma}
#' }
#'
#' where \eqn{T} is air temperature (°C), \eqn{RH} is relative humidity (%),
#' and the empirical constants are \eqn{a = 17.62} and \eqn{b = 243.12} (for water over a
#' liquid surface). This formulation is based on improvements to the original
#' Magnus equation proposed by **Alduchov and Eskridge (1996)**.
#'
#' For air temperatures below 0 °C, slightly different coefficients are sometimes
#' used to account for ice surfaces, but the above constants are generally suitable
#' for most temperature ranges encountered in agricultural or meteorological data.
#'
#' @references
#' Alduchov, O. A., & Eskridge, R. E. (1996). *Improved Magnus Form Approximation of Saturation Vapor Pressure*.
#' Journal of Applied Meteorology, 35(4), 601–609. \doi{10.1175/1520-0450(1996)035<0601:IMFAOS>2.0.CO;2}
#'
#' @examples
#' # Dew point for 10 °C and 80% RH
#' dp_mint(10, 80)
#'
#' # Vectorized input
#' dp_mint(c(5, 10, 15), c(90, 70, 50))
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
