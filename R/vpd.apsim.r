# * Author:    Scott Chapman (Scott.Chapman@csiro.au)
# * Created:   09/04/2010
# *

#' Calculate the vpd using apsim method
#' 
#' Calculate the vpd using apsim method
#' @param maxt The maximum temperature
#' @param mint The minimum temperature
#' @return vpd
#' @export
vpd.apsim <- function( maxt, mint )
{
    vpd <- 0.1 * 0.75 * ( 6.1078 * exp( ( 17.269 * maxt ) / ( 273.3 + maxt ) )
                   - 6.1078 * exp ( ( 17.269 * mint ) / ( 273.3 + mint ) ) )
    vpd[ vpd < 0.01 ] <- 0.01
    return( vpd )
}
