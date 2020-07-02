# * Author:    Bangyou Zheng (Bangyou.Zheng@csiro.au)
# * Created:   15:13 Tuesday, 3 May 2011
# *

#' Calculate the sphere distance
#'
#' @param lat1 Latitude
#' @param lon1 Longitude
#' @param lat2 Latitude
#' @param lon2 Longitude
#' @return Distance in km
#' @export
sphericalDistance <- function( lat1, lon1, lat2, lon2 )
{
    lon1 <- lon1 * pi /180
    lat1 <- lat1 * pi /180
    lon2 <- lon2 * pi /180
    lat2 <- lat2 * pi /180
    dLat <- lat2 - lat1
    dLon <- lon1 - lon2

    a <- sin(dLat/2) * sin(dLat/2) +
            cos(lat1) * cos(lat2) * 
            sin(dLon/2) * sin(dLon/2)
    c <- 2 * atan2(sqrt(a), sqrt(1-a)) 
    d <- 6371 * c
    return( d )
}
