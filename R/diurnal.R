# * Author:    Bangyou Zheng (Bangyou.Zheng@csiro.au)
# * Created:  1:47 PM Monday, 13 August 2012
# * Copyright: AS IS
# *



#' Calculate the diurnal variation in air temperature with Parton and Logan, 1981
#'
#' Calculate the diurnal variation in air temperature.
#' Parton WJ, Logan JA (1981) A model for diurnal variation in soil and air temperature. 
#' Agricultural Meteorology, 23, 205–216.
#' Codes copied from APSIM Utilities.cpp
#' @param maxt maximum daily temperature
#' @param mint minimum daily temperature
#' @param hour hour from 1 to 24
#' @param latitude latitude in radians
#' @param A is the time lag in temperature after noon
#' @param B is coef that controls temperature decrease at night
#' @param C is the time lag for min temperature after sunrise
#' @examples
#' diurnalT(maxt = 20, mint = 10, doy  = 1, 
#'    hour = seq(from = 1, to = 23.99, by = 0.1), 
#'    latitude = -10, A = 1.5, B = 4, C = 1)
#' @export
diurnalT <- function(maxt, mint, doy, hour, latitude, A = 1.5, B = 4, C = 1)
{
    doy <- rep(doy, length = length(hour))
    # hour <- (as.numeric(datetime) - 
        # as.numeric(as.POSIXlt(format(datetime, '%Y-%m-%d')))) / 3600
    
    aDelt <- 0.4014 * sin(2 * pi * (doy - 77.0) / 365.0)
    temp1 <- 1.0 - (-tan(latitude) * (aDelt))^2.0
    temp1 <- sqrt(temp1)
    temp2 <- (-tan(latitude) * tan(aDelt))
    aHou <- atan2(temp1, temp2)
    ady <- (aHou / pi) * 24.0
    ani <- (24.0 - ady)
    bb <- 12.0 - ady / 2.0 + C
    be <- 12.0 + ady / 2.0

    temperature <- NULL
    for (i in seq(along = hour))
    {
        if(hour[i] >= bb[i] & hour[i] < be[i])
        {
            bbd <- hour[i] - bb[i]
            temperature <- c(temperature, 
                (maxt - mint) * sin((pi * bbd) / (ady[i] + 2 * A)) + mint)
        }else
        {
            if(hour[i] > be[i]) bbd <- hour[i] - be[i]
            if(hour[i] < bb[i]) bbd <- (24.0 - be[i]) + hour[i]
            ddy <- ady[i] - C
            tsn <- (maxt - mint) * sin((pi * ddy) / (ady[i] + 2 * A)) + mint
            temperature <- c(temperature, 
                mint + (tsn - mint) * exp(-B * bbd / ani[i]))
        }
    }
    return (temperature)
}

