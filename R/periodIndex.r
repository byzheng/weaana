# * Author:    Bangyou Zheng (Bangyou.Zheng@csiro.au)
# * Created:   15/04/2010
# *

#' Calcualte the index according year and days at a specific period
#' 
#' @param year A vector of year
#' @param day A vector of year
#' @param period The periods to count
#' @return Generated index according to period
#' @export
periodIndex <- function( year, day, period )
{
    if ( length( period ) == length( year ) )
    {
        return( period )
    }
    keys <- NULL
    if ( period == "year" )
    {
        keys <- year
    } else if ( period == "month" )
    {
        keys <- as.numeric( format( as.Date( day - 1, 
                                origin = paste( year, "-01-01", sep = "" ) ), "%m" ) )
    } else if ( period == "week" )
    {
        keys <- ceiling( day / 7 )
        # We assume each year only have 52 weeks. the last one day (two days for leap years) was counted into the 52th week.
        keys[keys>=53] <- 52
    } else if ( period == "day" )
    {
        keys <- as.numeric( format( as.Date( day - 1, 
                                origin = paste( year, "-01-01", sep = "" ) ), "%d" ) )
    }else if ( period == "doy" )
    {
        keys <- day
    } else
    {
        keys <- ceiling( day / period )
    }
    return(keys)
}
