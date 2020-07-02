# * Author:    Bangyou Zheng (Bangyou.Zheng@csiro.au)
# * Created:   15/04/2010
# *

#' Judge a leap or common year
#' @param year A numeric of year
#' @return True for leap year or False for common year
leapYear <- function( year )
{
    return ( year %% 400 == 0 | ( year %% 4 == 0 & year %% 100 != 0 ) )
}
