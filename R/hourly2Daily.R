# * Author:    Bangyou Zheng (Bangyou.Zheng@csiro.au)
# * Created:   1:08 PM Wednesday, 12 September 2012
# * Copyright: AS IS
# *

#' Convert hourly weather records for daily APSIM format
#'
#' @param records hourly weather records
#' @export
hourly2Daily <- function(records)
{
    names(records) <- tolower(names(records))
    records$year <-    format(records$date, '%Y')
    records$day <- DOY(records$date)
    records$dmy <- format(records$date, '%Y-%m-%d')
    
    res <- NULL
    res$year <- as.numeric(as.character(tapply(records$year, records$dmy, function(x) x[1])))
    res$day <- as.numeric(as.character(tapply(records$day, records$dmy, function(x) x[1])))
    res$maxt <- as.numeric(as.character(tapply(records$airt, records$dmy, max, na.rm = TRUE)))
    res$mint <- as.numeric(as.character(tapply(records$airt, records$dmy, min, na.rm = TRUE)))
    res <- as.data.frame(res)
    return(res)
}
