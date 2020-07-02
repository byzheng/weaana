# * Author:    Bangyou Zheng (Bangyou.Zheng@csiro.au)
# * Created:   15:16 Tuesday, 16 August 2011
# *

#' Calculate day of year according to date
#' 
#' @param Date a string or date vector.
#' @param format date format when Date is a string vector
#' @export
DOY <- function(Date, format = NULL)
{
    if (is.null(format))
    {
        format <- '%d/%m/%Y'
    }
    if (mode(Date) == 'character')
    {
        Date <- as.Date(Date, format = format)
    }
    if (mode(Date) == 'numeric' & 'Date' %in% class(Date))
    {
        pos <- !is.na(Date)
        last_year <- rep(NA, length(Date))
        last_year[pos] <- as.Date(paste(as.numeric(format(Date[pos],"%Y"))-1,
            "-12-31",sep=""))
        return(as.numeric(Date - last_year))
    } else if (mode(Date) %in% c("numeric", 'list') & "POSIXt" %in% class(Date)) {
        pos <- !is.na(Date)
        last_year <- as.POSIXlt(rep(NA, length(Date)))
        last_year[pos] <- as.POSIXlt(paste(as.numeric(format(Date[pos], 
            "%Y")) - 1, "-12-31", sep = ""))
        return(floor(as.numeric(Date - last_year)))
    }
    return (NULL)
}
