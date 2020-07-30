# * Author:    Bangyou Zheng (Bangyou.Zheng@csiro.au)
# * Created:   10:43 PM Wednesday, 2 January 2013
# * Copyright: AS IS
# *

#' Change weather records
#' 
#' @param object A WeaAna object.
#' @param ... Not used
setGeneric('changeWeatherRecords', 
        function(object, ...)
        {
            standardGeneric('changeWeatherRecords')
        }
)


#' Change weather records
#' 
#' @docType methods
#' @param object A WeaAna object.
#' @param ... New weather records
#' @export
setMethod(f = 'changeWeatherRecords', 
    signature = c(object = 'WeaAna'),
    definition = function(object, ...)
    {
        new_values <- list(...)
        if (object@num > 1)
        {
            stop('There are more than one met files')
        }
        records <- getWeaAnaSiteByPos(object, 1)
        new_cols <- names(new_values)
        records_row <- length(records$value@year)
        var_cols <- c('year', 'day', 'radn', 'maxt', 'mint', 'rain', 'evap', 'vp', 'code')
        if (!all(new_cols %in% var_cols))
        {
            stop(sprintf('Check new values for %s',
                paste(new_cols[!(new_cols %in% var_cols)], 
                    collapse = ', ')))
        }
        for (i in seq(along = new_values))
        {
            methods::slot(records$value, new_cols[i]) <- rep(new_values[[i]], 
                length.out = records_row)
        }
    }
)
