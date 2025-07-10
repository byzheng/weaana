# * Author:    Bangyou Zheng (Bangyou.Zheng@csiro.au)
# * Created:   12:58 Tuesday, 6 September 2011
# *

#' Write weather records into file
#' 
#' @param object A WeaAna object.
#' @param ... Not used
#' @docType methods
#' @rdname writeWeatherRecords-methods
setGeneric('writeWeatherRecords', 
        function(object, ...)
        {
            standardGeneric('writeWeatherRecords')
        }
)


#' Write weather records into file
#' 
#' @param object A WeaAna object.
#' @param file Path of output file.
#' @param cols Columns to export. All columns exported if NULL
#' @return No return values
#' @export
#' @rdname writeWeatherRecords-methods
#' @aliases writeWeatherRecords,WeaAna,WeaAna-method
setMethod(f = 'writeWeatherRecords', 
    signature = c(object = 'WeaAna'),
    definition = function(object, file, cols = NULL)
    {
        if (object@num != length(file))
        {
            stop(sprintf('The output files are not equal to %s', 
                object@num))
        }
        
        for (i in seq(length = object@num))
        {
            records <- getWeaAnaSiteByPos(object, i)
            records <- records$value
            
            if (!is.null(records@extra) && 
                !is.null(records@extra$rhmint) && 
                !is.null(records@extra$rhmaxt)) {
                var_name <- c('year', 'day', 'radn','maxt', 'mint', 'rain', 'evap', 'vp', "rhmint", "rhmaxt", 'code')
                var_unit <- c('()', '()', '(mj/m2)', '(oC)', '(oC)', '(mm)', '(mm)', '(hPa)', "(%)", "(%)", '()')
                var_width <- c(4, 5, 6, 6, 6, 6, 6, 6, 6, 6, 7)
                nsmall <- c(0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0)
                var_cols <- c('year', 'day', 'radn', 'maxt', 'mint', 'rain', 'evap', 'vp', "rhmint", "rhmaxt", 'code')
            } else {
                var_name <- c('year', 'day', 'radn','maxt', 'mint', 'rain', 'evap', 'vp', 'code')
                var_unit <- c('()', '()', '(mj/m2)', '(oC)', '(oC)', '(mm)', '(mm)', '(hPa)', '()')
                var_width <- c(4, 5, 6, 6, 6, 6, 6, 6, 7)
                nsmall <- c(0, 0, 0, 0, 0, 0, 0, 0, 0)
                var_cols <- c('year', 'day', 'radn', 'maxt', 'mint', 'rain', 'evap', 'vp', 'code')
            }
            if (!is.null(cols)) {
                cols <- unique(c('year', 'day', cols))
                pos <- var_cols %in% cols
                var_cols <- var_cols[pos]
                var_name <- var_name[pos]
                var_unit <- var_unit[pos]
                var_width <- var_width[pos]
                nsmall <- nsmall[pos]
            }
            
            res_str <- NULL
            res_str <- c(res_str, 
                sprintf('!station number = %s', 
                    records@number))
            res_str <- c(res_str, 
                sprintf('!station name = %s', 
                    records@name))
            res_str <- c(res_str, 
                sprintf('latitude = %s  (DECIMAL DEGREES)', 
                    format(records@latitude, nsmall = 2)))
            res_str <- c(res_str, 
                sprintf('longitude = %s  (DECIMAL DEGREES)', 
                    format(records@longitude, nsmall = 2)))
            res_str <- c(res_str, 
                sprintf('tav = %s (oC) ! Annual average ambient temperature', 
                    format(records@tav, nsmall = 2)))
            res_str <- c(res_str, 
                sprintf('amp = %s (oC) ! Annual amplitude in mean monthly temperature', 
                    format(records@amp, nsmall = 2)))

            res_str <- c(res_str, '') 
            
            values <- NULL
            pos <- NULL
            for (j in seq(along = var_cols))
            {
                if (var_cols[j] %in% c("rhmint", "rhmaxt")) {
                    v <- methods::slot(records, "extra")[[var_cols[j]]]
                } else {
                    v <- methods::slot(records, var_cols[j])
                }
                if (length(v) > 0)
                {
                    values[[var_cols[j]]] <- format(v, 
                        width = var_width[j], justify = 'right',
                        nsmall = nsmall[j])
                    pos <- c(pos, j)
                }
            }
            values <- as.data.frame(values)
            values <- apply(values, 1, FUN = function(x)
                {
                    return(paste(x, collapse = ' '))
                })
            var_name <- paste(format(var_name[pos], 
                width = var_width[pos], justify = 'right',
                nsmall = nsmall[pos]), collapse = ' ')
            var_unit <- paste(format(var_unit[pos], 
                width = var_width[pos], justify = 'right',
                nsmall = nsmall[pos]), collapse = ' ')
            res_str <- c(res_str, var_name, var_unit) 
            
            res_str <- c(res_str, values)
            writeLines(res_str, file[i])
        }
    }
)

