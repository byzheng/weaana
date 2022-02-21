#' Calculate weather variables through function or a string formula.
#' 
#' @docType methods
#' @param object A WeaAna objects.
#' @param ... Not used
#' @rdname wcal-methods
#' @export
setGeneric( "wcal", 
        function( object, ... )
        {
            standardGeneric( "wcal" )
        }
)

#' Calculate weather variables through function or a string formula.
#'
#' There are two modes to use \code{wcal}, function mode if \code{FUN} is not null, 
#' and string formula mode if \code{FUN} is NULL.
#' 
#' @param object A WeaAna objects.
#' @param FUN A function to be used which results should have the same length as original records.
#' @param ... Optional arguments to \code{FUN} in function mode. 
#' String formulas if \code{FUN} is NULL.
#' @param var.args Arguments of weather variable pass to \code{FUN}.
#' @param var.name Variable name is used if \code{FUN} is not NULL.
#' @rdname wcal-methods
#' @aliases wcal,WeaAna,WeaAna-method
#' @export
#' @examples
#' library(weaana)
#' data( "records" ) 
#' # Daily mean temperature
#' wcal( records, avgt2 = "( maxt + mint ) / 2" )
#' # Moving average temperature
#' wcal( records, FUN = mov.avg, var.args = "avgt", k = 5, shift = "begin", var.name = "mov.avg" )
setMethod( f = "wcal", 
        signature = c( object = "WeaAna" ),
        definition = function( object, FUN = NULL, ..., var.args = NULL, var.name = NULL )
        {
            load.later <- waGetPara( "load.later" )
            # check arguments
            funs <- NULL
            n.funs <- NULL
            if ( is.null( FUN ) )#' for string mode
            {
                # check functions
                funs <- list( ... )
                n.funs <- names( funs )
                if ( is.null( n.funs ) )
                {
                    stop( "NO function is defined." )
                }
                for ( i in seq( along = n.funs ) )
                {
                    if ( nchar( n.funs[i]) == 0 )
                    { 
                        stop( paste( "NO result name is defined for ", funs[i], sep = "" ) )
                    }
                }
            } else # for function mode
            {
                if ( is.null( var.args ) )
                {
                    stop( "var.args must be specified." )
                }
                if ( is.null( var.name ) )
                {
                    stop( "var.name must be specified." )
                }
                if ( !is.character( as.character( var.name ) ) )
                {
                    stop( "var.name must be character" )
                }
            }
            
            # check weather station number
            if ( object@num == 0 )
            {
                warning( "No weather records in this object." )
                return( NULL )
            }
            # for each weather station
            for ( i in 1:object@num )
            {
                # obtain all weather records
                records <- getWeaAnaSiteByPos( object, i )
                record <- records$value
                w.data <- NULL
                if ( !load.later )
                {
                    w.data <- getWeatherRecords( object[i] )
                }
                # for string mode
                if ( is.null( FUN ) )
                {    
                    for ( j in seq( along = n.funs ) )
                    {
                        if ( load.later )
                        {
                            if ( n.funs[j] %in% getAvaiVars( record ) )
                            {
                                warning( paste( n.funs[j], " existed, overwriting.", sep = "" ) )
                            }
                            records$value@extra[[n.funs[j]]] <- list( fun.str = funs[j] )
                        } else
                        {
                            res <- wcalStr( w.data, funs[j] )
                            if ( n.funs[j] %in% getAvaiVars( record ) )
                            {
                                warning( paste( n.funs[j], " existed, overwriting.", sep = "" ) )
                            }
                            records$value@extra[[n.funs[j]]] <- res
                        }
                    }
                } else # for fun mode
                {
                    if ( load.later )
                    {
                        if ( var.name %in% getAvaiVars( record ) )
                        {
                            warning( paste(  var.name, " existed, overwriting.", sep = "" ) )
                        }
                        records$value@extra[[var.name]] <- list( fun.name = FUN,
                                var.args = var.args,
                                other.args = list( ... ) )
                        # print( names( records$value@extra ) )
                    } else
                    {
                        fun.args <- as.list( NULL )
                        res <- wcalFun( w.data, FUN, var.args,    list( ... ) )
                        
                        if ( var.name %in% getAvaiVars( record ) )
                        {
                            warning( paste(  var.name, " existed, overwriting.", sep = "" ) )
                        }
                        records$value@extra[[var.name]] <- res
                    
                    }
                }
            }
        }
)


