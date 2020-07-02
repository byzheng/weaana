# * Author:    Bangyou Zheng (Bangyou.Zheng@csiro.au)
# * Created:   09/02/2011
# *

#' Calculate weather variables through function or a string formula.
#'
#' @param object A WeaAna objects.
#' @param ... Not used
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
#' @docType methods
#' @param object A WeaAna objects.
#' @param FUN A function to be used which results should have the same length as original records.
#' @param ... Optional arguments to \code{FUN} in function mode. 
#' String formulas if \code{FUN} is NULL.
#' @param var.args Arguments of weather variable pass to \code{FUN}.
#' @param var.name Variable name is used if \code{FUN} is not NULL.
#' @examples
#' library(weaana)
#' data( "WeatherRecordsDemo" ) 
#' # Daily mean temperature
#' wcal( records, avgt = "( maxt + mint ) / 2" )
#' # Moving average temperature
#' wcal( records, FUN = mov.avg, var.args = "avgt", k = 5, shift = "begin", var.name = "mov.avg" )
#' @export
setMethod( f = "wcal", 
        signature = c( object = "WeaAna" ),
        definition = function( object, FUN = NULL, ..., var.args = NULL, var.name = NULL )
        {
            load.later <- waGetPara( "load.later" )
            # check arguments
            funs <- NULL
            n.funs <- NULL
            if ( is.null( FUN ) )# for string mode
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


#' Calculate weather variables through a string formula.
#'
#' @param x A data frame contained all weather records
#' @param str A string function
#' @param len The length of result
wcalStr <- function( x, str = NULL, len = length( x[[1]] ) )
{
    temp.env <- new.env()
    x.names <- names( x )
    for ( j in seq( along = x.names ) )
    {
        assign( x.names[j], x[[x.names[j]]], envir = temp.env )
    }
    res <- eval( parse( text = as.character( str ) ), envir = temp.env )
    
    if ( length( res ) != len )
    {
        stop( "The result length is not equal to original length" )
    }
    return( res )
}


#' Calculate weather variables through a function.
#'
#' @param x A data frame contained all weather records
#' @param FUN A function to be used which results should have the same length as original records.
#' @param var.args Arguments of weather variable pass to \code{FUN}.
#' @param other.args Optional arguments to \code{FUN}
#' @param len The length of result
wcalFun <- function( x, FUN, var.args,    other.args = NULL, len = length( x[[1]] ) )
{
    temp.env <- new.env()
    x.names <- names( x )
    for ( j in seq( along = x.names ) )
    {
        assign( x.names[j], x[[x.names[j]]], envir = temp.env )
    }
    
    fun.args <- as.list( NULL )
    for ( j in seq( along = var.args ) )
    {
        if ( var.args[j] %in% x.names )
        {
            fun.args[[j]] <- x[[var.args[j]]]
        } else
        {
            stop( paste( "Can not find variable ", var.args[j], sep = "" ) )
        }
    }
    fun.args <- c( fun.args, other.args )
    res <- do.call( FUN, fun.args )
    
    if ( length( res ) != len )
    {
        stop( "The result length is not equal to original length" )
    }
    return( res )
}
