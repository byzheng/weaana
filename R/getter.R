# * Author:    Bangyou Zheng (Bangyou.Zheng@csiro.au)
# * Created:   07/01/2011
# *


#' Getter to access the weather data at a specific position.
#' 
#' Getter to access the weather data at a specific position.
#' @param x A WeaAna object.
#' @param i the specific position which will access.
#' @param j None use parameter.
#' @param drop None use parameter.
#' @return A WeaAnaSite object at the position i.
#' @examples
#' library(weaana)
#' data( "WeatherRecordsDemo" ) 
#' records[1]
#' records[1:2]
#' records[2:2]
#' 
#' @rdname getter
setMethod( f = "[", 
        signature = signature( "WeaAna" ),
        definition = function( x, i, j, drop )
        {
            sites.records <- NULL
            num <- 0
            i.len <- length( i )
            
            for ( k in 1:i.len )
            {
                if ( i[k] <= x@num &&  i[k] >= 1 )
                {
                    site.record <- x@records[i[k]][[1]]
                    site.record <- site.record$value
                    sites.records <- c( sites.records, newPointer( site.record ) )
                    num <- num + 1
                } else
                {
                    warning( paste( "\"", i[k], "\" out of bound, skip it.", sep = "" ) )
                }
                    
            }
            
            records <- methods::new( "WeaAna",
                    num = num,
                    records = sites.records,
                    result = x@result )
            return( records )
        }
)


# Get WeaAnaSite object by position
# 
# @param object A WeaAna object.
# @param ... Not used
# @docType methods
# @rdname getWeaAnaSiteByPos-methods
setGeneric( "getWeaAnaSiteByPos", 
        function( object, ... )
        {
            standardGeneric( "getWeaAnaSiteByPos" )
        }
)

# Plot site position in a map
#
# @docType methods
# @param object A WeaAna object.
# @param pos The position would be return.
# @examples
# library(weaana)
# data( "WeatherRecordsDemo" ) 
# getWeaAnaSiteByPos( records, 1 )
# @return A new WeaAna object at pos.
# @export
# @rdname getWeaAnaSiteByPos-methods
# @aliases getWeaAnaSiteByPos,WeaAna,WeaAna-method
setMethod( f = "getWeaAnaSiteByPos", 
        signature = c( object = "WeaAna" ),
        definition = function( object, pos )
        {
            if ( length( pos ) > 1 )
            {
                warning( "Only one element is supported. Other will be omitted." )
            }
            pos <- pos[1]
            if ( pos > object@num )
            {
                stop( "Subscript out of bound." )
            }
            records <- object@records[pos][[1]]
            return( records )
        }
)



# Get results by name
# 
# @param object A WeaAna object.
# @param ... Not used
# @docType methods
# @rdname getResults-methods
setGeneric( "getResults", 
        function( object, ... )
        {
            standardGeneric( "getResults" )
        }
)

# Get results by name
#
# @docType methods
# @param object A WeaAna object.
# @param name The results with these name are returned.
# @examples
# library(weaana)
# data( "WeatherRecordsDemo" ) 
# getResults( records, "result" )
# @return Results for weaana object
# @export
# @rdname getResults-methods
# @aliases getResults,WeaAna,WeaAna-method
setMethod( f = "getResults", 
        signature = c( object = "WeaAna" ),
        definition = function( object, name = NULL )
        {
            if ( is.null( name ) )
            {
                stop( "name argument must be specified." )
            }
            if ( length( name ) > 1 )
            {
                warning( "Only one element for name is supported. Other will be omitted." )
                name <- name[1]
            }
            type <- findResType( object, name )
            res <- NULL
            if ( is.null( type ) )
            { 
                warning( paste( "Result \"", name, "\" is not existed.", sep = "" ) )
                return( NULL )
            }
            if ( type == "data.frame" )
            {
                for ( i in 1:object@num )
                {
                    records <- getWeaAnaSiteByPos( object, i )
                    site.res <- records$value@res[[name]]    
                    res <- rbind( res, site.res )
                }
            }
            # TODO: For other data format
            res <- as.data.frame( res, stringsAsFactors = FALSE )
            row.names( res ) <- 1:length( res[[1]] )
            return( res )                        
        }
)

#' Get site information
#' 
#' @param object A WeaAna object.
#' @param ... Not used
#' @docType methods
#' @rdname siteInfor-methods
setGeneric( "siteInfor", 
        function( object, ... )
        {
            standardGeneric( "siteInfor" )
        }
)

#' Get site information
#'
#' @docType methods
#' @param object A WeaAna object.
#' @param load.now Whether load site information 
#' @examples
#' library(weaana)
#' data( "WeatherRecordsDemo" ) 
#' siteInfor( records )
#' siteInfor( records, load.now = TRUE )
#' @export
#' @return Site information in the WeaAna object
#' @rdname siteInfor-methods
#' @aliases siteInfor,WeaAna,WeaAna-method
setMethod( f = "siteInfor", 
        signature = c( object = "WeaAna" ),
        definition = function( object, load.now = FALSE )
        {
            res <- NULL
            if ( object@num < 1 )
            {
                return( res )
            }
            for ( i in 1:object@num )
            {
                records <- getWeaAnaSiteByPos( object, i )
                record <- records$value
                site.res <- siteInfor( record, load.now )
                if ( load.now )
                {
                    records$value@name <- site.res$Name
                    records$value@number <- site.res$Number
                    records$value@latitude <- site.res$Latitude
                    records$value@longitude <- site.res$Longitude
                }
                res <- rbind( res, site.res )
            }
            if ( !is.null( res ) ) res <- as.data.frame( res, stringsAsFactors = FALSE )
            return( res )
        }
)


#' Get site information
#'
#' @docType methods
#' @param object A WeaAnaSite object.
#' @param load.now Whether load site information 
#' @export
#' @return Site information in the WeaAnaSite object
#' @rdname siteInfor-methods
#' @aliases siteInfor,WeaAnaSite,WeaAnaSite-method
setMethod( f = "siteInfor", 
        signature = c( object = "WeaAnaSite" ),
        definition = function( object, load.now = FALSE )
        {
            if ( load.now & object@load.later )
            {
                object <-  readSite( object@file.path, 
                        dataFormat = object@data.format )
            }
            res <- NULL
            
            if ( length( object@name ) )
            {
                res$Name <- object@name
            } else if ( object@load.later )
            {
                res$Name <- "Load later"
            }
            if ( length( object@number ) )
            {
                res$Number <- object@number
            } else
            {
                res$Number <- as.character( NA )
            }
            if ( length( object@latitude ) ) 
            {
                res$Latitude <- object@latitude
            } else
            {
                res$Latitude <- as.numeric( NA ) 
            }
            if ( length( object@longitude ) )
            {
                res$Longitude <- object@longitude
            } else
            {
                res$Longitude <- as.numeric( NA )
            }
            res <- c( res, object@marker )
            
            res <- as.data.frame( res, stringsAsFactors = FALSE )
            return( res )    
        }
)

# Get name and type of results
# 
# @param object A WeaAna object.
# @param ... Not used
# @docType methods
# @rdname resultsInfor-methods
setGeneric( "resultsInfor", 
        function( object, ... )
        {
            standardGeneric( "resultsInfor" )
        }
)

# Get name and type of results
#
# @docType methods
# @param object A WeaAna object.
# @examples
# library(weaana)
# data( "WeatherRecordsDemo" ) 
# resultsInfor( records )
# 
# @export
# @return Information for results
# @rdname resultsInfor-methods
# @aliases resultsInfor,WeaAna,WeaAna-method
setMethod( f = "resultsInfor", 
        signature = c( object = "WeaAna" ),
        definition = function( object )
        {
            if ( is.null( object@result ) )
            {
                return( NULL )
            }
            name <- object@result[[1]]$value@name
            type <- object@result[[1]]$value@type
            res <- cbind( name = name, type = type )
            res <- as.data.frame( res, stringsAsFactors = FALSE )
            return( res )                        
        }
)


# Get result type by name
# 
# @param object A WeaAna object.
# @param name Result name.
# @docType methods
# @rdname findResType-methods
setGeneric( "findResType", 
        function( object, name )
        {
            standardGeneric( "findResType" )
        }
)

# Get result type by name
# 
# @param object A WeaAna object.
# @param name Result name.
# @export
# @rdname findResType-methods
# @aliases resultsInfor,WeaAna,WeaAna-method
setMethod( f = "findResType", 
        signature = c( object = "WeaAna", name = "character" ),
        definition = function( object, name )
        {
            if ( is.null( object@result ) )
            {
                return( NULL )
            }
            if ( length( name ) > 1 )
            {
                warning( "Only one element for name is supported. Other will be omitted." )
                name <- name[1]
            }
            old.name <- object@result[[1]]$value@name
            old.type <- object@result[[1]]$value@type
            res <- old.type[old.name %in% name]
            if ( length( res ) == 0 )
            {
                return( NULL )
            }
            return ( res )                
        }
)



#' Get all weather records by year range
#' 
#' @param object A WeaAna object.
#' @param ... Not used
#' @docType methods
#' @rdname getWeatherRecords-methods
setGeneric( "getWeatherRecords", 
        function( object, ... )
        {
            standardGeneric( "getWeatherRecords" )
        }
)

#' Get all weather records by year range
#' 
#' @param object A WeaAna object.
#' @param yrange Year range.
#' @param vars Variable
#' @param ... Other arguments
#' @examples
#' library(weaana)
#' data( "WeatherRecordsDemo" ) 
#' getWeatherRecords( records, yrange = c( 2008, 2009 ) )
#' getWeatherRecords( records, yrange = c( 2008, 2009 ), length = 10 )
#' 
#' @export
#' @return A data frame with all weather records
#' @rdname getWeatherRecords-methods
#' @aliases getWeatherRecords,WeaAna,WeaAna-method
setMethod( f = "getWeatherRecords", 
        signature = c( object = "WeaAna" ),
        definition = function( object, yrange = NULL, vars = "all", ... )
        {
            vars <- c( vars, waGetPara( "records.index" ) )
            fixed.vars <- c( waGetPara( "records.index" ),
                    waGetPara( "records.vars" ) )
            
            res <- NULL
            if ( object@num == 0 )
            {
                return( NULL )
            }
            for ( i in 1:object@num )
            {
                s.records <- getWeaAnaSiteByPos( object, i )
                s.record <- s.records$value
                s.extra <- as.list( NULL )
                site.res <- NULL
                if ( s.record@load.later )
                {
                    s.extra <- s.record@extra
                    s.record <-  readSite(s.record@file.path, 
                            dataFormat = s.record@data.format )
                    s.records$value@name <- s.record@name
                    s.records$value@number <- s.record@number
                    s.records$value@latitude <- s.record@latitude
                    s.records$value@longitude <- s.record@longitude
                }
                if ( object@num > 1 )
                {
                    site.res$name <- s.record@name
                    site.res$number <- s.record@number
                    site.res$latitude <- s.record@latitude
                    site.res$longitude <- s.record@longitude
                }
                for ( j in seq( along = fixed.vars ) )
                {
                    if ( "all" %in% vars | fixed.vars[j] %in% vars )
                    {
                        if (length(methods::slot(s.record, fixed.vars[j])) > 0 )
                        {
                            site.res[[fixed.vars[j]]] <- methods::slot(s.record, fixed.vars[j])
                        }
                    }
                }
                # for extra variables which calculate by read functions
                extra.names <- names( s.record@extra )
                for ( j in seq( along = extra.names ) )
                {
                    if ( "all" %in% vars | extra.names[j] %in% vars )
                    {
                        site.res[[extra.names[j]]] <- s.record@extra[[extra.names[j]]]
                    }
                }
                # for extra variables calculate ( load.later )
                ns.extra <- names( s.extra ) 
                
                for ( j in seq( along = s.extra ) )
                {
                    # string mode
                    if ( !is.null( s.extra[[j]]$fun.str ) )
                    {
                        site.res[[ns.extra[j]]] <- wcalStr( site.res, 
                                str = s.extra[[j]]$fun.str, len = length( site.res$day ) )
                    } else # function mode
                    {
                        site.res[[ns.extra[j]]] <- wcalFun( site.res, 
                                s.extra[[j]]$fun.name, 
                                s.extra[[j]]$var.args,
                                s.extra[[j]]$other.args,
                                len = length( site.res$day ) )
                    }
                }
                site.res <- as.data.frame( site.res, stringsAsFactors = FALSE )
                if ( !is.null( yrange ) )
                {
                    site.res <- ( site.res[ site.res$year %in% yrange, ] )
                }
                other.args <- list( ... )
                if ( !is.null( other.args$length ) )
                {
                    if ( other.args$length < 1 )
                    {
                        warnings( "length must be more than 0 and less than total records." )
                        other.args$length <- 1
                        
                    } else if ( other.args$length > nrow( site.res ) )
                    {
                        warnings( "length must be more than 0 and less than total records." )
                        other.args$length <- nrow( site.res )
                    }
                    site.res <- site.res[seq( other.args$length ),]
                }
                res <- rbind( res, site.res )
                rm( s.records, s.record, site.res )
            }
            res <- as.data.frame( res, stringsAsFactors = FALSE, seq( along = res[[1]] ) )
            return( res )
        }
)

#TODO: Change this function for load later
# Get a string which contains the basic information of weather station.
# 
# @param object A WeaAna object.
# @param ... Not used
setGeneric( "getStationInforByString", 
        function( object, ... )
        {
            standardGeneric( "getStationInforByString" )
        }
)

# Get a string which contains the basic information of weather station.
# 
# @param object A WeaAna object.
# @param yrange Year range.
# @export
setMethod( f = "getStationInforByString", 
        signature = c( object = "WeaAnaSite" ),
        definition = function( object, yrange = NULL )
        {
            if ( is.null( yrange )  )
            {
                yrange <- range( object@year )
            }
            sub.title = paste(    "Site No: ",
                    object@number, 
                    " Site Name: ",
                    object@name,
                    "\n (",
                    object@latitude,
                    " ",
                    object@longitude,
                    ", ",
                    paste( yrange, collapse = "-" ),
                    ")",
                    sep = ""
            )
            return( sub.title )
        }
)

#------------------------------------------------------------------------------
# Get available variables of weather records
# 
# @param object A WeaAna object.
# @param ... Not used
setGeneric( "getAvaiVars", 
        function( object, ... )
        {
            standardGeneric( "getAvaiVars" )
        }
)

# Get available variables of weather records
# 
# @param object A WeaAna object.
# @examples
# library(weaana)
# data( "WeatherRecordsDemo" ) 
# getAvaiVars( records )
# @export
setMethod( f = "getAvaiVars", 
        signature = c( object = "WeaAna" ),
        definition = function( object )
        {
            if ( object@num )
            {
                fixed.vars <- waGetPara( "records.vars" )
                records <- getWeaAnaSiteByPos( object, 1 )
                extra.names <- names( records$value@extra )
                return( c( fixed.vars, extra.names ) )    
            } else
            {
                return( NULL )
            }
        }
)

# Get available variables of weather records
# 
# @param object A WeaAnaSite object.
setMethod( f = "getAvaiVars", 
        signature = c( object = "WeaAnaSite" ),
        definition = function( object )
        {
            fixed.vars <- waGetPara( "records.vars" )
            extra.names <- names( object@extra )
            return( c( fixed.vars, extra.names ) )    
        }
)



#------------------------------------------------------------------------------
# Add marker for each site
# 
# @param object A WeaAna object.
# @param ... Not used
setGeneric( "addMarkers", 
        function( object, ... )
        {
            standardGeneric( "addMarkers" )
        }
)

# Add marker for each site
# 
# @param object A WeaAna object.
# @param ... marker arguments
# @examples
# library(weaana)
# data( "WeatherRecordsDemo" ) 
# addMarkers( records, model = c( "SILO", "A2" ) ) 
# 
# @export
setMethod( f = "addMarkers", 
        signature = c( object = "WeaAna" ),
        definition = function( object, ... )
        {
            c.args <- list( ... )
            name.args <- attr( c.args, "name")
            if ( is.null( name.args ) )
            {
                stop( "No arguments are specified." )
            }
            for ( i in 1:length( c.args ) )
            {
                if ( !nchar( name.args[i] ) )
                {
                    warning( paste( "No name for ", c.args[i], ". Skip it.", sep = "" ) )
                    next()
                }
                c.value <- c.args[[i]]
                if ( length( c.value ) != object@num )
                {
                    warning( paste( "The length of ", name.args[i], 
                                    " is not equal to station number. Replicated it.", sep = "") )
                    c.value <- rep( c.value, length = object@num )
                }
                for ( j in 1:object@num )
                {
                    s.records <- getWeaAnaSiteByPos( object, j )
                    s.records$value@marker[[name.args[i]]] <- c.value[j] 
                }
            }
        }
)


#------------------------------------------------------------------------------
# Delete marker for each site
# 
# @param object A WeaAna object.
# @param name A vector of name to remove
setGeneric( "delMarkers", 
        function( object, name )
        {
            standardGeneric( "delMarkers" )
        }
)

# Delete marker for each site
# 
# @param object A WeaAna object.
# @param name A vector of name to remove
# @examples
# library(weaana)
# data( "WeatherRecordsDemo" ) 
# addMarkers( records, model = c( "SILO", "A2" ) ) 
# delMarkers( records, name = "model" )
# 
# @export
setMethod( f = "delMarkers", 
        signature = c( object = "WeaAna" ),
        definition = function( object, name = NULL )
        {
            for ( i in 1:object@num )
            {
                s.records <- getWeaAnaSiteByPos( object, i )
                for ( j in seq( name ) )
                {
                    s.records$value@marker[[name[j]]] <- NULL    
                }
            }    
        }
)



#------------------------------------------------------------------------------
# Site number
# 
# @param object A WeaAna object.
# @param ... Not used
setGeneric( "siteNum", 
        function( object, ... )
        {
            standardGeneric( "siteNum" )
        }
)

# Site number
# 
# @param object A WeaAna object.
# @param ... marker arguments
# @examples
# library(weaana)
# data( "WeatherRecordsDemo" ) 
# siteNum( records )
# @export
setMethod( f = "siteNum", 
        signature = c( object = "WeaAna" ),
        definition = function( object, ... )
        {
            return( object@num )
        }
)



#------------------------------------------------------------------------------
# Get parameters
# @param name Variable name which will return. All parameters will return if NULL
# @examples
# library(weaana)
# data("records") 
# waGetPara( "yrange" )
# # for lower level parameters
# waGetPara( "extreme$maxt$hot.day$value" )
# @export
waGetPara <- function( name = NULL )
{
    weaana.glb.para <- defaultPara()

    if ( is.null( name ) )
    {
        return( weaana.glb.para )
    }
    
    s.name <- strsplit( name, "\\$" )[[1]]
    c.para <- weaana.glb.para
    for ( i in seq( s.name ) )
    {
        c.para <- c.para[[s.name[i]]]
        if ( is.null( c.para ) )
        {
            warning( "Can not found parameters. All parameters are returned." )
            return( weaana.glb.para )
        }
    }        
    return( c.para )        
}


#------------------------------------------------------------------------------
# Set parameters
# 
# @param ... Parameter arguments
# @export
setPara <- function( ... )
{
    settings::stop_if_reserved(...)
    WA_OPTIONS(...)
}



#------------------------------------------------------------------------------
#  Obtain version of package weaana
# 
# @export
weaanaVersion <- function( )
{
    infor <- utils::sessionInfo() 
    
    if ( is.null( infor$otherPkgs ) )
    {
        stop( "weaana package is not loaded" )
    }
    weaana.infor <- infor$otherPkgs$weaana
    
    if ( is.null( weaana.infor ) )
    {
        stop( "weaana package is not loaded" )
    }
    
    res <- NULL
    res$Version <- weaana.infor$Version
    
    vers <- as.numeric( strsplit( res$Version, ".", fixed = TRUE )[[1]] )
    res$Major <- vers[1]
    res$Minor <- vers[2]
    res$Build <- vers[3]
    res$Revision <- vers[4]
    return( res )
}



# Get filename
# 
# @param object A WeaAna object.
setGeneric("getFilenames", 
        function(object)
        {
            standardGeneric("getFilenames")
        }
)

# Get filename
#
# @docType methods
# @param object A WeaAna object.
# @export
setMethod(f = "getFilenames", 
        signature = c(object = "WeaAna"),
        definition = function(object)
        {
            res <- NULL
            if (object@num < 1)
            {
                return(res)
            }
            
            for (i in 1:object@num)
            {
                records <- getWeaAnaSiteByPos(object, i)
                record <- records$value
                res <- c(res, record@file.path)
            }
            return( res )
        }
)

