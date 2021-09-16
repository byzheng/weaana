# * Author:    Bangyou Zheng (Bangyou.Zheng@csiro.au)
# * Created:   09/02/2011
# *

# Statistics weather records by a certain period and function
#
# @param object A WeaAna objects.
# @param ... Not used
setGeneric( "wapply", 
        function( object, ... )
        {
            standardGeneric( "wapply" )
        }
)

# Statistics weather records by a certain period and function
#
# \code{wapply} is a powerful and flexible function to statistics weather records. 
# It could be used to calculate multiple variables with different functions 
# and different parameters for all levels in a certain period.
# 
# \code{vars} could be a vector to list all variables which need to calculate. 
# Available variables could be obtained through function \code{\link{getAvaiVars}}.
# 
# \code{period} could be one of "year", "month", "week", "day", or any number of day.
# It will be complicated to the same length of \code{vars}.
# 
# \code{FUN} could be a vector. \code{FUN} will be replicated to the same length as \code{vars}.
# Each \code{FUN} will be applied to each \code{vars} as the sequence of \code{vars}. 
# 
# Optional arguments \code{args} is a list which have the same length as \code{FUN}.
# Each element of \code{args} is the optional arguments for each \code{FUN}. 
# Each argument will be replicated to same length as level number which 
# determines by period. Then, each level will be passed to different parameter. 
# 
# @docType methods
# @param object A WeaAna objects.
# @param period A period to apply FUN. It could be one of "year", "month", "week", "day", or 
# any number of day.
# @param vars A variable vector to apply \code{FUN}.
# @param FUN A function vector to be applied.
# @param ARGS Optional arguments to \code{FUN}.
# @param site.ARGS Arguments for each site
# @param res.name A result name to store and return.
# @param yrange Year range to statistics.
# @param as.data.frame Logical, if TRUE, a data frame will be returned, but just a period supported.
# if FALSE, a list will be returned.
# @param extra A list or data.frame whose elements have the same length of sites
# @examples
# library(weaana)
# data( "records" ) 
# # calculate yearly mean temperature
# wapply( records, vars = "avgt", period = "year", FUN = mean )
# # yealy total rainfall
# wapply( records, vars = "rain", period = "year", FUN = sum )
# # yearly mean temperature
# wapply( records, vars = "avgt", period = "year", FUN = mean )
# # monthly mean temperature
# wapply( records, vars = "avgt", period = "month", FUN = mean )
# # 10 days mean temperature
# wapply( records, vars = "avgt", period = 10, FUN = mean )
# # yearly mean temperature, total raifall and mean radiation.
# wapply( records, vars = c( "avgt", "rain", "radn" ), 
#         period = "year", 
#         FUN = c( mean, sum ), 
#         res.name = c( "mean.avgt", "total.rain", "mean.radn" ) )
# # calculate thermal time from last frost day to first heat day for year range 1960 to 2009
# last.frost <- lastFrostDay( records, yrange = c( 1960, 2009 ) )
# first.heat <- firstHeatDay( records, yrange = c( 1960, 2009 ) )
# wapply( records, vars = "avgt", period = "year", FUN = function ( x, start, end, base = 0 )
# {
#             if ( is.na( start ) | is.na( end ) )
#             {
#                 return( NA )
#             } else
#             {
#                 x <- x[ seq( start, end )]
#                 x <- x[ x > base ] - base
#                 return( sum( x, na.rm = TRUE ) )
#             }
#         }, ARGS = list( list( start = last.frost[,6], end = first.heat[,6] ) ), 
#         res.name = "tt", yrange = c( 1960, 2009 ) )
setMethod( f = "wapply", 
        signature = c( object = "WeaAna" ),
        definition = function( object, vars, period, FUN, ARGS = NULL, site.ARGS = NULL,
                               res.name = "result",
                               yrange = waGetPara( "yrange" ), as.data.frame = FALSE,
                               extra = NULL)
        {
            # check parameters
            res <- NULL
            if ( as.data.frame == TRUE )
            {
                if ( length( period ) > 1 )
                {
                    stop( "Only one period supported for data frame results" )
                }
            } else
            {
                res <- as.list( NULL )
                for ( i in 1:length( vars ) )
                {
                    res[[i]] <- as.list( NULL )
                }
            }
            # res.name
            for ( i in seq( along = vars ) )
            {
                if ( is.na( res.name[i] ) )
                {
                    res.name[i] <- paste( "result", i, sep = "" )
                }
            }
            
            # Check extra
            if (!is.null(extra))
            {
                extra <- as.data.frame(extra)
            }
            
            # convert FUN to a list 
            # FUN
            if ( is.null( FUN ) )
            {
                stop( "FUN can not be NULL." )
            }
            t.fun <- as.list( NULL )
            if ( length( FUN ) > 1 )
            {    
                for ( i in seq( along = FUN ) )
                {
                    t.fun[[i]] <- FUN[[i]]
                }
            } else
            {
                t.fun[[1]] <- FUN
            }
            # make sure the length of FUN is same as vars
            t.fun <- rep( t.fun, length.out = length( vars ) )
            # period
            if ( length( period ) > 1 )
            {
                old.period <- period
                period <- NULL
                period[[1]] <- old.period
            }

            period <- rep( period, length.out = length( vars ) )
            period <- as.list( period )
            
            # ARGS and site.SRGS
            if ( !is.null( ARGS ) ) ARGS <- rep( ARGS, length.out = length( vars ) )
            if ( !is.null( site.ARGS ) ) 
            {
                for ( i in seq( site.ARGS ) )
                {
                    site.ARGS[[i]] <- rep( site.ARGS[[i]], length.out = object@num )
                }
            }
            
            # check weather station number
            if ( object@num == 0 )
            {
                warning( "No weather records in this object." )
                return( NULL )
            }
            
            # for each weather station
            used.args <- 0
            for ( i in 1:object@num )
            {
                # obtain all weather records
                records <- getWeaAnaSiteByPos( object, i )
                record <- records$value
                w.data <- getWeatherRecords( object[i], yrange = yrange, vars = vars )
                # Check variables, skip variables don't exist.
                n.vars <- names( w.data )
                
                for ( j in seq( along = vars ) )
                {
                    if ( !( vars[j] %in% n.vars ) )
                    {
                        warning( paste( "Variable(s) not exist, skip it:",
                                        paste( vars[j], collapse = ", " ) ) )
                        next()
                    }
                    # generate index according period.
                    key <- periodIndex( w.data$year, w.data$day, period[[j]] )
                    w.levels <- as.numeric( levels( as.factor( key ) ) )
                    
                    # check level number. go to next station when no levels need to calculate
                    if ( length( w.levels ) == 0 )
                    {
                        warning( "No any levels which need to calculate. Skip this site." )
                        next()
                    }
                    # generate basic information for results.
                    site.res <- NULL
                    site.res$Name = records$value@name
                    site.res$Number = records$value@number
                    site.res$Latitude = records$value@latitude 
                    site.res$Longitude = records$value@longitude
                    site.res[[as.character( period[[j]][1] )]] = w.levels
                    w.nlevels <- length( w.levels )
                    # for each levels
                    w.res <- NULL
                    w.args <- ARGS[[j]]
                    n.w.args <- names( w.args )
                    
                    for ( m in seq( along = w.args ) )
                    {
                        w.args[[m]] <- rep( w.args[[m]], length = used.args + w.nlevels )
                    }
                    
                    for ( k in 1:w.nlevels )
                    {
                        # get weather data of this level.
                        l.data <- as.numeric( w.data[ key == w.levels[k], vars[j] ] )
                        
                        l.args <- as.list( NULL )
                        l.args[[1]] <- l.data
                        for ( m in seq( along = w.args ) )
                        {
                            l.args[[n.w.args[m]]] <- w.args[[m]][k + used.args]    
                        }
                        n.site.args <- names( site.ARGS )
                        for ( m in seq( along = site.ARGS ) )
                        {
                            l.args[[n.site.args[m]]] <- site.ARGS[[m]][i]
                        }                        
                        # call function to obtain results
                        l.res <- do.call( as.function( t.fun[[j]] ), l.args )
                        
                        # Check results 
                        if ( length( l.res ) > 1 )
                        {
                            warning( "Only first result is used." )
                        }
                        w.res <- c( w.res, l.res[1] )
                    }
                    used.args <- used.args + w.nlevels
                    
                    site.res[[res.name[j]]] <- w.res
                    site.res <- as.data.frame( site.res, stringsAsFactors = FALSE )
                    if (!is.null(extra))
                    {
                        names_site_res <- c(names(site.res), names(extra))
                        site.res <- cbind(site.res, extra[i,])
                        names(site.res) <- names_site_res
                    }
                    row.names( site.res ) <- seq( along = site.res[[1]] )
                    records$value@res[[res.name[j]]] <- site.res
                    if ( as.data.frame == TRUE )
                    {
                        if ( is.null( res ) )
                        {
                            res <- site.res
                        } else
                        {
                            res <- cbind( res, site.res[[6]] )
                        }
                    } else
                    {
                        res[[j]] <- rbind( res[[j]], site.res  )
                    }
                }
            }
            if ( as.data.frame == TRUE )
            {
                res <- as.data.frame( res, stringsAsFactors = FALSE )
                names( res ) <- c( "Name", "Number", "Latitude", "Longitude", period[[1]][1], res.name )
                row.names( res ) <- seq( along = res[[1]] )
                return( res )
            } else
            {
                # save results
                n.res <- NULL
                for ( i in 1:length( vars ) )
                {
                    temp <- as.data.frame( res[[i]], stringsAsFactors = FALSE )
                    row.names( temp ) <- seq( along = temp[[1]] )
                    registerRes( object, res.name[i], "data.frame" )
                    n.res[[res.name[i]]] <- temp
                }            
                rm( res )
                gc()
                if ( length( vars ) == 1 )
                {
                    return( n.res[[1]] )
                }
                return( n.res )
            }
        }
)
