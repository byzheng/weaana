# * Author:    Bangyou Zheng (Bangyou.Zheng@csiro.au)
# * Created:   14:40 Wednesday, 8 June 2011
# *


#' Calculate the moving extreme temperature by days
#' 
#' @param object A WeaAna object.
#' @param ... Not used
setGeneric( "movExtremeByDay", 
        function( object, ... )
        {
            standardGeneric( "movExtremeByDay" )
        }
)


#' Calculate the moving extreme temperature by days
#'
#' @docType methods
#' @param object A WeaAna object.
#' @param indices The data will be classify according the classes
#' @param period The periods for moving window
#' @param shift The method for calculating percentage of moving extreme temperature.
#' @param numdays The maximum days for statistics 
#' @param yrange The year range for statistics
#' @param res.name The name for result
#' @return The probability occurring a certain days extreme temperature
#' @export
setMethod( f = "movExtremeByDay", 
        signature = c( object = "WeaAna" ),
        definition = function( object, 
                               indices = waGetPara( "extreme" ),
                               period = waGetPara( "mov.window" ), 
                               shift = waGetPara( "shift" ), 
                               yrange = waGetPara( "yrange" ), 
                               numdays = waGetPara( "numdays" ),
                               res.name = "movExtremeByDay" )
        {
            # Check parameters
            if ( numdays == 0 )
            {
                return( NULL )
            }    
            
            res <- NULL
            for ( i in 1:object@num )
            {
                records <- getWeaAnaSiteByPos( object, i )
                record <- records$value
                w_data <- getWeatherRecords( object[i], yrange = yrange )
                year <- w_data$year
                day <- w_data$day
                day[day == 366] <- NA
                
                var_names <- names( indices )
                site_res <- NULL
                for ( j in seq( length = length( indices ) ) )
                {
                    value <- w_data[[var_names[j]]]

                    var_indices <- indices[[j]]
                    var_in_names <- names( var_indices )
                    for ( k in seq( length = length( var_indices ) ) )
                    {
                        this_index <- var_indices[[k]]
                        this_index_value <- rep( 0, length( day ) )
                        if ( is.null( this_index$more.or.less ) )
                        {
                            this_index$more.or.less <- 1
                        }
                        if ( this_index$more.or.less )
                        {
                            this_index_value[ value > this_index$value] <- 1
                        } else
                        {
                            this_index_value[ value < this_index$value] <- 1
                        }
                        
                        this_index_movday <- mov.sum( this_index_value, period, shift = shift )
                        
                        year_num <- tapply( this_index_movday, day, FUN = function( x ) sum( !is.na( x ) ) )
                        year_num[year_num==0] <- 1
                        
                        
                        mov_percent <- NULL
                        mov_percent$Name = record@name
                        mov_percent$Number = record@number
                        mov_percent$Latitude = record@latitude 
                        mov_percent$Longitude = record@longitude
                        mov_percent$Day <- seq( 1, 365 )    
                        mov_percent$Extreme <- var_in_names[k]
                        for ( m in seq( length = numdays ) )
                        {
                            index_name = paste( "per", as.character(m), "d", sep = "" )
                            mov_percent[[index_name]] <- as.numeric( tapply( this_index_movday, day, 
                                FUN = function(x) sum( x >= m, na.rm = TRUE ) ) /
                                year_num * 100 )
                        }
                        mov_percent[[paste( "daysMean", sep = "" )]] <- as.numeric( 
                                tapply( this_index_movday, day, 
                                        FUN = function(x) sum( x, na.rm = TRUE ) ) / year_num )
                        mov_percent[[paste( "daysSD", sep = "" )]] <- as.numeric( 
                                tapply( this_index_movday, day, 
                                        FUN = function(x) stats::sd( x, na.rm = TRUE ) ) / year_num )
                        mov_percent <- as.data.frame( mov_percent, stringsAsFactors = FALSE )
                        
                        site_res <- rbind( site_res, mov_percent )
                    }
                    
                }
                records$value@res[[res.name[j]]] <- site_res
                res <- rbind( res, site_res )
                
            }
            
            registerRes( object, res.name, "data.frame" )
            return( res )
        }
)


#' Calculate the moving extreme temperature by commulated temperature
#' 
#' @param object A WeaAna object.
#' @param ... Not used
setGeneric( "movExtremeByDegreeDay", 
        function( object, ... )
        {
            standardGeneric( "movExtremeByDegreeDay" )
        }
)

#' Calculate the moving extreme temperature by commulated temperature
#' 
#' @docType methods
#' @param object WeaAna object
#' @param indices The data will be classify according the classes
#' @param period The periods for moving window
#' @param shift The method for calculating percentage of moving extreme temperature.
#' @param numdays The maximum days for statistics 
#' @param yrange The year range for statistics
#' @param res.name Name of results
#' @param ... Other argument to calculate thermal time 
#' @export
setMethod( f = "movExtremeByDegreeDay", 
        signature = c( object = "WeaAna" ),
        definition = function( object, 
                indices = waGetPara( "extreme" ),
                period = 400, 
                shift = waGetPara( "shift" ), 
                yrange = waGetPara( "yrange" ), 
                numdays = waGetPara( "numdays" ),
                res.name = "movExtremeByDegreeDay", ...  )
        {
            # Check parameters
            if ( numdays == 0 )
            {
                return( NULL )
            }
            
            res <- NULL
            
            for ( i in 1:object@num )
            {
                records <- getWeaAnaSiteByPos( object, i )
                record <- records$value
                w_data <- getWeatherRecords( object[i], yrange = yrange )
                year <- w_data$year
                day <- w_data$day                
                day[day == 366] <- NA
    
                
                # Calculate the commulated temperature
                degree_days <- thermalTimeDaily( w_data$maxt, w_data$mint, ... )
                
                # numdays just for reducing computing time
                max_days <- maximumDays( degree_days, period )
                
                y <- stats::embed( degree_days, max_days )
                z <- NULL
                if ( shift == "centre" )
                {
                    midPoint <- ceiling( max_days / 2 )
                    z <- cbind( z, y[,midPoint] )
                    for ( j in (midPoint+1):max_days )
                    {
                        z <- cbind( z, z[,1] + y[,j] )
                        z <- cbind( z[,ncol(z)] + y[,midPoint - ( j - midPoint ) ], z )    
                    }
                } else if ( shift == "end" )
                {
                    z <- cbind( z, y[,1] )
                    for ( j in 2:max_days )
                    {
                        z <- cbind( z, z[, j - 1] + y[, j] )
                    }    
                } else
                {
                    z <- cbind( z, y[,max_days] )
                    for ( j in (max_days - 1):1 )
                    {
                        z <- cbind( z[, 1] + y[, j], z )
                    }
                }
                
                z[ z <= period ] <- 1
                z[ z > period ] <- 0
                
                
                var_names <- names( indices )
                site_res <- NULL
                for ( j in seq( length = length( indices ) ) )
                {
                    value <- w_data[[var_names[j]]]
                    
                    var_indices <- indices[[j]]
                    var_in_names <- names( var_indices )
                    for ( k in seq( length = length( var_indices ) ) )
                    {
                        this_index <- var_indices[[k]]
                        this_index_value <- rep( 0, length( day ) )
                        if ( is.null( this_index$more.or.less ) )
                        {
                            this_index$more.or.less <- 1
                        }
                        if ( this_index$more.or.less )
                        {
                            this_index_value[ value > this_index$value] <- 1
                        } else
                        {
                            this_index_value[ value < this_index$value] <- 1
                        }
                        
                        mm <- stats::embed( this_index_value, max_days )    
                        mm <- mm * z
                        this_index_movday <- NULL
                        if ( shift == "centre" )
                        {
                            shiftnum <- ceiling( max_days / 2 ) - 1
                            this_index_movday <- c( rep( NA, shiftnum ), apply( mm, 1, FUN = sum ), rep( NA, max_days - shiftnum - 1 ) )
                        } else if ( shift == "end" )
                        {
                            this_index_movday <- c( rep( NA, max_days - 1 ), apply( mm, 1, FUN = sum ) )
                        } else
                        {
                            this_index_movday <- c( apply( mm, 1, FUN = sum ), rep( NA, max_days - 1 ) )
                        }
                        
                        mov_percent <- NULL
                        
                        mov_percent$Name = record@name
                        mov_percent$Number = record@number
                        mov_percent$Latitude = record@latitude 
                        mov_percent$Longitude = record@longitude
                        mov_percent$Day <- seq( 1, 365 )    
                        mov_percent$Extreme <- var_in_names[k]
                        
                        year_num <- tapply( this_index_movday, day, FUN = function( x ) sum( !is.na( x ) ) )
                        year_num[year_num==0] <- 1
                        
                        for ( m in seq( length = numdays ) )
                        {
                            index_name = paste( "per", as.character(m), "d", sep = "" )
                            mov_percent[[index_name]] <- as.numeric( tapply( this_index_movday, day, 
                                        FUN = function(x) sum( x >= m, na.rm = TRUE ) ) /
                                year_num * 100 )
                        }
                        mov_percent[[paste( "daysMean", sep = "" )]] <- as.numeric( 
                                tapply( this_index_movday, day, 
                                        FUN = function(x) sum( x, na.rm = TRUE ) ) / year_num )
                        mov_percent[[paste( "daysSD", sep = "" )]] <- as.numeric( 
                                tapply( this_index_movday, day, 
                                        FUN = function(x) stats::sd( x, na.rm = TRUE ) ) / year_num )
                        mov_percent <- as.data.frame( mov_percent, stringsAsFactors = FALSE )
                        site_res <- rbind( site_res, mov_percent )
                    
                    }
                    
                }
                records$value@res[[res.name[j]]] <- site_res
                res <- rbind( res, site_res )
            }
            registerRes( object, res.name, "data.frame" )
            return( res )
        }
)

#' Calculate the maximum days which degree days is more than "key.degree.day"
#' @param degree.day A vector of accumulated temperature
#' @param key.degree.day The periods for moving window
#' @return The probability occuring a certain days extreme temperature
maximumDays <- function( degree.day, key.degree.day = 400 )
{
    pos <- which.min( degree.day )
    t <- degree.day[pos]
    pre <- pos
    nex <- pos
    total.num <- length( degree.day )
    for ( i in 1:365 )
    {
        pre <- pre - 1
        nex <- nex + 1
        if ( pre > 0 )
        {
            t <- t + degree.day[pos-i]
            
        }
        if ( nex < total.num )
        {
            t <- t + degree.day[pos+i]
        }
        
        if ( t > key.degree.day ) 
        {
            break
        }
        
    }
    max_days <- nex - pre
    if ( max_days %% 2 == 1 )
    {
        max_days <- max_days + 4
    } else
    {
        max_days <- max_days + 5
    }
    return ( max_days )
}
