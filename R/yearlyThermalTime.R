# * Author:    Bangyou Zheng (Bangyou.Zheng@csiro.au)
# * Created:   06/01/2011
# *

#' Calculate yearly thermal time
#' 
#' @param object A WeaAna object.
#' @param ... Not used
setGeneric( "yearlyThermalTime", 
        function( object, ... )
        {
            standardGeneric( "yearlyThermalTime" )
        }
)

#' Calculate yearly thermal time
#'
#' @docType methods
#' @param object A WeaAna object.
#' @param yrange The year range for statistics 
#' @param day.start The start day to calculate thermal time
#' @param day.end The end day to calculate thermal time
#' @param base.temperature The base temperature to calculate the degree days
#' @export
setMethod( f = "yearlyThermalTime", 
        signature = c( object = "WeaAna" ),
        definition = function( object, 
                yrange = waGetPara( object, "yrange" ),
                base.temperature = waGetPara( "base.temperature" ),
                day.begin = waGetPara( "day.begin" ), 
                day.end = waGetPara( "day.end" )  )
        {
            res <- wapply( object, vars = "avgt", period = "year", 
                    FUN = yearlyThermalTime, 
                    ARGS = list( yearlyThermalTime = list( base.temperature = base.temperature,
                                    day.begin = day.begin,
                                    day.end = day.end ) ),
                    yrange = yrange, res.name = c( "yearlyThermalTime" ) )
            
            return( res )
        }
)


#' Calculate yearly thermal time
#'
#' @docType methods
#' @param object A numeric object.
#' @param day.start The start day to calculate thermal time
#' @param day.end The end day to calculate thermal time
#' @param base.temperature The base temperature to calculate the degree days
#' @export
setMethod( f = "yearlyThermalTime", 
        signature = c( object = "numeric" ),
        definition = function( object, 
                base.temperature = waGetPara( "base.temperature" ),
                day.begin = waGetPara( "day.begin" ), 
                day.end = waGetPara( "day.end" )  )
        {
            if ( is.na( day.begin ) | is.na( day.end ) )
            {
                return( NA )
            } else if ( ( day.end - day.begin < 0 ) )
            {
                warning( "day.end must not less than day.begin" )
                return( NA )
            } else
            {
                object <- object[ seq( day.begin, day.end )]
                object <- object[ object > base.temperature ] - base.temperature
                return( sum( object, na.rm = TRUE ) )
            }
        }
)
