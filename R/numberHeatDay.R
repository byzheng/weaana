# * Author:    Bangyou Zheng (Bangyou.Zheng@csiro.au)
# * Created:   01/02/2011
# *

# Calculate number of heat day
# 
# @param object A WeaAna object.
# @param ... Not used
setGeneric( "numberHeatDay", 
        function( object, ... )
        {
            standardGeneric( "numberHeatDay" )
        }
)

# Calculate number of heat day
#
# @docType methods
# @param object A WeaAna object.
# @param yrange The year range for statistics 
# @param stress The stress temperature for heat
# @export
setMethod( f = "numberHeatDay", 
        signature = c( object = "WeaAna" ),
        definition = function( object,
                stress = waGetPara( "extreme$maxt$hot.day$value" ), 
                yrange = waGetPara( "yrange" ) )
        {
            res <- wapply( object, vars = "maxt", period = "year", 
                    FUN = numberHeatDay, 
                    ARGS = list( numberHeatDay = list( stress = stress ) ),
                    yrange = yrange, res.name = c( "numberHeatDay" ) )
            
            return( res )
        }
)

# Calculate number of heat day
#
# @docType methods
# @param object A numeric object. 
# @param stress The stress temperature for heat
# @export
setMethod( f = "numberHeatDay", 
        signature = c( object = "numeric" ),
        definition = function( object, stress = waGetPara( "extreme$maxt$hot.day$value" ) )
        {
            return( sum( object > stress ) )
        }
)
