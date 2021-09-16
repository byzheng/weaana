# * Author:    Bangyou Zheng (Bangyou.Zheng@csiro.au)
# * Created:   01/02/2011
# *

# Calculate number of frost day
# 
# @param object A WeaAna object.
# @param ... Not used
setGeneric( "numberFrostDay", 
        function( object, ... )
        {
            standardGeneric( "numberFrostDay" )
        }
)

# Calculate number of frost day
#
# @docType methods
# @param object A WeaAna object.
# @param yrange The year range for statistics 
# @param stress The stress temperature for frost
# @export
setMethod( f = "numberFrostDay", 
        signature = c( object = "WeaAna" ),
        definition = function( object,
                stress = waGetPara( "extreme$mint$frost.night$value" ), 
                yrange = waGetPara( "yrange" ) )
        {
            res <- wapply( object, vars = "mint", period = "year", 
                    FUN = numberFrostDay, 
                    ARGS = list( numberFrostDay = list( stress = stress ) ),
                    yrange = yrange, res.name = c( "numberFrostDay" ) )
            return( res )
        }
)


# Calculate number of frost day
#
# @docType methods
# @param object A numeric object.
# @param stress The stress temperature for frost
# @export
setMethod( f = "numberFrostDay", 
        signature = c( object = "numeric" ),
        definition = function( object, stress = waGetPara( "extreme$mint$frost.night$value" ) )
        {
            return( sum( object < stress ) )
        }
)
