# * Author:    Bangyou Zheng (Bangyou.Zheng@csiro.au)
# * Created:   12/16/2010
# *

#' Show basic information of class WeaAna
#'
#' Show the name, number, latitude, longitude of all weather stations.
#' @docType methods
#' @param object WeaAna objects
#' @examples
#' library(weaana)
#' data( "WeatherRecordsDemo" ) 
#' show( records )
#' records
#' @importFrom methods show
setMethod( "show", signature = c(  object = "WeaAna" ),
        definition = function( object )
        {
            infor <- siteInfor( object )
            if ( !( is.null( infor ) & waGetPara( "load.later" )  ) )
            {
                print( infor )
            } else
            {
                msg <- paste( "There are ", object@num, " weather stations,",
                        " but they will be loaded later.", sep = "" )
                print( msg )
            }
        }
)

