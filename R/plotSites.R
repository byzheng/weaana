# * Author:    Bangyou Zheng (Bangyou.Zheng@csiro.au)
# * Created:   06/05/2010
# *

#' Plot site position in a map
#' @param object A WeaAna object.
#' @param ... Not used
setGeneric( "plotSites", 
        function( object, ... )
        {
            standardGeneric( "plotSites" )
        }
)


#' Plot site position in a map
#'
#' @docType methods
#' @param object A WeaAna object.
#' @param lines A vector of lines for maps boundaries. Australia map from oz Package will be showed.
#' @param sitename Whether to show site name.
#' @param col The colour of site symbol.
#' @param pch The symbol type of site. 
#' @export plotSites
#' 
#' @examples
#' library(weaana)
#' data( "WeatherRecordsDemo" ) 
#' plotSites( records )
setMethod( f = "plotSites", 
        signature = c( object = "WeaAna" ),
        definition = function( object, lines = ozRegion(), sitename = FALSE, col = "red", pch = 24, ... )
        {
            library(oz)
            library(grid)
            infor <- siteInfor( object )
            infor <- na.omit( infor )
            if ( length( infor[[1]] ) == 0 )
            {
                stop( "No coordinate data load or existed. Try it later.")
            }

            pushViewport( viewport( x = 0.5, y = 0.5, height = 1, width = 1, clip = "off" ) )
            grid.ozGrob( lines, ... )
            downViewport("ozvp")
            
            grid.points( infor$Longitude, infor$Latitude, 
                    pch = pch, gp = gpar( col = col, cex = 0.8 ) )
            if ( sitename )
            {
                grid.text( infor$name, 
                        x = unit( infor$Longitude, "native" ), 
                        y = unit( infor$Latitude, "native" ), 
                        just = "centre", 
                        gp = gpar( cex = 0.8, 
                                col = col ) )
            }
            upViewport(0)    
        }
)


#' Plot site position in a map
#'
#' @docType methods
#' @param object A data.frame object.
#' @param lines A vector of lines for maps boundaries. Australia map from oz Package will be showed.
#' @param sitename Whether to show site name.
#' @param col The colour of site symbol.
#' @param pch The symbol type of site. 
#' @export plotSites
#' 
#' @examples
#' library(weaana)
#' data( "WeatherRecordsDemo" ) 
#' plotSites( records )
setMethod( f = "plotSites", 
        signature = c( object = "data.frame" ),
        definition = function( object, lines = ozRegion(), sitename = FALSE, 
            col = "red", pch = 24, text.cex = 0.8, ... )
        {
            if ( length( object[[1]] ) == 0 )
            {
                stop( "No coordinate data load or existed. Try it later.")
            }
            library(oz)
            library(grid)
            pushViewport( viewport( x = 0.5, y = 0.5, height = 1, width = 1, clip = "off" ) )
            grid.ozGrob( lines, ... )
            downViewport("ozvp")
            
            grid.points( object$Longitude, object$Latitude, 
                    pch = pch, gp = gpar( col = col, cex = 0.8 ) )
            if ( sitename )
            {
                grid.text( object$Name, 
                        x = unit( object$Longitude, "native" ), 
                        y = unit( object$Latitude - 1, "native" ), 
                        just = "centre", 
                        gp = gpar( cex = text.cex, 
                                col = col ) )
            }
            a <- upViewport(0)    
        }
)
