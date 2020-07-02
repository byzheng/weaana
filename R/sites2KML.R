# * Author:    Bangyou Zheng (Bangyou.Zheng@csiro.au)
# * Created:   17/02/2011
# *

ge.symbols <- c( "info-i", "heliport", "donut", "open-diamond", "flag_maps", 
        "golf",    "horsebackriding", "horsebackriding", "police", "flag", 
        "placemark_circle", "firedept_maps", "sunny", "marina", 
        "polygon", "star", "square", "schools", 
        "coffee", "poi", "hospitals", "arrow", "movies", 
        "picnic", "toilets", "movies_maps", "camera_maps", 
        "shaded_dot", "woman", "man", "target", "cycling", 
        "parking_lot", "info", "info_circle", "snack_bar", 
        "trail", "dining", "webcam", "gas_stations", "star_maps", 
        "realestate_maps", "road_shield1_maps", "falling_rocks", 
        "triangle", "parks_maps.shadow", "subway", "bars_maps", 
        "firedept", "rail", "bus", "parks", "camera", 
        "snowflake_simple", "euro", "swimming", "church", 
        "caution", "hiker", "grocery", "sailing" )

#' Convert site position to KML file
#'
#' @param object A WeaAna object.
#' @param ... Not used
setGeneric( "sites2KML", 
        function( object, ... )
        {
            standardGeneric( "sites2KML" )
        }
)


#' Convert site position to KML file
#'
#' @docType methods
#' @param object A WeaAna object.
#' @param file The output file name.
#' @param name whether to show site name. 
#' @param symbos The symbol for Google earth.
#' @export
#' 
#' @examples
#' library( weaana )
#' data( "WeatherRecordsDemo" ) 
#' sites2KML( records )
setMethod( f = "sites2KML", 
        signature = c( object = "WeaAna" ),
        definition = function( object, file = "site.kml", name = TRUE, symbol = 1 )
        {
            site.infor <- siteInfor( object )
            sites2KML( site.infor, file, name, symbol )    
        }
)

#' Convert site position to KML file
#'
#' @docType methods
#' @param object A data.frame object.
#' @param file The output file name.
#' @param name whether to show site name. 
#' @param symbos The symbol for Google earth.
#' @export
#' 
#' @examples
#' library( weaana )
#' data( "WeatherRecordsDemo" ) 
#' sites2KML( records )
setMethod( f = "sites2KML", 
        signature = c( object = "data.frame" ),
        definition = function( object, file = "site.kml", name = TRUE, symbol = 1 )
        {
            symbol <- symbol[[1]]
            if ( is.numeric( symbol ) )
            {
                symbol <- ge.symbols[symbol]
            }
            
            # check data.frame
            n.site <- names( object )
            if ( !all( c( "Latitude","Longitude" ) %in% n.site   ) )
            {
                stop( "Data.frame must contain these four columns: Latitude, Longitude." )
            }
            if (!('Name' %in% n.site))
            {
                name = FALSE
            }
            
            object$Latitude <- as.numeric( as.character( object$Latitude ) )
            object$Longitude <- as.numeric( as.character( object$Longitude ) )
            kml <- NULL
            kml[1] <- "<?xml version=\"1.0\" encoding=\"UTF-8\" ?>"
            kml[2] <- "<kml xmlns=\"http://www.opengis.net/kml/2.2\" xmlns:gx=\"http://www.google.com/kml/ext/2.2\" xmlns:kml=\"http://www.opengis.net/kml/2.2\" xmlns:atom=\"http://www.w3.org/2005/Atom\">"
            kml[3] <- "\t<Document>"
            kml[4] <- paste( "\t\t<name>", file, "</name>", sep = "" )
            
            c.len <- length( kml )
            kml[c.len+1] <- "\t\t<Style id=\"ge_n\">"
            kml[c.len+2] <- "\t\t\t<IconStyle>"
            kml[c.len+3] <- "\t\t\t\t<scale>1.1</scale>"
            kml[c.len+4] <- "\t\t\t\t<Icon>"
            kml[c.len+5] <- paste( "\t\t\t\t\t<href>http://maps.google.com/mapfiles/kml/shapes/",
                            symbol, ".png</href>", sep = "" )
            kml[c.len+6] <- "\t\t\t\t</Icon>"
            kml[c.len+7] <- "\t\t\t\t<hotSpot x=\"20\" y=\"2\" xunits=\"pixels\" yunits=\"pixels\"/>"
            kml[c.len+8] <- "\t\t\t</IconStyle>"
            kml[c.len+9] <- "\t\t</Style>"
            
            c.len <- length( kml )
            kml[c.len+1] <- "\t\t<Style id=\"ge_h\">"
            kml[c.len+2] <- "\t\t\t<IconStyle>"
            kml[c.len+3] <- "\t\t\t\t<scale>1.3</scale>"
            kml[c.len+4] <- "\t\t\t\t<Icon>"
            kml[c.len+5] <- paste( "\t\t\t\t\t<href>http://maps.google.com/mapfiles/kml/shapes/",
                    symbol, ".png</href>", sep = "" )
            kml[c.len+6] <- "\t\t\t\t</Icon>"
            kml[c.len+7] <- "\t\t\t\t<hotSpot x=\"20\" y=\"2\" xunits=\"pixels\" yunits=\"pixels\"/>"
            kml[c.len+8] <- "\t\t\t</IconStyle>"
            kml[c.len+9] <- "\t\t</Style>"
            
            c.len <- length( kml )
            kml[c.len+1] <- "\t\t<StyleMap id=\"ge_style\">"
            kml[c.len+2] <- "\t\t\t<Pair>"
            kml[c.len+3] <- "\t\t\t\t<key>normal</key>"
            kml[c.len+4] <- "\t\t\t\t<styleUrl>#ge_n</styleUrl>"
            kml[c.len+5] <- "\t\t\t</Pair>"
            kml[c.len+6] <- "\t\t\t<Pair>"
            kml[c.len+7] <- "\t\t\t\t<key>highlight</key>"
            kml[c.len+8] <- "\t\t\t\t<styleUrl>#ge_h</styleUrl>"
            kml[c.len+9] <- "\t\t\t</Pair>"
            kml[c.len+10] <- "\t\t</StyleMap>"
            
            
            c.len <- length( kml )
            kml[c.len+1] <- "\t\t<Folder>"
            kml[c.len+2] <- paste( "\t\t\t<name>", basename( file ), "</name>", sep = "" )
            if (is.null(object$Description))
            {
                object$Description <- paste(object$Name, " (", object$Number, ")", sep ='')
            }
            for ( i in seq( along = object[[1]] ) )
            {
                c.len <- length( kml )
                c.site <- object[i,]
                if ( is.na( c.site$Longitude ) | is.na( c.site$Latitude ) )
                {
                    next
                }
                
                kml[c.len + 1] <- "\t\t\t<Placemark>"
                if ( name )
                {
                    kml[c.len + 2] <- paste( "\t\t\t\t<name>", c.site$Name,"</name>", sep = "" )
                } else
                {
                    kml[c.len + 2] <- paste( "\t\t\t\t<name></name>", sep = "" )
                }
                
                kml[c.len + 3] <- paste( "\t\t\t\t<description>",
                                c.site$Description, "</description>", sep = "" )
                kml[c.len + 4] <- "\t\t\t\t<LookAt>"
                c.len <- length( kml )
                kml[c.len + 1] <- paste( "\t\t\t\t\t<longitude>", c.site$Longitude, "</longitude>", sep = "" )
                kml[c.len + 2] <- paste( "\t\t\t\t\t<latitude>", c.site$Latitude, "</latitude>", sep = "" )
                kml[c.len + 3] <- "\t\t\t\t\t<altitude>0</altitude>"
                kml[c.len + 4] <- "\t\t\t\t\t<heading>-0.1069020979914499</heading>"
                kml[c.len + 5] <- "\t\t\t\t\t<tilt>0</tilt>"
                kml[c.len + 6] <- "\t\t\t\t\t<range>64448.52908438035</range>"
                kml[c.len + 7] <- "\t\t\t\t\t<altitudeMode>relativeToGround</altitudeMode>"
                kml[c.len + 8] <- "\t\t\t\t\t<gx:altitudeMode>relativeToSeaFloor</gx:altitudeMode>"
                kml[c.len + 9] <- "\t\t\t\t</LookAt>"
                kml[c.len + 10] <- "\t\t\t\t<styleUrl>#ge_style</styleUrl>"
                c.len <- length( kml )
                kml[c.len + 1] <- "\t\t\t\t<Point>"
                kml[c.len + 2] <- paste( "\t\t\t\t\t<coordinates>", c.site$Longitude,
                        ",", c.site$Latitude, "</coordinates>", sep = "" )
                kml[c.len + 3] <- "\t\t\t\t</Point>"
                kml[c.len + 4] <- "\t\t\t</Placemark>"                
            }
            c.len <- length( kml )
            kml[c.len + 1] <- "\t\t</Folder>"
            kml[c.len + 2] <- "\t</Document>"
            kml[c.len + 3] <- "</kml>"
            writeLines( kml, con = file )
        }
)

#' Show all Google Earth symbols
#'
#' @docType methods
#' @param object A data.frame object.
#' @param file The output file name.
#' @param whether to show site name. 
#' @export
#' 
#' @examples
#' sites2KML()
setMethod( f = "sites2KML", 
        signature = c( object = "missing" ),
        definition = function( object )
        {
            return( ge.symbols )
        }
)
