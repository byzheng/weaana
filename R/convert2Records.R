# * Author:    Bangyou Zheng (Bangyou.Zheng@csiro.au)
# * Created:   18/03/2011
# *

#' Convert a data frame to weaana class
#' @param infor A list or data frame of site information
#' @param records A data frame will convert to records
#' @return A new WeaAna object
#' @export 
convert2Records <- function( infor, records )
{
    d.names <- names( records )
    n.vars <- c( waGetPara( "records.index" ), waGetPara( "records.vars" ) )
    if( !identical( n.vars %in% d.names, rep( TRUE, length( n.vars ) ) ) )
    {
        stop( paste( "Records columns needed: ", paste( n.vars, collapse  = ", ") ) )
    }
    i.vars <- c( "Name", "Number", "Latitude", "Longitude" )
    if( !identical( i.vars %in% names( infor ), rep( TRUE, length( i.vars ) ) ) )
    {
        stop( paste( "Infor columns needed: ", paste( i.vars, collapse  = ", ") ) )
    }
    
    a <- NULL
    for ( i in seq ( along = n.vars ) )
    {
        a[[n.vars[i]]] <- records[[n.vars[i]]]
    }
    
    extra <- NULL
    extra$avgt <- ( a$maxt + a$mint ) / 2
    extra$vpd <- vpd.apsim( a$maxt, a$mint )
    record <- methods::new( "WeaAnaSite", name = infor$Name,
            number = infor$Number,
            latitude = as.numeric( infor$Latitude ),
            longitude = as.numeric( infor$Longitude ),
            year = a$year,
            day = a$day,
            radn = a$radn,
            maxt = a$maxt,
            mint = a$mint,
            rain = a$rain,
            evap = a$evap,
            vp = a$vp,
            code = as.character( a$code ),
            extra = extra,
            file.path = as.character( NA ),
            data.format = as.character( NA ),
            load.later = FALSE )
    
    result <- c(NULL, newPointer( methods::new( "result", 
                            name = as.character( NULL ), 
                            type = as.character( NULL ) ) ) ) 
    records <- methods::new( "WeaAna", 
            num = 1,
            records = c( NULL, newPointer( record ) ),
            result = result )
    return( records )
}
