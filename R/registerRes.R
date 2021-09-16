# * Author:    Bangyou Zheng (Bangyou.Zheng@csiro.au)
# * Created:   21/01/2011
# *

setGeneric( "registerRes", 
        function( object, name, type )
        {
            standardGeneric( "registerRes" )
        }
)

setMethod( f = "registerRes", 
        signature = c( object = "WeaAna", name = "character", type = "character" ),
        definition = function( object, name, type )
        {
            
            old.name <- object@result[[1]]$value@name
            old.type <- object@result[[1]]$value@type
            if ( is.null( name ) || is.na( name ) )
            {
                return
            }
            if ( !( name %in% old.name ) )
            {
                object@result[[1]]$value@name <- c( old.name, name )
                object@result[[1]]$value@type <- c( old.type, type )
            }
        }
)

setGeneric( "addResult", 
        function( object, an.res )
        {
            standardGeneric( "addResult" )
        }
)

setMethod( f = "addResult", 
        signature = c( object = "result", an.res = "result" ),
        definition = function( object, an.res )
        {
            old.name <- object@name
            old.type <- object@type
            an.name <- an.res@name
            an.type <- an.res@type
            pos <- ! ( an.name %in% old.name )
            object@name <- c( old.name, an.name[pos] )
            object@type <- c( old.type, an.type[pos] )
            return( object )
        }
)
