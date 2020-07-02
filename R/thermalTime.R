# * Author:    Bangyou Zheng (Bangyou.Zheng@csiro.au)
# * Created:   21:35 Tuesday, 7 June 2011
# *


#' Calculate thermal time
#'
#' @param maxt The maximum temperature
#' @param mint The minimum temperature
#' @param base The base temperature
#' @param method The method to calculate thermal time. 
#' The default method is ( maxt + mint ) / 2 - base. 
#' The three hour temperature methods will be usesd if method = '3hr'
#' @param opt The optimum temperature
#' @param max The maximum temperature
#' @export
thermalTime <- function( maxt, mint, base = 0, method = NULL,
        opt = NULL, max = NULL )
{
    meant <- ( maxt + mint ) / 2
    meant[meant < base] <- base
    if ( is.null( method ) )
    {
        return ( meant - base )
    }
    
    tt <- rep( 0, length( maxt ) )
    pos1 <- mint >= base & maxt <= opt
    tt[pos1] <- ( meant[pos1] - base ) / ( opt - base)
    pos2 <- ( maxt <= base )
    tt[pos2] <- 0.0
    
    pos <- !( pos1 | pos2 )
    
    h_maxt <- maxt[pos]
    h_mint <- mint[pos]
    h_tt <- rep( 0, sum( pos ) )
    for( hour in 1:8 )
    {
        tmfak <- 0.92105 + 0.1140 * hour - 0.0703 * 
                    hour * hour + 0.0053 * hour * hour * hour
        ttmp <- h_mint + tmfak * ( h_maxt - h_mint )
        h_pos <- ttmp > base & ttmp <= opt
        h_tt[h_pos] <- h_tt[h_pos] + 
                ( ( ttmp[h_pos] - base ) / ( opt - base ) ) / 8.0
        h_pos <- ttmp > opt & ttmp < max
        h_tt[h_pos] <- h_tt[h_pos] + ( 1.0 - ( ttmp[h_pos] - opt ) / 
                ( max - opt ) ) / 8.0    
    }
    tt[pos] <- h_tt
    return( tt * ( opt - base ) )
}
