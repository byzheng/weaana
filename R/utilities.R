# * Author:    Bangyou Zheng (Bangyou.Zheng@csiro.au)
# * Created:   4:29 PM Friday, 15 February 2013
# * Copyright: AS IS
# *

# Transfer of sign - from FORTRAN.
# The result is of the same type and kind as a. Its value is the abs(a) of a,
# if b is greater than or equal positive zero; and -abs(a), if b is less than
# or equal to negative zero.
# Example a = sign_apsim (30,-2) ! a is assigned the value -30
# 
# @param a value 1
# @param b value 2
sign_apsim <- function( a, b ) {
    if ( b >= 0 )
    {
        return( abs( a ) )
    } else
    {
        return( -abs(a) )
    }
}

# Some utility functions for weather analysis
#' Significantly t-test with auto-correlation for time serial data 
#' 
#' Method is presented by Santer et al. 2000
#' @param y A vector of time serial data
#' @param slope Whether export slope
#' @return p values of t-test
#' @export
ttest_ts <- function(y, slope = NULL)
{
    if(sum(is.na(y)) == 0) 
    {
        y <- as.numeric(y)
        num <- length(y)
        x <- seq(along = y)
        if (is.null(slope))
        {
            slope <- stats::cor(x, y) * stats::sd(y)/stats::sd(x)
        }
       
        sb_m <- sqrt(sum((x - mean(x)) ^ 2))
        inercept <- (sum(y) - slope * sum(x)) / num
        et_x <- y - (inercept + slope * x)
        ne_x <- stats::cor(et_x[-1], et_x[-(num)])
        ne_x <- num * (1 - ne_x) / (1 + ne_x)
        se_x <- sqrt((1 / (ne_x - 2)) * sum(et_x * et_x, na.rm = TRUE))
        sb_x <- se_x / sb_m
        tb_x <- abs(slope / sb_x)
        p_x <- (1 - stats::pt(tb_x, df = ne_x - 2)) * 2
        return (p_x)
    } else 
    {
        return (NA)
    }
}


# Calculate the spatial slope and aspect
# 
# Burrough, P. A., and McDonell, R. A., 1998. Principles of Geographical Information Systems (Oxford University Press, New York)
# @param x A matrix for spatial data with row for longitude and column for latitude.
# dimnames must be specified for values of longitude and latitude
# @param slope Logical, whether return slope
# @param aspect Logical, whether return aspect
spatial <- function(x, slope = TRUE, aspect = TRUE)
{
    x_dim <- dim(x)
    x_template <- array(rep(NA, prod(x_dim)), dim = x_dim)
    f_a <- x_template
    f_a[seq(2, x_dim[1]), seq(2, x_dim[2])] <- x[-x_dim[1],-x_dim[2]]
    f_b <- x_template
    f_b[seq(2, x_dim[1]),] <- x[-x_dim[1],]
    f_c <- x_template
    f_c[seq(2, x_dim[1]), seq(1, x_dim[2] - 1)] <- x[-x_dim[1],-1]
    f_d <- x_template
    f_d[,seq(2, x_dim[2])] <- x[,-x_dim[2]]
    f_e <- x
    f_f <- x_template
    f_f[,seq(1, x_dim[2] - 1)] <- x[,-1]
    f_g <- x_template
    f_g[seq(1, x_dim[1] - 1), seq(2, x_dim[2])] <- x[-1,-x_dim[2]]
    f_h <- x_template
    f_h[seq(1, x_dim[1] - 1),] <- x[-1,]
    f_i <- x_template
    f_i[seq(1, x_dim[1] - 1), seq(1, x_dim[2] - 1)] <- x[-1,-1]
    
    x_dimnames <- dimnames(x)
    x_cellsize <- array(rep(111.325 * cos(as.numeric(x_dimnames[[2]]) * pi / 180) * 0.05, 
        times = length(x_dimnames[[1]])), dim = x_dim)
    y_cellsize <- array(rep(111.325, prod(x_dim)), dim = x_dim) * 0.05


    dz_dx <- ((f_c + 2 * f_f + f_i) - (f_a + 2 * f_d + f_g)) / (8 * x_cellsize)    
    dz_dy <- ((f_g + 2 * f_h + f_i) - (f_a + 2 * f_b + f_c)) / (8 * y_cellsize)
    
    slope_v <- sqrt(dz_dx * dz_dx + dz_dy * dz_dy)

    dimnames(slope_v) <- x_dimnames
    aspect_v <- (180 / pi) * atan2(dz_dy, -dz_dx) 
    dimnames(aspect_v) <- x_dimnames
    if (slope & aspect)
    {
        return(list(slope = slope_v, aspect = aspect_v))
    } else if (slope)
    {
        return (slope_v)
    } else if (aspect)
    {
        return (aspect_v)
    }
    return (NULL)
}

#' The time elapsed in hours between the specified sun angle 
#' from 90 degree in am and pm. +ve above the horizon, -ve below the horizon.
#' @param  doy day of year number    
#' @param lat latitude of site (deg) 
#' @param  angle angle to measure time between, such as twilight (deg).
#' angular distance between 90 deg and end of twilight - altitude of sun. +ve up, -ve down.
#' @return day length in hours
#' @export
dayLength <- function( doy, lat, angle = -6 )
{
    # Constant Values
    aeqnox <- 82.25
    dg2rdn <- ( 2.0 * pi ) / 360.0
    decsol <- 23.45116 * dg2rdn
    dy2rdn <- ( 2.0 * pi ) / 365.25
    rdn2hr <- 24.0 / ( 2.0 *pi )
    
    sun_alt <- angle * dg2rdn;
    dec <- decsol * sin( dy2rdn * ( doy - aeqnox ) )
    
    
    if ( ( abs( lat ) == 90.0 ) )
    {
        coshra <- rep( sign_apsim( 1.0, -dec) * sign_apsim( 1.0, lat ), 
                times = length( doy ) )
    } else 
    {
        latrn <- lat * dg2rdn
        slsd <- sin( latrn ) * sin( dec )
        clcd <- cos( latrn ) * cos( dec )
        
        altmn <- asin( min( max( slsd - clcd, -1.0 ), 1.0 ) )
        altmx <- asin( min( max( slsd + clcd, -1.0 ), 1.0 ) )
        alt <- min( max( sun_alt, altmn ), altmx )
        
        coshra <- (sin( alt ) - slsd ) / clcd
        coshra[coshra < -1]  <- -1
        coshra[coshra > 1]  <- 1
    }
    
    hrangl <- acos( coshra )
    hrlt <- hrangl * rdn2hr * 2.0
    return( hrlt )
}


#'Return a y value from a linear interpolation function
#'
#' @param x x
#' @param y y
#' @param values values
#' @param split split
#' @return The interpolated values
#' @export
interpolationFunction <- function( x, y, values, split = '\\s+' )
{
    if (is.character(x) & length(x) == 1)
    {
        x <- as.numeric(strsplit(x, split)[[1]])
    }
    if (is.character(y) & length(y) == 1)
    {
        y <- as.numeric(strsplit(y, split)[[1]])
    }
    res <- rep(NA, length(values))

    pos <- values < x[1]
    res[pos] <- y[1]

    for (i in seq(length = length(x) - 1))
    {
        pos <- values >= x[i] & values < x[i + 1]
        slope <- (y[i+1] - y[i] ) / (x[i+1] - x[i])
        res[pos] <- y[i] + slope * (values[pos] - x[i])
    }
    pos <- values >= x[length(x)]
    res[pos] <- y[length(y)]
    return ( res )
}



# Calculate weather variables through a string formula.
#
# @param x A data frame contained all weather records
# @param str A string function
# @param len The length of result
wcalStr <- function( x, str = NULL, len = length( x[[1]] ) )
{
    temp.env <- new.env()
    x.names <- names( x )
    for ( j in seq( along = x.names ) )
    {
        assign( x.names[j], x[[x.names[j]]], envir = temp.env )
    }
    res <- eval( parse( text = as.character( str ) ), envir = temp.env )
    
    if ( length( res ) != len )
    {
        stop( "The result length is not equal to original length" )
    }
    return( res )
}


# Calculate weather variables through a function.
#
# @param x A data frame contained all weather records
# @param FUN A function to be used which results should have the same length as original records.
# @param var.args Arguments of weather variable pass to \code{FUN}.
# @param other.args Optional arguments to \code{FUN}
# @param len The length of result
wcalFun <- function( x, FUN, var.args, other.args = NULL, len = length( x[[1]] ) )
{
    temp.env <- new.env()
    x.names <- names( x )
    for ( j in seq( along = x.names ) )
    {
        assign( x.names[j], x[[x.names[j]]], envir = temp.env )
    }
    
    fun.args <- as.list( NULL )
    for ( j in seq( along = var.args ) )
    {
        if ( var.args[j] %in% x.names )
        {
            fun.args[[j]] <- x[[var.args[j]]]
        } else
        {
            stop( paste( "Can not find variable ", var.args[j], sep = "" ) )
        }
    }
    fun.args <- c( fun.args, other.args )
    res <- do.call( FUN, fun.args )
    
    if ( length( res ) != len )
    {
        stop( "The result length is not equal to original length" )
    }
    return( res )
}