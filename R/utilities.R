# * Author:    Bangyou Zheng (Bangyou.Zheng@csiro.au)
# * Created:   4:29 PM Friday, 15 February 2013
# * Copyright: AS IS
# *


# Some utility functions for weather analysis
#' Significantly t-test with autocorrelation for time serial data 
#' 
#' Method is presented by Santer et al. 2000
#' @param x A vector of time serial data
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
            slope <- cor(x, y) * sd(y)/sd(x)
        }
       
        sb_m <- sqrt(sum((x - mean(x)) ^ 2))
        inercept <- (sum(y) - slope * sum(x)) / num
        et_x <- y - (inercept + slope * x)
        ne_x <- cor(et_x[-1], et_x[-(num)])
        ne_x <- num * (1 - ne_x) / (1 + ne_x)
        se_x <- sqrt((1 / (ne_x - 2)) * sum(et_x * et_x, na.rm = TRUE))
        sb_x <- se_x / sb_m
        tb_x <- abs(slope / sb_x)
        p_x <- (1 - pt(tb_x, df = ne_x - 2)) * 2
        return (p_x)
    } else 
    {
        return (NA)
    }
}


#' Calculate the spatial slope and aspect
#' 
#' Burrough, P. A., and McDonell, R. A., 1998. Principles of Geographical Information Systems (Oxford University Press, New York)
#' @param x A matrix for spatial data with row for longitude and column for latitude.
#' dimnames must be specified for values of longitude and latitude
#' @param slope Logical, whether return slope
#' @param aspect Logical, whether return aspect
#' @export
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
    # slope_v <- atan(slope_v) * 180 / pi
    hist(slope_v)
    
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
    return (NULl)
}

#' The time elapsed in hours between the specified sun angle 
#' from 90 degree in am and pm. +ve above the horizon, -ve below the horizon.
#' @param  doy day of year number    
#' @param lat latitude of site (deg) 
#' @param  angle angle to measure time between, such as twilight (deg).
#' angular distance between 90 deg and end of twilight - altitude of sun. +ve up, -ve down.
#' @export
dayLength <- function( doy, lat, angle = 6 )
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
