# * Author:    Bangyou Zheng (Bangyou.Zheng@csiro.au)
# * Created:   08/04/2010
# *


# Use Calculate the moving average. For compatibility only.
# 
# Note that for n = odd, can average at central period. If n = even, must average at
# end of period and then shift values
# @param x A vector to calculate moving average
# @param k The moving windows
# @param shift if shift = "centre", then values are shifted to centre. 
# if shift = "begin", then values are at begin of period.
# if shift = "end", then values are at end of period. 
# The default value (centre) will be used if shift is other value.
# @return The moving average of vector x at moving windows n
# @export
mov.avg <- function( x, k = 10, shift = "centre" )
{    
    return( mov( x, k, shift, "mean" ) ) 
}

# Calculate the moving maximum. For compatibility only.
# 
# @param x A vector to calculate moving maximum
# @param k The moving windows
# @param shift if shift = "centre", then values are shifted to centre.  
# if shift = "begin", then values are at begin of period.
# if shift = "end", then values are at end of period.
# The default value (centre) will be used if shift is other value.
# @return The moving maximum of vector x at moving windows k
# @export
mov.max = function( x, k, shift = "centre" ) 
{
    return( mov( x, k, shift, "max" ) ) 
}

# Calculate the moving minimum. For compatibility only.
# 
# @param x A vector to calculate moving minimum
# @param k The moving windows
# @param shift if shift = "centre", then values are shifted to centre.  
# if shift = "begin", then values are at begin of period.
# if shift = "end", then values are at end of period.
# The default value (centre) will be used if shift is other value.
# @return The moving minimum of vector x at moving windows k
# @export
mov.min = function( x, k, shift = "centre" ) 
{
    return( mov( x, k, shift, "min" ) ) 
}

# Calculate the moving sum. For compatibility only.
# 
# @param x A vector to calculate moving sum
# @param k The moving windows
# @param shift if shift = "centre", then values are shifted to centre. 
# if shift = "begin", then values are at begin of period.
# if shift = "end", then values are at end of period.
# The default value (centre) will be used if shift is other value.
# @return The moving sum of vector x at moving windows k
# @export
mov.sum = function( x, k, shift = "centre" ) 
{
    return( mov( x, k, shift, "sum" ) ) 
}

# Calculate the moving values
# 
# @param x A vector to calculate moving values
# @param k The moving windows
# @param shift if shift = "centre", then values are shifted to centre. 
# if shift = "begin", then values are at begin of period.
# if shift = "end", then values are at end of period.
# The default value (centre) will be used if shift is other value.
# @param fun The method to calculate moving values. Curruntly, only
# "mean", "max", "min", and "sum" are supported. A NULL will be returned
# for any other values
# @return The moving value of vector x at moving windows k. A NULL will be returned
# for any unsupported fun
# @export
mov <- function( x, k = 10, shift = "centre", fun = "mean" )
{    
    y <- NULL
    if ( sum( fun == c( "mean", "max", "min", "sum" ) ) )
    {
        y <- apply( stats::embed( x, k ), 1, fun )
        
        if ( shift == "begin" )
        {
            y <- c(  y, rep( NA, k - 1 ) )
        }
        else if ( shift == "end" )
        {
            y <- c(  rep( NA, k - 1 ), y )
        }
        else
        {    
            shiftnum <- ceiling( k / 2 ) - 1
            y <- c( rep( NA, shiftnum ), y, rep( NA, k - shiftnum - 1 ) )
        }
    }
    return( as.numeric( y ) ) 
}


