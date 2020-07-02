# * Author:    Bangyou Zheng (Bangyou.Zheng@csiro.au)
# * Created:   06/04/2010
# *

#' String function in this package
#'
#' Get the length of a string
#' @rdname string
#' @param str Input string
#' @return The character number of the string
len <- function( str )
{
    return( length( unlist( strsplit( str, "" ) ) ) )
}

#' String function in this package
#'
#' Get the several characters of a string from left
#' @rdname string
#' @param str Input string
#' @param num The number of character will be returned from left
#' @return the character vector of the string from left
left <- function( str, num = 1 )
{
    return( substr( str, 1, num ) ) 
}

#' String function in this package
#'
#' Get the several characters of a string from right
#' @rdname string
#' @param str Input string
#' @param num The number of character will be returned from right
#' @return the character vector of the string from right
right <- function( str, num = 1 )
{
    len =  len( str )
    return( substr( str, len - num + 1, len ) )
}

#' String function in this package
#'
#' Omit the blank of a string
#' @rdname string
#' @param str Input string
#' @return the character vector except the blank
omitBlank <- function( str )
{
    str <- unlist( strsplit( str, "" ) )
    str[str==" "] <- ""
    res <- NULL
    for ( i in 1:length(str) )
    {
        res <- paste( res, str[i], sep = "" )
    }
    return( res )
}
#' String function in this package
#'
#' Search a character from a string
#' @rdname string
#' @param str Input string
#' @param search The character will be searched
#' @param start The start position to search
#' @return The postion where character firstly appear 
searchChar <- function( str, search, start = 1 )
{
    str <- unlist( strsplit( str, "" ) )
    pos <- NULL
    for ( i in start:length(str) )
    {
        if ( str[i] == search )
        {
            pos <- i
            break
        }
    }
    return( pos )
}

#' String function in this package
#'
#' Omit the start and end blank charcter
#' @rdname string
#' @param str Input string
#' @return the character vector except the blank at the start and end
omitBlankSE <- function( str )
{
    str <- unlist( strsplit( str, "" ) )
    for ( i in 1:length(str) )
    {
        if ( str[i] != " " )
        {

            str <- str[ i:length( str ) ]
            break
        }
        
    }
    for ( i in length(str):1 )
    {
        if ( str[i] != " " )
        {
            str <- str[ 1:i ]
            break
        }
        
    }
    res <- NULL
    for ( i in 1:length(str) )
    {
        res <- paste( res, str[i], sep = "" )
    }
    return( res )
}

#' Omit the start and end blank charcter
#'
#' Convert vertor to string with a separater
#' @rdname string
#' @param vector The input vector to convert
#' @param sep The separater with default value ", "
#' @return The string which connected all members of vector
vector2string <- function( vector, sep = ", " )
{
    s <- as.character( vector[1] )
    if ( length( vector ) > 1 )
    {
        for ( i in 2:length( vector ) )
        {
            s <- paste( s, vector[i], sep = sep )
        }
    }
    return( s )
}
