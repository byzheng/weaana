# * Author:    Bangyou Zheng (Bangyou.Zheng@csiro.au)
# * Created:   04/05/2010
# *

#' Get all file path from a vector of files and folders
#' 
#' @param dataFiles A vector of weather data files
#' @param dataFolders A vector of weather data folders
#' @param dataFormat The format for weather data files. 
#' "dataFroamt" should be One of "APSIM" and "RDATA". 
#' @return A vector of all file path
fileList <- function( dataFiles = NULL, dataFolders = NULL, dataFormat = "APSIM" )
{
    file.suffix <- NULL
    if ( dataFormat == "APSIM" )
    {
        file.suffix <- c( "MET", "met" )
    } else if ( dataFormat == "RDATA" )
    {
        file.suffix <- c( "RData" )
    } else if (dataFormat == 'GHCN')
    {
        file.suffix <- c('dly')
    }else
    {
        stop( paste( "Data format \"", dataFormat, "\" can not supported. ",
                        "Please use \"APSIM\", \"RDATA\" or \"GHCN\"as data format.", sep = "" ) )
    }    
    if ( !is.null( dataFolders ) )
    {
        for ( i in 1:length( dataFolders ) )
        {
            dataFiles <- c( dataFiles, 
                            list.files( dataFolders[i], 
                            full.names = TRUE ) )
        }
    }
    
    fileLists <- NULL
    if ( !is.null( dataFiles ) )
    {
        for ( i in 1:length( dataFiles ) )
        {
            if ( !file.exists( dataFiles[i] ) )
            {
                warning( paste( "File \"", dataFiles[i], 
                                "\" is not exists.", sep = "" ), 
                        call. = FALSE )
            }
            
            if ( right( dataFiles[i], nchar( file.suffix[1] ) ) %in% file.suffix )
            {
                fileLists <- c( fileLists, dataFiles[i] )        
            }
        }
    }
    
    if ( is.null( fileLists ) ) 
    { 
        stop( paste( "No file found with data format \"", dataFormat,
                "\" in the specified file and folder lists.", sep = "" ) ) 
    }
    
    
    return( unique( fileLists ) )
}

#' Get all file name from a vector of files
#' 
#' @param fileLists A vector of files
#' @return A vector of file name
siteList <- function( fileLists = NULL )
{
    siteLists <- NULL
    if ( !is.null( fileLists ) )
    {
        sites <- basename( fileLists )
        for ( i in 1:length( sites ) )
        {
            siteLists[i] <- left( sites[i], len( sites[i] ) - 4 )
        }
    }
    return( unique( siteLists ) )
}
