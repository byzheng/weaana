# * Author:    Bangyou Zheng (Bangyou.Zheng@csiro.au)
# * Created:   02/12/2010
# *

#' Convert met file to RData files
#' 
#' @param dataFiles A character vector to specify the path of weather data files.
#' @param dataFolders A character vector to specify the path of weather data folders. 
#' @param outputFolder All RData files would be put into this folder. The default position
#' @param dataFormat The format of weather data file. 
#' is the original folder
#' @export 
Met2RData <- function( dataFiles = NULL, dataFolders = NULL, 
        outputFolder = NULL, dataFormat = "APSIM"
        )
{
    
    station.list <- fileList( dataFiles, dataFolders, dataFormat )
    
    if ( !is.null( outputFolder ) )
    {
        dir.create( outputFolder , showWarnings = FALSE )
    }
    for ( i in 1:length( station.list ) )
    {
        # READ File        
        records <-  readWeatherRecords( station.list[i], dataFormat = dataFormat )
        
        file.name <- basename( station.list[i] )
        file.name <- paste( substr( file.name, 1, nchar( file.name ) - 3 ), "RData", sep = "" )
        res.file.path <- NULL
        if ( is.null( outputFolder ) )
        {
            res.file.path <- file.path( dirname( station.list[i] ), file.name )
        } else
        {
            res.file.path <- file.path( outputFolder, file.name )
        }
        
        save( records, file = res.file.path )
    }
    
}



