% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/fileList.R
\name{fileList}
\alias{fileList}
\title{Get all file path from a vector of files and folders}
\usage{
fileList(dataFiles = NULL, dataFolders = NULL, dataFormat = "APSIM")
}
\arguments{
\item{dataFiles}{A vector of weather data files}

\item{dataFolders}{A vector of weather data folders}

\item{dataFormat}{The format for weather data files. 
"dataFroamt" should be One of "APSIM" and "RDATA".}
}
\value{
A vector of all file path
}
\description{
Get all file path from a vector of files and folders
}
