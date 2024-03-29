% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/wcal.R
\docType{methods}
\name{wcal}
\alias{wcal}
\alias{wcal,WeaAna-method}
\alias{wcal,WeaAna,WeaAna-method}
\title{Calculate weather variables through function or a string formula.}
\usage{
wcal(object, ...)

\S4method{wcal}{WeaAna}(object, FUN = NULL, ..., var.args = NULL, var.name = NULL)
}
\arguments{
\item{object}{A WeaAna objects.}

\item{...}{Optional arguments to \code{FUN} in function mode. 
String formulas if \code{FUN} is NULL.}

\item{FUN}{A function to be used which results should have the same length as original records.}

\item{var.args}{Arguments of weather variable pass to \code{FUN}.}

\item{var.name}{Variable name is used if \code{FUN} is not NULL.}
}
\description{
There are two modes to use \code{wcal}, function mode if \code{FUN} is not null, 
and string formula mode if \code{FUN} is NULL.
}
\examples{
library(weaana)
data( "records" ) 
# Daily mean temperature
wcal( records, avgt2 = "( maxt + mint ) / 2" )
# Moving average temperature
wcal( records, FUN = mov.avg, var.args = "avgt", k = 5, shift = "begin", var.name = "mov.avg" )
}
