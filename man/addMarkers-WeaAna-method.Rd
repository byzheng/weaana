% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/getter.R
\name{addMarkers,WeaAna-method}
\alias{addMarkers,WeaAna-method}
\title{Add marker for each site}
\usage{
\S4method{addMarkers}{WeaAna}(object, ...)
}
\arguments{
\item{object}{A WeaAna object.}

\item{...}{marker arguments}
}
\description{
Add marker for each site
}
\examples{
library(weaana)
data( "WeatherRecordsDemo" ) 
addMarkers( records, model = c( "SILO", "A2" ) ) 

}
