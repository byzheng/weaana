% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/getter.R
\docType{methods}
\name{siteInfor,WeaAna-method}
\alias{siteInfor,WeaAna-method}
\title{Get site information}
\usage{
\S4method{siteInfor}{WeaAna}(object, load.now = FALSE)
}
\arguments{
\item{object}{A WeaAna object.}

\item{load.now}{Whether load site information}
}
\description{
Get site information
}
\examples{
library(weaana)
data( "WeatherRecordsDemo" ) 
siteInfor( records )
siteInfor( records, load.now = TRUE )
}
