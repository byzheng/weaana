## ---- include = FALSE---------------------------------------------------------
knitr::opts_chunk$set(
  collapse = TRUE,
  comment = "#>",
  warning = FALSE
)

## ----setup--------------------------------------------------------------------
library(weaana)

## -----------------------------------------------------------------------------
records <- readWeatherRecords(dataFolders = system.file("data", package = "weaana"))
met_files <- list.files(system.file("data", package = "weaana"), "*.met", full.names = TRUE)
records <- readWeatherRecords(met_files)
records

## -----------------------------------------------------------------------------
records <- readWeatherRecords(met_files, load.later = TRUE )
# Collect information of weather stations
siteInfor(records, load.now = TRUE)

## -----------------------------------------------------------------------------
addMarkers(records, model = c("Model1", "Model2"))
records
delMarkers(records, name = "model")
records

## -----------------------------------------------------------------------------
new.record <- records[1]
new.record
new.record <- records[1:2]
new.record

## -----------------------------------------------------------------------------
siteInfor(records)

## -----------------------------------------------------------------------------
#sites2KML(records, file = "sites.kml", name = FALSE)

## -----------------------------------------------------------------------------
plotSites(records, lines = ozRegion(), sitename = FALSE, col = "red", pch = 24)

## -----------------------------------------------------------------------------
data("WeatherRecordsDemo")
wcal(records, avgt2 = "(maxt + mint)/2")

## -----------------------------------------------------------------------------
# Moving average temperature
wcal(records, FUN = mov.avg, var.args = "avgt", k = 5, shift = "begin", var.name = "mov.avg")
# The codes get the same results
wcal(records, mov.avg = "mov.avg(avgt k = 5, shift = \"begin\")")

## -----------------------------------------------------------------------------
# daily mean temperature
a <- wapply(records, vars = "avgt", period = "day", FUN = mean)
# yealy total rainfall
a <- wapply(records, vars = "rain", period = "year", FUN = sum)
# yearly mean temperature
a <- wapply(records, vars = "avgt", period = "year", FUN = mean)
# monthly mean temperature
a <- wapply(records, vars = "avgt", period = "month", FUN = mean)
# 10 days mean temperature
a <- wapply(records, vars = "avgt", period = 10, FUN = mean)

head(a)

## -----------------------------------------------------------------------------
# yearly mean temperature, total raifall and mean radiation.
a <- wapply(records, vars = c( "avgt", "rain", "radn" ), period = "year", 
        FUN = c( mean, sum ),
        res.name = c( "mean.avgt", "total.rain", "mean.radn"))
head(a[[1]])

