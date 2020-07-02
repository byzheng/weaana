# * Author:    Bangyou Zheng (Bangyou.Zheng@csiro.au)
# * Created:   12/16/2010
# *

#' Define the class of WeaAna 
#' 
#' @docType class
#' @slot name Name of weather station
#' @slot number Station number of weather station
#' @slot latitude Latitude of weather station
#' @slot longitude Latitude of weather station
#' @slot tav Annual average ambient temperature
#' @slot amp Annual amplitude in mean monthly temperature
#' @slot marker The extra marker for this site
#' @slot year A vector of year of weather station
#' @slot day A vector of day of weather station
#' @slot radn A vector of radiation of weather station
#' @slot maxt A vector of maximum temperature of weather station
#' @slot mint A vector of minimum temperature of weather station
#' @slot evap A vector of evaporation of weather station
#' @slot rain A vector of rainfall of weather station
#' @slot vp A vector of pressure atmosphere of weather station
#' @slot code The 6 digit code indicates the source of the 6 data columns
#' @slot extra A list of variables need to store
#' @slot res All statistics results store in this slot
#' @slot figures A list to store all plotted figures.
#' @slot file.path The file path for this site.
#' @slot data.format The data format for this site.
#' @slot load.later Whether are records loaded laterly.
setClass(
        Class="WeaAnaSite",
        representation = representation(
                name = "character",
                number = "character",
                latitude = "numeric",
                longitude = "numeric",
                tav = "numeric",
                amp = "numeric",
                marker = "list",
                year = "numeric",
                day = "numeric",
                radn = "numeric",
                maxt = "numeric",
                mint = "numeric",
                rain = "numeric",
                evap = "numeric",
                vp = "numeric",
                code = "character",
                extra = "list",
                res = "list",
                figures = "list",
                file.path = "character",
                data.format = "character",
                load.later = "logical"
        )
)

#' Define the class for multiple sites
#' 
#' @docType class
#' @slot num total number of weather station
#' @slot records A pointer vector to weather records of each site
#' @slot result A pointer for all results name and type.
#' @exportClass WeaAna
setClass(
        Class="WeaAna",
        representation = representation(
                num = "numeric",
                records = "vector",
                result = "vector"
        )
)

#' Define the class for statistics results
#' 
#' @docType class
#' @slot name Name of result
#' @slot type Type of result
setClass(
        Class="result",
        representation = representation(
                name = "character",
                type = "character"
        )
)
