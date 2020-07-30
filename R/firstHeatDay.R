# * Author:    Bangyou Zheng (Bangyou.Zheng@csiro.au)
# * Created:   20/01/2011
# *

#' Calculate the first heat day
#' 
#' @param object A WeaAna object.
#' @param ... Not used
setGeneric("firstHeatDay", 
        function(object, ...)
        {
            standardGeneric("firstHeatDay")
        }
)

#' Calculate the first heat day
#'
#' @docType methods
#' @param object A WeaAna object.
#' @param yrange The year range for statistics
#' @param stress The stress temperature for frost
#' @param conse Number of consecutive days
#' @param offset offset
#' The first heat day of each year will return if prob equal to NULL.
#' @export
setMethod(f = "firstHeatDay", 
        signature = c(object = "WeaAna"),
        definition = function(object, 
                stress = waGetPara("extreme$maxt$hot.day$value"), 
                yrange = waGetPara("yrange"), conse = 1, offset = 0)
        {
            res <- wapply(object, vars = "maxt", period = "year", 
                    FUN = firstHeatDay, 
                    ARGS = list(firstHeatDay = list(stress = stress)),
                    site.ARGS = list(latitude = siteInfor(object)$Latitude, conse = conse,
                        offset = offset),
                    yrange = yrange, res.name = c("firstHeatDay"))
            return(res)
        }
)

#' Calculate the first heat day
#'
#' @docType methods
#' @param object A numeric object.
#' @param stress The stress temperature for frost
#' @param latitude Latitude of site.
#' @param conse Number of consecutive days
#' @param offset offset
#' @export
setMethod(f = "firstHeatDay", 
        signature = c(object = "numeric"),
        definition = function(object, 
                stress = waGetPara("extreme$maxt$hot.day$value"), 
                latitude = -20, conse = 1, offset = 0)
        {
            max.value <- 1000
            if (length(object) < 365)
            {
                warning("There are some years don't have complete data, NA return.")
                return(NA_integer_)
            }
            if (latitude < 0)
            {
                object_n <- object[181:length(object)]
                if (max(object_n) < stress)
                {
                    return(NA_integer_)
                }
                
                pos <- rep(FALSE, length(object_n))
                pos[ object_n > stress] <- TRUE
                if (conse > 1)
                {
                    pos <- mov.sum(pos, conse, shift = "begin")
                    pos >= conse
                }
                if (max(pos, na.rm = TRUE) < conse)
                {
                    return(NA_integer_)
                }
                max.pos <- which.max(pos) + 180
                if (offset != 0)
                {
                    bt <- waGetPara("base.temperature")
                    tt <- cumsum(ifelse(object > bt, object - bt, 0))
                    max.pos <- which.min(abs(tt - (tt[max.pos] + offset)))
                }
                return(max.pos)
            } else
            {
                if (max(object) < stress)
                {
                    return(NA_integer_)
                }
                
                pos <- rep(FALSE, length(object))
                pos[ object > stress] <- TRUE
                if (conse > 1)
                {
                    pos <- mov.sum(pos, conse, shift = "begin")
                    pos >= conse
                }
                if (max(pos, na.rm = TRUE) < conse)
                {
                    return(NA_integer_)
                }
                return(which.max(pos))
                return(max.pos)                            
            }
        }
)
