# * Author:    Bangyou Zheng (Bangyou.Zheng@csiro.au)
# * Created:   06/01/2011
# *

# Calculate the last frost
# 
# @param object A WeaAna object.
# @param ... Not used
setGeneric("lastFrostDay", 
        function(object, ...)
        {
            standardGeneric("lastFrostDay")
        }
)

# Calculate the last frost
#
# @docType methods
# @param object A WeaAna object.
# @param yrange The year range for statistics 
# @param stress The stress temperature for frost
# @param offset offset
# @export
setMethod(f = "lastFrostDay", 
        signature = c(object = "WeaAna"),
        definition = function(object,
                stress = waGetPara("extreme$mint$frost.night$value"), 
                yrange = waGetPara("yrange"),
                offset = 0)
        {
        
            res <- wapply(object, vars = "mint", period = "year", 
                    FUN = lastFrostDay, 
                    ARGS = list(lastFrostDay = list(stress = stress)),
                    site.ARGS = list(latitude = siteInfor(object)$Latitude,
                        offset = offset),
                    yrange = yrange, res.name = c("lastFrostDay"))
            return(res)
        }
)


# Calculate the last frost
#
# @docType methods
# @param object A numeric object.
# @param stress The stress temperature for frost
# @param latitude Latitude of site.
# @param offset offset.
# @export
setMethod(f = "lastFrostDay", 
        signature = c(object = "numeric"),
        definition = function(object,
                stress = waGetPara("extreme$mint$frost.night$value"), 
                latitude = -20,
                offset = 0)
        {
            min.value <- -1000
            res <- NULL
            if (length(object) < 365)
            {
                warning("There are some years don't have complete data, NA return.")
                return(NA_integer_)
            }
            if (latitude < 0)
            {
                object_n <- rev(object)
                object_n[ object_n < stress] <- min.value
                min.pos <- which.min(object_n) 
                if (min(object_n) > min.value)
                {
                    return(NA_integer_)
                } 
                min.pos <- length(object_n) + 1 - min.pos
                if (offset != 0)
                {
                    bt <- waGetPara("base.temperature")
                    tt <- cumsum(ifelse(object > bt, object - bt, 0))
                    min.pos <- which.min(abs(tt - (tt[min.pos] + offset)))
                }
                return(min.pos)
            } else
            {
                stop('Not implemented')
                # object <- rev(object[1:182])
                # object[ object < stress] <- min.value
                # min.pos <- which.min(object) 
                # if (min(object) > min.value)
                # {
                    # min.pos <- NA
                # } else
                # {
                    # min.pos <- 183 - min.pos
                # }
                # return(min.pos)                                
            } 
            return (NA_integer_)
        }
)
