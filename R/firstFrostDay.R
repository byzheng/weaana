# * Author:    Bangyou Zheng (Bangyou.Zheng@csiro.au)
# * Created:   4:15 PM Wednesday, 22 August 2012
# * Copyright: AS IS
# *

# Calculate the first frost event
# 
# @param object A WeaAna object.
# @param ... Not used
setGeneric("firstFrostDay", 
        function(object, ...)
        {
            standardGeneric("firstFrostDay")
        }
)

# Calculate the last frost
#
# @docType methods
# @param object A WeaAna object.
# @param yrange The year range for statistics 
# @param stress The stress temperature for frost
# @export
setMethod(f = "firstFrostDay", 
        signature = c(object = "WeaAna"),
        definition = function(object,
                stress = waGetPara("extreme$mint$frost.night$value"), 
                yrange = waGetPara("yrange") )
        {
        
            res <- wapply(object, vars = "mint", period = "year", 
                    FUN = firstFrostDay, 
                    ARGS = list(firstFrostDay = list(stress = stress)),
                    site.ARGS = list(latitude = siteInfor(object)$Latitude),
                    yrange = yrange, res.name = c("firstFrostDay"))
            return(res)
        }
)


# Calculate the last frost
#
# @docType methods
# @param object A numeric object.
# @param stress The stress temperature for frost
# @param latitude Latitude of site.
# @export
setMethod(f = "firstFrostDay", 
        signature = c(object = "numeric"),
        definition = function(object,
                stress = waGetPara("extreme$mint$frost.night$value"), 
                latitude = -20 )
        {
            min.value <- -1000
            res <- NULL
            if (length(object) < 365)
            {
                warning("There are some years don't have complete data, NA return.")
                return(NA)
            }
            if (latitude < 0)
            {
                if (min(object) > stress)
                {
                    return(NA)
                }
                
                pos <- rep(FALSE, length(object))
                pos[object < stress] <- TRUE
                max_pos <- which.max(pos)
                if (max_pos == 1)
                {
                    return(NA)
                }
                return(max_pos)
            } else
            {
                stop('Not implemented')                        
            } 
            return (NA)
        }
)
