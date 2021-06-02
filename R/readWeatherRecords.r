# * Author:    Bangyou Zheng (Bangyou.Zheng@csiro.au)
# * Created:   12/16/2010
# *

#' Create new pointer object
#' 
#' @param value The value for this pointer
#' @return A new pointer to value
#' 
#' @rdname readWeatherRecords
#' @export 
newPointer <- function(value)
{ 
    object <- new.env(parent = globalenv()) 
    object$value <- value 
    class(object) <- 'pointer'
    return(object) 
}  

#' Read weather records from a file list and/or a folder list
#' 
#' @param dataFiles A character vector to specify the path of weather data files.
#' @param dataFolders A character vector to specify the path of weather data folders. 
#' @param dataFormat The format of weather data file. 
#' @param dataWeather A data.frame for existing data. 
#' @param load.later Whether load weather records now or later. 
#' "dataFroamt" should be One of "APSIM" and "RDATA". 
#' @param ... Other arguments
#' @return A WeaAna class which contains all weather data.
#' @export readWeatherRecords
#' 
#' @rdname readWeatherRecords
readWeatherRecords <- function(dataFiles = NULL, 
                                dataFolders = NULL, 
                                dataFormat = "APSIM",
                                dataWeather = NULL,
                                load.later = FALSE, 
                                ...)
{
    fileLists <- fileList(dataFiles, dataFolders, dataFormat)
    records <- NULL
    if(load.later)
    {
        sites.records <- NULL
        num <- 0
        for (i in seq(along = fileLists))
        {
            site.record <- methods::new("WeaAnaSite", 
                    file.path = fileLists[i],
                    data.format = dataFormat,
                    load.later = TRUE)
            sites.records <- c(sites.records, newPointer(site.record))
            num <- num + 1
        }
        result <- c(NULL, newPointer(methods::new("result", 
                                name = as.character(NULL), 
                                type = as.character(NULL)))) 
        records <- methods::new("WeaAna", num = num,
                records = sites.records,
                result = result)
    } else
    {
        sites.records <- NULL
        num <- 0
        for (i in 1:length(fileLists))
        {
            site.record <- readSite(fileLists[i], dataFormat = dataFormat, ...)
            sites.records <- c(sites.records, newPointer(site.record))
            num <- num + 1
        }
        result <- c(NULL, newPointer(methods::new("result", 
                                name = as.character(NULL), 
                                type = as.character(NULL)))) 
        records <- methods::new("WeaAna", 
                num = num,
                records = sites.records,
                result = result)
        yearDay2Date <- function(day, year)
        {
            return (as.Date(day, origin = as.Date(paste(year - 1, '-12-31', sep = ''), 
                format = '%Y-%m-%d')))
        }
        wcal(records, FUN = yearDay2Date, var.args = c("day", "year"), 
            var.name = "date")
    }
    
    if (is.null(records))
    {
        stop("No weather records are found.")
    }
    
    setPara(data.format = dataFormat)
    setPara(load.later = load.later)
    return(records)
}

#' Read weather records from a weather data file
#' 
#' @param filename The file name of weather data file.
#' @param dataFormat The format of weather data file.
#' @param ... Other arguments
#' @return A WeaAnaSite class which contains all weather data.
#' @export 
#' @rdname readWeatherRecords
readSite <- function(filename, dataFormat = "APSIM", ...)
{    
    record <- NULL
    if (dataFormat == "APSIM")
    {
        record <- readSiteAPSIM(filename)
    } else if (dataFormat == "RDATA")
    {
        record <- readSiteRDATA(filename)
    } else if (dataFormat == 'GHCN')
    {
        record <- readSiteGHCN(filename, ...)
    } else
    {
        stop ("Wrong data format!")
    }
    return(record)
}

#' Read weather records from a weather data file with APSIM format
#' 
#' @param filename The file name of weather data file.
#' @return A WeaAnaSite class which contains all weather data.
#' 
#' @rdname readWeatherRecords
readSiteAPSIM <- function(filename)
{    
    a <- NULL
    station.number <- as.character(NA)
    station.name <- as.character(NA)
    latitude <- as.numeric(NA)
    longitude <- as.numeric(NA)
    temp <- readLines(filename, n = 100)
    
    sta.num.str <- temp[grep("!station number", temp)]
    
    if (length((sta.num.str)) > 0)
    {
        station.number <- omitBlankSE(substr(sta.num.str, 19, 1000))
    } 
    
    sta.name.str <- temp[grep("!station name", temp)]
    if (length((sta.name.str)) > 0)
    {
        station.name <- omitBlankSE(substr(sta.name.str, 16, 1000))
    } 
    lat.str <- temp[grep("latitude", tolower(temp))]
    if (length((lat.str)) > 0)
    {
        latitude <- gsub("^latitude( |\t)*=( |\t)*(-?\\d*\\.{0,1}\\d*).*$", "\\3", tolower(lat.str))
        latitude <- as.numeric(latitude)
    }
    
    lon.str <- temp[grep("longitude", tolower(temp))]
    if (length((lon.str)) > 0)
    {    
        longitude <- gsub("^longitude( |\t)*=( |\t)*(-?\\d*\\.{0,1}\\d*).*$", "\\3", tolower(lon.str))
        longitude <- as.numeric(longitude)
    } 
    
    tav.str <- temp[grep("^tav", temp)]
    tav <- -999
    if (length((tav.str)) > 0)
    {    
        tav <- as.numeric(
            gsub('(^tav += +)(\\d+\\.?\\d*)( .*$)', '\\2', tav.str))
    } 
    
    amp.str <- temp[grep("^amp", temp)]
    amp <- -999
    if (length((amp.str)) > 0)
    {    
        amp <- as.numeric(
            gsub('(^amp += +)(\\d+\\.?\\d*)( .*$)', '\\2', amp.str))
    } 
    
    # for year 
    start.line <- grep("^.*(year|Year|date|Date)", temp)
    if (length(start.line)  == 0) { 
        stop("Keywords year or date ae not found.")
    }
    
    a <- utils::read.table(filename, head = FALSE, sep = "", skip = start.line + 1,
        col.names = scan(filename, "", sep = "", skip = start.line - 1, nlines = 1,
            quiet = TRUE),
        as.is = TRUE)
    names(a) <- tolower(names(a))
    # Convert date
    if (!is.null(a$date)) {
        date_format <- scan(filename, "", sep = "", skip = start.line, nlines = 1,
                            quiet = TRUE)
        date_format <- date_format[which(names(a) == "date")]
        if (nchar(date_format) == 0) {
            stop("Date format is not found")
        }
        date_format <- gsub("(\\(|\\))", "", date_format)
        date_format <- "%d/%m/%Y"
        a$date <- as.Date(a$date, format = date_format)
        if (sum(is.na(a$date)) > 0) {
            stop("NA values are found for date columns.")
        }
        a$year <- format(a$date, "%Y")
        a$day <- format(a$date, "%j")
    }
    a$year <- as.numeric(a$year)
    a$day <- as.numeric(a$day)
    
    if (!is.null(a$pan))
    {
        a$evap <- a$pan
    }
    extra <- NULL
    a$maxt <- as.numeric(a$maxt)
    a$mint <- as.numeric(a$mint)
    a$radn <- as.numeric(a$radn)
    a$rain <- as.numeric(a$rain)
    if (!is.null(a$evap)) {
        
        a$evap <- as.numeric(a$evap)
    }
    if (is.null(a$avgt))
    {
        extra$avgt <- (a$maxt + a$mint) / 2
    } else
    {
        extra$avgt <- a$avgt
    }
    if (is.null(a$vpd))
    {
        extra$vpd <- vpd.apsim(a$maxt, a$mint)
    } else
    {
        extra$vpd <- a$vpd
    }
    records <- methods::new("WeaAnaSite", name  =  station.name,
            number  =  station.number,
            latitude  =  latitude,
            longitude  =  longitude,
            tav = tav,
            amp = amp,
            year = as.numeric(a$year),
            day = as.numeric(a$day),
            radn = as.numeric(a$radn),
            maxt = as.numeric(a$maxt),
            mint = as.numeric(a$mint),
            rain = as.numeric(a$rain),
            evap = as.numeric(a$evap),
            vp = as.numeric(a$vp),
            code = as.character(a$code),
            extra = extra,
            file.path = filename,
            data.format = "APSIM",
            load.later = FALSE)
    rm(a, temp)
    gc()
    return(records)
}

#' Read weather records from a weather data file with RDATA format
#' 
#' @param filename The file name of weather data file.
#' @return A WeaAnaSite class which contains all weather data.
#' 
#' @rdname readWeatherRecords
readSiteRDATA <- function(filename)
{    
    temp.env <- new.env()
    vars <- load(filename, temp.env)
    if (length(vars) > 1)
    {
        warning("Only first variable used. ")
    }
    record <- NULL
    temp.records <- get(vars[1], envir = temp.env)
    if (class(temp.records) == "WeaAna")
    {
        if (temp.records@num > 1)
        {
            warning(paste("There are more than one weather station in ",
                            filename, ". Only first station is used.", sep = ""))
        }
        site.record <- getWeaAnaSiteByPos(temp.records, 1)
        
        record <- methods::new("WeaAnaSite", name = site.record$value@name,
                number = site.record$value@number,
                latitude = site.record$value@latitude,
                longitude = site.record$value@longitude,
                year = site.record$value@year,
                day = site.record$value@day,
                radn = site.record$value@radn,
                maxt = site.record$value@maxt,
                mint = site.record$value@mint,
                rain = site.record$value@rain,
                evap = site.record$value@evap,
                vp = site.record$value@vp,
                code = site.record$value@code,
                extra = site.record$value@extra,
                file.path = filename,
                data.format = "RDATA",
                load.later = FALSE)
    }
    rm(list = vars, envir = temp.env)
    rm(site.record )
    rm(temp.env)
    gc()
    return(record)
}

#' Read weather records from a weather data file with GHCN format
#' 
#' @param filename The file name of weather data file.
#' @param ... Other arguments for site information
#' @return A WeaAnaSite class which contains all weather data.
#' 
#' @rdname readWeatherRecords
readSiteGHCN <- function(filename, ...)
{   
    
    others <- list(...)
    temp <- readLines(filename)
    
    out <- strsplit(temp, '')
    out <- as.matrix(do.call(rbind, out))
    pos_matrix <- matrix(c(12, 16, 18, seq(0, 30) * 8 + 22,
        15, 17, 21, seq(0, 30) * 8 + 26), ncol = 2)
    
    traits_map <- data.frame(name = c('PRCP', 'TMAX', 'TMIN'),
        apsim = c('rain', 'maxt', 'mint'),
        ratio = c(0.1, 0.1, 0.1), stringsAsFactors = FALSE)
    out <- apply(pos_matrix, 1, function(x)
        {
            substr(temp, x[1], x[2])
        })
    year <- as.numeric(out[,1])
    month <- as.numeric(out[,2])
    trait <- out[,3]
    pos <- match(trait, traits_map$name)
    trait <- traits_map$apsim[pos]
    value <- t(out[,-(1:3)])
    value <- matrix(as.numeric(value), nrow = nrow(value))
    value[value == -9999] <- NA
    ratio <- matrix(rep(traits_map$ratio[pos], 
        each = nrow(value)), nrow = nrow(value))
    value <- value * ratio
    value <- reshape2::melt(value)
    names(value) <- c('day', 'pos', 'value')
    value$year <- year[value$pos]
    month <- month[value$pos]
    value$trait <- trait[value$pos]
    date <- ISOdate(value$year, 
        month, value$day)
    value$day <- lubridate::yday(date)
    value <- value[!is.na(value$day),]
    weather <- reshape2::dcast(value, year + day ~ trait, value.var = 'value')
    

    records <- methods::new("WeaAnaSite", name = others$Name,
            number  =  others$Number,
            latitude  =  others$Latitude,
            longitude  =  others$Longitude,
            tav = -999,
            amp = -999,
            year = as.numeric(weather$year),
            day = as.numeric(weather$day),
            radn = as.numeric(weather$radn),
            maxt = as.numeric(weather$maxt),
            mint = as.numeric(weather$mint),
            rain = as.numeric(weather$rain),
            evap = as.numeric(weather$evap),
            vp = as.numeric(weather$vp),
            code = as.character(weather$code),
            extra = weather[,!(names(weather) %in% 
                c('year', 'day', 'radn', 'maxt', 'mint', 'rain',
                    'evap', 'vp'))],
            file.path = filename,
            data.format = "GHCN",
            load.later = FALSE)
    rm(weather, temp)
    gc()
    return(records)
}



#' create WeaAna class
#'
#' @param mets A list contained information of weather records.
#' @export
createWeaAna <- function(mets)
{
    if (!is.null(mets$Records))
    {
        tmp <- mets
        mets <- as.list(NULL)
        mets[[1]] <- tmp
    }
    tmp <- mets
    site_record <- NULL
    result <- NULL
    for (i in seq(along = tmp))
    {
        mets <- tmp[[i]]
        extra <- NULL
        if (is.null(mets$Records$avgt))
        {
            extra$avgt <- (mets$Records$maxt + mets$Records$mint) / 2
        } else
        {
            extra$Records$avgt <- mets$Records$avgt
        }
        if (is.null(mets$Records$vpd))
        {
            extra$Records$vpd <- vpd.apsim(mets$Records$maxt, mets$Records$mint)
        } else
        {
            extra$Records$vpd <- mets$Records$vpd
        }
        if (is.null(mets$Records$evap) & !is.null(mets$Records$pan))
        {
            mets$Records$evap <- mets$Records$pan
        }
        if (is.null(mets$tav) | is.null(mets$amp))
        {
            records <- mets$Records
            records$date <- as.Date(records$day, origin = as.Date(paste(
                records$year - 1, '-12-31', sep = ''), 
                    format = '%Y-%m-%d'))
            records$ym <- format(records$date, format = '%Y-%m')
            records$m <- format(records$date, format = '%m')
            records$avgt <- (records$maxt + records$mint) / 2
            amp <- tapply(records$avgt, records$ym, FUN = mean)
            month <- tapply(records$m, records$ym, 
                FUN = function(x) as.numeric(as.character(x[1])))
            year <- tapply(records$year, records$ym, 
                FUN = function(x) as.numeric(as.character(x[1])))
            amp_max <- tapply(amp, year, FUN = max)
            amp_min <- tapply(amp, year, FUN = min)
            tav <- round(mean(amp), 2)
            amp <-round(mean(amp_max - amp_min), 2)
            mets$tav <- tav
            mets$amp <- amp
        }
        site_rd <- methods::new("WeaAnaSite", name = as.character(mets$Name),
            number = as.character(mets$Number),
            latitude = mets$Latitude,
            longitude = mets$Longitude,
            tav = as.numeric(mets$tav),
            amp = as.numeric(mets$amp),
            year = as.numeric(mets$Records$year),
            day = as.numeric(mets$Records$day),
            radn = as.numeric(mets$Records$radn),
            maxt = as.numeric(mets$Records$maxt),
            mint = as.numeric(mets$Records$mint),
            rain = as.numeric(mets$Records$rain),
            evap = as.numeric(mets$Records$evap),
            vp = as.numeric(mets$Records$vp),
            code = as.character(mets$Records$code),
            extra = extra,
            file.path = as.character(NA),
            data.format = "APSIM",
            load.later = FALSE)
        
        site_record <- c(site_record, newPointer(site_rd))
        result <- c(result, newPointer(methods::new("result", 
            name = as.character(NULL), 
            type = as.character(NULL))))
    }
    
    records <- methods::new("WeaAna", 
            num = length(tmp),
            records = site_record,
            result = result)
    yearDay2Date <- function(day, year)
    {
        return (as.Date(day, origin = as.Date(paste(year - 1, '-12-31', sep = ''), 
            format = '%Y-%m-%d')))
    }
    wcal(records, FUN = yearDay2Date, var.args = c("day", "year"), 
        var.name = "date")
    return(records)
}
