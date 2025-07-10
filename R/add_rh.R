
#' Add relative humidity to APSIM weather data
#' 
#' This function adds relative humidity data to APSIM weather records based on a standard weather file.
#' @param apsim A character string specifying the path to the APSIM weather file.
#' @param standard A character string specifying the path to the standard weather file containing relative humidity data.
#' @param output A character string specifying the path to save the modified APSIM weather file with relative humidity data. If NULL, the function will not save the file.
#' @param overwrite A logical value indicating whether to overwrite the output file if it already exists. Default is FALSE.
#' 
#' @return A data frame containing the new weather records with relative humidity added.
#' @export 
add_rh <- function(apsim, standard, output = NULL, overwrite = FALSE) {
    stopifnot(is.character(apsim), length(apsim) == 1)
    stopifnot(is.character(standard), length(standard) == 1)
    stopifnot(file.exists(apsim))
    stopifnot(file.exists(standard))
    
    if (!is.null(output)) {
        stopifnot(is.character(output), length(output) == 1)
        if (file.exists(output) && !overwrite) {
            stop(paste("Output file", output, "already exists. Please choose a different name or delete the existing file."))
        }
        dir.create(dirname(output), showWarnings = FALSE, recursive = TRUE)
    }
    
    met_apsim <- readWeatherRecords(apsim, dataFormat = "APSIM")
    met_apsim_records <- getWeatherRecords(met_apsim)
    met_standard <- readWeatherRecords(standard, dataFormat = "STANDARD") |> 
        getWeatherRecords()
    
    if (is.null(met_standard$rhmint)) {
        stop("The standard weather data does not have relative humidity at minimum temperature (rhmint) column.")
    }
    if (is.null(met_standard$rhmaxt)) {
        stop("The standard weather data does not have relative humidity at maximum temperature (rhmaxt) column.")
    }
    
    record_new <- met_standard |> 
        dplyr::select("date", "rhmint", "rhmaxt") |>
        dplyr::right_join(met_apsim_records, by = "date") |> 
        dplyr::arrange(date) 
    
    if (!is.null(output)) {
        met_apsim_info <- siteInfor(met_apsim)
        records <- getWeaAnaSiteByPos(met_apsim, 1)
        records <- records$value
        extra <- records@extra
        extra$rhmint <- record_new$rhmint
        extra$rhmaxt <- record_new$rhmaxt
        records2 <- methods::new("WeaAnaSite", name  =  met_apsim_info$Name,
                                number  =  met_apsim_info$Number,
                                latitude  =  met_apsim_info$Latitude,
                                longitude  =  met_apsim_info$Longitude,
                                tav = records@tav,
                                amp = records@amp,
                                year = as.numeric(record_new$year),
                                day = as.numeric(record_new$day),
                                radn = as.numeric(record_new$radn),
                                maxt = as.numeric(record_new$maxt),
                                mint = as.numeric(record_new$mint),
                                rain = as.numeric(record_new$rain),
                                evap = as.numeric(record_new$evap),
                                vp = as.numeric(record_new$vp),
                                code = as.character(records@code),
                                extra = extra,
                                file.path = output,
                                data.format = "APSIM",
                                load.later = FALSE)
        result <- c(NULL, newPointer( methods::new( "result", 
                                                    name = as.character( NULL ), 
                                                    type = as.character( NULL ) ) ) ) 
        records3 <- methods::new( "WeaAna", 
                                 num = 1,
                                 records = c( NULL, newPointer( records2 ) ),
                                 result = result )
        writeWeatherRecords(records3, output)
    }
    return (record_new)
}
