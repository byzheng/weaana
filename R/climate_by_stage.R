
#' @title Summarise the climate variable by growth stages
#' 
#' @details 
#' Define of growth stages
#' 
#' \itemize{
#'    \item{S0: From start of year to emergence}
#'    \item{S1: From emergence to flowering time - 300Cd}
#'    \item{S2: From flowering time - 300Cd to flowering time + 100Cd}
#'    \item{S3: From flowering time + 100 Cd to flowering time + 600Cd}
#'    \item{S4: From flowering + 600Cd to maturity}
#' }
#' 
#' Climate variables
#' \itemize{
#'    \item{stage: defination of stages}
#'    \item{n: Number of days in each stage}
#'    \item{avgt: average temperature (C)}
#'    \item{sum.tt: total thermal time (Cd) with base temperature 0C}
#'    \item{avg.mint: average minimum temperature}
#'    \item{avg.maxt: average maximum temperature}
#'    \item{sum.rain: total rainfall}
#'    \item{avg.evap: average evapration}
#'    \item{avg.radn average radiation}
#'    \item{hot.days: number of hot days (daily maximum temperature is more than 30C)}
#'    \item{very.hot.days: number of very hot days (daily maximum temperature is more than 35C)}
#'    \item{frost.days: number of frost days (daily minimum temperature is less than 0C)}
#'    \item{hot.sum: total thermal time above 30C of hot days (daily maximum temperature is more than 30C)}
#'    \item{very.hot.sum: total thermal time above 35C of very hot days (daily maximum temperature is more than 35C)}
#'    \item{frost.sum: total thermal time below 0C of frost days (daily minimum temperature is less than 0C)}
#'    \item{vpd: vapour-pressure deficit }
#'    \item{te: transpiration efficiency}
#'    \item{bio.radn: bio.radn}
#'    \item{bio.water: bio.water}
#'    \item{bio.tt: bio.tt}
#'    \item{ptq: photothermal quotient}
#'    \item{avt.diffuse.radn: average diffuse radiation}
#' }
#' 
#' @param climates a data.frame for climate records
#' @param sowing date. an vector of sowing date
#' @param emergence numeric (days after sowing). an vector of emergence date
#' @param heading numeric (days after sowing). an vector of heading date (optional. see details) 
#' @param flowering numeric (days after sowing). an vector of flowering time (optional. see details)
#' @param maturity numeric (days after sowing). an vector of maturity time
#' @param latitude latitude
#'
#' @return a data.frame for summarised climate variable by stages. See details for more information.
#' @export
#'
#' @examples
#' if (FALSE) {
#'    sowing <- rep(as.Date("1981-05-01"), 10)
#'    emergence <- rep(10, 10)
#'    heading <- NULL
#'    flowering <- runif(10) * 20 + 50
#'    maturity <- runif(10) * 20 + 100
#'    latitude <- -27
#'    res <- climate_by_stages(climates = climates, 
#'                             sowing = sowing,
#'                             emergence = emergence, 
#'                             heading = heading,
#'                             flowering = flowering,
#'                             maturity = maturity,
#'                             latitude = latitude)
#' }
climate_by_stages <- function(climates, sowing, emergence, 
    heading = NULL,
    flowering = NULL,
    maturity,
    latitude)
{

    sta_vars <- c("avgt", "sum.tt", "avg.mint", "avg.maxt", "sum.rain", "avg.evap", "avg.radn",
            "sum.radn", "hot.days", "very.hot.days", "frost.days", 
            'hot.sum', 'very.hot.sum', 'frost.sum',
            "ptq", "avt.diffuse.radn", "vpd",
            "te", "bio.radn", "bio.water", "bio.tt")

    sta_vars_index <- c("sum.rain.index", "hot.days.index", "very.hot.days.index", "frost.days.index", "ptq.index",
            "sum.rain.index.name", "hot.days.index.name", "very.hot.days.index.name", "frost.days.index.name", "ptq.index.name")

    sta_vars <- c(sta_vars, sta_vars_index)
    if (class(sowing) != "Date") {
        stop("sowing should be Date format")
    }
    if (is.null(heading) && is.null(flowering)) {
        stop("one of heading or flowering should be specified")
    }
    if (is.null(heading)) {
        heading <- rep(NA, length.out = length(sowing))
    }
    if (is.null(flowering)) {
        flowering <- rep(NA, length.out = length(sowing))
    }
    
    sowing <- as.Date(round(as.numeric(sowing)), origin = "1970-01-01")
    # Create a data frame to store phenology
    phenology_ori <- as.data.frame(list(tmp_sowing = sowing,
        tmp_emergence = as.numeric(round(emergence)),
        tmp_heading = as.numeric(round(heading)),
        tmp_flowering = as.numeric(round(flowering)),
        tmp_maturity = as.numeric(round(maturity))))
    
    # Simply into unique values
    phenology_sim <- phenology_ori %>% 
        unique %>%
        dplyr::mutate(
            emergence_date = .data$tmp_emergence + .data$tmp_sowing,
            flowering_date = .data$tmp_flowering + .data$tmp_sowing,
            heading_date = .data$tmp_heading + .data$tmp_sowing,
            maturity_date = .data$tmp_maturity + .data$tmp_sowing,
            idx = seq_len(dplyr::n()))

    # Calculate the heat and frost sum        
    heatFrostSum <- function(maxt, mint, year, day, latitude) {
        latitude <- latitude
        hotsum <- NULL
        veryhotsum <- NULL
        frostsum <- NULL
        for (i in seq(along = year))
        {
            hourt <- diurnalT(maxt[i], mint[i], day[i], 
                seq(from = 0, to = 23.99, by = 0.1),
                latitude * pi / 180)
            hotsum <- c(hotsum,
                sum(ifelse(hourt > 30, hourt - 30, 0)) * 0.1 / 24)
            veryhotsum <- c(veryhotsum,
                sum(ifelse(hourt > 35, hourt - 35, 0)) * 0.1 / 24)
            frostsum <- c(frostsum,
                sum(ifelse(hourt < 0, 0 - hourt, 0)) * 0.1 / 24)
        }
        return(list(hotsum = hotsum,
            veryhotsum = veryhotsum,
            frostsum = frostsum))
    }
    
    # Diffuse factor
    diffuseFactor <- function(day, radn, latitude) {
        DEC <- 23.45 * sin(2. * 3.14159265 / 365.25 * (day - 82.25))
        DECr <- DEC * 2. * 3.14159265 / 360.
        LATr <- latitude * 2. * 3.14159265 / 360.
        HS <- acos(-tan(LATr) * tan(DECr))
        Q <- 86400. * 1360. * (HS * sin(LATr) * sin(DECr) + cos(LATr) * cos(DECr) * sin(HS)) / 3.14159265 / 1000000.
        X1 <- 0.80 - 0.0017 * abs(latitude) + 0.000044 * 
            latitude * latitude
        A1 <- (0.05 - 0.96) / (X1 - 0.26)
        A0 <- 0.05 - A1 * X1
        Diffuse_fr <- A0 + A1 * radn / Q
        Diffuse_fr[Diffuse_fr < 0] <- 0
        Diffuse_fr[Diffuse_fr > 1] <- 1
        diffuse.radn <- Diffuse_fr * radn
        diffuse.radn
    }
    
    climates <- climates %>% 
        dplyr::mutate(
            tt = ifelse(.data$avgt > 0, .data$avgt, 0),
            cum_tt = cumsum(.data$tt),
            diffuse.radn = diffuseFactor(.data$day, .data$radn, latitude))
    heatfrostsum <- heatFrostSum(climates$maxt,
        climates$mint, climates$year,
        climates$day, latitude)
    
    climates$hotsum <- heatfrostsum$hotsum    
    climates$veryhotsum <- heatfrostsum$veryhotsum
    climates$frostsum <- heatfrostsum$frostsum
    
    
    
    # Subset the climate records and calculate the stages
    sub_records <- list()
    for (j in seq(length = nrow(phenology_sim))) {
        # Get the subset climates
        sub_record_j <- climates %>% 
            dplyr::filter(date <= phenology_sim$maturity_date[j])
        if (nrow(sub_record_j) == 0) {
            stop("No weather records found")
        }
        # Get the thermal time at sowing and emergence
        cum_tt <- sub_record_j$cum_tt
        cum_tt_sowing <- cum_tt[match(phenology_sim$tmp_sowing[j], sub_record_j$date)]
        cum_tt_emergence <- cum_tt[match(phenology_sim$emergence_date[j], sub_record_j$date)]
        
        # Calculate the stages
        if (sum(is.na(heading)) == length(heading)) {
            # Get the thermal time at flowering
            cum_tt_flow <- cum_tt[match(phenology_sim$flowering_date[j], sub_record_j$date)]
            if (is.na(cum_tt_flow)) {
                stop("Flowering time is not found")
            }
            sub_record_j <- sub_record_j %>% 
                dplyr::mutate(
                stage = ifelse(.data$cum_tt > cum_tt_emergence & .data$cum_tt < cum_tt_flow - 300, 1,
                        ifelse(.data$cum_tt - cum_tt_flow >= -300 & .data$cum_tt - cum_tt_flow < 100, 2, 
                        ifelse(.data$cum_tt - cum_tt_flow >= 100 & .data$cum_tt - cum_tt_flow < 600, 3,
                        ifelse(.data$cum_tt - cum_tt_flow >= 600, 4, 0)))))
        } else {
            # Get the thermal time at heading
            cum_tt_heading <- cum_tt[match(phenology_sim$heading_date[j], sub_record_j$date)]
            
            if (is.na(cum_tt_heading)) {
                stop("Heading time is not found")
            }
            sub_record_j <- sub_record_j %>% 
                dplyr::mutate(
                stage = ifelse(.data$cum_tt > cum_tt_emergence & .data$cum_tt < cum_tt_heading - 200, 1,
                        ifelse(.data$cum_tt - cum_tt_heading >= -200 & .data$cum_tt - cum_tt_heading < 200, 2, 
                        ifelse(.data$cum_tt - cum_tt_heading >= 200 & .data$cum_tt - cum_tt_heading < 700, 3,
                        ifelse(.data$cum_tt - cum_tt_heading >= 700, 4, 0)))))
            
        }
        sub_record_j$idx <- j
        sub_records[[j]] <- sub_record_j
    }
    
    sub_records <- dplyr::bind_rows(sub_records)

    # Classify some variables
    indexVars <- function(x, key, name) {
        index <- rep(name[1], length = length(x))
        for (i in 1:(length(key) - 1))
        {
            index[x >= key[i] & x < key[i + 1]] <- name[i+1]
        }
        index[x >= key[length(key)]] <- name[length(key) + 1]
        return (index) 
    }
    # Calculate the climate variables by stage
    wea_sta_stage <- sub_records %>% 
        dplyr::group_by(.data$idx, .data$stage) %>%
        dplyr::summarise(n = dplyr::n(),
                avgt = mean(.data$avgt),
                sum.tt = sum(.data$tt),
                avg.mint = mean(.data$mint),
                avg.maxt = mean(.data$maxt),
                sum.rain = sum(.data$rain),
                avg.evap = mean(.data$evap),
                avg.radn = mean(.data$radn),
                sum.radn = sum(.data$radn),
                hot.days = sum(.data$maxt > 30),
                very.hot.days = sum(.data$maxt > 35),
                frost.days = sum(.data$mint < 0),
                hot.sum = sum(.data$hotsum),
                very.hot.sum = sum(.data$veryhotsum),
                frost.sum = sum(.data$frostsum),
                vpd = mean(.data$vpd),
                te = pmax(pmin(0.006 / .data$vpd * 1, 0.009), 0.003),
                bio.radn = 0.8 * .data$sum.radn * 1.24, # RUE = 1.24
                bio.water = pmin(0.85 * .data$sum.rain * .data$te, .data$bio.radn),
                bio.tt = .data$bio.water/.data$sum.tt,
                ptq = .data$sum.radn / .data$sum.tt,
                avt.diffuse.radn = mean(.data$diffuse.radn),
                sum.rain.index = indexVars(.data$sum.rain, c(50, 150), c(0, 1, 2)),
                hot.days.index = indexVars(.data$hot.days, c(1, 2, 3), c(0, 1, 2, 3)),
                very.hot.days.index = indexVars(.data$very.hot.days, c(1, 2, 3), c(0, 1, 2, 3)),
                frost.days.index = indexVars(.data$frost.days, c(1, 2, 3), c(0, 1, 2, 3)),
                ptq.index = indexVars(.data$ptq, c(1.2, 1.4), c(0, 1, 2)),
                sum.rain.index.name = indexVars(.data$sum.rain, c(50, 150), c("<50", "[50,150)", ">=150")),
                hot.days.index.name = indexVars(.data$hot.days, c(1, 2, 3), c("0d", "1d", '2d', ">2d")),
                very.hot.days.index.name = indexVars(.data$very.hot.days, c(1, 2, 3), c("0d", "1d", '2d',">2d")),
                frost.days.index.name = indexVars(.data$frost.days, c(1, 2, 3), c("0d", "1d", "2d", ">2d")),
                ptq.index.name = indexVars(.data$ptq, c(1.2, 1.4), c("<1.2", "[1.2,1.4)", ">=1.4")), 
                .groups = "drop"
        )
        
    # Join with the phenology_sim
    res <- phenology_sim %>% 
        dplyr::select(-dplyr::contains('date')) %>% 
        dplyr::left_join(wea_sta_stage, by = 'idx') %>% 
        dplyr::select(-"idx") %>% 
        tibble::tibble()
    # Join with original input
    res <- phenology_ori %>% 
        dplyr::left_join(res, 
        by = c('tmp_sowing', 'tmp_emergence', 'tmp_heading', 'tmp_flowering', 'tmp_maturity')) %>% 
        dplyr::select(-"tmp_sowing", -"tmp_emergence", -"tmp_heading", -"tmp_flowering", -"tmp_maturity") %>% 
        tibble::tibble()
    res    
}
