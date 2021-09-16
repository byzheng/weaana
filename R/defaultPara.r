# * Author:    Bangyou Zheng (Bangyou.Zheng@csiro.au)
# * Created:   27/01/2011
# *

# initialize the parameters
# Improvements - consider reading these from a CSV file or similar

WA_OPTIONS <- settings::options_manager( 
    data.format = "APSIM", # data format
    load.later = FALSE, # whether load all weather records when read file.
    yrange = 1799:as.numeric( format(Sys.time(), "%Y") ), # The year range to calculate
    records.index = c( "year", "day" ),
    records.vars = c( "radn", "maxt", "mint", "rain", "evap", "vp" ),
    base.temperature = 0, # the base temperature to calculate the degree days
    key.degree.days = 400, # the key degree days to calculate the extreme temperature in this periods
    day.begin = 1, # days start to calculate
    day.end = 366, # days end to calculate
    extreme = list(
        maxt = list(
            hot.day = list(
                value = 35,
                more.or.less = 1,
                label = "Hot days" ),
            very.hot.day = list(
                value = 40,
                more.or.less = 1,
                label = "Very hot days" ) ),
        mint = list( 
            frost.night = list(
                value = 0,
                more.or.less = 0,
                label = "Frost nights" ) ) ),
    #            trim.incomplete.year = TRUE, # Trim the incomplete data (year) from original data
    #            evap.year = 1967,
    mov.window = 10, # moving window
    #            
    #            wea.vars.lables = c( "VPD", "evaporation perday", "rainfall", "radiation", "minimum temperature", "maximum temperature" ),
    #            wea.vars.units = c( "(hPa)", "(mm)", "(mm total)", "(MJ/m2)", "(oC)", "(oC)" ),
    #            wea.vars.strips = c( "VPD (hPa)",
    #                    "Evaporation per day (mm)",
    #                    "Rainfall (mm total)",
    #                    expression( paste( "Radiation", ~"("*MJ^2*")" ) ),
    #                    expression( paste( "Minimum Temperature", ~"("*degree*"C)" ) ),
    #                    expression( paste( "Maximum Temperature", ~"("*degree*"C)" ) ) ),
    #            daily.yrange = c( as.numeric( format(Sys.time(), "%Y") ) - 10,
    #                              as.numeric( format(Sys.time(), "%Y") ) ),
    #            # extreme indeces for extreme maximum temperature
    #            extreme.indices.maxt = list( var = "maxt",
    #                    names = c( "very.hot.days", "hot.days", "cold.days", "very.cold.days" ),
    #                    lables = c( "very hot days", "hot days", "cold days", "very cold days" ),
    #                    values = c( 40, 35, 15, 10 ),
    #                    more.or.less = c( 1, 1, 0, 0 )
    #            ),
    #            # extreme indeces for extreme minimum temperature
    #            extreme.indices.mint = list( var = "mint",
    #                    names = c( "very.hot.nights","hot.nights", "cold.nights", "frost.nights" ),
    #                    lables = c( "very hot nights","hot nights", "cold nights", "frost nights" ),
    #                    values = c( 25, 20, 5, 0 ),
    #                    more.or.less = c( 1, 1, 0, 0 )
    #            ),
    #            # define the stress conditions.
    #            stress.temp.indices = c( "mint<5", "30<maxt<35" ),
    
    shift = "begin", # The shift methods when calculate the moving extreme temperature.
    # if extreme.temp.shift = "centre", then values are shifted to centre. 
    # if extreme.temp.shift = "begin", then values are at begin of period.
    # if extreme.temp.shift = "end", then values are at end of period.
    #            countPeriod = "month", # the period at which we count the numbers of a specific condition. It could be "year", "month", "week" or any number of days 
    numdays = 5,
    #            
    #            is.out.fig.each.site = TRUE, # Whether output figures to files
    #            is.out.fig.each.type = TRUE, # Whether output figures to files
    #            is.out.fig.using.map = TRUE, # Whether output figures to files
    #            
    #            output.format = "pdf",
    #            output.prefix = NA,
    #            plot.days.range = c( 1, 365 ), # The day range to show in the plots
    #            
    #            plot.year.range = c( as.numeric( format(Sys.time(), "%Y") ) - 10, as.numeric( format(Sys.time(), "%Y") ) ),# The year range to show in the plots
    #            plot.maxt.keypoints = c(20, 35), # The key points for maximum temperature
    #            plot.mint.keypoints = c(10, 20), # The key points for minimum temperature
    #            plot.maxt.colors = c( "blue", "red", "blue", "red", "blue" ), # the colors for maximum temperature
    #            plot.mint.colors = c( "black", "green", "black", "green", "black" ), # the colores of minumum temperature
    #            
    #            
    placeholder = NA # a placeholder to conveniently add new parameters
)

# Get the all/a default parameter value
# 
# Get the all/a default parameter value
# @param para.name The paramer with this name will be returned
# @return The all default parameter will be returned if the para.name is not specified, or
# just the default paramter whose name is para.name
# @export
defaultPara <- function( para.name = NULL )
{
    if ( !is.null( para.name ) )
    {
        return( WA_OPTIONS()[[para.name]] )
    }
    else
    {
        return( WA_OPTIONS() )
    }
}

