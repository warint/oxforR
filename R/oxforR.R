# Function 1: Data collection

#' Title
#'
#' @description This function allows you to find and display the Oxford COVID-19 Government Response Tracker (OxCGRT) data according to the selected parameters.
#' If no arguments are filled, all data will be displayed.
#' @param country Countries' ISO code.
#' @param indicator Indicators from the Oxford COVID-19 Government Response Tracker (OxCGRT)
#'
#' @import curl
#' @import dplyr
#' @import reshape2
#' @import gsheet
#' @return Data for the country and indicator requested
#' @export
#'
#' @examples
#' myData <-oxforr_data("CAN","C1)
oxforr_data <- function(country = data_long_country,
                        indicator = data_long_indicator) {
  CountryCode <- IndicatorCode <- NULL
  out <- dplyr::filter(data_long,
                       CountryCode %in% country,
                       IndicatorCode %in% indicator)
  return(out)
}

# downloading the data file
url <- "https://raw.githubusercontent.com/OxCGRT/covid-policy-tracker/master/data/OxCGRT_latest.csv"
file_path <- file.path(tempdir(), "temp.csv")
curl::curl_download(url, file_path)
oxford_data <- read.csv(file_path)

# putting the date as a date format
oxford_data$Date <- as.Date(gsub("(\\d{4})(\\d{2})(\\d{2})$","\\1-\\2-\\3", oxford_data$Date))


data_long <- reshape2::melt(oxford_data,
                            # ID variables - all the variables to keep but not split apart on
                            id.vars = c("CountryName", "CountryCode", "Date", "ConfirmedCases", "ConfirmedDeaths", "StringencyIndex", "StringencyIndexForDisplay","LegacyStringencyIndex","LegacyStringencyIndexForDisplay"),
                            # The source columns
                            measure.vars = colnames(oxford_data)[4:30],
                            # Name of the destination column that will identify the original
                            # column that the measurement came from
                            variable.name = "IndicatorName",
                            value.name = "Value")

data_long$IndicatorName <- gsub("(^|[[:space:]])([[:alpha:]])", "\\1\\U\\2", data_long$IndicatorName, perl=TRUE)

data_long$IndicatorName <- gsub(" ", "", data_long$IndicatorName, fixed = TRUE)

oxforr_indicators_natural_language <- gsheet::gsheet2tbl("https://docs.google.com/spreadsheets/d/1QzRoYOf08OS59CLcGlFjCiogGN3N2IQ0rhuE2R_MD00/edit?usp=sharing")

oxford_indicators <- dplyr::select(oxforr_indicators_natural_language, IndicatorName, IndicatorCode)

data_long <- dplyr::left_join(data_long, oxford_indicators, by = "IndicatorName")

data_long <- dplyr::select(data_long, CountryName, CountryCode, Date, ConfirmedCases, ConfirmedDeaths, StringencyIndex, StringencyIndexForDisplay, LegacyStringencyIndex,LegacyStringencyIndexForDisplay, IndicatorName, IndicatorCode, Value)

# Creating the default values for the function query
# IF an entry is missing, all the observations of this variable will be displayed

data_long_country <- base::unique(data_long[, 2])
data_long_indicator <- base::unique(data_long[, 10])

# Function 2: Indicators' symbols query
# If the user does not know the code of an indicator, s.he has access to the answer in natural language through this query

#' oxforr_indicator
#' @description  This function allows you to find and search the right indicator code from the  The Oxford COVID-19 Government Response Tracker (OxCGRT) you want to use.
#' If no argument is filed, all indicators will be displayed.
#' @param indicators Indicator code
#'
#' @return list of indicators
#' @export
#'
#' @examples
#' indicators <-oxforr_indicator()
#' public_indicators <-oxforr_indicator("public")



oxforr_indicator <- function(indicators) {
  if (missing(indicators)) {
    oxforr_indicators_natural_language
  } else {
    oxforr_indicators_natural_language[grep(indicators, oxforr_indicators_natural_language$IndicatorNaturalLanguage, ignore.case = TRUE), ]
  }
}



# Function 3: Countries' code reconciliation
# If the user does not know the ISO code of a country, s.he has access to the answer in natural language through this query

#' oxforr_country
#'
#' @description This function allows you to find and search the right country code associated with the Oxford COVID-19 Government Response Tracker (OxCGRT).
#' If no argument is filed, all indicators will be displayed.
#'
#' @param country Country's ISO code.
#'
#' @return
#' @export
#'
#' @examples
#' mycountry <- oxforr_country()
#' mycountry <- oxforr_country("canada)
#'

oxforr_country <- function(country) {
  oxford_countries_natural_language <- unique(oxford_data[, 1:2])
  if (missing(country)) {
    oxford_countries_natural_language
  } else {
    oxford_countries_natural_language[grep(country, oxford_countries_natural_language$CountryName, ignore.case = TRUE), ]
  }
}
