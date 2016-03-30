#' @title Agronomic Norms Fields.
#'
#' @description
#' \code{agronomic_norms_fields} calls Historic Agronomic Norms Endpoint of API using Field Location Construct
#'
#' @details
#' This is a flexible API that allows you to calculate the averages for agronomic values
#' and accumulations across any range of years for which we have data. Whereas the Agronomic
#' Values and Accumulations API only supports up to 30 months of daily data, this API allow
#' you to compare this year and the previous year to the long-term normals (however many years
#' you want to include).  Uses the aWhere API Fields construct to request data. Uses default
#' units returned by the API
#'
#' @references http://developer.awhere.com/api/reference/agronomics/norms
#'
#' @param - field_id: the field_id having previously been created with the createField Function
#' @param - month_day_start: character string of the month and day for the start
#'                         of the range of days you are calculating norms for, e.g., '07-01' (July 1)
#' @param - month_day_end: character string of the month and day for the end of the
#'                       range of days you are calculating norms for, e.g., '07-10' (July 10)
#' @param - year_start: the starting year (inclusive) of the range of years for which
#'                     you're calculating norms, as a string, e.g., '2008'
#' @param - year_end: the end year (inclusive) of the range of years for which you're
#'                     calculating norms, as a string, e.g., '2015'
#' @param - exclude_years: You can opt to exclude one or more years from the range, and
#'                        it's values will not be included in the averages. To exclude
#'                        multiple years, separate them with a comma. Note: You must always have
#'                        at least three years of data to average
#' @param - accumulation_start_date: If you want to start counting accumulations from
#'                                 before the specified start date (or before the
#'                                 planting date if using the most recent Planting),
#'                                 use this parameter to specify the date from which
#'                                 you wish to start counting. The daily values object
#'                                 will still only return the days between the start
#'                                 and end date. This date must come before the start date.
#' @param - gdd_method: There are variety of equations available for calculating growing degree-days.
#'                     Valid entries are: 'standard', 'modifiedstandard', 'min-temp-cap', 'min-temp-constant'
#'                     See the API documentation for a description of each method.  The standard
#'                     method will be used if none is specified
#' @param - gdd_base_temp: The base temp to use for the any of the GDD equations. The default value of 10 will
#'                       be used if none is specified
#' @param - gdd_min_boundary: The minimum boundary to use in the selected GDD equation.
#'                           The behavior of this value is different depending on the equation you're using
#'                           The default value of 10 will be used if none is specified
#' @param - gdd_max_boundary: The max boundary to use in the selected GDD equation. The
#'                          behavior of this value is different depending on the equation you're using.
#'                          The default value of 30 will be used if none is specified
#'
#' @return data.table of requested data for dates requested
#'
#' @import httr
#' @import data.table
#' @import lubridate
#' @import jsonlite
#'
#' @examples
#' agronomic_norms_fields('field123','07-01', '07-10', '2008', '2015','2010,2011','','standard','10','10','30')

#' @export


agronomic_norms_fields <- function(field_id, month_day_start = '', month_day_end = '',
                            year_start = '', year_end = '',exclude_years = '',
                            accumulation_start_date = '',gdd_method = 'standard',gdd_base_temp = '10',
                            gdd_min_boundary = '10', gdd_max_boundary = '30') {

  #############################################################
  #Checking Input Parameters
  if (exists('awhereEnv75247') == FALSE) {
    warning('Please Run the Command \'get_token()\' and then retry running command. \n')
    return()
  }

  if (exists('uid', envir = awhereEnv75247) == FALSE |
      exists('secret', envir = awhereEnv75247) == FALSE |
      exists('token', envir = awhereEnv75247) == FALSE) {
    warning('Please Run the Command \'get_token()\' and then retry running command. \n')
    return()
  }

  currentFields <- get_fields(field_id)
  if ((field_id %in% currentFields$field_id) == FALSE) {
    warning('The Provided field name is not a field currently associated with your account. \n
             Please create the field before proceeding. \n')
    return()
  }

  if (month_day_start != '') {
    month_day_startTest <- strsplit(month_day_start,'-')
    for (z in 1:length(month_day_startTest[[1]])) {
      if (nchar(month_day_startTest[[1]][z]) != 2) {
        warning('The parameter month_day_start is not properly formatted.  Please correct. \n')
        return()
      }
    }
    if ((as.integer(month_day_startTest[[1]][1]) >= 1 & as.integer(month_day_startTest[[1]][1]) <= 12) == FALSE) {
      warning('The month parameter in month_day_start is not a valid value.  Please correct. \n')
      return()
    }
    if (month_day_startTest[[1]][1] %in% c('4','6','9','11')) {
      if ((as.integer(month_day_startTest[[1]][2]) >= 1 & as.integer(month_day_startTest[[1]][2]) <= 30) == FALSE) {
        warning('The day parameter in month_day_start is not a valid value.  Please correct. \n')
        return()
      }
    } else if (month_day_startTest[[1]][1] %in% c('2')) {
      if ((as.integer(month_day_startTest[[1]][2]) >= 1 & as.integer(month_day_startTest[[1]][2]) <= 28) == FALSE) {
        warning('The day parameter in month_day_start is not a valid value.  Please correct. \n')
        return()
      }
    } else {
      if ((as.integer(month_day_startTest[[1]][2]) >= 1 & as.integer(month_day_startTest[[1]][2]) <= 31) == FALSE) {
        warning('The day parameter in month_day_start is not a valid value.  Please correct. \n')
        return()
      }
    }
  }

  if (month_day_end != '') {
    if (month_day_start == '') {
      warning('If the parameter month_day_end is specified so must month_day_start.  Please correct. \n')
      return()
    }
    month_day_endTest <- strsplit(month_day_end,'-')
    for (z in 1:length(month_day_endTest[[1]])) {
      if (nchar(month_day_endTest[[1]][z]) != 2) {
        warning('The parameter month_day_end is not properly formatted.  Please correct. \n')
        return()
      }
    }
    if ((as.integer(month_day_endTest[[1]][1]) >= 1 & as.integer(month_day_endTest[[1]][1]) <= 12) == FALSE) {
      warning('The month parameter in month_day_end is not a valid value.  Please correct. \n')
      return()
    }
    if (month_day_endTest[[1]][1] %in% c('4','6','9','11')) {
      if ((as.integer(month_day_endTest[[1]][2]) >= 1 & as.integer(month_day_endTest[[1]][2]) <= 30) == FALSE) {
        warning('The day parameter in month_day_end is not a valid value.  Please correct. \n')
        return()
      }
    } else if (month_day_endTest[[1]][1] %in% c('2')) {
      if ((as.integer(month_day_endTest[[1]][2]) >= 1 & as.integer(month_day_endTest[[1]][2]) <= 28) == FALSE) {
        warning('The day parameter in month_day_end is not a valid value.  Please correct. \n')
        return()
      }
    } else {
      if ((as.integer(month_day_endTest[[1]][2]) >= 1 & as.integer(month_day_endTest[[1]][2]) <= 31) == FALSE) {
        warning('The day parameter in month_day_end is not a valid value.  Please correct. \n')
        return()
      }
    }
  }

  if (year_start != '') {
    if (as.integer(year_start) < 1994 | as.integer(year_start) > year(Sys.Date())) {
      warning('The year_start parameter must be between 1994 and the current year.  Please correct. \n')
      return()
    }
  }

  if (year_end != '') {
    if (as.integer(year_end) < 1994 | as.integer(year_end) > year(Sys.Date())) {
      warning('The year_end parameter must be between 1994 and the current year.  Please correct. \n')
      return()
    }
  }

  if (year_start != '' & month_day_start != '') {
    if (ymd(paste0(year_start,'-',month_day_start)) > ymd(Sys.Date())) {
      warning('The combination of year_start and month_day_start implies data from the future must be retrieved.  Please correct. \n')
      return()
    }
  }

  if (year_end != '' & month_day_end != '') {
    if (ymd(paste0(year_end,'-',month_day_end)) > ymd(Sys.Date())) {
      warning('The combination of year_end and month_day_end implies data from the future must be retrieved.  Please correct. \n')
      return()
    }
  }

  if ((((year_start != '') & (year_end == '')) | ((year_start == '') & (year_end != ''))) == TRUE) {
    warning('Both the starting and ending years myst be specified explicitly if using years. \n')
    return()
  }

  if ((year_start != '') & (year_end != '')) {
    yearsToRequest <- seq(as.integer(year_start),as.integer(year_end))

    if (exclude_years != '') {
      exclude_yearsTest <- strsplit(exclude_years,',')
      for (z in 1:length(exclude_yearsTest[[1]])) {
        if (as.integer(exclude_yearsTest[[1]][z]) < 1994 | as.integer(exclude_yearsTest[[1]][z]) > year(Sys.Date())) {
          warning('One of the years included in the exclude_years parameter is not in the \n
                   proper range (1994-CurrentYear).  Please correct. \n')
          return()
        }
        yearsToRequest <- yearsToRequest[yearsToRequest != as.integer(exclude_yearsTest[[1]][z])]
      }
    }

    if (length(yearsToRequest) <= 3) {
      warning('At least three unique years must be used in this query. Please correct. \n')
      return()
    }
  }

  if ((accumulation_start_date != '') == TRUE) {
    accumulation_start_dateTest <- strsplit(accumulation_start_date,'-')
    for (z in 1:length(accumulation_start_dateTest [[1]])) {
      if (nchar(accumulation_start_dateTest[[1]][z]) != 2) {
        warning('The parameter accumulation_start_date is not properly formatted.  Please correct. \n')
        return()
      }
    }
    if ((as.integer(accumulation_start_dateTest [[1]][1]) >= 1 & as.integer(accumulation_start_dateTest [[1]][1]) <= 12) == FALSE) {
      warning('The month parameter in accumulation_start_date is not a valid value.  Please correct. \n')
      return()
    }
    if (accumulation_start_dateTest [[1]][1] %in% c('4','6','9','11')) {
      if ((as.integer(accumulation_start_dateTest [[1]][2]) >= 1 & as.integer(accumulation_start_dateTest[[1]][2]) <= 30) == FALSE) {
        warning('The day parameter in accumulation_start_date is not a valid value.  Please correct. \n')
        return()
      }
    } else if (accumulation_start_dateTest [[1]][1] %in% c('2')) {
      if ((as.integer(accumulation_start_dateTest [[1]][2]) >= 1 & as.integer(accumulation_start_dateTest [[1]][2]) <= 28) == FALSE) {
        warning('The day parameter in accumulation_start_date is not a valid value.  Please correct. \n')
        return()
      }
    } else {
      if ((as.integer(accumulation_start_dateTest [[1]][2]) >= 1 & as.integer(accumulation_start_dateTest[[1]][2]) <= 31) == FALSE) {
        warning('The day parameter in accumulation_start_date is not a valid value.  Please correct. \n')
        return()
      }
    }
    if (accumulation_start_dateTest[[1]][1] == month_day_startTest[[1]][1]) {
      if (accumulation_start_dateTest[[1]][2] > month_day_startTest[[1]][2]) {
        warning('The accumulation_start_date parameter must come before the startDate parameter.  Please correct. \n')
        return()
      }
    }
  }

  if ((gdd_method %in% c('standard','modifiedstandard','min-temp-cap','min-temp-constant')) == FALSE) {
    warning('Valid values for the GDD method used to calculate growing degree days are \n
            \'standard\', \'modifiedstandard\', \'min-temp-cap\', \'min-temp-constant\'.\n
            Please change gdd_method to one of these values. \n')
    return()
  }


  if (suppressWarnings(is.na(as.integer(gdd_base_temp))) == TRUE) {
    warning('The gdd_base_temp parameter is not a valid value.  Please correct. \n')
    return()
  }

  if (suppressWarnings(is.na(as.integer(gdd_min_boundary))) == TRUE) {
    warning('The gdd_min_boundary parameter is not a valid value.  Please correct. \n')
    return()
  }

  if (suppressWarnings(is.na(as.integer(gdd_max_boundary))) == TRUE) {
    warning('The gdd_max_boundary parameter is not a valid value.  Please correct. \n')
    return()
  }

  ##############################################################################
  dataList <- list()

  # Create query

  urlAddress <- "https://api.awhere.com/v2/agronomics"

  strBeg <- paste0('/fields')
  strCoord <- paste0('/',field_id)
  strType <- paste0('/agronomicnorms')

  if (month_day_start != '' & month_day_end != '') {
    strMonthsDays <- paste0('/',month_day_start,',',month_day_end)
  } else if (month_day_end != '') {
    strMonthsDays <- paste0('/',month_day_start,',',month_day_start)
  } else {
    strMonthsDays <- ''
  }

  if (exclude_years != '') {
    strexclude_years <- paste0('&excludeYears=',exclude_years)
  } else {
    strexclude_years <- ''
  }

  gdd_methodString      <- paste0('?gddMethod=',gdd_method)
  gdd_base_tempString    <- paste0('&gddBaseTemp=',gdd_base_temp)
  gdd_min_boundaryString <- paste0('&gddMinBoundary=',gdd_min_boundary)
  gdd_max_boundaryString <- paste0('&gddMaxBoundary=',gdd_max_boundary)

  if (year_start != '' & year_end != '') {
    strYearsType <- paste0('/years')
    strYears <- paste0('/',year_start,',',year_end)
    address <- paste0(urlAddress, strBeg, strCoord, strType, strMonthsDays, strYearsType,
                      strYears,gdd_methodString,gdd_base_tempString,gdd_min_boundaryString,gdd_max_boundaryString,exclude_years)
  } else {
    address <- paste0(urlAddress, strBeg, strCoord, strType, strMonthsDays,gdd_methodString,
                      gdd_base_tempString,gdd_min_boundaryString,gdd_max_boundaryString,exclude_years)
  }
  doWeatherGet <- TRUE
  while (doWeatherGet == TRUE) {
    requestString <- 'request <- GET(address,
  	                                    add_headers(Authorization =
  	                                    paste0(\"Bearer \", awhereEnv75247$token)))'
    # Make request
    eval(parse(text = requestString))

    a <- suppressMessages(content(request, as = "text"))

    #The JSONLITE Serializer properly handles the JSON conversion

    x <- jsonlite::fromJSON(a,flatten = TRUE)

    if (grepl('API Access Expired',a)) {
      get_token(awhereEnv75247$uid,awhereEnv75247$secret)
    } else {
      doWeatherGet <- FALSE
    }
  }

  data <- as.data.table(x[[3]])

  varNames <- colnames(data)
  #This removes the non-data info returned with the JSON object
  data[,grep('_links',varNames) := NULL, with = FALSE]
  data[,grep('.units',varNames) := NULL, with = FALSE]

#   varNames <- colnames(data)
#
#   for (x in 1:length(varNames)) {
#
#     if (varNames[x] == 'day'){
#       varNames[x] <- 'monthDay'
#     } else if (varNames[x] == 'location.latitude'){
#       varNames[x] <- 'latitude'
#     } else if (varNames[x] == 'location.longitude'){
#       varNames[x] <- 'longitude'
#     } else if (varNames[x] == 'meanTemp.average'){
#       varNames[x] <- 'avgMeanTemperature'
#     } else if (varNames[x] == 'meanTemp.stdDev'){
#       varNames[x] <- 'avgMeanTemperatureStdDev'
#     } else if (varNames[x] == 'minTemp.average'){
#       varNames[x] <- 'avgMinTemperature'
#     } else if (varNames[x] == 'minTemp.stdDev'){
#       varNames[x] <- 'avgMinTemperatureStdDev'
#     } else if (varNames[x] == 'maxTemp.average'){
#       varNames[x] <- 'avgMaxTemperature'
#     } else if (varNames[x] == 'maxTemp.stdDev'){
#       varNames[x] <- 'avgMaxTemperatureStdDev'
#     } else if (varNames[x] == 'precipitation.average'){
#       varNames[x] <- 'avgPrecipitation'
#     } else if (varNames[x] == 'precipitation.stdDev'){
#       varNames[x] <- 'avgPrecipitationStdDev'
#     } else if (varNames[x] == 'solar.average'){
#       varNames[x] <- 'avgSolarEnergy'
#     } else if (varNames[x] == 'solar.stdDev'){
#       varNames[x] <- 'avgSolarEnergyStdDev'
#     } else if (varNames[x] == 'minHumidity.average'){
#       varNames[x] <- 'avgMinHumidity'
#     } else if (varNames[x] == 'minHumidity.stdDev'){
#       varNames[x] <- 'avgMinHumidityStdDev'
#     } else if (varNames[x] == 'maxHumidity.average'){
#       varNames[x] <- 'avgMaxHumidity'
#     } else if (varNames[x] == 'maxHumidity.stdDev'){
#       varNames[x] <- 'avgMaxHumidityStdDev'
#     } else if (varNames[x] == 'dailyMaxWind.average'){
#       varNames[x] <- 'avgDailyMaxWind'
#     } else if (varNames[x] == 'dailyMaxWind.stdDev'){
#       varNames[x] <- 'avgDailyMaxWindStdDev'
#     } else if (varNames[x] == 'averageWind.average'){
#       varNames[x] <- 'avgWind'
#     } else if (varNames[x] == 'averageWind.stdDev'){
#       varNames[x] <- 'avgWindStdDev'
#     }
#

  setnames(data,varNames)
  # setkey(data,day)

  return(as.data.frame(data))
}

#' @title Agronomic Norms Latitude & Longitude.
#'
#' @description
#' \code{agronomic_norms_latlng} calls Historic Agronomic Norms by Geolocation Endpoint of API using Lat/Lon
#'
#' @details
#' This is a flexible API that allows you to calculate the averages for agronomic values
#' and accumulations across any range of years for which we have data. Whereas the Agronomic
#' Values and Accumulations API only supports up to 30 months of daily data, this API allow
#' you to compare this year and the previous year to the long-term normals (however many years
#' you want to include).  Uses the aWhere API Lat/Lon construct to request data. Uses default
#' units returned by the API
#'
#' @references http://developer.awhere.com/api/reference/agronomics/norms/geolocation
#'
#' @param - latitude: the latitude for the location for which you want data
#' @param - longitude: the latitude for the location for which you want data
#' @param - month_day_start: character string of the month and day for the start
#'                         of the range of days you are calculating norms for, e.g., '07-01' (July 1)
#' @param - month_day_end: character string of the month and day for the end of the
#'                       range of days you are calculating norms for, e.g., '07-10' (July 10)
#' @param - year_start: the starting year (inclusive) of the range of years for which
#'                     you're calculating norms, as a string, e.g., '2008'
#' @param - year_end: the end year (inclusive) of the range of years for which you're
#'                     calculating norms, as a string, e.g., '2015'
#' @param - exclude_years: You can opt to exclude one or more years from the range, and
#'                        it's values will not be included in the averages. To exclude
#'                        multiple years, separate them with a comma. Note: You must always have
#'                        at least three years of data to average
#' @param - accumulation_start_date: If you want to start counting accumulations from
#'                                 before the specified start date (or before the
#'                                 planting date if using the most recent Planting),
#'                                 use this parameter to specify the date from which
#'                                 you wish to start counting. The daily values object
#'                                 will still only return the days between the start
#'                                 and end date. This date must come before the start date.
#' @param - gdd_method: There are variety of equations available for calculating growing degree-days.
#'                     Valid entries are: 'standard', 'modifiedstandard', 'min-temp-cap', 'min-temp-constant'
#'                     See the API documentation for a description of each method.  The standard
#'                     method will be used if none is specified
#' @param - gdd_base_temp: The base temp to use for the any of the GDD equations. The default value of 10 will
#'                       be used if none is specified
#' @param - gdd_min_boundary: The minimum boundary to use in the selected GDD equation.
#'                           The behavior of this value is different depending on the equation you're using
#'                           The default value of 10 will be used if none is specified
#' @param - gdd_max_boundary: The max boundary to use in the selected GDD equation. The
#'                          behavior of this value is different depending on the equation you're using.
#'                          The default value of 30 will be used if none is specified
#'
#' @return data.table of requested data for dates requested
#'
#' @import httr
#' @import data.table
#' @import lubridate
#' @import jsonlite
#'
#' @examples
#' agronomic_norms_latlng('39.8282', '-98.5795', '07-01', '07-10', '2008', '2015','2010,2011','','standard','10','10','30')

#' @export


agronomic_norms_latlng <- function(latitude, longitude, month_day_start, month_day_end = '',
                                   year_start, yearEnd,exclude_years = '',
                                   accumulation_start_date = '',gdd_method = 'standard',gdd_base_temp = '10',
                                   gdd_min_boundary = '10', gdd_max_boundary = '30') {

  #############################################################
  #Checking Input Parameters
  if (exists('awhereEnv75247') == FALSE) {
    warning('Please Run the Command \'get_token()\' and then retry running command. \n')
    return()
  }

  if (exists('uid', envir = awhereEnv75247) == FALSE |
      exists('secret', envir = awhereEnv75247) == FALSE |
      exists('token', envir = awhereEnv75247) == FALSE) {
    warning('Please Run the Command \'get_token()\' and then retry running command. \n')
    return()
  }

  if (suppressWarnings(is.na(as.double(latitude))) == FALSE) {
    if ((as.double(latitude) >= -90 & as.double(latitude) <= 90) == FALSE) {
      warning('The entered Latitude Value is not valid. Please correct\n')
      return()
    }
  } else {
    warning('The entered Latitude Value is not valid. Please correct\n')
    return()
  }

  if (suppressWarnings(is.na(as.double(longitude))) == FALSE) {
    if ((as.double(longitude) >= -180 & as.double(longitude) <= 180) == FALSE) {
      warning('The entered Longitude Value is not valid. Please correct\n')
      return()
    }
  } else {
    warning('The entered Longitude Value is not valid. Please correct\n')
    return()
  }

  month_day_startTest <- strsplit(month_day_start,'-')
  for (z in 1:length(month_day_startTest[[1]])) {
    if (nchar(month_day_startTest[[1]][z]) != 2) {
      warning('The parameter month_day_start is not properly formatted.  Please correct. \n')
      return()
    }
  }
  if ((as.integer(month_day_startTest[[1]][1]) >= 1 & as.integer(month_day_startTest[[1]][1]) <= 12) == FALSE) {
    warning('The month parameter in month_day_start is not a valid value.  Please correct. \n')
    return()
  }
  if (month_day_startTest[[1]][1] %in% c('4','6','9','11')) {
    if ((as.integer(month_day_startTest[[1]][2]) >= 1 & as.integer(month_day_startTest[[1]][2]) <= 30) == FALSE) {
      warning('The day parameter in month_day_start is not a valid value.  Please correct. \n')
      return()
    }
  } else if (month_day_startTest[[1]][1] %in% c('2')) {
    if ((as.integer(month_day_startTest[[1]][2]) >= 1 & as.integer(month_day_startTest[[1]][2]) <= 28) == FALSE) {
      warning('The day parameter in month_day_start is not a valid value.  Please correct. \n')
      return()
    }
  } else {
    if ((as.integer(month_day_startTest[[1]][2]) >= 1 & as.integer(month_day_startTest[[1]][2]) <= 31) == FALSE) {
      warning('The day parameter in month_day_start is not a valid value.  Please correct. \n')
      return()
    }
  }

  if (month_day_end != '') {
    month_day_endTest <- strsplit(month_day_end,'-')
    for (z in 1:length(month_day_endTest[[1]])) {
      if (nchar(month_day_endTest[[1]][z]) != 2) {
        warning('The parameter month_day_end is not properly formatted.  Please correct. \n')
        return()
      }
    }
    if ((as.integer(month_day_endTest[[1]][1]) >= 1 & as.integer(month_day_endTest[[1]][1]) <= 12) == FALSE) {
      warning('The month parameter in month_day_end is not a valid value.  Please correct. \n')
      return()
    }
    if (month_day_endTest[[1]][1] %in% c('4','6','9','11')) {
      if ((as.integer(month_day_endTest[[1]][2]) >= 1 & as.integer(month_day_endTest[[1]][2]) <= 30) == FALSE) {
        warning('The day parameter in month_day_end is not a valid value.  Please correct. \n')
        return()
      }
    } else if (month_day_endTest[[1]][1] %in% c('2')) {
      if ((as.integer(month_day_endTest[[1]][2]) >= 1 & as.integer(month_day_endTest[[1]][2]) <= 28) == FALSE) {
        warning('The day parameter in month_day_end is not a valid value.  Please correct. \n')
        return()
      }
    } else {
      if ((as.integer(month_day_endTest[[1]][2]) >= 1 & as.integer(month_day_endTest[[1]][2]) <= 31) == FALSE) {
        warning('The day parameter in month_day_end is not a valid value.  Please correct. \n')
        return()
      }
    }
  }

  if (year_start != '') {
    if (as.integer(year_start) < 1994 | as.integer(year_start) > year(Sys.Date())) {
      warning('The year_start parameter must be between 1994 and the current year.  Please correct. \n')
      return()
    }

    if (month_day_start != '') {
      if (ymd(paste0(year_start,'-',month_day_start)) > ymd(Sys.Date())) {
        warning('The combination of year_start and month_day_start implies data from the future must be retrieved.  Please correct. \n')
        return()
      }
    }
  }

  if (yearEnd != '') {
    if (as.integer(yearEnd) < 1994 | as.integer(yearEnd) > year(Sys.Date())) {
      warning('The yearEnd parameter must be between 1994 and the current year.  Please correct. \n')
      return()
    }
    if (month_day_end != '') {
      if (ymd(paste0(yearEnd,'-',month_day_end)) > ymd(Sys.Date())) {
        warning('The combination of yearEnd and month_day_end implies data from the future must be retrieved.  Please correct. \n')
        return()
      }
    }
  }


  yearsToRequest <- seq(as.integer(year_start),as.integer(yearEnd))

  if (exclude_years != '') {
    exclude_yearsTest <- strsplit(exclude_years,',')
    for (z in 1:length(exclude_yearsTest[[1]])) {
      if (as.integer(exclude_yearsTest[[1]][z]) < 1994 | as.integer(exclude_yearsTest[[1]][z]) > year(Sys.Date())) {
        warning('One of the years included in the exclude_years parameter is not in the \n
                proper range (1994-CurrentYear).  Please correct. \n')
        return()
      }
      yearsToRequest <- yearsToRequest[yearsToRequest != as.integer(exclude_yearsTest[[1]][z])]
    }
    }

  if (length(yearsToRequest) <= 3) {
    warning('At least three unique years must be used in this query. Please correct. \n')
    return()
  }

  if ((((year_start != '') & (yearEnd == '')) | ((year_start == '') & (yearEnd != ''))) == TRUE) {
    warning('Both the starting and ending years myst be specified explicitly if using years. \n')
    return()
  }

  if ((accumulation_start_date != '') == TRUE) {
    accumulation_start_dateTest <- strsplit(accumulation_start_date,'-')
    for (z in 1:length(accumulation_start_dateTest [[1]])) {
      if (nchar(accumulation_start_dateTest[[1]][z]) != 2) {
        warning('The parameter accumulation_start_date is not properly formatted.  Please correct. \n')
        return()
      }
    }
    if ((as.integer(accumulation_start_dateTest [[1]][1]) >= 1 & as.integer(accumulation_start_dateTest [[1]][1]) <= 12) == FALSE) {
      warning('The month parameter in accumulation_start_date is not a valid value.  Please correct. \n')
      return()
    }
    if (accumulation_start_dateTest [[1]][1] %in% c('4','6','9','11')) {
      if ((as.integer(accumulation_start_dateTest [[1]][2]) >= 1 & as.integer(accumulation_start_dateTest[[1]][2]) <= 30) == FALSE) {
        warning('The day parameter in accumulation_start_date is not a valid value.  Please correct. \n')
        return()
      }
    } else if (accumulation_start_dateTest [[1]][1] %in% c('2')) {
      if ((as.integer(accumulation_start_dateTest [[1]][2]) >= 1 & as.integer(accumulation_start_dateTest [[1]][2]) <= 28) == FALSE) {
        warning('The day parameter in accumulation_start_date is not a valid value.  Please correct. \n')
        return()
      }
    } else {
      if ((as.integer(accumulation_start_dateTest [[1]][2]) >= 1 & as.integer(accumulation_start_dateTest[[1]][2]) <= 31) == FALSE) {
        warning('The day parameter in accumulation_start_date is not a valid value.  Please correct. \n')
        return()
      }
    }
    if (accumulation_start_dateTest[[1]][1] == month_day_startTest[[1]][1]) {
      if (accumulation_start_dateTest[[1]][2] > month_day_startTest[[1]][2]) {
        warning('The accumulation_start_date parameter must come before the startDate parameter.  Please correct. \n')
        return()
      }
    }
  }

  if ((gdd_method %in% c('standard','modifiedstandard','min-temp-cap','min-temp-constant')) == FALSE) {
    warning('Valid values for the GDD method used to calculate growing degree days are \n
            \'standard\', \'modifiedstandard\', \'min-temp-cap\', \'min-temp-constant\'.\n
            Please change gdd_method to one of these values. \n')
    return()
  }

  if (suppressWarnings(is.na(as.integer(gdd_base_temp))) == TRUE) {
    warning('The gdd_base_temp parameter is not a valid value.  Please correct. \n')
    return()
  }

  if (suppressWarnings(is.na(as.integer(gdd_min_boundary))) == TRUE) {
    warning('The gdd_min_boundary parameter is not a valid value.  Please correct. \n')
    return()
  }

  if (suppressWarnings(is.na(as.integer(gdd_max_boundary))) == TRUE) {
    warning('The gdd_max_boundary parameter is not a valid value.  Please correct. \n')
    return()
  }

  ##############################################################################
  dataList <- list()

  # Create query

  urlAddress <- "https://api.awhere.com/v2/agronomics"

  strBeg <- paste0('/locations')
  strCoord <- paste0('/',latitude,',',longitude)
  strType <- paste0('/agronomicnorms')

  if (month_day_end != '') {
    strMonthsDays <- paste0('/',month_day_start,',',month_day_end)
  } else {
    strMonthsDays <- paste0('/',month_day_start,',',month_day_start)
  }

  if (exclude_years != '') {
    strexclude_years <- paste0('&excludeYears=',exclude_years)
  } else {
    strexclude_years <- ''
  }

  gdd_methodString      <- paste0('?gddMethod=',gdd_method)
  gdd_base_tempString    <- paste0('&gddBaseTemp=',gdd_base_temp)
  gdd_min_boundaryString <- paste0('&gddMinBoundary=',gdd_min_boundary)
  gdd_max_boundaryString <- paste0('&gddMaxBoundary=',gdd_max_boundary)

  if (year_start != '' & yearEnd != '') {
    strYearsType <- paste0('/years')
    strYears <- paste0('/',year_start,',',yearEnd)
    address <- paste0(urlAddress, strBeg, strCoord, strType, strMonthsDays, strYearsType,
                      strYears,gdd_methodString,gdd_base_tempString,gdd_min_boundaryString,gdd_max_boundaryString,exclude_years)
  } else {
    address <- paste0(urlAddress, strBeg, strCoord, strType, strMonthsDays,gdd_methodString,
                      gdd_base_tempString,gdd_min_boundaryString,gdd_max_boundaryString,exclude_years)
  }
  doWeatherGet <- TRUE
  while (doWeatherGet == TRUE) {
    requestString <- 'request <- GET(address,
    add_headers(Authorization =
    paste0(\"Bearer \", awhereEnv75247$token)))'
    # Make request
    eval(parse(text = requestString))

    a <- suppressMessages(content(request, as = "text"))

    #The JSONLITE Serializer properly handles the JSON conversion

    x <- jsonlite::fromJSON(a,flatten = TRUE)

    if (grepl('API Access Expired',a)) {
      get_token(awhereEnv75247$uid,awhereEnv75247$secret)
    } else {
      doWeatherGet <- FALSE
    }
  }

  data <- as.data.table(x[[3]])

  varNames <- colnames(data)
  #This removes the non-data info returned with the JSON object
  data[,grep('_links',varNames) := NULL, with = FALSE]
  data[,grep('.units',varNames) := NULL, with = FALSE]

  #   varNames <- colnames(data)
  #
  #   for (x in 1:length(varNames)) {
  #
  #     if (varNames[x] == 'day'){
  #       varNames[x] <- 'monthDay'
  #     } else if (varNames[x] == 'location.latitude'){
  #       varNames[x] <- 'latitude'
  #     } else if (varNames[x] == 'location.longitude'){
  #       varNames[x] <- 'longitude'
  #     } else if (varNames[x] == 'meanTemp.average'){
  #       varNames[x] <- 'avgMeanTemperature'
  #     } else if (varNames[x] == 'meanTemp.stdDev'){
  #       varNames[x] <- 'avgMeanTemperatureStdDev'
  #     } else if (varNames[x] == 'minTemp.average'){
  #       varNames[x] <- 'avgMinTemperature'
  #     } else if (varNames[x] == 'minTemp.stdDev'){
  #       varNames[x] <- 'avgMinTemperatureStdDev'
  #     } else if (varNames[x] == 'maxTemp.average'){
  #       varNames[x] <- 'avgMaxTemperature'
  #     } else if (varNames[x] == 'maxTemp.stdDev'){
  #       varNames[x] <- 'avgMaxTemperatureStdDev'
  #     } else if (varNames[x] == 'precipitation.average'){
  #       varNames[x] <- 'avgPrecipitation'
  #     } else if (varNames[x] == 'precipitation.stdDev'){
  #       varNames[x] <- 'avgPrecipitationStdDev'
  #     } else if (varNames[x] == 'solar.average'){
  #       varNames[x] <- 'avgSolarEnergy'
  #     } else if (varNames[x] == 'solar.stdDev'){
  #       varNames[x] <- 'avgSolarEnergyStdDev'
  #     } else if (varNames[x] == 'minHumidity.average'){
  #       varNames[x] <- 'avgMinHumidity'
  #     } else if (varNames[x] == 'minHumidity.stdDev'){
  #       varNames[x] <- 'avgMinHumidityStdDev'
  #     } else if (varNames[x] == 'maxHumidity.average'){
  #       varNames[x] <- 'avgMaxHumidity'
  #     } else if (varNames[x] == 'maxHumidity.stdDev'){
  #       varNames[x] <- 'avgMaxHumidityStdDev'
  #     } else if (varNames[x] == 'dailyMaxWind.average'){
  #       varNames[x] <- 'avgDailyMaxWind'
  #     } else if (varNames[x] == 'dailyMaxWind.stdDev'){
  #       varNames[x] <- 'avgDailyMaxWindStdDev'
  #     } else if (varNames[x] == 'averageWind.average'){
  #       varNames[x] <- 'avgWind'
  #     } else if (varNames[x] == 'averageWind.stdDev'){
  #       varNames[x] <- 'avgWindStdDev'
  #     }
  #

  setnames(data,varNames)
  setkey(data,day)

  return(as.data.frame(data))
}

