#' @title weather_norms_fields.
#'
#' @description
#' \code{weather_norms_fields} calls Historic Weather Norms Endpoint of API using Field Location Construct
#'
#' @details
#' This is a flexible API that allows you to calculate the averages for weather attribute
#' across any range of years for which we have data. Whereas the Daily Observed API only
#' supports up to 30 months of daily data, this API allow you to compare this year and
#' the previous year to the long-term normals (however many years you want to include).
#' Each day's worth of data also includes the standard deviation for the average.
#'
#' @references http://developer.awhere.com/api/reference/weather/norms
#'
#' @param - field_id: the field_id having previously been created with the createField Function
#' @param - monthday_start: character string of the month and day for the start
#'                         of the range of days you are calculating norms for, e.g., '07-01' (July 1)
#' @param - monthday_end: character string of the month and day for the end of the
#'                       range of days you are calculating norms for, e.g., '07-10' (July 10)
#' @param - year_start: the starting year (inclusive) of the range of years for which
#'                     you're calculating norms, as a string, e.g., '2008'
#' @param - year_start: the end year (inclusive) of the range of years for which you're
#'                     calculating norms, as a string, e.g., '2015'
#' @param - exclude_years: You can opt to exclude one or more years from the range, and
#'                        it's values will not be included in the averages. To exclude
#'                        multiple years, separate them with a comma. Note: You must always have
#'                        at least three years of data to average
#'
#' @return data.table of requested data for dates requested
#'
#' @import httr
#' @import data.table
#' @import lubridate
#' @import jsonlite
#'
#' @examples
#' weather_norms_fields('field123','07-01', '07-10', '2008', '2015','2010,2011')

#' @export


weather_norms_fields <- function(field_id, monthday_start = '', monthday_end = '',
                            year_start = '', year_end = '',exclude_years = '') {

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

  if (monthday_start != '') {
    monthday_startTest <- strsplit(monthday_start,'-')
    for (z in 1:length(monthday_startTest[[1]])) {
      if (nchar(monthday_startTest[[1]][z]) != 2) {
        warning('The parameter monthday_start is not properly formatted.  Please correct. \n')
        return()
      }
    }
    if ((as.integer(monthday_startTest[[1]][1]) >= 1 & as.integer(monthday_startTest[[1]][1]) <= 12) == FALSE) {
      warning('The month parameter in monthday_start is not a valid value.  Please correct. \n')
      return()
    }
    if (monthday_startTest[[1]][1] %in% c('4','6','9','11')) {
      if ((as.integer(monthday_startTest[[1]][2]) >= 1 & as.integer(monthday_startTest[[1]][2]) <= 30) == FALSE) {
        warning('The day parameter in monthday_start is not a valid value.  Please correct. \n')
        return()
      }
    } else if (monthday_startTest[[1]][1] %in% c('2')) {
      if ((as.integer(monthday_startTest[[1]][2]) >= 1 & as.integer(monthday_startTest[[1]][2]) <= 28) == FALSE) {
        warning('The day parameter in monthday_start is not a valid value.  Please correct. \n')
        return()
      }
    } else {
      if ((as.integer(monthday_startTest[[1]][2]) >= 1 & as.integer(monthday_startTest[[1]][2]) <= 31) == FALSE) {
        warning('The day parameter in monthday_start is not a valid value.  Please correct. \n')
        return()
      }
    }
  }

  if (monthday_end != '') {
    if (monthday_start == '') {
      warning('If the parameter monthday_end is specified so must monthday_start.  Please correct. \n')
      return()
    }
    monthday_endTest <- strsplit(monthday_end,'-')
    for (z in 1:length(monthday_endTest[[1]])) {
      if (nchar(monthday_endTest[[1]][z]) != 2) {
        warning('The parameter monthday_end is not properly formatted.  Please correct. \n')
        return()
      }
    }
    if ((as.integer(monthday_endTest[[1]][1]) >= 1 & as.integer(monthday_endTest[[1]][1]) <= 12) == FALSE) {
      warning('The month parameter in monthday_end is not a valid value.  Please correct. \n')
      return()
    }
    if (monthday_endTest[[1]][1] %in% c('4','6','9','11')) {
      if ((as.integer(monthday_endTest[[1]][2]) >= 1 & as.integer(monthday_endTest[[1]][2]) <= 30) == FALSE) {
        warning('The day parameter in monthday_end is not a valid value.  Please correct. \n')
        return()
      }
    } else if (monthday_endTest[[1]][1] %in% c('2')) {
      if ((as.integer(monthday_endTest[[1]][2]) >= 1 & as.integer(monthday_endTest[[1]][2]) <= 28) == FALSE) {
        warning('The day parameter in monthday_end is not a valid value.  Please correct. \n')
        return()
      }
    } else {
      if ((as.integer(monthday_endTest[[1]][2]) >= 1 & as.integer(monthday_endTest[[1]][2]) <= 31) == FALSE) {
        warning('The day parameter in monthday_end is not a valid value.  Please correct. \n')
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

  if (year_start != '' & monthday_start != '') {
    if (ymd(paste0(year_start,'-',monthday_start)) > ymd(Sys.Date())) {
      warning('The combination of year_start and monthday_start implies data from the future must be retrieved.  Please correct. \n')
      return()
    }
  }

  if (year_end != '' & monthday_end != '') {
    if (ymd(paste0(year_end,'-',monthday_end)) > ymd(Sys.Date())) {
      warning('The combination of year_end and monthday_end implies data from the future must be retrieved.  Please correct. \n')
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

  ##############################################################################
  dataList <- list()

  # Create query

  urlAddress <- "https://api.awhere.com/v2/weather"

  strBeg <- paste0('/fields')
  strCoord <- paste0('/',field_id)
  strType <- paste0('/norms')

  if (monthday_start != '' & monthday_end != '') {
    strMonthsDays <- paste0('/',monthday_start,',',monthday_end)
  } else if (monthday_end != '') {
    strMonthsDays <- paste0('/',monthday_start,',',monthday_start)
  } else {
    strMonthsDays <- ''
  }


  if (exclude_years != '') {
    strexclude_years <- paste0('?excludeYears=',exclude_years)
  } else {
    strexclude_years <- ''
  }

    if (year_start != '' & year_end != '') {
    strYearsType <- paste0('/years')
    strYears <- paste0('/',year_start,',',year_end)
    address <- paste0(urlAddress, strBeg, strCoord, strType, strMonthsDays, strYearsType,strYears,strexclude_years)
  } else {
    address <- paste0(urlAddress, strBeg, strCoord, strType, strMonthsDays,strexclude_years)
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

  data <- as.data.table(x$norms)

  varNames <- colnames(data)
  #This removes the non-data info returned with the JSON object
  data[,grep('_links',varNames) := NULL, with = FALSE]
  data[,grep('.units',varNames) := NULL, with = FALSE]

  setnames(data,varNames)
  setkey(data,day)

  return(as.data.frame(data))
}

#' @title Get Weather Norms Lat Lon
#'
#' @description
#' \code{weather_norms_latlng} calls Historic Agronomic Norms by Geolocation Endpoint of API using Lat/Lon
#'
#' @details
#' This is a flexible API that allows you to calculate the averages for weather attribute
#' across any range of years for which we have data. Whereas the Daily Observed API only
#' supports up to 30 months of daily data, this API allow you to compare this year and
#' the previous year to the long-term normals (however many years you want to include).
#' Each day's worth of data also includes the standard deviation for the average.
#'
#' @references http://developer.awhere.com/api/reference/weather/norms
#'
#' @param - latitude: the latitude for the location for which you want data
#' @param - longitude: the latitude for the location for which you want data
#' @param - monthday_start: character string of the month and day for the start
#'                         of the range of days you are calculating norms for, e.g., '07-01' (July 1)
#' @param - monthday_end: character string of the month and day for the end of the
#'                       range of days you are calculating norms for, e.g., '07-10' (July 10)
#' @param - year_start: the starting year (inclusive) of the range of years for which
#'                     you're calculating norms, as a string, e.g., '2008'
#' @param - year_end: the end year (inclusive) of the range of years for which you're
#'                     calculating norms, as a string, e.g., '2015'
#' @param - exclude_years: You can opt to exclude one or more years from the range, and
#'                        it's values will not be included in the averages. To exclude
#'                        multiple years, separate them with a comma. Note: You must always have
#'                        at least three years of data to average
#'
#' @return data.table of requested data for dates requested
#'
#' @import httr
#' @import data.table
#' @import lubridate
#' @import jsonlite
#'
#' @examples
#' weather_norms_latlng('39.8282', '-98.5795', '07-01', '07-10', '2008', '2015','2010,2011')

#' @export


weather_norms_latlng <- function(latitude, longitude, monthday_start, monthday_end = '', year_start, year_end, exclude_years = '') {

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

  monthday_startTest <- strsplit(monthday_start,'-')
  for (z in 1:length(monthday_startTest[[1]])) {
    if (nchar(monthday_startTest[[1]][z]) != 2) {
      warning('The parameter monthday_start is not properly formatted.  Please correct. \n')
      return()
    }
  }
  if ((as.integer(monthday_startTest[[1]][1]) >= 1 & as.integer(monthday_startTest[[1]][1]) <= 12) == FALSE) {
    warning('The month parameter in monthday_start is not a valid value.  Please correct. \n')
    return()
  }
  if (monthday_startTest[[1]][1] %in% c('4','6','9','11')) {
    if ((as.integer(monthday_startTest[[1]][2]) >= 1 & as.integer(monthday_startTest[[1]][2]) <= 30) == FALSE) {
      warning('The day parameter in monthday_start is not a valid value.  Please correct. \n')
      return()
    }
  } else if (monthday_startTest[[1]][1] %in% c('2')) {
    if ((as.integer(monthday_startTest[[1]][2]) >= 1 & as.integer(monthday_startTest[[1]][2]) <= 28) == FALSE) {
      warning('The day parameter in monthday_start is not a valid value.  Please correct. \n')
      return()
    }
  } else {
    if ((as.integer(monthday_startTest[[1]][2]) >= 1 & as.integer(monthday_startTest[[1]][2]) <= 31) == FALSE) {
      warning('The day parameter in monthday_start is not a valid value.  Please correct. \n')
      return()
    }
  }

  if (monthday_end != '') {
    monthday_endTest <- strsplit(monthday_end,'-')
    for (z in 1:length(monthday_endTest[[1]])) {
      if (nchar(monthday_endTest[[1]][z]) != 2) {
        warning('The parameter monthday_end is not properly formatted.  Please correct. \n')
        return()
      }
    }
    if ((as.integer(monthday_endTest[[1]][1]) >= 1 & as.integer(monthday_endTest[[1]][1]) <= 12) == FALSE) {
      warning('The month parameter in monthday_end is not a valid value.  Please correct. \n')
      return()
    }
    if (monthday_endTest[[1]][1] %in% c('4','6','9','11')) {
      if ((as.integer(monthday_endTest[[1]][2]) >= 1 & as.integer(monthday_endTest[[1]][2]) <= 30) == FALSE) {
        warning('The day parameter in monthday_end is not a valid value.  Please correct. \n')
        return()
      }
    } else if (monthday_endTest[[1]][1] %in% c('2')) {
      if ((as.integer(monthday_endTest[[1]][2]) >= 1 & as.integer(monthday_endTest[[1]][2]) <= 28) == FALSE) {
        warning('The day parameter in monthday_end is not a valid value.  Please correct. \n')
        return()
      }
    } else {
      if ((as.integer(monthday_endTest[[1]][2]) >= 1 & as.integer(monthday_endTest[[1]][2]) <= 31) == FALSE) {
        warning('The day parameter in monthday_end is not a valid value.  Please correct. \n')
        return()
      }
    }
  }

  if (year_start != '') {
    if (as.integer(year_start) < 1994 | as.integer(year_start) > year(Sys.Date())) {
      warning('The year_start parameter must be between 1994 and the current year.  Please correct. \n')
      return()
    }

    if (monthday_start != '') {
      if (ymd(paste0(year_start,'-',monthday_start)) > ymd(Sys.Date())) {
        warning('The combination of year_start and monthday_start implies data from the future must be retrieved.  Please correct. \n')
        return()
      }
    }
  }

  if (year_end != '') {
    if (as.integer(year_end) < 1994 | as.integer(year_end) > year(Sys.Date())) {
      warning('The year_end parameter must be between 1994 and the current year.  Please correct. \n')
      return()
    }
    if (monthday_end != '') {
      if (ymd(paste0(year_end,'-',monthday_end)) > ymd(Sys.Date())) {
        warning('The combination of year_end and monthday_end implies data from the future must be retrieved.  Please correct. \n')
        return()
      }
    }
  }


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

  if ((((year_start != '') & (year_end == '')) | ((year_start == '') & (year_end != ''))) == TRUE) {
    warning('Both the starting and ending years myst be specified explicitly if using years. \n')
    return()
  }


  ##############################################################################
  dataList <- list()

  # Create query

  urlAddress <- "https://api.awhere.com/v2/weather"

  strBeg <- paste0('/locations')
  strCoord <- paste0('/',latitude,',',longitude)
  strType <- paste0('/norms')

  if (monthday_end != '') {
    strMonthsDays <- paste0('/',monthday_start,',',monthday_end)
  } else {
    strMonthsDays <- paste0('/',monthday_start,',',monthday_start)
  }

  if (exclude_years != '') {
    strexclude_years <- paste0('?excludeYears=',exclude_years)
  } else {
    strexclude_years <- ''
  }


  if (year_start != '' & year_end != '') {
    strYearsType <- paste0('/years')
    strYears <- paste0('/',year_start,',',year_end)
    address <- paste0(urlAddress, strBeg, strCoord, strType, strMonthsDays, strYearsType,strYears,strexclude_years)
  } else {
    address <- paste0(urlAddress, strBeg, strCoord, strType, strMonthsDays,strexclude_years)
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

  data <- as.data.table(x$norms)

  varNames <- colnames(data)
  #This removes the non-data info returned with the JSON object
  data[,grep('_links',varNames) := NULL, with = FALSE]
  data[,grep('.units',varNames) := NULL, with = FALSE]


  setnames(data,varNames)
  setkey(data,day)

  return(as.data.frame(data))
}
