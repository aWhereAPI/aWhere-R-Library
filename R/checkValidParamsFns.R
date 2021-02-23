#' @title Check Valid Credentials Loaded
#'
#' @description
#' \code{checkCredentials} Checks to see if valid aWhere API credentials are loaded
#'
#' @param keyToUse aWhere API key to use
#' @param secretToUse aWhere API secret to use
#' @param tokenToUse aWhere API token to use

checkCredentials <- function(keyToUse,secretToUse,tokenToUse) {

  if (is.null(keyToUse) | is.null(secretToUse) | is.null(tokenToUse)) {
    stop('Please Run the Command \'get_token()\' and then retry running command. \n')
  }
}

#' @title Check Valid Field ID
#'
#' @description
#' \code{checkValidField} Checks to see if the Field ID is valid
#'
#' @param field_id the field_id having previously been created with the createField Function
#' @param keyToUse aWhere API key to use
#' @param secretToUse aWhere API secret to use
#' @param tokenToUse aWhere API token to use


checkValidField <- function(field_id
                            ,keyToUse
                            ,secretToUse
                            ,tokenToUse) {

  currentFields <- get_fields(field_id
                              ,keyToUse = keyToUse
                              ,secretToUse = secretToUse
                              ,tokenToUse = tokenToUse)
  
  if ((field_id %in% currentFields$fieldId) == FALSE) {
    stop('The Provided field name is not a field currently associated with your account. \n
            Please create the field before proceeding. \n')
  }
}

#' @title Check Valid Lat/Long
#'
#' @description
#' \code{checkValidLatLong} Checks to see if Lat/Long coordinates are valid
#'
#' @param latitude the latitude for the location for which you want data
#' @param longitude the latitude for the location for which you want data

checkValidLatLong <- function(latitude,longitude) {
  if (suppressWarnings(is.na(as.double(latitude))) == FALSE) {
    if ((as.double(latitude) >= -90 & as.double(latitude) <= 90) == FALSE) {
      stop('The entered Latitude Value is not valid. Please correct\n')
    }
  } else {
    stop('The entered Latitude Value is not valid. Please correct\n')
  }

  if (suppressWarnings(is.na(as.double(longitude))) == FALSE) {
    if ((as.double(longitude) >= -180 & as.double(longitude) <= 180) == FALSE) {
      stop('The entered Longitude Value is not valid. Please correct\n')
    }
  } else {
    stop('The entered Longitude Value is not valid. Please correct\n')
  }
}

#' @title Check Valid Start End Dates
#'
#' @description
#' \code{checkValidStartEndDates} Checks to see if Start/End Dates passed are valid
#'
#' @param day_start character string of start date in form: YYYY-MM-DD
#' @param day_end character string of end date in form: YYYY-MM-DD

checkValidStartEndDates <- function(day_start,day_end) {

  if (day_start == '' & day_end != '') {
    stop('The day_end is specified so must day_start. Please correct\n')
  }

  if (day_end != '') {
    if (lubridate::ymd(day_start) > lubridate::ymd(day_end)) {
      stop('The endDate must come after the startDate. Please correct\n')
    }
  }


  if ((day_start != '') == TRUE) {
    if (suppressWarnings(is.na(lubridate::ymd(day_start))) == TRUE) {
      stop('The Start Date is Not Properly Formatted.  Please change to proper format. \n')
    } #else if (lubridate::ymd(day_start) > lubridate::ymd(Sys.Date()) - lubridate::days(1)) {
      #stop('By default, this function can only be used to access data up until yesterday. \n
      #        Use the GetForecast function to request data from today onward.\n')
    #}
  }

  if ((day_end != '') == TRUE) {
    if (suppressWarnings(is.na(lubridate::ymd(day_end))) == TRUE) {
      stop('The End Date is Not Properly Formatted.  Please change to proper format. \n')
    } #else if (lubridate::ymd(day_end) > lubridate::ymd(Sys.Date()) - lubridate::days(1)) {
      #stop('By default, this function can only be used to access data up until yesterday. \n
      #        Use the GetForecast function to request data from today onward.\n')
    #}
  }
}

#' @title Check Valid Start End Dates for Agronomics Endpoint
#'
#' @description
#' \code{checkValidStartEndDates} Checks to see if Start/End Dates passed are valid
#'
#' @param day_start character string of start date in form: YYYY-MM-DD
#' @param day_end character string of end date in form: YYYY-MM-DD

checkValidStartEndDatesAgronomics <- function(day_start,day_end) {

  if (day_start == '' & day_end != '') {
    stop('The day_end is specified, therefor day_start must also be. Please correct\n')
  }

  if (day_end != '') {
    if (lubridate::ymd(day_start) > lubridate::ymd(day_end)) {
      stop('The endDate must come after the startDate. Please correct\n')
    } #else if ((lubridate::ymd(day_end) - lubridate::ymd(Sys.Date())) > 15) {
      #stop('Forecast data only availabe 15 days into future\n')
    #}
  }

  if ((day_start != '') == TRUE) {
    if (suppressWarnings(is.na(lubridate::ymd(day_start))) == TRUE) {
      stop('The Start Date is Not Properly Formatted.  Please change to proper format. \n')
    }
  }

  if ((day_end != '') == TRUE) {
    if (suppressWarnings(is.na(lubridate::ymd(day_end))) == TRUE) {
      stop('The End Date is Not Properly Formatted.  Please change to proper format. \n')
    }
  }
}

#' @title Check Forecast Parameters
#'
#' @description
#' \code{checkForecastParams} Checks to see if Forecast Params are valid
#'
#' @param day_start character string of start date in form: YYYY-MM-DD
#' @param block_size Integer value that corresponds to the number of hours to include in each time block.

checkForecastParams <- function(block_size) {

  if ((24 %% block_size) != 0){
    stop('The block size must divide evenly into 24. Please correct\n')
  }
}

#' @title Check Valid Start End Dates for Forecast
#'
#' @description
#' \code{checkValidStartEndDatesForecast} Checks to see if Start/End Dates passed are valid for Forecast
#'
#' @param day_start character string of start date in form: YYYY-MM-DD
#' @param day_end character string of end date in form: YYYY-MM-DD

checkValidStartEndDatesForecast <- function(day_start,day_end) {

  if (day_start == '' & day_end != '') {
    stop('The day_end is specified so must day_start. Please correct\n')
  }

  if ((day_start != '') == TRUE) {
    if (suppressWarnings(is.na(lubridate::ymd(day_start))) == TRUE) {
      stop('The Start Date is Not Properly Formatted.  Please change to proper format. \n')
    } #else if (lubridate::ymd(day_start) < lubridate::ymd(Sys.Date())) {
      #stop('By default, this function can only be used to access data from today onward. \n
      #      Use the GetWeatherObservationsHist function to request data from yesterday backwards.\n')
    #}
  }

  if ((day_end != '') == TRUE) {
    if (lubridate::ymd(day_start) > lubridate::ymd(day_end)) {
      stop('The endDate must come after the startDate. Please correct\n')
    } else if (suppressWarnings(is.na(lubridate::ymd(day_end))) == TRUE) {
      stop('The End Date is Not Properly Formatted.  Please change to proper format. \n')
    } #else if (lubridate::ymd(day_end) < lubridate::ymd(Sys.Date())) {
      #stop('This function can only access data from today onward.\n')
    #} else if ((lubridate::ymd(day_end) - lubridate::ymd(day_start)) > 14) {
      #stop('Forecast data only availabe 15 days into future\n')
    #}
  }
}

#' @title Check Valid GDD Params
#'
#' @description
#' \code{checkGDDParams} Checks to see if GDD params are valid
#'
#' @param gdd_method There are variety of equations available for calculating growing degree-days.
#'                     Valid entries are: 'standard', 'modifiedstandard', 'min-temp-cap', 'min-temp-constant'
#'                     See the API documentation for a description of each method.  The standard
#'                     method will be used if none is specified
#' @param gdd_base_temp The base temp to use for the any of the GDD equations. The default value of 10 will
#'                       be used if none is specified
#' @param gdd_min_boundary The minimum boundary to use in the selected GDD equation.
#'                           The behavior of this value is different depending on the equation you're using
#'                           The default value of 10 will be used if none is specified
#' @param gdd_max_boundary The max boundary to use in the selected GDD equation. The
#'                          behavior of this value is different depending on the equation you're using.
#'                          The default value of 30 will be used if none is specified

checkGDDParams <- function(gdd_method,gdd_base_temp,gdd_min_boundary,gdd_max_boundary) {
  if ((gdd_method %in% c('standard','modifiedstandard','min-temp-cap','min-temp-constant')) == FALSE) {
    stop('Valid values for the GDD method used to calculate growing degree days are \n
            \'standard\', \'modifiedstandard\', \'min-temp-cap\', \'min-temp-constant\'.\n
            Please change gdd_method to one of these values. \n')
  }

  if (suppressWarnings(is.na(as.integer(gdd_base_temp))) == TRUE) {
    stop('The gdd_base_temp parameter is not a valid value.  Please correct. \n')
  }

  if (suppressWarnings(is.na(as.integer(gdd_min_boundary))) == TRUE) {
    stop('The gdd_min_boundary parameter is not a valid value.  Please correct. \n')
  }

  if (suppressWarnings(is.na(as.integer(gdd_max_boundary))) == TRUE) {
    stop('The gdd_max_boundary parameter is not a valid value.  Please correct. \n')
  }
}

#' @title Check Accumulation Start Date
#'
#' @description
#' \code{checkAccumulationStartDate} Checks to see if the accumulation start date is valid
#'
#' @param accumulation_start_date If you want to start counting accumulations from
#'                                 before the specified start date (or before the
#'                                 planting date if using the most recent Planting),
#'                                 use this parameter to specify the date from which
#'                                 you wish to start counting. The daily values object
#'                                 will still only return the days between the start
#'                                 and end date. This date must come before the start date.
#' @param - month_day_start

checkAccumulationStartDate <- function(accumulation_start_date,month_day_start) {
  if ((accumulation_start_date != '') == TRUE) {

    accumulation_start_dateTest <- strsplit(accumulation_start_date,'-')

    if (nchar(accumulation_start_dateTest[[1]][1]) != 4) {
      stop('The parameter accumulation_start_date is not properly formatted.  Please correct. \n')
    }

    if ((as.integer(accumulation_start_dateTest [[1]][2]) >= 1 & as.integer(accumulation_start_dateTest [[1]][2]) <= 12) == FALSE) {
      stop('The month parameter in accumulation_start_date is not a valid value.  Please correct. \n')
    }

    if (accumulation_start_dateTest [[1]][2] %in% c('4','6','9','11')) {
      if ((as.integer(accumulation_start_dateTest [[1]][3]) >= 1 & as.integer(accumulation_start_dateTest[[1]][3]) <= 30) == FALSE) {
        stop('The day parameter in accumulation_start_date is not a valid value.  Please correct. \n')
      }
    } else if (accumulation_start_dateTest [[1]][2] %in% c('2')) {
      if ((as.integer(accumulation_start_dateTest [[1]][3]) >= 1 & as.integer(accumulation_start_dateTest [[1]][3]) <= 28) == FALSE) {
        stop('The day parameter in accumulation_start_date is not a valid value.  Please correct. \n')
      }
    } else{
      if ((as.integer(accumulation_start_dateTest [[1]][3]) >= 1 & as.integer(accumulation_start_dateTest[[1]][3]) <= 31) == FALSE) {
        stop('The day parameter in accumulation_start_date is not a valid value.  Please correct. \n')
      }
    }
    month_day_startTest <- strsplit(month_day_start,'-')

    if (accumulation_start_dateTest[[1]][1] == month_day_startTest[[1]][1]) {
      if (accumulation_start_dateTest[[1]][2] > month_day_startTest[[1]][2]) {
        stop('The accumulation_start_date parameter must come before the startDate parameter.  Please correct. \n')
      } else if (accumulation_start_dateTest[[1]][1] == month_day_startTest[[1]][2]) {
        if (accumulation_start_dateTest[[1]][3] > month_day_startTest[[1]][3]) {
          stop('The accumulation_start_date parameter must come before the startDate parameter.  Please correct. \n')
        }
      }
    }

    if(difftime(lubridate::ymd(Sys.Date()),lubridate::ymd(accumulation_start_date),units = 'days') > 365) {
      stop('The parameter accumulation_start_date must be withinin 365 days of todays date.  Please correct. \n')
    }

    if(difftime(lubridate::ymd(Sys.Date()),lubridate::ymd(accumulation_start_date),units = 'days') < 1) {
      stop('The parameter accumulation_start_date must be no earlier than yesterday.  Please correct. \n')
    }
  }
}

#' @title Check Accumulation Start Date Norms
#'
#' @description
#' \code{checkAccumulationStartDate} Checks to see if the accumulation start date is valid
#'
#' @param accumulation_start_date If you want to start counting accumulations from
#'                                 before the specified start date (or before the
#'                                 planting date if using the most recent Planting),
#'                                 use this parameter to specify the date from which
#'                                 you wish to start counting. The daily values object
#'                                 will still only return the days between the start
#'                                 and end date. This date must come before the start date.
#' @param - month_day_start

checkAccumulationStartDateNorms <- function(accumulation_start_date,month_day_start) {
  if ((accumulation_start_date != '') == TRUE) {
    accumulation_start_dateTest <- strsplit(accumulation_start_date,'-')
    if ((as.integer(accumulation_start_dateTest [[1]][1]) >= 1 & as.integer(accumulation_start_dateTest [[1]][1]) <= 12) == FALSE) {
      stop('The month parameter in accumulation_start_date is not a valid value.  Please correct. \n')
    }
    if (accumulation_start_dateTest [[1]][1] %in% c('4','6','9','11')) {
      if ((as.integer(accumulation_start_dateTest [[1]][2]) >= 1 & as.integer(accumulation_start_dateTest[[1]][2]) <= 30) == FALSE) {
        stop('The day parameter in accumulation_start_date is not a valid value.  Please correct. \n')
      }
    } else if (accumulation_start_dateTest [[1]][1] %in% c('2')) {
      if ((as.integer(accumulation_start_dateTest [[1]][2]) >= 1 & as.integer(accumulation_start_dateTest [[1]][2]) <= 28) == FALSE) {
        stop('The day parameter in accumulation_start_date is not a valid value.  Please correct. \n')
      }
    } else {
      if ((as.integer(accumulation_start_dateTest [[1]][2]) >= 1 & as.integer(accumulation_start_dateTest[[1]][2]) <= 31) == FALSE) {
        stop('The day parameter in accumulation_start_date is not a valid value.  Please correct. \n')
      }
    }
    month_day_startTest <- strsplit(month_day_start,'-')

    if (accumulation_start_dateTest[[1]][1] > month_day_startTest[[1]][1]) {
      stop('The accumulation_start_date parameter must come before the startDate parameter.  Please correct. \n')
    } else if (accumulation_start_dateTest[[1]][1] == month_day_startTest[[1]][1]) {
      if (accumulation_start_dateTest[[1]][2] > month_day_startTest[[1]][2]) {
        stop('The accumulation_start_date parameter must come before the startDate parameter.  Please correct. \n')
      }
    }
  }
}


#' @title Check Forecast Sources
#'
#' @description
#' \code{checkForecastSources} Checks that the requested Forecast source is valid

checkForecastSources <- function(sources) {
  if ((sources %in% c('metar','mesonet','metar-mesonet','pws','all')) == FALSE) {
    stop('The specified source is not valid. Please correct. \n')
  }
}

#' @title Checks Start/End Dates for Norms
#'
#' @description
#' \code{checkNormsStartEndDates} Checks Start/End Dates for Norms endpoint are valid
#'
#' @param monthday_start character string of the month and day for the start
#'                         of the range of days you are calculating norms for, e.g., '07-01' (July 1)
#' @param monthday_end character string of the month and day for the end of the
#'                       range of days you are calculating norms for, e.g., '07-10' (July 10)

checkNormsStartEndDates <- function(month_day_start,month_day_end) {
  month_day_startTest <- strsplit(month_day_start,'-')

  if ((as.integer(month_day_startTest[[1]][1]) >= 1 & as.integer(month_day_startTest[[1]][1]) <= 12) == FALSE) {
    stop('The month parameter in month_day_start is not a valid value.  Please correct. \n')
  }
  if (month_day_startTest[[1]][1] %in% c('4','6','9','11')) {
    if ((as.integer(month_day_startTest[[1]][2]) >= 1 & as.integer(month_day_startTest[[1]][2]) <= 30) == FALSE) {
      stop('The day parameter in month_day_start is not a valid value.  Please correct. \n')
    }
  } else if (month_day_startTest[[1]][1] %in% c('2')) {
    if ((as.integer(month_day_startTest[[1]][2]) >= 1 & as.integer(month_day_startTest[[1]][2]) <= 28) == FALSE) {
      stop('The day parameter in month_day_start is not a valid value.  Please correct. \n')
    }
  } else {
    if ((as.integer(month_day_startTest[[1]][2]) >= 1 & as.integer(month_day_startTest[[1]][2]) <= 31) == FALSE) {
      stop('The day parameter in month_day_start is not a valid value.  Please correct. \n')
    }
  }

  month_day_endTest <- strsplit(month_day_end,'-')

  if ((as.integer(month_day_endTest[[1]][1]) >= 1 & as.integer(month_day_endTest[[1]][1]) <= 12) == FALSE) {
    stop('The month parameter in month_day_end is not a valid value.  Please correct. \n')
  }
  if (month_day_endTest[[1]][1] %in% c('4','6','9','11')) {
    if ((as.integer(month_day_endTest[[1]][2]) >= 1 & as.integer(month_day_endTest[[1]][2]) <= 30) == FALSE) {
      stop('The day parameter in month_day_end is not a valid value.  Please correct. \n')
    }
  } else if (month_day_endTest[[1]][1] %in% c('2')) {
    if ((as.integer(month_day_endTest[[1]][2]) >= 1 & as.integer(month_day_endTest[[1]][2]) <= 28) == FALSE) {
      stop('The day parameter in month_day_end is not a valid value.  Please correct. \n')
    }
  } else {
    if ((as.integer(month_day_endTest[[1]][2]) >= 1 & as.integer(month_day_endTest[[1]][2]) <= 31) == FALSE) {
      stop('The day parameter in month_day_end is not a valid value.  Please correct. \n')
    }
  }
}

#' @title Check Years to Request for Norms
#'
#' @description
#' \code{checkNormsYearsToRequest} Check that the Years to be Requested for Norms is valid
#'
#' @param year_start the starting year (inclusive) of the range of years for which
#'                     you're calculating norms, e.g., 2008
#' @param year_end the end year (inclusive) of the range of years for which you're
#'                   calculating norms, e.g., 2015
#' @param exclude_years You can opt to exclude one or more years from the range, and
#'                        it's values will not be included in the averages. To exclude
#'                       multiple years, provide a vector of years. Note: You must always have
#'                       at least three years of data to average

checkNormsYearsToRequest <- function(year_start,year_end,month_day_start,month_day_end,exclude_years) {
  if (year_start != '') {
    if (as.integer(year_start) < 1994 | as.integer(year_start) > year(Sys.Date())) {
      stop('The year_start parameter must be between 1994 and the current year.  Please correct. \n')
    }
  }
  if (year_end != '') {
    if (as.integer(year_end) < 1994 | as.integer(year_end) > year(Sys.Date())) {
      stop('The year_end parameter must be between 1994 and the current year.  Please correct. \n')
    }
  }
  if (year_start != '' & month_day_start != '') {
    if (lubridate::ymd(paste0(year_start,'-',month_day_start)) > lubridate::ymd(Sys.Date())) {
      stop('The combination of year_start and month_day_start implies data from the future must be retrieved.  Please correct. \n')
    }
  }

  if (year_end != '' & month_day_end != '') {
    if (lubridate::ymd(paste0(year_end,'-',month_day_end)) > lubridate::ymd(Sys.Date())) {
      stop('The combination of year_end and month_day_end implies data from the future must be retrieved.  Please correct. \n')
    }
  }

  if ((((year_start != '') & (year_end == '')) | ((year_start == '') & (year_end != ''))) == TRUE) {
    stop('Both the starting and ending years myst be specified explicitly if using years. \n')
  }

  if ((year_start != '') & (year_end != '')) {
    yearsToRequest <- seq(as.integer(year_start),as.integer(year_end))

    if (length(exclude_years) != 0) {
      for (z in 1:length(exclude_years)) {
        if (as.integer(exclude_years[z]) < 1994 | as.integer(exclude_years[z]) > year(Sys.Date())) {
          stop('One of the years included in the exclude_years parameter is not in the \n
                  proper range (1994-CurrentYear).  Please correct. \n')
        }
        yearsToRequest <- yearsToRequest[yearsToRequest != as.integer(exclude_years[z])]
      }
      }

    if (length(yearsToRequest) <= 3) {
      stop('At least three unique years must be used in this query. Please correct. \n')
    }
  }
}

#' @title Check Properties for Weather Endpoint
#'
#' @description
#' \code{checkPropertiesWeather} Check that the Years to be Requested for Norms is valid
#'
#' @param propertiesToInclude vector of properties requested from API

checkPropertiesEndpoint <- function(endpoint,propertiesToInclude) {


  if(endpoint == 'weather') {
    validProperties <- c('temperatures'
                         ,'precipitation'
                         ,'solar'
                         ,'relativeHumidity'
                         ,'wind')
  } else if (endpoint == 'weather_norms') {
    validProperties <- c('meanTemp'
                         ,'maxTemp'
                         ,'minTemp'
                         ,'precipitation'
                         ,'solar'
                         ,'maxHumidity'
                         ,'minHumidity'
                         ,'dailyMaxWind')
  } else if (endpoint == 'agronomics') {
    validProperties <- c('gdd'
                         ,'pet'
                         ,'ppet'
                         ,'accumulatedGdd'
                         ,'accumulatedPrecipitation'
                         ,'accumulatedPet'
                         ,'accumulatedPpet'
                         ,'accumulations')
  }



  if (propertiesToInclude[1] != '' | length(propertiesToInclude) > 1 ) {
    if (length(base::setdiff(propertiesToInclude,validProperties)) > 0) {
      stop(paste0('Invalid Entry for Properties Attributes.  Valid values are ',paste0(validProperties,collapse = ', ')))
    }
  }
}


