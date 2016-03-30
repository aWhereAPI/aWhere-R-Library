#' @title Get Agronomic Values Fields.
#'
#' @description
#' \code{agronomic_values_fields} calls Agronomic Values and Accumulations Endpoint of API using Field Location Construct
#'
#' @details
#' Agronomic Values are calculated numbers that can be used to show the agronomic status of a field or crop.
#' These figures can be used, for example, to track and predict plant growth or identify water stress.
#' Accumulated values allow growers to easily identify how the weather has been over the season.
#' Both sets of data are commonly used on small and large farms alike.  This is a very flexible API
#' that supports a wide variety of configurations to get exactly the data you want as efficiently as
#' possible. It's also designed to work with the Fields and Plantings system to reduce the amount of input.
#' While a planting is not required to use this API, be sure to create Plantings for your Fields in order
#' to get the most out of the aWhere platform.  This function returns GDDs, Pet, P/Pet, accumulated GDD,
#' accumulated Precipitation, accumulated Pet, accumulated P/Pet.  Returns data in API default units (metric)
#'
#' @references http://developer.awhere.com/api/reference/agronomics/values
#'
#' @param - field_id: the field_id having previously been created with the createField Function
#' @param - day_start: character string of start date in form: YYYY-MM-DD
#'                    Defaults to using the associated planting date if no date set
#' @param - day_end: character string of end date in form: YYYY-MM-DD
#'                  If Not included will return data only for start date
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
#' @return data.table of requested data for dates requested
#'
#' @import httr
#' @import data.table
#' @import lubridate
#' @import jsonlite
#'
#' @examples
#' agronomic_values_fields('field123','2015-07-01','2015-07-31','','standard','10','10','30')

#' @export


agronomic_values_fields <- function(field_id,
                                    day_start = '', day_end = '',
                                    accumulation_start_date = '',gdd_method = 'standard',gdd_base_temp = '10',
                                    gdd_min_boundary = '10', gdd_max_boundary = '30') {

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

  if (day_start == '' & day_end != '') {
    warning('The day_end is specified so must day_start. Please correct\n')
    return()
  }

  if ((day_start != '') == TRUE) {
    if (suppressWarnings(is.na(ymd(day_start))) == TRUE) {
      warning('The Start Date is Not Properly Formatted.  Please change to proper format. \n')
      return()
    } else if (ymd(day_start) > ymd(Sys.Date()) - days(1)) {
      warning('By default, this function can only be used to access data up until yesterday. \n
              Use the GetForecast function to request data from today onward.\n')
      return()
    }# else if (ymd(day_start) <= ymd(Sys.Date())-months(30)) {
    # warning('By default, the aWhere APIs only allow daily data from the previous 30 months. \n
    #        Use the Norms API for long-term averages or speak to your account manager for longer access.\n')
    # return()
    #}
  }

  if ((day_end != '') == TRUE) {
    if (suppressWarnings(is.na(ymd(day_end))) == TRUE) {
      warning('The End Date is Not Properly Formatted.  Please change to proper format. \n')
      return()
    } else if (ymd(day_end) > ymd(Sys.Date()) - days(1)) {
      warning('By default, this function can only be used to access data up until yesterday. \n
              Use the GetForecast function to request data from today onward.\n')
      return()
    }# else if (ymd(day_end) <= ymd(Sys.Date())-months(30)) {
    # warning('By default, the aWhere APIs only allow daily data from the previous 30 months. \n
    #        Use the Norms API for long-term averages or speak to your account manager for longer access.\n')
    # return()
    #}

    if ((day_start != '') == TRUE) {
      if ((ymd(day_start) > ymd(day_end)) == TRUE) {
        warning('The Start Date must come before or be equal to the End Date.  Please change. \n')
        return()
      }
    }
  }

  if ((accumulation_start_date != '') == TRUE) {
    if (suppressWarnings(is.na(ymd(accumulation_start_date))) == TRUE) {
      warning('The Accumulation Start Date is Not Properly Formatted.  Please change to proper format. \n')
      return()
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



  # Create query

  urlAddress <- "https://api.awhere.com/v2/agronomics"

  strBeg <- paste0('/fields')
  strCoord <- paste0('/',field_id)
  strType <- paste0('/agronomicvalues')

  if (as.character(day_start) != '' & as.character(day_end) != '') {
    strDates <- paste0('/',day_start,',',day_end)
  } else if (day_end != '') {
    strDates <- paste0('/',day_start,',',day_start)
  } else {
    strDates <- ''
  }

  gdd_methodString      <- paste0('?gddMethod=',gdd_method)
  gdd_base_tempString    <- paste0('&gddBaseTemp=',gdd_base_temp)
  gdd_min_boundaryString <- paste0('&gddMinBoundary=',gdd_min_boundary)
  gdd_max_boundaryString <- paste0('&gddMaxBoundary=',gdd_max_boundary)

  accumulation_start_dateString = paste0('&accumulationStartDate=',accumulation_start_date)


  if (accumulation_start_date == '') {
    address <- paste0(urlAddress, strBeg, strCoord, strType, strDates,
                      gdd_methodString,gdd_base_tempString,gdd_min_boundaryString,gdd_max_boundaryString)
  } else {
    address <- paste0(urlAddress, strBeg, strCoord, strType, strDates,
                      gdd_methodString,gdd_base_tempString,gdd_min_boundaryString,gdd_max_boundaryString,accumulation_start_dateString)
  }

  doWeatherGet <- TRUE

  while (doWeatherGet == TRUE) {

    requestString <- paste0('request <- GET(address,
  	                                    add_headers(Authorization =
  	                                    paste0(\"Bearer \", awhereEnv75247$token)))')

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

  #    varNames <- colnames(data)
  #
  #     for (x in 1:length(varNames)) {
  #
  #       if (varNames[x] == 'location.latitude'){
  #         varNames[x] <- 'latitude'
  #       } else if (varNames[x] == 'location.longitude'){
  #         varNames[x] <- 'longitude'
  #       } else if (varNames[x] == 'temperatures.max'){
  #         varNames[x] <- 'maxTemperature'
  #       } else if (varNames[x] == 'temperatures.min'){
  #         varNames[x] <- 'minTemperature'
  #       } else if (varNames[x] == 'precipitation.amount'){
  #         varNames[x] <- 'precipitation'
  #       } else if (varNames[x] == 'solar.amount'){
  #         varNames[x] <- 'solarEnergy'
  #       } else if (varNames[x] == 'relativeHumidity.max'){
  #         varNames[x] <- 'maxRH'
  #       } else if (varNames[x] == 'relativeHumidity.min'){
  #         varNames[x] <- 'minRH'
  #       } else if (varNames[x] == 'wind.morningMax'){
  #         varNames[x] <- 'maxMorningWind'
  #       } else if (varNames[x] == 'wind.dayMax'){
  #         varNames[x] <- 'maxDayWind'
  #       } else if (varNames[x] == 'wind.average'){
  #         varNames[x] <- 'avgWind'
  #       } else if (varNames[x] == 'location.field_id') {
  #         varNames[x] <- 'field_id'
  #       }
  #     }

  #    setnames(data,varNames)



  return(as.data.frame(data))
}


#' @title agronomic_values_latlng.
#'
#' @description
#' \code{agronomic_values_latlng} calls Agronomic Values and Accumulations by Geolocation Endpoint of API using Lat/Lon
#'
#' @details
#' Agronomic Values are calculated numbers that can be used to show the agronomic status of a field or crop.
#' These figures can be used, for example, to track and predict plant growth or identify water stress.
#' Accumulated values allow growers to easily identify how the weather has been over the season.
#' Both sets of data are commonly used on small and large farms alike.  This is a very flexible API
#' that supports a wide variety of configurations to get exactly the data you want as efficiently as
#' possible. This function used the Lat/Lon construct to request data.  This function returns GDDs, Pet,
#' P/Pet, accumulated GDD, accumulated Precipitation, accumulated Pet, accumulated P/Pet.  Returns data
#' in API default units (metric)
#'
#' @references http://developer.awhere.com/api/reference/agronomics/values/geolocation
#'
#' @param - latitude: the latitude of the requested location
#' @param - longitude: the longitude of the requested locations
#' @param - day_start: character string of start date in form: YYYY-MM-DD
#'                    Defaults to system date -1 if left blank
#' @param - day_end: character string of end date in form: YYYY-MM-DD
#'                  If Not included will return data only for start date
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
#' @return data.table of requested data for dates requested
#'
#' @import httr
#' @import data.table
#' @import lubridate
#' @import jsonlite
#'
#' @examples
#' agronomic_values_latlng('39.8282', '-98.5795','2015-07-01','2015-07-31','','standard','10','10','30')

#' @export


agronomic_values_latlng <- function(latitude, longitude,
                                    day_start = ymd(Sys.Date()) - days(1), day_end = '',
                                    accumulation_start_date = '',gdd_method = 'standard',gdd_base_temp = '10',
                                    gdd_min_boundary = '10', gdd_max_boundary = '30') {

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

  if (suppressWarnings(is.na(ymd(day_start))) == TRUE) {
    warning('The Start Date is Not Properly Formatted.  Please change to proper format. \n')
    return()
  }

  if ((day_end != '') == TRUE) {
    if (suppressWarnings(is.na(ymd(day_end))) == TRUE) {
      warning('The End Date is Not Properly Formatted.  Please change to proper format. \n')
      return()
    } else if (ymd(day_end) > ymd(Sys.Date()) - days(1)) {
      warning('By default, this function can only be used to access data up until yesterday. \n
              Use the GetForecast function to request data from today onward.\n')
      return()
    }
  }

  if ((accumulation_start_date != '') == TRUE) {
    if (suppressWarnings(is.na(ymd(accumulation_start_date))) == TRUE) {
      warning('The Accumulation Start Date is Not Properly Formatted.  Please change to proper format. \n')
      return()
    }
  }

  #  if (ymd(day_start) <= ymd(Sys.Date())-months(30)) {
  #    warning('By default, the aWhere APIs only allow daily data from the previous 30 months. \n
  #             Use the Norms API for long-term averages or speak to your account manager for longer access.\n')
  #    return()
  #  }

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


    # Create query

    urlAddress <- "https://api.awhere.com/v2/agronomics"

    strBeg <- paste0('/locations')
    strCoord <- paste0('/',latitude,',',longitude)
    strType <- paste0('/agronomicvalues')
    strDates <- paste0('/',day_start,',',day_end)

    gdd_methodString      <- paste0('?gddMethod=',gdd_method)
    gdd_base_tempString    <- paste0('&gddBaseTemp=',gdd_base_temp)
    gdd_min_boundaryString <- paste0('&gddMinBoundary=',gdd_min_boundary)
    gdd_max_boundaryString <- paste0('&gddMaxBoundary=',gdd_max_boundary)

    accumulation_start_dateString = paste0('&accumulationStartDate=',accumulation_start_date)


    if (accumulation_start_date == '') {
      address <- paste0(urlAddress, strBeg, strCoord, strType, strDates,
                        gdd_methodString,gdd_base_tempString,gdd_min_boundaryString,gdd_max_boundaryString)
    } else {
      address <- paste0(urlAddress, strBeg, strCoord, strType, strDates,
                        gdd_methodString,gdd_base_tempString,gdd_min_boundaryString,gdd_max_boundaryString,accumulation_start_dateString)
    }

    doWeatherGet <- TRUE

    while (doWeatherGet == TRUE) {

      requestString <- paste0('request <- GET(address,
                              add_headers(Authorization =
                              paste0(\"Bearer \", awhereEnv75247$token)))')

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

    #    varNames <- colnames(data)
    #
    #     for (x in 1:length(varNames)) {
    #
    #       if (varNames[x] == 'location.latitude'){
    #         varNames[x] <- 'latitude'
    #       } else if (varNames[x] == 'location.longitude'){
    #         varNames[x] <- 'longitude'
    #       } else if (varNames[x] == 'temperatures.max'){
    #         varNames[x] <- 'maxTemperature'
    #       } else if (varNames[x] == 'temperatures.min'){
    #         varNames[x] <- 'minTemperature'
    #       } else if (varNames[x] == 'precipitation.amount'){
    #         varNames[x] <- 'precipitation'
    #       } else if (varNames[x] == 'solar.amount'){
    #         varNames[x] <- 'solarEnergy'
    #       } else if (varNames[x] == 'relativeHumidity.max'){
    #         varNames[x] <- 'maxRH'
    #       } else if (varNames[x] == 'relativeHumidity.min'){
    #         varNames[x] <- 'minRH'
    #       } else if (varNames[x] == 'wind.morningMax'){
    #         varNames[x] <- 'maxMorningWind'
    #       } else if (varNames[x] == 'wind.dayMax'){
    #         varNames[x] <- 'maxDayWind'
    #       } else if (varNames[x] == 'wind.average'){
    #         varNames[x] <- 'avgWind'
    #       } else if (varNames[x] == 'location.field_id') {
    #         varNames[x] <- 'field_id'
    #       }
    #     }

    #    setnames(data,varNames)


  return(as.data.frame(data))
}

