#' @title agronomic_values_fields
#'
#' @description
#' \code{agronomic_values_fields} pulls agronomic data from aWhere's API based on field id
#'
#' @details
#' This function returns agronomic data on growing degree days (GDDs), potential evapotranspiration (PET), Precipitation over
#' potential evapotranspiration (P/PET), accumulated GDDs, accumulated precipitation, accumulated PET, and
#' accumulated P/PET.  Default units are returned by the API.
#'
#' Agronomic Values are calculated numbers that can be used to show the agronomic status of a field or crop.
#' These figures can be used, for example, to track and predict plant growth or identify water stress.
#' Accumulated values allow growers to easily identify how the weather has been over the season.
#' Both sets of data are commonly used on small and large farms alike.  This is a very flexible API
#' that supports a wide variety of configurations to get exactly the data you want as efficiently as
#' possible. It's also designed to work with the Fields and Plantings system to reduce the amount of input.
#' While a planting is not required to use this API, creating a Planting for your Fields will allow you
#' to get the most out of the aWhere platform.
#'
#' Note about dates: The system does not adjust for any difference in dates between the location of the user
#'           and where data is being requested from.  It is the responsibility of the user to ensure a valid
#'           date range is specified given any differences in timezone.  These differences can have implications
#'           for whether a given date should be requested from the daily_observed functions or the forecast functions
#'
#' @references http://developer.awhere.com/api/reference/agronomics/values
#'
#' @param - field_id: the field_id associated with the location for which you want to pull data.
#' Field IDs are created using the create_field function. (string)
#' @param - day_start: character string of the first day for which you want to retrieve data, in the form: YYYY-MM-DD
#' @param - day_end: character string of the last day for which you want to retrieve data, in the form: YYYY-MM-DD
#' @param - propertiesToInclude: character vector of properties to retrieve from API.  Valid values are accumulations, gdd, pet, ppet, accumulatedGdd, accumulatedPrecipitation, accumulatedPet, accumulatedPpet (optional)
#' @param - accumulation_start_date: Allows the user to start counting accumulations from
#'                                 before the specified start date (or before the
#'                                 planting date if using the most recent planting).
#'                                 Use this parameter to specify the date from which
#'                                 you wish to start counting, in the form: YYYY-MM-DD.
#'                                 The daily values object
#'                                 will still only return the days between the start
#'                                 and end date. This date must come before the start date. (optional)
#' @param - gdd_method: There are variety of equations available for calculating growing degree-days.
#'                     Valid entries are: 'standard', 'modifiedstandard', 'min-temp-cap', 'min-temp-constant'
#'                     See the API documentation for a description of each method.  The standard
#'                     method will be used if none is specified. (character - optional)
#' @param - gdd_base_temp: The base temp to use for the any of the GDD equations. The default value of 10 will
#'                       be used if none is specified. (optional)
#' @param - gdd_min_boundary: The minimum boundary to use in the selected GDD equation.
#'                           The behavior of this value is different depending on the equation you're using
#'                           The default value of 10 will be used if none is specified. (optional)
#' @param - gdd_max_boundary: The max boundary to use in the selected GDD equation. The
#'                          behavior of this value is different depending on the equation you're using.
#'                          The default value of 30 will be used if none is specified. (optional)
#' @param - keyToUse: aWhere API uid to use.  For advanced use only.  Most users will not need to use this parameter (optional)
#' @param - secretToUse: aWhere API secret to use.  For advanced use only.  Most users will not need to use this parameter (optional)
#' @param - tokenToUse: aWhere API token to use.  For advanced use only.  Most users will not need to use this parameter (optional)
#'
#' @import httr
#' @import data.table
#' @import lubridate
#' @import jsonlite
#'
#' @return dataframe of requested data for dates requested
#'
#' @examples
#' \dontrun{agronomic_values_fields(field_id = "field_test"
#'                                  ,day_start = "2018-11-01"
#'                                  ,day_end = "2018-11-30"
#'                                  ,gdd_method = "modifiedstandard"
#'                                  ,gdd_base_temp = 10
#'                                  ,gdd_min_boundary = 10
#'                                  ,gdd_max_boundary = 30)}
#' @export

agronomic_values_fields <- function(field_id
                                    ,day_start
                                    ,day_end
                                    ,propertiesToInclude = ''
                                    ,accumulation_start_date = ''
                                    ,gdd_method = 'standard'
                                    ,gdd_base_temp = 10
                                    ,gdd_min_boundary = 10
                                    ,gdd_max_boundary = 30
                                    ,keyToUse = awhereEnv75247$uid
                                    ,secretToUse = awhereEnv75247$secret
                                    ,tokenToUse = awhereEnv75247$token) {

  checkCredentials(keyToUse,secretToUse,tokenToUse)
  checkValidField(field_id,keyToUse,secretToUse,tokenToUse)
  #checkValidStartEndDatesAgronomics(day_start,day_end)
  checkGDDParams(gdd_method,gdd_base_temp,gdd_min_boundary,gdd_max_boundary)
  checkAccumulationStartDate(accumulation_start_date, day_start)
  checkPropertiesEndpoint('agronomics',propertiesToInclude)

  # Create Logic of API Request
  numObsReturned <- 120
  calculateAPIRequests <- TRUE
  continueRequestingData <- TRUE

  dataList <- list()

  # loop through, making requests in chunks of size numObsReturned
  while (continueRequestingData == TRUE | calculateAPIRequests == TRUE) {

    #If this clause is triggered the progression of API calls will be
    #calculated.  After each API call the return will be checked for an error
    #indicating that the request was too large.  If that occurs this loop will
    #be reenentered to calculate using the smaller return size

    ############################################################################
    if (calculateAPIRequests == TRUE) {

      calculateAPIRequests <- FALSE
      temp <- plan_APICalls(day_start
                            ,day_end
                            ,numObsReturned)
      allDates <- temp[[1]]
      loops <- temp[[2]]
    }

    #This for loop will make the API requests as calculated from above
    ############################################################################
    for (i in 1:loops) {

      starting = numObsReturned*(i-1)+1
      ending = numObsReturned*i
      day_start_toUse <- allDates[starting]
      day_end_toUse <- allDates[ending]
      if(is.na(day_end_toUse)) {
        tempDates <- allDates[c(starting:length(allDates))]
        day_start_toUse <- tempDates[1]
        day_end_toUse   <- tempDates[length(tempDates)]
      }

      # Create query
      urlAddress <- "https://api.awhere.com/v2/agronomics"

      strBeg <- paste0('/fields')
      strCoord <- paste0('/',field_id)
      strType <- paste0('/agronomicvalues')

      strDates <- paste0('/',day_start_toUse,',',day_end_toUse)

      limitString <- paste0('?limit=',numObsReturned)

      #Because of the fact that we have logic after the API calls for making
      #right the accumulation information, we only use the user specified
      #paramater on the first call.  This allows us to use the R function to
      #request arbitrarily long date ranges
      if (accumulation_start_date != ''  & i == 1) {
        strAccumulation <- paste0('&accumulationStartDate=',accumulation_start_date)
      } else {
        strAccumulation <- ''
      }

      gdd_methodString       <- paste0('&gddMethod=',gdd_method)
      gdd_base_tempString    <- paste0('&gddBaseTemp=',gdd_base_temp)
      gdd_min_boundaryString <- paste0('&gddMinBoundary=',gdd_min_boundary)
      gdd_max_boundaryString <- paste0('&gddMaxBoundary=',gdd_max_boundary)

      if (propertiesToInclude[1] != '') {
        propertiesString <- paste0('&properties=',paste0(propertiesToInclude,collapse = ','))
      } else {
        propertiesString <- ''
      }

      url <- URLencode(paste0(urlAddress
                              ,strBeg
                              ,strCoord
                              ,strType
                              ,strDates
                              ,limitString
                              ,gdd_methodString
                              ,gdd_base_tempString
                              ,gdd_min_boundaryString
                              ,gdd_max_boundaryString
                              ,strAccumulation
                              ,propertiesString))

      doWeatherGet <- TRUE
      while (doWeatherGet == TRUE) {
        postbody = ''
        request <- httr::GET(url, body = postbody, httr::content_type('application/json'),
                             httr::add_headers(Authorization =paste0("Bearer ", tokenToUse)))

        a <- suppressMessages(httr::content(request, as = "text"))

        temp <- check_JSON(a
                           ,request
                           ,keyToUse
                           ,secretToUse
                           ,tokenToUse)
        
        doWeatherGet <- temp[[1]]
        
        #if the token was updated, this will cause it to be used through function
        tokenToUse <- temp[[3]]

        #The temp[[2]] will only not be NA when the limit param is too large.
        if(!is.na(temp[[2]] == TRUE)) {
          numObsReturned <- temp[[2]]
          goodReturn <- FALSE

          break
        } else {
          goodReturn <- TRUE
        }

        rm(temp)
      }

      if (goodReturn == TRUE) {
        #The JSONLITE Serializer properly handles the JSON conversion
        x <- jsonlite::fromJSON(a,flatten = TRUE)

        if (propertiesToInclude[1] != '' & any(grepl('accumulated',propertiesToInclude,fixed = TRUE)) == FALSE) {
          data <- as.data.table(x[[1]])
        } else if (propertiesToInclude[1] != '' & any(grepl('accumulated',propertiesToInclude,fixed = TRUE)) == TRUE) {
          data <- as.data.table(x[[2]])
        } else {
          data <- as.data.table(x[[3]])
        }
        
        data <- removeUnnecessaryColumns(data)

        dataList[[length(dataList) + 1]] <- data

      } else {
        #This will break out of the current loop of making API requests so that
        #the logic of the API requests can be recalculated

        calculateAPIRequests <- TRUE
      }
    }
    continueRequestingData <- FALSE
  }

  ##############################################################################
  #Because of the fact that the above code will allow the user to specify an arbitray
  #date range and automatically figure out an API call plan, the accumulation information
  #may not be properly returned.  Because it is calculatable based on other information returned
  #we are going to do so here so that the function returns what the user would be expecting

  dataList <- recalculateAccumulations(dataList)
  ##############################################################################

  data <- unique(rbindlist(dataList
                           ,use.names = TRUE
                           ,fill = TRUE))

  currentNames <- data.table::copy(colnames(data))

  data[,field_id  := field_id]
  data.table::setcolorder(data,c('field_id',currentNames))

  checkDataReturn_daily(data,day_start,day_end)

  return(as.data.frame(data))
}


#' @title agronomic_values_latlng.
#'
#' @description
#' \code{agronomic_values_latlng} pulls agronomic data from aWhere's API based on latitude & longitude
#'
#' @details
#' This function returns agronomic data on GDDs, potential evapotranspiration (PET), Precipitation over
#' potential evapotranspiration (P/PET), accumulated GDDs, accumulated precipitation, accumulated PET, and
#' accumulated P/PET.  Default units are returned by the API.
#'
#' Agronomic Values are calculated numbers that can be used to show the agronomic status of a field or crop.
#' These figures can be used, for example, to track and predict plant growth or identify water stress.
#' Accumulated values allow growers to easily identify how the weather has been over the season.
#' Both sets of data are commonly used on small and large farms alike.  This is a very flexible API
#' that supports a wide variety of configurations to get exactly the data you want as efficiently as
#' possible.
#'
#' Note about dates: The system does not adjust for any difference in dates between the location of the user
#'           and where data is being requested from.  It is the responsibility of the user to ensure a valid
#'           date range is specified given any differences in timezone.  These differences can have implications
#'           for whether a given date should be requested from the daily_observed functions or the forecast functions
#'
#' @references http://developer.awhere.com/api/reference/agronomics/values
#'
#' @param - latitude: the latitude of the requested location (double)
#' @param - longitude: the longitude of the requested locations (double)
#' @param - day_start: character string of the first day for which you want to retrieve data, in the form: YYYY-MM-DD
#' @param - day_end: character string of the last day for which you want to retrieve data, in the form: YYYY-MM-DD
#' @param - propertiesToInclude: character vector of properties to retrieve from API.
#'                               Valid values are accumulations, gdd, pet, ppet, accumulatedGdd,
#'                               accumulatedPrecipitation, accumulatedPet, accumulatedPpet (optional)
#' @param - accumulation_start_date: Allows the user to start counting accumulations from
#'                                 before the specified start date (or before the
#'                                 planting date if using the most recent planting).
#'                                 Use this parameter to specify the date from which
#'                                 you wish to start counting, in the form: YYYY-MM-DD.
#'                                 The daily values object
#'                                 will still only return the days between the start
#'                                 and end date. This date must come before the start date. (optional)
#' @param - gdd_method: There are variety of equations available for calculating growing degree-days.
#'                     Valid entries are: 'standard', 'modifiedstandard', 'min-temp-cap', 'min-temp-constant'
#'                     See the API documentation for a description of each method.  The standard
#'                     method will be used if none is specified. (character - optional)
#' @param - gdd_base_temp: The base temp to use for the any of the GDD equations. The default value of 10 will
#'                       be used if none is specified. (optional)
#' @param - gdd_min_boundary: The minimum boundary to use in the selected GDD equation.
#'                           The behavior of this value is different depending on the equation you're using
#'                           The default value of 10 will be used if none is specified. (optional)
#' @param - gdd_max_boundary: The max boundary to use in the selected GDD equation. The
#'                          behavior of this value is different depending on the equation you're using.
#'                          The default value of 30 will be used if none is specified. (optional)
#' @param - keyToUse: aWhere API key to use.  For advanced use only.  Most users will not need to use this parameter (optional)
#' @param - secretToUse: aWhere API secret to use.  For advanced use only.  Most users will not need to use this parameter (optional)
#' @param - tokenToUse: aWhere API token to use.  For advanced use only.  Most users will not need to use this parameter (optional)
#'
#' @import httr
#' @import data.table
#' @import lubridate
#' @import jsonlite
#'
#' @return data.frame of requested data for dates requested
#'
#' @examples
#' \dontrun{agronomic_values_latlng(latitude = 39.8282
#'                                  ,longitude = -98.5795
#'                                  ,day_start = '2018-11-01'
#'                                  ,day_end = '2018-11-30')}
#' @export


agronomic_values_latlng <- function(latitude
                                    ,longitude
                                    ,day_start
                                    ,day_end
                                    ,propertiesToInclude = ''
                                    ,accumulation_start_date = ''
                                    ,gdd_method = 'standard'
                                    ,gdd_base_temp = 10
                                    ,gdd_min_boundary = 10
                                    ,gdd_max_boundary = 30
                                    ,keyToUse = awhereEnv75247$uid
                                    ,secretToUse = awhereEnv75247$secret
                                    ,tokenToUse = awhereEnv75247$token) {

  checkCredentials(keyToUse,secretToUse,tokenToUse)
  checkValidLatLong(latitude,longitude)
  #checkValidStartEndDatesAgronomics(day_start,day_end)
  checkGDDParams(gdd_method,gdd_base_temp,gdd_min_boundary,gdd_max_boundary)
  checkAccumulationStartDate(accumulation_start_date, day_start)
  checkPropertiesEndpoint('agronomics',propertiesToInclude)

  # Create Logic of API Request
  numObsReturned <- 120
  calculateAPIRequests <- TRUE
  continueRequestingData <- TRUE

  dataList <- list()

  # loop through, making requests in chunks of size numObsReturned
  while (continueRequestingData == TRUE | calculateAPIRequests == TRUE) {

    #If this clause is triggered the progression of API calls will be
    #calculated.  After each API call the return will be checked for an error
    #indicating that the request was too large.  If that occurs this loop will
    #be reenentered to calculate using the smaller return size

    ############################################################################
    if (calculateAPIRequests == TRUE) {

      calculateAPIRequests <- FALSE
      temp <- plan_APICalls(day_start
                            ,day_end
                            ,numObsReturned)
      allDates <- temp[[1]]
      loops <- temp[[2]]
    }

    #This for loop will make the API requests as calculated from above
    ############################################################################
    for (i in 1:loops) {

      starting = numObsReturned*(i-1)+1
      ending = numObsReturned*i
      day_start_toUse <- allDates[starting]
      day_end_toUse <- allDates[ending]
      if(is.na(day_end_toUse)) {
        tempDates <- allDates[c(starting:length(allDates))]
        day_start_toUse <- tempDates[1]
        day_end_toUse   <- tempDates[length(tempDates)]
      }

      # Create query
      urlAddress <- "https://api.awhere.com/v2/agronomics"

      strBeg <- paste0('/locations')
      strCoord <- paste0('/',latitude,',',longitude)
      strType <- paste0('/agronomicvalues')

      strDates <- paste0('/',day_start_toUse,',',day_end_toUse)

      limitString <- paste0('?limit=',numObsReturned)


      #Because of the fact that we have logic after the API calls for making
      #right the accumulation information, we only use the user specified
      #paramater on the first call.  This allows us to use the R function to
      #request arbitrarily long date ranges
      if (accumulation_start_date != ''  & i == 1) {
        strAccumulation <- paste0('&accumulationStartDate=',accumulation_start_date)
      } else {
        strAccumulation <- ''
      }

      gdd_methodString       <- paste0('&gddMethod=',gdd_method)
      gdd_base_tempString    <- paste0('&gddBaseTemp=',gdd_base_temp)
      gdd_min_boundaryString <- paste0('&gddMinBoundary=',gdd_min_boundary)
      gdd_max_boundaryString <- paste0('&gddMaxBoundary=',gdd_max_boundary)

      if (propertiesToInclude[1] != '') {
        propertiesString <- paste0('&properties=',paste0(propertiesToInclude,collapse = ','))
      } else {
        propertiesString <- ''
      }

      url <- URLencode(paste0(urlAddress
                              ,strBeg
                              ,strCoord
                              ,strType
                              ,strDates
                              ,limitString
                              ,gdd_methodString
                              ,gdd_base_tempString
                              ,gdd_min_boundaryString
                              ,gdd_max_boundaryString
                              ,strAccumulation
                              ,propertiesString))

      doWeatherGet <- TRUE
      while (doWeatherGet == TRUE) {
        postbody = ''
        request <- httr::GET(url, body = postbody, httr::content_type('application/json'),
                             httr::add_headers(Authorization =paste0("Bearer ", tokenToUse)))

        a <- suppressMessages(httr::content(request, as = "text"))

        temp <- check_JSON(a
                           ,request
                           ,keyToUse
                           ,secretToUse
                           ,tokenToUse)
        
        doWeatherGet <- temp[[1]]
        
        #if the token was updated, this will cause it to be used through function
        tokenToUse <- temp[[3]]

        #The temp[[2]] will only not be NA when the limit param is too large.
        if(!is.na(temp[[2]] == TRUE)) {
          numObsReturned <- temp[[2]]
          goodReturn <- FALSE

          break
        } else {
          goodReturn <- TRUE
        }

        rm(temp)
      }

      if (goodReturn == TRUE) {
        #The JSONLITE Serializer properly handles the JSON conversion
        x <- jsonlite::fromJSON(a,flatten = TRUE)

        if (propertiesToInclude[1] != '' & any(grepl('accumulated',propertiesToInclude,fixed = TRUE)) == FALSE) {
          data <- as.data.table(x[[1]])
        } else if (propertiesToInclude[1] != '' & any(grepl('accumulated',propertiesToInclude,fixed = TRUE)) == TRUE) {
          data <- as.data.table(x[[2]])
        } else {
          data <- as.data.table(x[[3]])
        }
        
        data <- removeUnnecessaryColumns(data)
        
        dataList[[length(dataList) + 1]] <- data

      } else {
        #This will break out of the current loop of making API requests so that
        #the logic of the API requests can be recalculated

        calculateAPIRequests <- TRUE
      }
    }
    continueRequestingData <- FALSE
  }

  ##############################################################################
  #Because of the fact that the above code will allow the user to specify an arbitray
  #date range and automatically figure out an API call plan, the accumulation information
  #may not be properly returned.  Because it is calculatable based on other information returned
  #we are going to do so here so that the function returns what the user would be expecting

  dataList <- recalculateAccumulations(dataList)
  ##############################################################################

  data <- unique(rbindlist(dataList
                           ,use.names = TRUE
                           ,fill = TRUE))

  currentNames <- data.table::copy(colnames(data))

  data[,latitude  := latitude]
  data[,longitude := longitude]

  data.table::setcolorder(data,c('latitude','longitude',currentNames))

  checkDataReturn_daily(data,day_start,day_end)

  return(as.data.frame(data))
}

#' @title agronomic_values_area
#'
#' @description
#' \code{agronomic_values_area} pulls agronomic data from aWhere's API based on a data.frame of lat/lon, polygon or extent
#'
#' @details
#' This function returns agronomic data on GDDs, potential evapotranspiration (PET), Precipitation over
#' potential evapotranspiration (P/PET), accumulated GDDs, accumulated precipitation, accumulated PET, and
#' accumulated P/PET.  Default units are returned by the API.
#'
#' Agronomic Values are calculated numbers that can be used to show the agronomic status of a field or crop.
#' These figures can be used, for example, to track and predict plant growth or identify water stress.
#' Accumulated values allow growers to easily identify how the weather has been over the season.
#' Both sets of data are commonly used on small and large farms alike.  This is a very flexible API
#' that supports a wide variety of configurations to get exactly the data you want as efficiently as
#' possible.
#'
#' The polygon should be either a SpatialPolygons object or a well-known text character string or an extent.
#'
#' Note about dates: The system does not adjust for any difference in dates between the location of the user
#'           and where data is being requested from.  It is the responsibility of the user to ensure a valid
#'           date range is specified given any differences in timezone.  These differences can have implications
#'           for whether a given date should be requested from the daily_observed functions or the forecast functions.
#'           Furthermore, because this function can take as input locations that may be in different timezones, it is
#'           the responsibility of the user to either ensure that the date range specified is valid for all relevant
#'           locations or to break the query into pieces.
#'
#' @references http://developer.awhere.com/api/reference/weather/observations/geolocation
#'
#' @param - polygon: either a data.frame with column names lat/lon, SpatialPolygons object,
#'                   well-known text string, or extent from raster package. If the object contains
#'                   multiple polygons, the union of them is used.  Information from each individal
#'                   polygon can be retrieved by returning spatial data and using
#'                   the over function from the sp package
#' @param - day_start: character string of the first day for which you want to retrieve data, in the form: YYYY-MM-DD
#' @param - day_end: character string of the last day for which you want to retrieve data, in the form: YYYY-MM-DD
#' @param - propertiesToInclude: character vector of properties to retrieve from API.  Valid values are accumulations, gdd, pet, ppet, accumulatedGdd, accumulatedPrecipitation, accumulatedPet, accumulatedPpet (optional)
#' @param - accumulation_start_date: Allows the user to start counting accumulations from
#'                                 before the specified start date (or before the
#'                                 planting date if using the most recent planting).
#'                                 Use this parameter to specify the date from which
#'                                 you wish to start counting, in the form: YYYY-MM-DD.
#'                                 The daily values object
#'                                 will still only return the days between the start
#'                                 and end date. This date must come before the start date. (optional)
#' @param - gdd_method: There are variety of equations available for calculating growing degree-days.
#'                     Valid entries are: 'standard', 'modifiedstandard', 'min-temp-cap', 'min-temp-constant'
#'                     See the API documentation for a description of each method.  The standard
#'                     method will be used if none is specified. (character - optional)
#' @param - gdd_base_temp: The base temp to use for the any of the GDD equations. The default value of 10 will
#'                       be used if none is specified. (optional)
#' @param - gdd_min_boundary: The minimum boundary to use in the selected GDD equation.
#'                           The behavior of this value is different depending on the equation you're using
#'                           The default value of 10 will be used if none is specified. (optional)
#' @param - gdd_max_boundary: The max boundary to use in the selected GDD equation. The
#'                          behavior of this value is different depending on the equation you're using.
#'                          The default value of 30 will be used if none is specified. (optional)
#' @param - numcores: number of cores to use in parallel loop. To check number of available cores: parallel::detectCores()
#'                    If you receive an error regarding the speed you are making calls, reduce this number
#' @param - bypassNumCallCheck: set to TRUE to avoid prompting the user to confirm that they want to begin making API calls
#' @param - returnSpatialData: returns the data as a SpatialPixels object.  Can be convered to raster with the command raster::stack
#'                             NOTE: if multiple days worth of data is returned, it is necessary to subset to specific day for working with
#'                             as spatial data (sp package: optional)
#' @param - keyToUse: aWhere API key to use.  For advanced use only.  Most users will not need to use this parameter (optional)
#' @param - secretToUse: aWhere API secret to use.  For advanced use only.  Most users will not need to use this parameter (optional)
#' @param - tokenToUse: aWhere API token to use.  For advanced use only.  Most users will not need to use this parameter (optional)
#'
#' @import httr
#' @import data.table
#' @import lubridate
#' @import jsonlite
#' @import foreach
#' @import doParallel
#' @import rgeos
#'
#' @return data.frame of requested data for dates requested
#'
#'
#' @examples
#' \dontrun{agronomic_values_area(polygon = raster::getData('GADM', country = "Gambia", level = 0, download = T),
#'                                ,day_start = '2018-04-28'
#'                                ,day_end = '2018-05-01'
#'                                ,numcores = 2)}

#' @export


agronomic_values_area <- function(polygon
                                  ,day_start
                                  ,day_end
                                  ,propertiesToInclude = ''
                                  ,accumulation_start_date = ''
                                  ,gdd_method = 'standard'
                                  ,gdd_base_temp = 10
                                  ,gdd_min_boundary = 10
                                  ,gdd_max_boundary = 30
                                  ,numcores = 2
                                  ,bypassNumCallCheck = FALSE
                                  ,returnSpatialData = FALSE
                                  ,verbose = TRUE
                                  ,keyToUse = awhereEnv75247$uid
                                  ,secretToUse = awhereEnv75247$secret
                                  ,tokenToUse = awhereEnv75247$token) {

  checkCredentials(keyToUse,secretToUse,tokenToUse)
  #checkValidStartEndDatesAgronomics(day_start,day_end)
  checkGDDParams(gdd_method,gdd_base_temp,gdd_min_boundary,gdd_max_boundary)
  checkAccumulationStartDate(accumulation_start_date)
  checkPropertiesEndpoint('agronomics',propertiesToInclude)

  if (!(all(class(polygon) %in% c('data.frame','data.table')))) {

    if (verbose == TRUE) {
      cat(paste0('Creating aWhere Raster Grid within Polygon\n'))
    }
    
    grid <- create_awhere_grid(polygon)
    
  } else {
    
    if (!(all(colnames(polygon) %in% c('lat','lon')) & length(colnames(polygon)) == 2)) {
      stop('Data.Frame of Lat/Lon coordinates improperly specified, please correct')
    }
    grid <-  polygon
    
    grid[,c('gridx'
            ,'gridy') := list(getGridX(longitude = lon)
                              ,getGridY(latitude = lat))]
  }

  verify_api_calls(grid,bypassNumCallCheck)

  if (verbose == TRUE) {
    cat(paste0('Requesting data using parallal API calls\n'))
  }
  
  grid <- split(grid, seq(1,nrow(grid),1))

  doParallel::registerDoParallel(cores=numcores)

  observed <- foreach::foreach(j=c(1:length(grid))
                               ,.packages = c("aWhereAPI")
                               ,.export = c('awhereEnv75247')) %dopar% {

    t <- agronomic_values_latlng(latitude = grid[[j]]$lat
                                 ,longitude = grid[[j]]$lon
                                 ,day_start = day_start
                                 ,day_end = day_end
                                 ,propertiesToInclude = propertiesToInclude)

    currentNames <- colnames(t)

    t$gridy <- grid[[j]]$gridy[i]
    t$gridx <- grid[[j]]$gridx[i]

    data.table::setcolorder(t, c(currentNames[c(1:2)], "gridy", "gridx", currentNames[c(3:length(currentNames))]))

    return(t)
  }

  indexToRemove <- c()
  for (x in 1:length(observed)) {
    if (any(class(observed[[x]]) == 'simpleError')) {
      indexToRemove <- c(indexToRemove,x)
    }
    grid <- data.table::rbindlist(grid)
    
    warning(paste0('The following locations returned errors and have been removed from the output.  Please investigate by running manually:\n'
                   ,paste0(grid[indexToRemove,paste0('(',lat,', ',lon,')')],collapse = ', ')
                   ,'\n'))
    
    grid <- grid[!indexToRemove]  
    
    observed[indexToRemove] <- NULL
  }
  
  observed <- data.table::rbindlist(observed,use.names = TRUE,fill = TRUE)

  if (returnSpatialData == TRUE) {
    sp::coordinates(observed) <- ~longitude + latitude
    sp::proj4string(observed) <- sp::CRS("+proj=longlat +datum=WGS84 +no_defs +ellps=WGS84 +towgs84=0,0,0")

    sp::gridded(observed) <- TRUE

    return(observed)
  }

  return(as.data.frame(observed))
}


