#' @title weather_norms_fields
#'
#' @description
#' \code{weather_norms_fields} pulls long term norm weather data from aWhere's API based on field id
#'
#' @details
#' This function allows you to calculate the averages for weather attributes
#' across any range of years for which data are available.  The data pulled includes
#' meanTemp, maxTemp, minTemp, precipitation average, solar radiation average,
#' minHumidity, maxHumidity, maxWind and averageWind, along with the standard deviations
#' for these variables.  The data pulled is for the field id identified.
#'
#' The data returned in this function
#' allow you to compare this year or previous years to the long-term normals, calculated as
#' the average of those weather conditions on that day in that location over the years specified.
#'
#' Note about dates: The system does not adjust for any difference in dates between the location of the user
#'           and where data is being requested from.  It is the responsibility of the user to ensure a valid
#'           date range is specified given any differences in timezone.  These differences can have implications
#'           for whether a given date should be requested from the daily_observed functions or the forecast functions
#'
#' @references http://developer.awhere.com/api/reference/weather/norms
#'
#' @param - field_id: the field_id associated with the location for which you want to pull data.
#'                    Field IDs are created using the create_field function. (string)
#' @param - monthday_start: character string of the first month and day for which you want to retrieve data,
#'                          in the form: MM-DD.  This is the start of your date range. e.g. '07-01' (July 1) (required)
#' @param - monthday_end: character string of the last month and day for which you want to retrieve data,
#'                          in the form: MM-DD.  This is the end of your date range. e.g. '07-01' (July 1) (required)
#' @param - year_start: character string of the starting year (inclusive) of the range of years for which
#'                     you're calculating norms, in the form YYYY. e.g., 2008 (required)
#' @param - year_end: character string of the last year (inclusive) of the range of years for which
#'                     you're calculating norms, in the form YYYY. e.g., 2015 (required)
#' @param - propertiesToInclude: character vector of properties to retrieve from API.
#'                               Valid values are meanTemp, maxTemp, minTemp, precipitation,
#'                               solar, maxHumidity, minHumidity, dailyMaxWind (optional)
#' @param - exclude_year: Year or years which you'd like to exclude from
#'                        your range of years on which to calculate norms. To exclude
#'                        multiple years, provide a vector of years. You must include
#'                       at least three years of data with which to calculate the norms. (numeric, optional)
#' @param - includeFeb29thData: Whether to keep data from Feb 29th on leap years.  Because weather/agronomics
#'                              summary statistics are calculated via the calendar date and 3 years are required
#'                              to generate a value, data from this date is more likely to be NA.  ALlows user
#'                              to drop this data to avoid later problems (defaults to TRUE)
#' @param - keyToUse: aWhere API key to use.  For advanced use only.  Most users will not need to use this parameter (optional)
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
#' \dontrun{weather_norms_fields(field_id = "field_test"
#'                               ,monthday_start = "06-01"
#'                               ,monthday_end = "09-01"
#'                               ,year_start = 2006
#'                               ,year_end = 2015)}
#' @export
#'
weather_norms_fields <- function(field_id
                                 ,monthday_start
                                 ,monthday_end
                                 ,year_start
                                 ,year_end
                                 ,propertiesToInclude = ''
                                 ,exclude_years = NULL
                                 ,includeFeb29thData = TRUE
                                 ,keyToUse = awhereEnv75247$uid
                                 ,secretToUse = awhereEnv75247$secret
                                 ,tokenToUse = awhereEnv75247$token) {

  #Checking Input Parameters
  checkCredentials(keyToUse,secretToUse,tokenToUse)
  checkValidField(field_id,keyToUse,secretToUse,tokenToUse)
  checkNormsStartEndDates(monthday_start,monthday_end)
  checkNormsYearsToRequest(year_start,year_end,monthday_start,monthday_end,exclude_years)
  checkPropertiesEndpoint('weather_norms',propertiesToInclude)

  # Create Logic of API Request
  numObsReturned <- 120
  calculateAPIRequests <- TRUE
  continueRequestingData <- TRUE

  yearsToInclude <- setdiff(seq(year_start,year_end,1),exclude_years)

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

      #We need to consider whether a year year is present to determine if Fe
      #29th data will be returned
      includesLeapYear <- any(is.leapyear(yearsToInclude))

      if (includesLeapYear == TRUE) {

        yearPrefix <- paste0(yearsToInclude[is.leapyear(yearsToInclude)][1],'-')

      } else {

        yearPrefix <- paste0(yearsToInclude[1],'-')

      }

      day_start <- ymd(paste0(yearPrefix
                              ,monthday_start))

      day_end <- ymd(paste0(yearPrefix
                            ,monthday_end))


      temp <- plan_APICalls(day_start
                            ,day_end
                            ,numObsReturned
                            ,includesLeapYear)
      allDates <- temp[[1]]
      loops <- temp[[2]]

      #remove the years
      allDates <-  gsub(pattern = '20\\d\\d-',replacement = '',x = allDates)
    }

    #This for loop will make the API requests as calculated from above
    ############################################################################
    for (i in 1:loops) {

      starting = numObsReturned*(i-1)+1
      ending = numObsReturned*i
      monthday_start_toUse <- allDates[starting]
      monthday_end_toUse <- allDates[ending]

      if(is.na(monthday_end_toUse)) {
        tempDates <- allDates[c(starting:length(allDates))]
        monthday_start_toUse <- tempDates[1]
        monthday_end_toUse   <- tempDates[length(tempDates)]
      }


      ##############################################################################

      # Create query

      urlAddress <- paste0(awhereEnv75247$apiAddress, "/weather")

      strBeg <- paste0('/fields')
      strCoord <- paste0('/',field_id)
      strType <- paste0('/norms')

      strMonthsDays <- paste0('/',monthday_start_toUse,',',monthday_end_toUse)

      limitString <- paste0('?limit=',numObsReturned)

      if (length(exclude_years) != 0) {
        strexclude_years <- paste0('&excludeYears=',toString(exclude_years))
      } else {
        strexclude_years <- ''
      }

      if (propertiesToInclude[1] != '') {
        propertiesString <- paste0('&properties=',paste0(propertiesToInclude,collapse = ','))
      } else {
        propertiesString <- ''
      }

      strYearsType <- paste0('/years')
      strYears <- paste0('/',year_start,',',year_end)

      url <- URLencode(paste0(urlAddress
                              ,strBeg
                              ,strCoord
                              ,strType
                              ,strMonthsDays
                              ,strYearsType
                              ,strYears
                              ,limitString
                              ,strexclude_years
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

        data <- data.table::as.data.table(x[[1]])

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

  data <- unique(rbindlist(dataList))

  #Get rid of leap yearData
  if (includeFeb29thData == FALSE) {
    data <- data[day != '02-29',]
  }

  currentNames <- data.table::copy(colnames(data))

  data[,field_id  := field_id]

  data.table::setcolorder(data,c('field_id',currentNames))

  checkDataReturn_norms(data,monthday_start,monthday_end,year_start,year_end,exclude_years,includeFeb29thData)

  return(as.data.frame(data))
}

#' @title weather_norms_latlng
#'
#' @description
#' \code{weather_norms_latlng} pulls long term norm weather data from aWhere's API based on latitude & longitude
#'
#' @details
#' This function allows you to calculate the averages for weather attributes
#' across any range of years for which data are available.  The data pulled includes
#' meanTemp, maxTemp, minTemp, precipitation average, solar radiation average,
#' minHumidity, maxHumidity, maxWind and averageWind, along with the standard deviations
#' for these variables.  The data pulled is for the latitude & longitude identified.
#'
#' The data returned in this function
#' allow you to compare this year or previous years to the long-term normals, calculated as
#' the average of those weather conditions on that day in that location over the years specified.
#'
#' Note about dates: The system does not adjust for any difference in dates between the location of the user
#'           and where data is being requested from.  It is the responsibility of the user to ensure a valid
#'           date range is specified given any differences in timezone.  These differences can have implications
#'           for whether a given date should be requested from the daily_observed functions or the forecast functions
#'
#' @references http://developer.awhere.com/api/reference/weather/norms
#'
#' @param - latitude: the latitude of the requested location (double, required)
#' @param - longitude: the longitude of the requested locations (double, required)
#' @param - monthday_start: character string of the first month and day for which you want to retrieve data,
#'                          in the form: MM-DD.  This is the start of your date range. e.g. '07-01' (July 1) (required)
#' @param - monthday_end: character string of the last month and day for which you want to retrieve data,
#'                          in the form: MM-DD.  This is the end of your date range. e.g. '07-01' (July 1) (required)
#' @param - year_start: character string of the starting year (inclusive) of the range of years for which
#'                     you're calculating norms, in the form YYYY. e.g., 2008 (required)
#' @param - year_end: character string of the last year (inclusive) of the range of years for which
#'                     you're calculating norms, in the form YYYY. e.g., 2015 (required)
#' @param - propertiesToInclude: character vector of properties to retrieve from API.
#'                               Valid values are meanTemp, maxTemp, minTemp, precipitation,
#'                               solar, maxHumidity, minHumidity, dailyMaxWind (optional)
#' @param - exclude_year: Year or years which you'd like to exclude from
#'                        your range of years on which to calculate norms. To exclude
#'                        multiple years, provide a vector of years. You must include
#'                       at least three years of data with which to calculate the norms. (numeric, optional)
#' @param - includeFeb29thData: Whether to keep data from Feb 29th on leap years.  Because weather/agronomics
#'                              summary statistics are calculated via the calendar date and 3 years are required
#'                              to generate a value, data from this date is more likely to be NA.  ALlows user
#'                              to drop this data to avoid later problems (defaults to TRUE)
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
#' \dontrun{weather_norms_latlng(latitude = 39.8282
#'                               ,longitude = -98.5795
#'                               ,monthday_start = '02-01'
#'                               ,monthday_end = '03-10'
#'                               ,year_start = 2008
#'                               ,year_end = 2015
#'                               ,exclude_years =  c(2010,2011))}
#' @export


weather_norms_latlng <- function(latitude
                                 ,longitude
                                 ,monthday_start
                                 ,monthday_end
                                 ,year_start
                                 ,year_end
                                 ,propertiesToInclude = ''
                                 ,exclude_years = NULL
                                 ,includeFeb29thData = TRUE
                                 ,keyToUse = awhereEnv75247$uid
                                 ,secretToUse = awhereEnv75247$secret
                                 ,tokenToUse = awhereEnv75247$token) {

  #Checking Input Parameters
  checkCredentials(keyToUse,secretToUse,tokenToUse)
  checkValidLatLong(latitude,longitude)
  checkNormsStartEndDates(monthday_start,monthday_end)
  checkNormsYearsToRequest(year_start,year_end,monthday_start,monthday_end,exclude_years)
  checkPropertiesEndpoint('weather_norms',propertiesToInclude)

  # Create Logic of API Request
  numObsReturned <- 120
  calculateAPIRequests <- TRUE
  continueRequestingData <- TRUE

  yearsToInclude <- setdiff(seq(year_start,year_end,1),exclude_years)

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

      #We need to consider whether a year year is present to determine if Fe
      #29th data will be returned
      includesLeapYear <- any(is.leapyear(yearsToInclude))

      if (includesLeapYear == TRUE) {

        yearPrefix       <- paste0(yearsToInclude[is.leapyear(yearsToInclude)][1],'-')

      } else {

        yearPrefix       <- paste0(yearsToInclude[1],'-')

      }

      day_start <- ymd(paste0(yearPrefix
                              ,monthday_start))

      day_end <- ymd(paste0(yearPrefix
                            ,monthday_end))


      temp <- plan_APICalls(day_start
                            ,day_end
                            ,numObsReturned
                            ,includesLeapYear)
      allDates <- temp[[1]]
      loops <- temp[[2]]

      #remove the years
      allDates <-  gsub(pattern = '20\\d\\d-',replacement = '',x = allDates)
    }

    #This for loop will make the API requests as calculated from above
    ############################################################################
    for (i in 1:loops) {

      starting = numObsReturned*(i-1)+1
      ending = numObsReturned*i
      monthday_start_toUse <- allDates[starting]
      monthday_end_toUse <- allDates[ending]

      if(is.na(monthday_end_toUse)) {
        tempDates <- allDates[c(starting:length(allDates))]
        monthday_start_toUse <- tempDates[1]
        monthday_end_toUse   <- tempDates[length(tempDates)]
      }


      ##############################################################################

      # Create query

      urlAddress <- paste0(awhereEnv75247$apiAddress, "/weather")

      strBeg <- paste0('/locations')
      strCoord <- paste0('/',latitude,',',longitude)
      strType <- paste0('/norms')

      strMonthsDays <- paste0('/',monthday_start_toUse,',',monthday_end_toUse)

      limitString <- paste0('?limit=',numObsReturned)

      if (length(exclude_years) != 0) {
        strexclude_years <- paste0('&excludeYears=',toString(exclude_years))
      } else {
        strexclude_years <- ''
      }

      if (propertiesToInclude[1] != '') {
        propertiesString <- paste0('&properties=',paste0(propertiesToInclude,collapse = ','))
      } else {
        propertiesString <- ''
      }

      strYearsType <- paste0('/years')
      strYears <- paste0('/',year_start,',',year_end)

      url <- URLencode(paste0(urlAddress
                              ,strBeg
                              ,strCoord
                              ,strType
                              ,strMonthsDays
                              ,strYearsType
                              ,strYears
                              ,limitString
                              ,strexclude_years
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

        data <- data.table::as.data.table(x[[1]])

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

  data <- unique(rbindlist(dataList))

  #Get rid of leap yearData
  if (includeFeb29thData == FALSE) {
    data <- data[day != '02-29',]
  }
  
  currentNames <- data.table::copy(colnames(data))

  data[,latitude  := latitude]
  data[,longitude := longitude]

  data.table::setcolorder(data,c('latitude','longitude',currentNames))

  checkDataReturn_norms(data,monthday_start,monthday_end,year_start,year_end,exclude_years,includeFeb29thData)

  return(as.data.frame(data))
}


#' @title weather_norms_area
#'
#' @description
#' \code{weather_norms_area} pulls long term norm weather data from aWhere's API based on a data.frame of lat/lon, polygon or extentt
#'
#' @details
#' This function allows you to calculate the averages for weather attributes
#' across any range of years for which data are available.  The data pulled includes
#' meanTemp, maxTemp, minTemp, precipitation average, solar radiation average,
#' minHumidity, maxHumidity, maxWind and averageWind, along with the standard deviations
#' for these variables.  The polygon should be either a SpatialPolygons object or
#' a well-known text character string or an extent.
#'
#' The data returned in this function
#' allow you to compare this year or previous years to the long-term normals, calculated as
#' the average of those weather conditions on that day in that location over the years specified.
#'
#' Note about dates: The system does not adjust for any difference in dates between the location of the user
#'           and where data is being requested from.  It is the responsibility of the user to ensure a valid
#'           date range is specified given any differences in timezone.  These differences can have implications
#'           for whether a given date should be requested from the daily_observed functions or the forecast functions.
#'           Furthermore, because this function can take as input locations that may be in different timezones, it is
#'           the responsibility of the user to either ensure that the date range specified is valid for all relevant
#'           locations or to break the query into pieces.
#'
#' @references http://developer.awhere.com/api/reference/weather/norms
#'
#' @param - polygon: either a data.frame with column names lat/lon, SpatialPolygons object,
#'                   well-known text string, or extent from raster package. If the object contains
#'                   multiple polygons, the union of them is used.  Information from each individal
#'                   polygon can be retrieved by returning spatial data and using
#'                   the over function from the sp package
#' @param - monthday_start: character string of the first month and day for which you want to retrieve data,
#'                          in the form: MM-DD.  This is the start of your date range. e.g. '07-01' (July 1) (required)
#' @param - monthday_end: character string of the last month and day for which you want to retrieve data,
#'                          in the form: MM-DD.  This is the end of your date range. e.g. '07-01' (July 1) (required)
#' @param - year_start: character string of the starting year (inclusive) of the range of years for which
#'                     you're calculating norms, in the form YYYY. e.g., 2008 (required)
#' @param - year_end: character string of the last year (inclusive) of the range of years for which
#'                     you're calculating norms, in the form YYYY. e.g., 2015 (required)
#' @param - propertiesToInclude: character vector of properties to retrieve from API.  Valid values are meanTemp, maxTemp, minTemp, precipitation, solar, maxHumidity, minHumidity, dailyMaxWind (optional)
#' @param - exclude_year: Year or years which you'd like to exclude from
#'                        your range of years on which to calculate norms. To exclude
#'                        multiple years, provide a vector of years. You must include
#'                       at least three years of data with which to calculate the norms. (numeric, optional)
#' @param - includeFeb29thData: Whether to keep data from Feb 29th on leap years.  Because weather/agronomics
#'                              summary statistics are calculated via the calendar date and 3 years are required
#'                              to generate a value, data from this date is more likely to be NA.  ALlows user
#'                              to drop this data to avoid later problems (defaults to TRUE)
#' @param - numcores: number of cores to use in parallel loop. To check number of available cores: parallel::detectCores().
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
#' @import raster
#' @import foreach
#' @import doParallel
#' @import rgeos
#'
#' @return data.frame of requested data for dates requested
#'
#' @examples
#' \dontrun{weather_norms_area(polygon = raster::getData('GADM', country = "Gambia", level = 0, download = T)
#'                               ,monthday_start = '02-01'
#'                               ,monthday_end = '03-10'
#'                               ,year_start = 2008
#'                               ,year_end = 2015
#'                               ,exclude_years =  c(2010,2011)
#'                               ,numcores = 2)}
#' @export


weather_norms_area <- function(polygon
                               ,monthday_start
                               ,monthday_end
                               ,year_start
                               ,year_end
                               ,propertiesToInclude = ''
                               ,exclude_years = NULL
                               ,includeFeb29thData = TRUE
                               ,numcores = 2
                               ,bypassNumCallCheck = FALSE
                               ,returnSpatialData = FALSE
                               ,verbose = TRUE
                               ,keyToUse = awhereEnv75247$uid
                               ,secretToUse = awhereEnv75247$secret
                               ,tokenToUse = awhereEnv75247$token) {

  #Checking Input Parameters
  checkCredentials(keyToUse,secretToUse,tokenToUse)
  checkNormsStartEndDates(monthday_start,monthday_end)
  checkNormsYearsToRequest(year_start,year_end,monthday_start,monthday_end,exclude_years)
  checkPropertiesEndpoint('weather_norms',propertiesToInclude)

  ##############################################################################

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

  norms <- foreach::foreach(j=c(1:length(grid))
                            ,.packages = c("aWhereAPI")
                            ,.export = c('awhereEnv75247')) %dopar% {

    t <- weather_norms_latlng(latitude = grid[[j]]$lat
                              ,longitude = grid[[j]]$lon
                              ,monthday_start = monthday_start
                              ,monthday_end = monthday_end
                              ,year_start = year_start
                              ,year_end = year_end
                              ,propertiesToInclude = propertiesToInclude
                              ,exclude_years =  exclude_years
                              ,includeFeb29thData = includeFeb29thData)

    currentNames <- colnames(t)

    t$gridy <- grid[[j]]$gridy[i]
    t$gridx <- grid[[j]]$gridx[i]

    data.table::setcolorder(t, c(currentNames[c(1:2)], "gridy", "gridx", currentNames[c(3:length(currentNames))]))

    return(t)
  }
  
  indexToRemove <- c()
  for (x in 1:length(norms)) {
    if (any(class(norms[[x]]) == 'simpleError')) {
      indexToRemove <- c(indexToRemove,x)
    }
    grid <- data.table::rbindlist(grid)
    
    warning(paste0('The following locations returned errors and have been removed from the output.  Please investigate by running manually:\n'
                   ,paste0(grid[indexToRemove,paste0('(',lat,', ',lon,')')],collapse = ', ')
                   ,'\n'))
    
    grid <- grid[!indexToRemove]  
    
    norms[indexToRemove] <- NULL
  }

  norms <- data.table::rbindlist(norms,use.names = TRUE,fill = TRUE)

  if (returnSpatialData == TRUE) {
    sp::coordinates(norms) <- ~longitude + latitude
    sp::proj4string(norms) <- sp::CRS("+proj=longlat +datum=WGS84 +no_defs +ellps=WGS84 +towgs84=0,0,0")

    sp::gridded(norms) <- TRUE

    return(norms)
  }

  return(as.data.frame(norms))
}
