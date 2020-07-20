#' @title daily_observed_latlng
#'
#' @description
#' \code{daily_observed_latlng} pulls historical weather data from aWhere's API based on latitude & longitude
#'
#' @details
#' This function returns weather data on Min/Max Temperature, Precipitation,
#' Min/Max Humidity, Solar Radiation, and Maximum Wind Speed,
#' Morning Max Windspeed, and Average Windspeed for the location specified by latitude and longitude.
#' Default units are returned by the API. Latitude and longitude must be in decimal degrees.
#'
#' The Weather APIs provide access to aWhere's agriculture-specific Weather Terrain system,
#' and allows retrieval and integration of data across all different time ranges, long term normals,
#' daily observed, current weather, and forecasts. These APIs are designed for efficiency,
#' allowing you to customize the responses to return just the attributes you need.
#'
#' Understanding the recent and long-term daily weather is critical for making in-season decisions.
#' This API opens the weather attributes that matter most to agriculture.
#'
#' Note about dates: The system does not adjust for any difference in dates between the location of the user
#'           and where data is being requested from.  It is the responsibility of the user to ensure a valid
#'           date range is specified given any differences in timezone.  These differences can have implications
#'           for whether a given date should be requested from the daily_observed functions or the forecast functions
#'
#' @references http://developer.awhere.com/api/reference/weather/observations/geolocation
#'
#' @param - latitude: the latitude of the requested location (double)
#' @param - longitude: the longitude of the requested locations (double)
#' @param - day_start: character string of the first day for which you want to retrieve data, in the form: YYYY-MM-DD
#' @param - day_end: character string of the last day for which you want to retrieve data, in the form: YYYY-MM-DD
#' @param - propertiesToInclude: character vector of properties to retrieve from API.  Valid values are temperatures, precipitation, solar, relativeHumidity, wind (optional)
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
#'
#' @examples
#' \dontrun{daily_observed_latlng(latitude = 39.8282
#'                                ,longitude = -98.5795
#'                                ,day_start = '2018-10-28'
#'                                ,day_end = '2018-12-01')}
#' @export
#' 
daily_observed_latlng <- function(latitude
                                  ,longitude
                                  ,day_start
                                  ,day_end
                                  ,propertiesToInclude = ''
                                  ,keyToUse = awhereEnv75247$uid
                                  ,secretToUse = awhereEnv75247$secret
                                  ,tokenToUse = awhereEnv75247$token
                                  ,apiAddressToUse = awhereEnv75247$apiAddress) {
  
  #checkCredentials(keyToUse,secretToUse,tokenToUse)
  #checkValidLatLong(latitude,longitude)
  #checkValidStartEndDates(day_start,day_end)
  #checkPropertiesEndpoint('weather',propertiesToInclude)
  
  # Create Logic of API Request
  numObsReturned <- 1
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
      urlAddress <- paste0(apiAddressToUse, "/weather")
      
      strBeg <- paste0('/locations')
      strCoord <- paste0('/',latitude,',',longitude)
      strType <- paste0('/observations')
      
      if (day_start_toUse == day_end_toUse) {
        strDates <- paste0('/',day_start_toUse)
      } else {
        strDates <- paste0('/',day_start_toUse,',',day_end_toUse)
      }
      
      #limitString <- paste0('?limit=',numObsReturned)
      limitString <- paste0('?units=usa')
      
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
                              ,propertiesString))
      
      doWeatherGet <- TRUE
      
      #The reason for the while loop is that if the token has expired a new token
      #will be automatically requsted and the query will be repeated
      while (doWeatherGet == TRUE) {
        postbody = ''
        request <- httr::GET(url, body = postbody, httr::content_type('application/json'),
                             httr::add_headers(Authorization =paste0("Bearer ", tokenToUse)))
        
        # Make request
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
  
  currentNames <- data.table::copy(colnames(data))
  
  data[,latitude  := latitude]
  data[,longitude := longitude]
  
  data.table::setcolorder(data,c('latitude','longitude',currentNames))
  
  #checkDataReturn_daily(data,day_start,day_end)
  
  return(as.data.frame(data))
}