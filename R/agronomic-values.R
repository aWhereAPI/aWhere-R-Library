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
                                    ,tokenToUse = awhereEnv75247$token
                                    ,apiAddressToUse = awhereEnv75247$apiAddress) {
  
  #checkCredentials(keyToUse,secretToUse,tokenToUse)
  #checkValidLatLong(latitude,longitude)
  #checkValidStartEndDatesAgronomics(day_start,day_end)
  #checkGDDParams(gdd_method,gdd_base_temp,gdd_min_boundary,gdd_max_boundary)
  #checkAccumulationStartDate(accumulation_start_date, day_start)
  #checkPropertiesEndpoint('agronomics',propertiesToInclude)
  
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
      urlAddress <- paste0(apiAddressToUse, "/agronomics")
      
      strBeg <- paste0('/locations')
      strCoord <- paste0('/',latitude,',',longitude)
      strType <- paste0('/agronomicvalues')
      
      
      if (day_start_toUse == day_end_toUse) {
        strDates <- paste0('/',day_start_toUse)
      } else {
        strDates <- paste0('/',day_start_toUse,',',day_end_toUse)
      }
      
      #limitString <- paste0('?limit=',numObsReturned)
      limitString <- paste0('?units=usa')
      
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
