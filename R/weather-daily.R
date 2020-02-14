#' @title daily_observed_fields
#'
#' @description
#' \code{daily_observed_fields} pulls historical weather data from aWhere's API based on field id
#'
#' @details
#' This function returns weather data on Min/Max Temperature, Precipitation,
#' Min/Max Humidity, Solar Radiation, and Maximum Wind Speed,
#' Morning Max Windspeed, and Average Windspeed for the field id specified.
#' Default units are returned by the API.
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
#' @references http://developer.awhere.com/api/reference/weather/observations
#'
#' @param - field_id: the field_id associated with the location for which you want to pull data.
#'          Field IDs are created using the create_field function.(string)
#' @param - day_start: character string of the first day for which you want to retrieve data, in the form: YYYY-MM-DD.
#' @param - day_end: character string of the last day for which you want to retrieve data, in form: YYYY-MM-DD
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
#' \dontrun{daily_observed_fields(field_id = 'field_test'
#'                                ,day_start = '2018-10-28'
#'                                ,day_end = '2018-12-01')}

#' @export
daily_observed_fields <- function(field_id
                                  ,day_start
                                  ,day_end
                                  ,propertiesToInclude = ''
                                  ,keyToUse = awhereEnv75247$uid
                                  ,secretToUse = awhereEnv75247$secret
                                  ,tokenToUse = awhereEnv75247$token) {
  
  checkCredentials(keyToUse,secretToUse,tokenToUse)
  checkValidField(field_id,keyToUse,secretToUse,tokenToUse)
 # checkValidStartEndDates(day_start,day_end)
  checkPropertiesEndpoint('weather',propertiesToInclude)
  
  
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
      
      if(paste(allDates,sep = '',collapse ='') != '') {
        day_start_toUse <- allDates[starting]
        day_end_toUse <- allDates[ending]
        if(is.na(day_end_toUse)) {
          tempDates <- allDates[c(starting:length(allDates))]
          day_start_toUse <- tempDates[1]
          day_end_toUse <- tempDates[length(tempDates)]
        }
      }
      
      
      # Create query
      
      urlAddress <- paste0(awhereEnv75247$apiAddress, "/weather")
      
      strBeg <- paste0('/fields')
      strCoord <- paste0('/',field_id)
      strType <- paste0('/observations')
      strDates <- paste0('/',day_start_toUse,',',day_end_toUse)
      
      limitString <- paste0('?limit=',numObsReturned)
      
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
  
  currentNames <- data.table::copy(colnames(data))
  
  data[,field_id  := field_id]
  data.table::setcolorder(data,c('field_id',currentNames))
  
  checkDataReturn_daily(data,day_start,day_end)
  
  return(as.data.frame(data))
}


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
                                  ,tokenToUse = awhereEnv75247$token) {
  
  checkCredentials(keyToUse,secretToUse,tokenToUse)
  checkValidLatLong(latitude,longitude)
  #checkValidStartEndDates(day_start,day_end)
  checkPropertiesEndpoint('weather',propertiesToInclude)
  
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
      urlAddress <- paste0(awhereEnv75247$apiAddress, "/weather")
      
      strBeg <- paste0('/locations')
      strCoord <- paste0('/',latitude,',',longitude)
      strType <- paste0('/observations')
      strDates <- paste0('/',day_start_toUse,',',day_end_toUse)
      
      limitString <- paste0('?limit=',numObsReturned)
      
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
  
  checkDataReturn_daily(data,day_start,day_end)
  
  return(as.data.frame(data))
}

#' @title daily_observed_area
#'
#' @description
#' \code{daily_observed_area} pulls historical weather data from aWhere's API based on a data.frame of lat/lon, polygon or extent
#'
#' @details
#' This function returns weather data on Min/Max Temperature, Precipitation,
#' Min/Max Humidity, Solar Radiation, and Maximum Wind Speed,
#' Morning Max Windspeed, and Average Windspeed for the polygon passed to the function.
#' Default units are returned by the API. The polygon should be either a SpatialPolygons object or
#' a well-known text character string or an extent.
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
#'           for whether a given date should be requested from the daily_observed functions or the forecast functions.
#'           Furthermore, because this function can take as input locations that may be in different timezones, it is
#'           the responsibility of the user to either ensure that the date range specified is valid for all relevant
#'           locations or to break the query into pieces.
#'
#' @references http://developer.awhere.com/api/reference/weather/observations/geolocation
#' 
#' @param - polygon: either a data.frame with column names lat/lon, SpatialPolygons object,
#'                  well-known text string, or extent from raster package. If the object contains
#'                  multiple polygons, the union of them is used.  Information from each individal
#'                  polygon can be retrieved by returning spatial data and using
#'                  the over function from the sp package
#' @param - day_start: character string of the first day for which you want to retrieve data, in the form: YYYY-MM-DD
#' @param - day_end: character string of the last day for which you want to retrieve data, in the form: YYYY-MM-DD
#' @param - propertiesToInclude: character vector of properties to retrieve from API.  Valid values are temperatures, precipitation, solar, relativeHumidity, wind (optional)
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
#' @import sp
#'
#' @return data.frame of requested data for dates requested
#'
#' @examples
#' \dontrun{daily_observed_area(polygon = raster::getData('GADM', country = "Gambia", level = 0, download = T),
#'                                ,day_start = '2018-04-28'
#'                                ,day_end = '2018-05-01'
#'                                ,numcores = 2)}
#'                                
#' @export


daily_observed_area <- function(polygon
                                ,day_start
                                ,day_end
                                ,propertiesToInclude = ''
                                ,numcores = 2
                                ,bypassNumCallCheck = FALSE
                                ,returnSpatialData = FALSE
                                ,verbose = TRUE
                                ,keyToUse = awhereEnv75247$uid
                                ,secretToUse = awhereEnv75247$secret
                                ,tokenToUse = awhereEnv75247$token) {
  
  checkCredentials(keyToUse,secretToUse,tokenToUse)
 # checkValidStartEndDates(day_start,day_end)
  
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
                               ,.export = c('awhereEnv75247')
                               ,.errorhandling = 'pass') %dopar% {
    
    t <- daily_observed_latlng(latitude = grid[[j]]$lat
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
