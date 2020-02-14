#' @title current_conditions_fields
#'
#' @description
#' \code{current_conditions_fields} calls Current Weather Conditions Endpoint of API using Fields Name Constuct
#'
#' @details
#' The Weather APIs provide access to aWhere's agriculture-specific Weather Terrain system,
#' and allows retrieval and integration of data across all different time ranges long term normals,
#' daily observed, current weather, and forecasts. These APIs are designed for efficiency,
#' allowing you to customize the responses to return just the attributes you need.
#'
#' Forecasts are useful for predicting today's weather at the start of the day, but conditions
#' can change quickly. At that point, having a more recent reading of the area's weather is key
#' for making decisions in the middle of the day. The Current Weather Conditions, or Nowcast,
#' API provides the current conditions. This API uses the data from the nearest weather station
#' that is capable of hourly updates.  Uses the Fields Name construct for requestion data.
#' Uses default units returned by the API
#'
#' @references http://developer.awhere.com/api/reference/weather/current
#'
#' @param - field_id: the field_id having previously been created with the createField Function
#' @param - sources: Which source to use for pulling the current conditions.  Valid values are
#'                    'metar', 'mesonet', 'metar-mesonet', 'pws', 'all'.  Default value is 'all'
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
#' \dontrun{current_conditions_fields('field_test','all')}

#' @export


current_conditions_fields <- function(field_id
                                      ,sources = 'all'
                                      ,keyToUse = awhereEnv75247$uid
                                      ,secretToUse = awhereEnv75247$secret
                                      ,tokenToUse = awhereEnv75247$token) {

  checkCredentials(keyToUse,secretToUse,tokenToUse)
  checkValidField(field_id,keyToUse,secretToUse,tokenToUse)
  checkForecastSources(sources)

  # Create query

  urlAddress <- "https://api.awhere.com/v2/weather"

  strBeg <- paste0('/fields')
  strCoord <- paste0('/',field_id)
  strType <- paste0('/currentconditions')
  strSources <- paste0('?sources=',sources)

  url <- paste0(urlAddress, strBeg, strCoord, strType, strSources)

  doWeatherGet <- TRUE

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
  }

  #The JSONLITE Serializer properly handles the JSON conversion
  x <- jsonlite::fromJSON(a,flatten = TRUE)

  data <- data.table::as.data.table(data.frame(as.list(unlist(x)),stringsAsFactors = FALSE))

  data <- removeUnnecessaryColumns(data)

  return(as.data.frame(data))
}


#' @title current_conditions_latlng
#'
#' @description
#' \code{current_conditions_latlng} calls Current Weather Conditions Endpoint of API using Lat/Lon
#'
#' @details
#' The Weather APIs provide access to aWhere's agriculture-specific Weather Terrain system,
#' and allows retrieval and integration of data across all different time ranges long term normals,
#' daily observed, current weather, and forecasts. These APIs are designed for efficiency,
#' allowing you to customize the responses to return just the attributes you need.
#'
#' Forecasts are useful for predicting today's weather at the start of the day, but conditions
#' can change quickly. At that point, having a more recent reading of the area's weather is key
#' for making decisions in the middle of the day. The Current Weather Conditions, or Nowcast,
#' API provides the current conditions. This API uses the data from the nearest weather station
#' that is capable of hourly updates.  Uses the Lat/Lon construct for requesting data.
#' Uses default units returned by the API
#'
#' @references http://developer.awhere.com/api/reference/weather/current
#'
#' @param - latitude: the latitude of the requested location
#' @param - longitude: the longitude of the requested locations
#' @param - sources: Which source to use for pulling the current conditions.  Valid values are
#'                    'metar', 'mesonet', 'metar-mesonet', 'pws', 'all'.  Default value is 'all'
#' @param - keyToUse: aWhere API key to use.  For advanced use only.  Most users will not need to use this parameter (optional)
#' @param - secretToUse: aWhere API secret to use.  For advanced use only.  Most users will not need to use this parameter (optional)
#' @param - tokenToUse: aWhere API token to use.  For advanced use only.  Most users will not need to use this parameter (optional)
#'
#' @return data.table of requested data for dates requested
#'
#' @import httr
#' @import data.table
#' @import lubridate
#' @import jsonlite
#'
#' @examples
#' \dontrun{current_conditions_latlng(39.8282, -98.5795, 'all')}

#' @export


current_conditions_latlng <- function(latitude
                                      ,longitude
                                      ,sources = 'all'
                                      ,keyToUse = awhereEnv75247$uid
                                      ,secretToUse = awhereEnv75247$secret
                                      ,tokenToUse = awhereEnv75247$token) {

  checkCredentials(keyToUse,secretToUse,tokenToUse)
  checkValidLatLong(latitude,longitude)
  checkForecastSources(sources)

  # Create query

  urlAddress <- "https://api.awhere.com/v2/weather"

  strBeg <- paste0('/locations')
  strCoord <- paste0('/',latitude,',',longitude)
  strType <- paste0('/currentconditions')
  strSources <- paste0('?sources=',sources)

  url <- paste0(urlAddress, strBeg, strCoord, strType, strSources)

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
  }

  #The JSONLITE Serializer properly handles the JSON conversion
  x <- jsonlite::fromJSON(a,flatten = TRUE)

  data <- data.table::as.data.table(data.frame(as.list(unlist(x)),stringsAsFactors = FALSE))

  data <- removeUnnecessaryColumns(data)

  return(as.data.frame(data))
}
