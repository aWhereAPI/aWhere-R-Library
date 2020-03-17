#' @title soils_fields
#'
#' @description
#' \code{soils_fields} pulls forecasted soil temp/moisutre data from aWhere's API based on field id
#'
#' @details
#' This function returns today's soil forecast plus the forecast for up to 15 more days. Forecasts are available
#' in many sizes of hourly blocks, from hourly to daily. The data this function returns is
#' the soil's temperature and volumetric moisture content for the location specified by field id for multiple depths
#' Default units are returned by the API. Latitude and longitude must be in decimal degrees.
#'
#' Note that when block_size = 1 the fields min/max will be NA
#'
#' The Weather APIs provide access to aWhere's agriculture-specific Weather Terrain system,
#' and allows retrieval and integration of data across all different time ranges long term normals,
#' daily observed, current weather, and forecasts. These APIs are designed for efficiency,
#' allowing you to customize the responses to return just the attributes you need.
#'
#' Note about dates: The system does not adjust for any difference in dates between the location of the user
#'           and where data is being requested from.  It is the responsibility of the user to ensure a valid
#'           date range is specified given any differences in timezone.  These differences can have implications
#'           for whether a given date should be requested from the daily_observed functions or the forecast functions
#'
#' @references http://developer.awhere.com/api/forecast-weather-api
#'
#' @param - field_id: the field_id associated with the location for which you want to pull data.
#' Field IDs are created using the create_field function. (string)
#' @param - day_start: character string of the first day for which you want to retrieve data, in the form: YYYY-MM-DD
#'                    Defaults to system date if left blank. (optional)
#' @param - day_end: character string of the last day for which you want to retrieve data, in form: YYYY-MM-DD
#'                 Returns all available forecast if left blank. (optional)
#' @param - block_size: Integer value that corresponds to the number of hours to include in each time block.
#'                     Defaults to a 1 hour block.  This value must divide evenly into 24. (integer - optional)
#' @param - useLocalTime: whether the data specified is the date specified at the location where data is
#'                        being requested from or at UTC = 0.  Default is TRUE
#' @param - keyToUse: aWhere API key to use.  For advanced use only.  Most users will not need to use this parameter (optional)
#' @param - secretToUse: aWhere API secret to use.  For advanced use only.  Most users will not need to use this parameter (optional)
#' @param - tokenToUse: aWhere API token to use.  For advanced use only.  Most users will not need to use this parameter (optional)
#'
#' @import httr
#' @import data.table
#' @import lubridate
#' @import jsonlite
#' @import lutz
#' 
#' @return data.frame of requested data for dates requested
#'
#' @examples
#' \dontrun{soils_fields(field_id = 'field_test'
#'                           ,day_start = as.character(Sys.Date())
#'                           , block_size = 12)
#'          soils_fields('field_test'
#'                           ,day_start = as.character(Sys.Date())
#'                           ,day_end = as.character(Sys.Date() + 5))}
#' @export


soils_fields <- function(field_id
                         ,day_start = as.character(Sys.Date())
                         ,day_end = ''
                         ,block_size = 1
                         ,useLocalTime = TRUE
                         ,keyToUse = awhereEnv75247$uid
                         ,secretToUse = awhereEnv75247$secret
                         ,tokenToUse = awhereEnv75247$token) {
  
  
  data <- 
    forecasts_fields(field_id = field_id
                     ,day_start = day_start
                     ,day_end = day_end
                     ,block_size = block_size
                     ,useLocalTime = useLocalTime
                     ,returnOnlySoilVars = TRUE
                     ,keyToUse = keyToUse
                     ,secretToUse = secretToUse
                     ,tokenToUse = tokenToUse) 
  
  return(data)
  
}

#' @title soils_latlng
#'
#' @description
#' \code{soils_latlng} pulls forecasted soil temp/moisutre data from aWhere's API based on latitude & longitude
#'
#' @details
#' This function returns today's soil forecast plus the forecast for up to 15 more days. Forecasts are available
#' in many sizes of hourly blocks, from hourly to daily. The data this function returns is
#' the soil's temperature and volumetric moisture content for the location specified by latitude and longitude for multiple depths
#' Default units are returned by the API. Latitude and longitude must be in decimal degrees.
#'
#' Note that when block_size = 1 the fields min/max will be NA
#'
#' The Weather APIs provide access to aWhere's agriculture-specific Weather Terrain system,
#' and allows retrieval and integration of data across all different time ranges long term normals,
#' daily observed, current weather, and forecasts. These APIs are designed for efficiency,
#' allowing you to customize the responses to return just the attributes you need.
#'
#' Note about dates: The system does not adjust for any difference in dates between the location of the user
#'           and where data is being requested from.  It is the responsibility of the user to ensure a valid
#'           date range is specified given any differences in timezone.  These differences can have implications
#'           for whether a given date should be requested from the daily_observed functions or the forecast functions
#'
#' @references http://developer.awhere.com/api/reference/weather/forecasts/geolocation
#'
#' @param - latitude: the latitude of the requested location (double)
#' @param - longitude: the longitude of the requested locations (double)
#' @param - day_start: character string of the first day for which you want to retrieve data, in the form: YYYY-MM-DD
#'                    Defaults to system date if left blank. (optional)
#' @param - day_end: character string of the last day for which you want to retrieve data, in form: YYYY-MM-DD
#'                  Returns all available forecast if left blank. (optional)
#' @param - block_size: Integer value that corresponds to the number of hours to include in each time block.
#'                     Defaults to a 1 hour block.  This value must divide evenly into 24. (integer - optional)
#' @param - useLocalTime: whether the data specified is the date specified at the location where data is
#'                        being requested from or at UTC = 0.  Default is TRUE
#' @param - keyToUse: aWhere API key to use.  For advanced use only.  Most users will not need to use this parameter (optional)
#' @param - secretToUse: aWhere API secret to use.  For advanced use only.  Most users will not need to use this parameter (optional)
#' @param - tokenToUse: aWhere API token to use.  For advanced use only.  Most users will not need to use this parameter (optional)
#'
#' @return data.frame of requested data for dates requested
#'
#' @import httr
#' @import data.table
#' @import lubridate
#' @import jsonlite
#' @import lutz
#'
#' @examples
#' \dontrun{soils_latlng(latitude = 39.8282
#'                           ,longitude =  -98.5795
#'                           ,day_start = as.character(Sys.Date())
#'                           ,day_end = as.character(Sys.Date() + 5)
#'                           ,block_size = 12)}

#' @export

soils_latlng <- function(latitude
                         ,longitude
                         ,day_start = as.character(Sys.Date())
                         ,day_end = ''
                         ,block_size = 1
                         ,useLocalTime = TRUE
                         ,keyToUse = awhereEnv75247$uid
                         ,secretToUse = awhereEnv75247$secret
                         ,tokenToUse = awhereEnv75247$token) {
  
  data <- 
    forecasts_latlng(latitude = latitude
                     ,longitude = longitude
                     ,day_start = day_start
                     ,day_end = day_end
                     ,block_size = block_size
                     ,useLocalTime = useLocalTime
                     ,returnOnlySoilVars = TRUE
                     ,keyToUse = keyToUse
                     ,secretToUse = secretToUse
                     ,tokenToUse = tokenToUse) 
  
  return(data)
}

#' @title soils_area
#'
#' @description
#' \code{soils_area} pulls forecasted soil temp/moisutre data from aWhere's API based on a data.frame of lat/lon, polygon or extent
#'
#' @details
#' This function returns today's soil forecast plus the forecast for up to 15 more days. Forecasts are available
#' in many sizes of hourly blocks, from hourly to daily. The data this function returns is
#' the soil's temperature and volumetric moisture content for the location specified by spatial information for multiple depths
#' Default units are returned by the API. Latitude and longitude must be in decimal degrees.
#'
#' Note that when block_size = 1 the fields min/max will be NA
#'
#' The Weather APIs provide access to aWhere's agriculture-specific Weather Terrain system,
#' and allows retrieval and integration of data across all different time ranges long term normals,
#' daily observed, current weather, and forecasts. These APIs are designed for efficiency,
#' allowing you to customize the responses to return just the attributes you need.
#'
#' Note about dates: The system does not adjust for any difference in dates between the location of the user
#'           and where data is being requested from.  It is the responsibility of the user to ensure a valid
#'           date range is specified given any differences in timezone.  These differences can have implications
#'           for whether a given date should be requested from the daily_observed functions or the forecast functions
#'
#' @references http://developer.awhere.com/api/reference/weather/forecasts/geolocation
#'
#' @param - polygon: either a data.frame with column names lat/lon, SpatialPolygons object,
#'                   well-known text string, or extent from raster package. If the object contains
#'                   multiple polygons, the union of them is used.  Information from each individal
#'                   polygon can be retrieved by returning spatial data and using
#'                   the over function from the sp package
#' @param - day_start: character string of the first day for which you want to retrieve data, in the form: YYYY-MM-DD
#' @param - day_end: character string of the last day for which you want to retrieve data, in the form: YYYY-MM-DD
#' @param - block_size: Integer value that corresponds to the number of hours to include in each time block.
#'                     Defaults to a 1 hour block.  This value must divide evenly into 24. (integer - optional)
#' @param - useLocalTime: whether the data specified is the date specified at the location where data is
#'                        being requested from or at UTC = 0.  Default is TRUE
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
#'
#' @examples
#' \dontrun{soils_area(polygon = raster::getData('GADM', country = "Gambia", level = 0, download = T),
#'                         ,day_start = as.character(Sys.Date())
#'                         ,day_end = as.character(Sys.Date() + 5)
#'                         ,block_size = 12)
#'                         ,numcores = 2)}
#'                         
#' @export


soils_area <- function(polygon
                           ,day_start = as.character(Sys.Date())
                           ,day_end = ''
                           ,block_size = 1
                           ,useLocalTime = TRUE
                           ,numcores = 2
                           ,bypassNumCallCheck = FALSE
                           ,returnSpatialData = FALSE
                           ,verbose = TRUE
                           ,keyToUse = awhereEnv75247$uid
                           ,secretToUse = awhereEnv75247$secret
                           ,tokenToUse = awhereEnv75247$token) {
  
  data <- 
    forecasts_area(polygon = polygon
                   ,day_start = day_start
                   ,day_end = day_end
                   ,block_size = block_size
                   ,useLocalTime = useLocalTime
                   ,numcores = numcores
                   ,bypassNumCallCheck = bypassNumCallCheck
                   ,returnSpatialData = returnSpatialData
                   ,returnOnlySoilVars = TRUE
                   ,verbose = verbose
                   ,keyToUse = keyToUse
                   ,secretToUse = secretToUse
                   ,tokenToUse = tokenToUse)
  
  return(data)
}

