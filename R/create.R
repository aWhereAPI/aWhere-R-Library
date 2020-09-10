#' @title Create Field
#'
#' @description
#' \code{create_field} This function will generate a new field associated with the user's account in the aWhere API.
#'   This is a one-time operation for each field.
#'
#' @details
#' Fields are how you manage the locations for which you're tracking weather, agronomics,
#' models, and progress over growing seasons in the aWhere API. By registering a field, you create a quick way
#' to consistently reference locations across all of our APIs, and allow our modeling APIs
#' to better operate and tune to the conditions and status of each specific field.
#'
#' Creating a field registers the location with the aWhere system, making it easier to reference
#' and track your locations as well as run agronomics and models automatically. You
#' only need to create a field once, after which you can reference the field by ID
#' (you'll use this ID in most URI endpoints in the aWhere system).
#'
#' All spaces will be converted to underscores to conform with the requirements of the API.
#'
#' @param - field_id: an ID of your choosing (string)
#' @param - latitude: the latitude of the field location in decimal format (double)
#' @param - longitude: the longitude of the field location in decimal format (double)
#' @param - farmid: an ID of your choosing for the farm to which this field belongs (string)
#' @param - field_name: a name of the location (optional - string)
#' @param - acres: the acres of the field (optional)
#' @param - keyToUse: aWhere API key to use.  For advanced use only.  Most users will not need to use this parameter (optional)
#' @param - secretToUse: aWhere API secret to use.  For advanced use only.  Most users will not need to use this parameter (optional)
#' @param - tokenToUse: aWhere API token to use.  For advanced use only.  Most users will not need to use this parameter (optional)
#'
#' @return - printed text that informs if the query succeeded or not
#'
#' @references https://docs.awhere.com/knowledge-base-docs/create-a-field-location/
#'
#' @import httr
#'
#' @examples
#' \dontrun{
#' create_field(field_id = "field123",latitude = 39.8282,longitude = -98.5795,farm_id = "farmA",field_name = "Some Field Location",acres = 100)
#' create_field(field_id = "field_test", latitude = 39.971906, longitude = -105.088773, farm_id = "Office")
#' }

#' @export
create_field <- function(field_id
                         ,latitude
                         ,longitude
                         ,farm_id
                         ,field_name = ""
                         ,acres = ""
                         ,verbose = TRUE
                         ,keyToUse = awhereEnv75247$uid
                         ,secretToUse = awhereEnv75247$secret
                         ,tokenToUse = awhereEnv75247$token) {

  #############################################################
  #Checking Input Parameters
  checkCredentials(keyToUse,secretToUse,tokenToUse)
  checkValidLatLong(latitude,longitude)

  if (acres != "") {
    if (suppressWarnings(is.na(as.double(acres))) == TRUE) {
      warning('The entered acres Value is not valid. Please correct\n')
      return()
    }
  }

  field_id <- gsub(' ','_',field_id)
  farm_id <- gsub(' ','_',farm_id)

  ## Create Request
  url <- paste0(awhereEnv75247$apiAddress, "/fields")

  postbody <- paste0('{"id":"', field_id, '",',
                     '"centerPoint":{"latitude":', latitude, ',"longitude":', longitude, '}',
                     ',"farmId":"', farm_id, '"')
  if(field_name != "") {
    postbody <- paste0(postbody, ',"name":"', field_name, '"')
  }
  if(acres != "") {
    postbody <- paste0(postbody, ',"acres":', acres)
  }
  postbody <- paste0(postbody, '}')

  doWeatherGet <- TRUE
  while (doWeatherGet == TRUE) {
    request <- httr::POST(url, body=postbody, httr::content_type('application/json'),
                          httr::add_headers(Authorization = paste0("Bearer ", tokenToUse)))

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
  if (verbose == TRUE) {
    cat(paste0('Operation Complete \n'))
  }
}


#' @title Create Planting
#'
#' @description
#' \code{create_planting} creates a planting in a field location in the aWhere platform for which you can request weather
#'
#' @details
#' Fields are how you manage the locations for which you're tracking weather, agronomics,
#' models, and progress over growing seasons. By registering a field, you create a quick way
#' to consistently reference locations across all our APIs, and allow our modeling APIs
#' to better operate and tune to the conditions and status of each specific field. A Planting
#' is the record of a crop's season in a given field, and is where you tell the platform
#' about what is planted there and when it was planted.
#'
#' Creating a planting will provide the aWhere platform the information needed to run models
#' and more efficiently calculate agronomic values. You can also use these properties to record
#' projections for the field, like yield or harvest date, to track the success of a field over
#' the course of a growing season. Recording projected and actual yield and harvest date also helps
#' aWhere tune the models for even greater accuracy.
#'
#' There can only be one active planting per field. You can create multiple plantings per field
#' but only the most recent one will be considered the "current" one. Use this functionality to
#' create historical records if you have them.
#'
#' When creating a planting, you must specify the crop and planting date.
#' The crop must be an option from the Crops API; but there is also a short cut where if you don't
#' know or want to use a specific crop ID, you can simply specify the crop name, such as "corn" or
#' "wheat" and the the API will select the default for that category.
#'
#' This script creates a planting in a field location in the aWhere platform. By setting an Id you can retrieve the weather
#' and agronomics for that location in all the other APIs. The planting ID corresponds to a planting within a field.
#'
#' @param - field_id: an ID of your choosing (string)
#' @param - crop: cropId or crop name (string)
#' @param - planting_date: date crop was planted in the field. Format as YYYY-MM-DD (string)
#' @param - proj_yield_amount: amount of projected yield from planting (string)
#' @param - proj_yield_units: units of projected yield (string - optional)
#' @param - proj_harvest_date: projected harvest date at the start of the season. Format as YYYY-MM-DD (string - optional)
#' @param - yield_amount: actual yield (string - optional)
#' @param - yield_units: units of actual yield (string - optional)
#' @param - harvest_date: actual harvest date at end of season. Format as YYYY-MM-DD (string - optional)
#' @param - keyToUse: aWhere API key to use.  For advanced use only.  Most users will not need to use this parameter (optional)
#' @param - secretToUse: aWhere API secret to use.  For advanced use only.  Most users will not need to use this parameter (optional)
#' @param - tokenToUse: aWhere API token to use.  For advanced use only.  Most users will not need to use this parameter (optional)
#'
#' @return - system generated planting id along with a print text that informs if the query succeeded or not
#'
#' @references https://docs.awhere.com/knowledge-base-docs/create-a-planting-in-a-field/
#'
#' @import httr
#'
#' @examples
#' \dontrun{create_planting(field_id='field_test',crop='corn', planting_date='2015-10-25', proj_yield_amount='100',
#'                          proj_yield_units='Bushels', proj_harvest_date='2016-02-01', yield_amount='110',
#'                          yield_units='Bushels', harvest_date='2016-02-01')
#' }
#' @export

create_planting <- function(field_id
                            ,crop
                            ,planting_date
                            ,proj_yield_amount = ""
                            ,proj_yield_units = ""
                            ,proj_harvest_date = ""
                            ,yield_amount = ""
                            ,yield_units = ""
                            ,harvest_date = ""
                            ,verbose = TRUE
                            ,keyToUse = awhereEnv75247$uid
                            ,secretToUse = awhereEnv75247$secret
                            ,tokenToUse = awhereEnv75247$token) {

  checkCredentials(keyToUse,secretToUse,tokenToUse)
  ## Error checking for valid entries

  if((proj_yield_amount != "" & proj_yield_units == "") || (proj_yield_amount == "" & proj_yield_units != "")) {
    stop("Must either have both projected yield amount and projected units, or neither")
  }

  if((yield_amount != "" & yield_units == "") | (yield_amount == "" & yield_units != "")) {
    stop("Must either have both yield amount and yield units, or neither")
  }

  checkValidField(field_id,keyToUse,secretToUse,tokenToUse)

  url <- paste0(awhereEnv75247$apiAddress, "/agronomics/fields/", field_id, "/plantings")

  postbody <- paste0('{',
                     '"crop":"', crop, '",',
                     '"plantingDate":"', planting_date, '"')
  if(proj_yield_amount != "" | proj_harvest_date != "") {
    postbody <- paste0(postbody, ',"projections":{')
    if(proj_yield_amount != "") {
      postbody <- paste0(postbody, '"yield":{',
                         '"amount":', proj_yield_amount,',',
                         '"units":"', proj_yield_units, '"}')
      if(proj_harvest_date != "") {
        postbody <- paste0(postbody, ",")
      }
    }
    if(proj_harvest_date != "") {
      postbody <- paste0(postbody, '"harvestDate":"', proj_harvest_date, '"',
                         '}')
    }
  }
  if(yield_amount != "") {
    postbody <- paste0(postbody, ',"yield":{',
                       '"amount":', yield_amount, ',',
                       '"units":"', yield_units, '"',
                       '},')
  }
  if(harvest_date != "") {
    postbody <- paste0(postbody, '"harvestDate":"', harvest_date, '"')
  }

  postbody <- paste0(postbody, '}')

  doWeatherGet <- TRUE
  while (doWeatherGet == TRUE) {
    request <- httr::POST(url, body=postbody, httr::content_type('application/json'),
                          httr::add_headers(Authorization = paste0("Bearer ", tokenToUse)))

    a <- suppressMessages(httr::content(request))

    temp <- check_JSON(a
                       ,request
                       ,keyToUse
                       ,secretToUse
                       ,tokenToUse)
    
    doWeatherGet <- temp[[1]]
    
    #if the token was updated, this will cause it to be used through function
    tokenToUse <- temp[[3]]
  }

  if (verbose == TRUE) {
    cat(paste0('Operation Complete \n Planting ID: ', a$id),'\n')
  }
  return(a$id)
}
