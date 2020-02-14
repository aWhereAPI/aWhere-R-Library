#' @title Delete Field
#'
#' @description
#' \code{delete_field} deletes a field_id for a location in the aWhere platform for which you can request weather
#'
#' @details
#' This script deletes a field location in the aWhere platform.
#' This API is a "hard delete" - the field record should actually be deleted from the system.
#' The delete should cascade, if there are associated records to a field, they are deleted as well.
#' This applies when we design/implement "Plantings" API.
#'
#' @param - field_id: an ID of your choosing (string)
#' @param - keyToUse: aWhere API key to use.  For advanced use only.  Most users will not need to use this parameter (optional)
#' @param - secretToUse: aWhere API secret to use.  For advanced use only.  Most users will not need to use this parameter (optional)
#' @param - tokenToUse: aWhere API token to use.  For advanced use only.  Most users will not need to use this parameter (optional)
#'
#' @return - a print text that informs if the query succeded or not
#'
#' @import httr
#'
#' @examples
#' \dontrun{delete_field("field123")}
#' @export

delete_field <- function(field_id
                         ,verbose = TRUE
                         ,keyToUse = awhereEnv75247$uid
                         ,secretToUse = awhereEnv75247$secret
                         ,tokenToUse = awhereEnv75247$token) {

  checkCredentials(keyToUse,secretToUse,tokenToUse)
  checkValidField(field_id,keyToUse,secretToUse,tokenToUse)

  url <- paste0("https://api.awhere.com/v2/fields/", field_id)

  postbody <- paste0('{', field_id, '}');

  doWeatherGet = TRUE
  while (doWeatherGet == TRUE) {
    ## Get data

    request <- httr::DELETE(url, body=postbody, httr::content_type('application/json'),
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
    cat(paste0('Operation Complete'))
  }
}

#' @title Delete Planting
#'
#' @description
#' \code{delete_planting} deletes a planting associated with a specific field_id in the aWhere platform
#'
#' @details
#' The aWhere API only references the most recent planting when calculating agronomics and running
#' models, but if you want to keep your planting records clean for reporting and historical tracking
#' purposes you can delete errant or incorrect plantings.
#'
#' @param - field_id: the ID of the field for which you want to delete an associated planting (string)
#' @param - planting_id: The planting Id that you want to delete.  You can also use "current" to delete the most recent planting (string)
#' @param - keyToUse: aWhere API key to use.  For advanced use only.  Most users will not need to use this parameter (optional)
#' @param - secretToUse: aWhere API secret to use.  For advanced use only.  Most users will not need to use this parameter (optional)
#' @param - tokenToUse: aWhere API token to use.  For advanced use only.  Most users will not need to use this parameter (optional)
#'
#' @return - a print text that informs if the query succeded or not
#'
#' @import httr
#'
#' @examples
#' \dontrun{delete_planting("field123",'133972')}

#' @export

delete_planting <- function(field_id
                            ,planting_id
                            ,verbose = TRUE
                            ,keyToUse = awhereEnv75247$uid
                            ,secretToUse = awhereEnv75247$secret
                            ,tokenToUse = awhereEnv75247$token) {

  checkCredentials(keyToUse,secretToUse,tokenToUse)
  checkValidField(field_id,keyToUse,secretToUse,tokenToUse)

  url <- paste0("https://api.awhere.com/v2/agronomics/fields/", field_id,'/plantings/',planting_id)

  doWeatherGet = TRUE
  while (doWeatherGet == TRUE) {
    ## Get data
    request <- httr::DELETE(url, httr::add_headers(Authorization = paste0("Bearer ", tokenToUse)))

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
    cat(paste0('Operation Complete'))
  }
}

