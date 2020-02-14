#' @title Get Crops
#'
#' @description
#' \code{get_crops} Calls Crops endpoint of API
#'
#' @details
#' Models are designed for specific crops, and often specific types or varieties of crops.
#' This API provides the list of available crops. When creating a planting record, you're encouraged
#' to specify the crop that is planted in the field using these records.
#'
#' Within each category of crop (organized by name), there is a default crop. When creating a planting,
#' you may not know or care about the specific variety, and can simply specify the crop name as
#' what is planted there. When you do this, the system will use the default crop for that category.
#' You'll know which crop record is the default by referencing the isDefaultForCrop property, described below.
#'
#' @references https://developer.awhere.com/api/reference/crops
#'
#' @param - crop_id: Either a crop id to retrieve information for that specific crop
#'                   or an empty string to retrieve information on all crops associated
#'                   with the user's aWhere API account (string - optional)
#' @param - keyToUse: aWhere API key to use.  For advanced use only.  Most users will not need to use this parameter (optional)
#' @param - secretToUse: aWhere API secret to use.  For advanced use only.  Most users will not need to use this parameter (optional)
#' @param - tokenToUse: aWhere API token to use.  For advanced use only.  Most users will not need to use this parameter (optional)
#'
#' @import httr
#'
#' @return - data.frame containing information about requested crop(s)
#'
#' @examples
#' \dontrun{get_crops('barley-generic')
#'          get_crops()
#' }

#' @export

get_crops <- function(crop_id = ''
                       ,keyToUse = awhereEnv75247$uid
                       ,secretToUse = awhereEnv75247$secret
                       ,tokenToUse = awhereEnv75247$token) {
  
  checkCredentials(keyToUse,secretToUse,tokenToUse)
  
  ## Create Request
  url <- "https://api.awhere.com/v2/agronomics/crops/"
  
  if(crop_id != "") {
    url <- paste0(url, crop_id)
  }
  
  doWeatherGet <- TRUE
  while (doWeatherGet == TRUE) {
    request <- httr::GET(url,
                         httr::content_type('application/json'),
                         httr::add_headers(Authorization =
                                             paste0("Bearer ", tokenToUse)))
    
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
  
  ## Create & fill data frame
  if(crop_id == "") {
    data <- as.data.frame(do.call(rbind, lapply(a$crops, rbind)))[, c(1:5)]
    colnames(data) <- c("cropId", "name", "type", "variety", "isDefaultForCrop")
    rownames(data) <- c(1:nrow(data))
    
    data <- data.frame(lapply(data, as.character), stringsAsFactors=FALSE)
    
    data <- as.matrix(data)
    data[sapply(data, is.null)] <- NA
    data <- as.data.frame(data)
    
  } else {
    data <- as.data.frame(rbind(a))[,c(1:5)]
    colnames(data) <- c("cropId", "name", "type", "variety", "isDefaultForCrop")
    rownames(data) <- c(1:nrow(data))
  }
  
  data <- dplyr::mutate_if(data, is.factor, as.character)
  
  if(nrow(data) == 0) {
    stop(a$simpleMessage)
  } else {
    return(as.data.frame(data))
  }
}
