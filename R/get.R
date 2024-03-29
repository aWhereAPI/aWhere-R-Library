#' @title Get Fields
#'
#' @description
#' \code{get_fields} calls Get Field Locations Endpoint of API
#'
#' @details
#' Fields are how you manage the locations for which you're tracking weather, agronomics,
#' models, and progress over growing seasons in the aWhere API. By registering a field, you create a quick way
#' to consistently reference locations across all of our APIs, and allow our modeling APIs
#' to better operate and tune to the conditions and status of each specific field.
#'
#' Before using aWhere's APIs you'll need to register the field locations.
#' This is a one-time step. Every field has an ID that you define, plus a latitude and longitude.
#' Fields are universal across all of our APIs, and as you provide information about a field, some APIs
#' (such as agronomics and models) can leverage that detail internally to more easily and seamlessly
#' calculate information for you.
#'
#' @references https://docs.awhere.com/knowledge-base-docs/get-field-locations/
#'
#' @param field_id Either a field id to retrieve information for that specific field
#'                   or an empty string to retrieve information on all fields associated
#'                   with the user's aWhere API account (string - optional)
#' @param offset The number of objects to skip before returning objects. Used in conjunction with offset to paginate. (optional)
#' @param limit The number of results to include on each of page of listed fields. Used in conjunction with offset to paginate. (optional)
#' @param requestAllFields Causes function to execute logic to return all of a users fields using the minimum number of API calls 
#'                            based on the limit parameter.  If used, offset must be set to default value (optional)
#' @param keyToUse aWhere API key to use.  For advanced use only.  Most users will not need to use this parameter (optional)
#' @param secretToUse aWhere API secret to use.  For advanced use only.  Most users will not need to use this parameter (optional)
#' @param tokenToUse aWhere API token to use.  For advanced use only.  Most users will not need to use this parameter (optional)
#' @param apiAddressToUse Address of aWhere API to use.  For advanced use only.  Most users will not need to use this parameter (optional)
#'
#' @import httr
#'
#' @return - data.frame containing information about requested field(s)
#'
#' @examples
#' \dontrun{get_fields('field_test')
#'          get_fields()
#' }

#' @export

get_fields <- function(field_id = ""
                       ,offset = ""
                       ,limit = 10
                       ,requestAllFields = TRUE
                       ,keyToUse = awhereEnv75247$uid
                       ,secretToUse = awhereEnv75247$secret
                       ,tokenToUse = awhereEnv75247$token
                       ,apiAddressToUse = awhereEnv75247$apiAddress) {
  
  checkCredentials(keyToUse,secretToUse,tokenToUse)
  
  if (requestAllFields == TRUE & offset != '') {
    stop('Cannot specify both offset parameter and have requestAllFields == TRUE')
  }
  
  ## Create Request
  url <- paste0(apiAddressToUse, "/fields/")
  
  if(field_id != "") {
    url <- paste0(url, field_id)
  }
  
  if(offset != "" || limit != "") {
    url <- paste0(url, "?")
    if(offset != "") {
      url <- paste0(url, "&offset=", offset)
    }
    if(limit != "") {
      url <- paste0(url, "&limit=", limit)
    }
  }
  
  doWeatherGet <- TRUE
  tryCount <- 0
  while (doWeatherGet == TRUE) {
    tryCount <- tryCount + 1
    
    request <- httr::GET(url,
                         httr::content_type('application/json'),
                         httr::add_headers(Authorization =
                                             paste0("Bearer ", tokenToUse)))
    
    a <- suppressMessages(httr::content(request))
    
    temp <- check_JSON(a
                       ,request
                       ,tryCount
                       ,keyToUse
                       ,secretToUse
                       ,tokenToUse)
    
    doWeatherGet <- temp[[1]]
    
    #if the token was updated, this will cause it to be used through function
    tokenToUse <- temp[[3]]
    
    #If the above query worked but user wants all fields, check if their is more fields to 
    #get and if so use the info it contains to request and append together
    if (requestAllFields == TRUE & doWeatherGet == FALSE & field_id == ''){
      #If the API indicates more fields to get, use URL it gives to get them
      if (c('next') %in% names(a[['_links']])) {
        
        url <- paste0(gsub(pattern = '/v2',replacement = '',x = awhereEnv75247$apiAddress),a[['_links']][['next']][['href']])
        doWeatherGet <- TRUE
        
        if (exists('tempReturn') == TRUE) {
          tempReturn$fields <- c(tempReturn$fields,a$fields)
        } else {
          tempReturn <- copy(a)
        }
        #If API says no more fields, take appended list of fields and return it
      } else {
        
        if (exists('tempReturn') == TRUE) {
          a$fields <- c(tempReturn$fields,a$fields)
        }
      }
    }
  }
  
  ## Create & fill data frame
  if(is.null(a$statusCode)) {
    if(field_id == "") {
      if (length(a$fields) > 0) {
        data <- as.data.frame(do.call(rbind, lapply(a$fields, rbind)))[, c(1:5)]
        data <- cbind(data, do.call(rbind, lapply(data$centerPoint, rbind)))
        data$centerPoint <- NULL
        colnames(data) <- c("fieldName", "Acres", "farmId", "fieldId", "Latitude", "Longitude")
        
        data <- as.matrix(data)
        data[sapply(data, is.null)] <- NA
        data <- as.data.frame(data)
        for(i in 1:ncol(data)) {
          data[,i] <- do.call(rbind, lapply(data[,i], rbind))[,1]
        }
        
        data$fieldName <- as.character(data$fieldName)
        data$Acres <- as.numeric(data$Acres)
        data$farmId <- as.character(data$farmId)
        data$fieldId <- as.character(data$fieldId)
        data$Latitude <- as.numeric(data$Latitude)
        data$Longitude <- as.numeric(data$Longitude)
        
      } else {
        #Return empty data.frame if no fields have been made
        data <- data.frame(fieldName = as.character(NA)
                           ,Acres = as.numeric(NA)
                           ,farmId = as.character(NA)
                           ,fieldId = as.character(NA)
                           ,Latitude = as.numeric(NA)
                           ,Longitude = as.numeric(NA)
                           ,stringsAsFactors = FALSE)[0,]
      }
    } else {
      a[sapply(a, is.null)] <- NA
      data <- data.frame(fieldName = as.character(unlist(a$name))
                         ,Acres = as.numeric(unlist(a$acres))
                         ,farmId = as.character(unlist(a$farmId))
                         ,fieldId = as.character(unlist(a$id))
                         ,Latitude = as.numeric(unlist(a$centerPoint$latitude))
                         ,Longitude = as.numeric(unlist(a$centerPoint$longitude))
                         ,stringsAsFactors = FALSE)
    }
  }
  
  if(!is.null(a$statusCode)) {
    stop(a$simpleMessage)
  } else {
    return(as.data.frame(data))
  }
}

#' @title Get Planting
#'
#' @description
#' \code{get_planting} Gets a planting or list of plantings in the aWhere platform for which you can request weather
#'
#' @details
#' Fields are how you manage the locations for which you're tracking weather, agronomics,
#' models, and progress over growing seasons. By registering a field, you create a quick way
#' to consistently reference locations across all our APIs, and allow our modeling APIs
#' to better operate and tune to the conditions and status of each specific field. A Planting
#' is the record of a crop's season in a given field, and is where you tell the platform
#' about what is planted there and when it was planted.
#'
#' Plantings are the way to manage crop and field activity in the aWhere API. Use this
#' API to record the type of crop, planting date, projections, and actuals to get the
#' most out of our more advanced APIs. Over time, this API also enables historical metrics
#' for any given field in the platform. In this function by setting an Id you can retrieve the weather
#' and agronomics for that location in all the other APIs.
#'
#' @param field_id a field ID to look within (string - optional)
#' @param planting_id a planting ID to look for (string - optional)
#' @param current whether to just get current plantings(T) or include historical plantings(F).
#'                   To get most recent planting record for a field, set current to TRUE and do not pass in a planting_id (boolean - optional)
#' @param offset The number of objects to skip before returning objects. Used in conjunction with offset to paginate. (optional)
#' @param limit The number of results to include on each of page of listed fields. Used in conjunction with offset to paginate. (optional)
#' @param requestAllPlantings Causes function to execute logic to return all of a users plantins using the minimum number of API calls 
#'                          based on the limit parameter and the value of current.  If used, offset must be set to default value (optional)
#' @param keyToUse aWhere API key to use.  For advanced use only.  Most users will not need to use this parameter (optional)
#' @param secretToUse aWhere API secret to use.  For advanced use only.  Most users will not need to use this parameter (optional)
#' @param tokenToUse aWhere API token to use.  For advanced use only.  Most users will not need to use this parameter (optional)
#' @param apiAddressToUse Address of aWhere API to use.  For advanced use only.  Most users will not need to use this parameter (optional)
#'
#' @return - data.frame containing information about requested field(s)
#'
#' @import httr
#'
#' @references https://docs.awhere.com/knowledge-base-docs/get-plantings/
#'
#' @examples
#' \dontrun{get_planting(field_id='field_test')
#' get_planting(field_id = 'field_test', planting_id = '156035')
#' get_planting('field_test', current = T)
#' get_planting(field_id='field_test', offset = '0', limit = '5')}
#' @export

get_planting <- function(field_id = ""
                         ,planting_id = ""
                         ,current = FALSE
                         ,offset = ""
                         ,limit = 10
                         ,requestAllPlantings = TRUE
                         ,keyToUse = awhereEnv75247$uid
                         ,secretToUse = awhereEnv75247$secret
                         ,tokenToUse = awhereEnv75247$token
                         ,apiAddressToUse = awhereEnv75247$apiAddress) {
  
  checkCredentials(keyToUse,secretToUse,tokenToUse)
  
  if (requestAllPlantings == TRUE & offset != '') {
    stop('Cannot specify both offset parameter and have requestAllFields == TRUE')
  }
  
  ## Create Request
  url <- paste0(apiAddressToUse, "/agronomics/")
  
  if(field_id != "") {
    checkValidField(field_id,keyToUse,secretToUse,tokenToUse)
    url <- paste0(url, "fields/", field_id, "/plantings")
  } else {
    url <- paste0(url, "plantings")
  }
  
  if(planting_id != "") {
    url <- paste0(url, "/", planting_id)
  }
  
  if(current) {
    url <- paste0(url, "/current")
  }
  
  if(offset != "" || limit != "") {
    url <- paste0(url, "?")
    if(offset != "") {
      url <- paste0(url, "&offset=", offset)
    }
    if(limit != "") {
      url <- paste0(url, "&limit=", limit)
    }
  }
  
  url <- paste0(url,'&sort=id')
  
  doWeatherGet <- TRUE
  tryCount <- 0
  while (doWeatherGet == TRUE) {
    tryCount <- tryCount + 1
    
    request <- httr::GET(url,
                         httr::content_type('application/json'),
                         httr::add_headers(Authorization =
                                             paste0("Bearer ", tokenToUse)))
    
    a <- suppressMessages(httr::content(request))
    
    temp <- check_JSON(a
                       ,request
                       ,tryCount
                       ,keyToUse
                       ,secretToUse
                       ,tokenToUse)
    
    doWeatherGet <- temp[[1]]
    
    #if the token was updated, this will cause it to be used through function
    tokenToUse <- temp[[3]]
    
    #If the above query worked but user wants all fields, check if their is more fields to 
    #get and if so use the info it contains to request and append together
    if (requestAllPlantings == TRUE & doWeatherGet == FALSE & field_id == ''){
      #If the API indicates more fields to get, use URL it gives to get them
      if (c('next') %in% names(a[['_links']])) {
        
        url <- paste0(gsub(pattern = '/v2',replacement = '',x = awhereEnv75247$apiAddress),a[['_links']][['next']][['href']])
        doWeatherGet <- TRUE
        
        if (exists('tempReturn') == TRUE) {
          tempReturn$plantings <- c(tempReturn$plantings,a$plantings)
        } else {
          tempReturn <- copy(a)
        }
        #If API says no more fields, take appended list of fields and return it
      } else {
        
        if (exists('tempReturn') == TRUE) {
          a$plantings <- c(tempReturn$plantings,a$plantings)
        }
      }
    }
  }
  
  ## Create & fill data frame
  if(is.null(a$statusCode)) {
    
    if (length(a$plantings) > 0 | length(a$id) > 0) {
      
      data <- as.data.frame(do.call(rbind, lapply(a$plantings, rbind)))
      
      # case if field has no plantings
      if (nrow(data) == 0) {
        stop(paste("field_id:", field_id, "has no planting.", a$detailedMessage))
      }
      data <- data[, c('id','crop','field','plantingDate','harvestDate','yield','projections')]
      
      #expand the nested column
      data <- cbind(data, do.call(rbind, lapply(data$yield, rbind)))
      #remove the original columns
      data$yield <- NULL
      
      #expand the nested column
      data <- cbind(data, do.call(rbind, lapply(data$projections, rbind)))
      #remove the original column
      data$projections <- NULL
      
      #expand the new nested yield column
      data <- cbind(data, do.call(rbind, lapply(data$yield, rbind)))
      #remove the original columns
      data$yield <- NULL

      colnames(data) <- c("planting_id", "crop", "field_id", "plantingDate"
                          ,"actualHarvestDate", "yieldAmount", "yieldUnits"
                          ,"projectedHarvestDate", "projectedYieldAmount"
                          ,"projectedYieldUnits")
      
      #Replace NULL with NA
      data[data == "NULL"] <- NA
      
      #Columns are currently of type list, need to convert.  
      #Doing explicitly for ease of following code logic
      data$planting_id <- as.integer(data$planting_id)
      data$crop <- as.character(data$crop)
      data$field_id <- as.character(data$field_id)
      data$plantingDate <- as.character(data$plantingDate)
      data$actualHarvestDate <- as.character(data$actualHarvestDate)
      data$yieldAmount <- as.numeric(data$yieldAmount)
      data$yieldUnits <- as.character(data$yieldUnits)
      data$projectedHarvestDate <- as.character(data$projectedHarvestDate)
      data$projectedYieldAmount <- as.numeric(data$projectedYieldAmount)
      data$projectedYieldUnits <- as.character(data$projectedYieldUnits)
      
    } else {
      a[sapply(a, is.null)] <- NA
      
      data <- data.frame(planting_id = as.integer(unlist(a$id))
                         ,crop = as.character(unlist(a$crop))
                         ,field_id = as.character(unlist(a$field))
                         ,plantingDate = as.character(unlist(a$plantingDate))
                         ,actualHarvestDate = as.character(unlist(a$harvestDate))
                         ,yieldAmount = as.numeric(unlist(a$yield$amount))
                         ,yieldUnits = as.character(unlist(a$yield$units))
                         ,projectedHarvestDate = as.character(unlist(a$projections$harvestDate))
                         ,projectedYieldAmount = as.numeric(unlist(a$projections$yield$amount))
                         ,projectedYieldUnits = as.character(unlist(a$projections$yield$units))
                         ,stringsAsFactors = FALSE)
    }
  } else {
    #Return empty data.frame if no fields have been made
    data <- data.frame(planting_id = as.integer(NA)
                       ,crop = as.character(NA)
                       ,field_id = as.character(NA)
                       ,plantingDate = as.character(NA)
                       ,actualHarvestDate = as.character(NA)
                       ,yieldAmount = as.numeric(NA)
                       ,yieldUnits = as.character(NA)
                       ,projectedHarvestDate = as.character(NA)
                       ,projectedYieldAmount = as.numeric(NA)
                       ,projectedYieldUnits = as.character(NA)
                       ,stringsAsFactors = FALSE)[0,]
  }
  
  if(!is.null(a$statusCode)) {
    stop(a$detailedMessage)
  } else {
    return(as.data.frame(data))
  }
}
