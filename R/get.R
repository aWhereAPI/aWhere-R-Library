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
#' @references http://developer.awhere.com/api/reference/fields/get-fields
#'
#' @param - field_id: Either a field id to retrieve information for that specific field
#'                   or an empty string to retrieve information on all fields associated
#'                   with the user's aWhere API account (string - optional)
#' @param - offset: The number of objects to skip before returning objects. Used in conjunction with offset to paginate. (optional)
#' @param - limit: The number of results to include on each of page of listed fields. Used in conjunction with offset to paginate. (optional)
#' @param - requestAllFields: Causes function to execute logic to return all of a users fields using the minimum number of API calls 
#'                            based on the limit parameter.  If used, offset must be set to default value (optional)
#' @param - keyToUse: aWhere API key to use.  For advanced use only.  Most users will not need to use this parameter (optional)
#' @param - secretToUse: aWhere API secret to use.  For advanced use only.  Most users will not need to use this parameter (optional)
#' @param - tokenToUse: aWhere API token to use.  For advanced use only.  Most users will not need to use this parameter (optional)
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
                       ,limit = 50
                       ,requestAllFields = TRUE
                       ,keyToUse = awhereEnv75247$uid
                       ,secretToUse = awhereEnv75247$secret
                       ,tokenToUse = awhereEnv75247$token) {

  checkCredentials(keyToUse,secretToUse,tokenToUse)

  if (requestAllFields == TRUE & offset != '') {
    stop('Cannot specify both offset parameter and have requestAllFields == TRUE')
  }
  
  ## Create Request
  url <- "https://api.awhere.com/v2/fields/"

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
    
    #If the above query worked but user wants all fields, check if their is more fields to 
    #get and if so use the info it contains to request and append together
    if (requestAllFields == TRUE & doWeatherGet == FALSE & field_id == ''){
      #If the API indicates more fields to get, use URL it gives to get them
      if (c('next') %in% names(a[['_links']])) {
        
        url <- paste0("https://api.awhere.com",a[['_links']][['next']][['href']])
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
      } else {
        #Return empty data.frame if no fields have been made
        data <-  data.frame(matrix(ncol = 6, nrow = 0))
        colnames(data) <- c("fieldName", "Acres", "farmId", "fieldId", "Latitude", "Longitude")
        
        return(as.data.frame(data))
      }
    } else {
      a[sapply(a, is.null)] <- NA
      data <- data.frame(fieldName = unlist(a$name)
                         ,Acres = unlist(a$acres)
                         ,farmId = unlist(a$farmId)
                         ,fieldId = unlist(a$id)
                         ,Latitude = unlist(a$centerPoint$latitude)
                         ,Longitude = unlist(a$centerPoint$longitude))
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
#' @param - field_id: a field ID to look within (string - optional)
#' @param - planting_id: a planting ID to look for (string - optional)
#' @param - current: whether to just get current plantings(T) or include historical plantings(F).
#'                   To get most recent planting record for a field, set current to TRUE and do not pass in a planting_id (boolean - optional)
#' @param - offset: The number of objects to skip before returning objects. Used in conjunction with offset to paginate. (optional)
#' @param - limit: The number of results to include on each of page of listed fields. Used in conjunction with offset to paginate. (optional)
#' @param - requestAllPlantings: Causes function to execute logic to return all of a users plantins using the minimum number of API calls 
#'                            based on the limit parameter and the value of current.  If used, offset must be set to default value (optional)
#' @param - keyToUse: aWhere API key to use.  For advanced use only.  Most users will not need to use this parameter (optional)
#' @param - secretToUse: aWhere API secret to use.  For advanced use only.  Most users will not need to use this parameter (optional)
#' @param - tokenToUse: aWhere API token to use.  For advanced use only.  Most users will not need to use this parameter (optional)
#'
#' @return - data.frame containing information about requested field(s)
#'
#' @import httr
#'
#' @references http://developer.awhere.com/api/reference/plantings/get-plantings
#'
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
                         ,limit = 50
                         ,requestAllPlantings = TRUE
                         ,keyToUse = awhereEnv75247$uid
                         ,secretToUse = awhereEnv75247$secret
                         ,tokenToUse = awhereEnv75247$token) {

  checkCredentials(keyToUse,secretToUse,tokenToUse)

  if (requestAllPlantings == TRUE & offset != '') {
    stop('Cannot specify both offset parameter and have requestAllFields == TRUE')
  }
  
  ## Create Request
  url <- "https://api.awhere.com/v2/agronomics/"

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
    
    #If the above query worked but user wants all fields, check if their is more fields to 
    #get and if so use the info it contains to request and append together
    if (requestAllPlantings == TRUE & doWeatherGet == FALSE & field_id == ''){
      #If the API indicates more fields to get, use URL it gives to get them
      if (c('next') %in% names(a[['_links']])) {
        
        url <- paste0("https://api.awhere.com",a[['_links']][['next']][['href']])
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
    
 #   if (c('plantings') %in% names(a)) {
    if (length(a$plantings) > 0 | length(a$id) > 0) {
      
      if((field_id != '' && planting_id == "" && current == FALSE) ||
         (field_id == '' && current == TRUE)) {
           
        data <- as.data.frame(do.call(rbind, lapply(a$plantings, rbind)))
      
        # case if field has no plantings
        if (nrow(data) == 0) {
          stop(paste("field_id:", field_id, "has no planting.", a$detailedMessage))
        }
        data <- data[, c(1:7)]
        data <- cbind(data, do.call(rbind, lapply(data$yield, rbind)))
        data$yield <- NULL
        data <- cbind(data, do.call(rbind, lapply(data$projections, rbind)))
        data <- cbind(data, do.call(rbind, lapply(data$yield, rbind)))
        data$yield <- NULL
        data$projections <- NULL
  
        colnames(data) <- c("planting_id", "crop", "field_id", "plantingDate"
                            ,"actualHarvestDate", "yieldAmount", "yieldUnits"
                            ,"projectedHarvestDate", "projectedYieldAmount"
                            ,"projectedYieldUnits")
        
        data <- as.matrix(data)
        data[sapply(data, is.null)] <- NA
        data <- as.data.frame(data)
        for(i in 1:ncol(data)) {
          data[,i] <- do.call(rbind, lapply(data[,i], rbind))[,1]
        }
  
      } else {
        a[sapply(a, is.null)] <- NA
        a$yield[sapply(a$yield, is.null)] <- NA
        a$projections$yield[sapply(a$projections$yield, is.null)] <- NA
        a$projections$harvestDate[is.null(a$projections$harvestDate)] <- NA
  
        data <- data.frame(planting_id = unlist(a$id)
                           ,crop = unlist(a$crop)
                           ,field_id = unlist(a$field)
                           ,plantingDate = unlist(a$plantingDate)
                           ,actualHarvestDate = unlist(a$harvestDate)
                           ,yieldAmount = unlist(a$yield$amount)
                           ,yieldUnits = unlist(a$yield$units)
                           ,projectedHarvestDate = unlist(a$projections$harvestDate)
                           ,projectedYieldAmount = unlist(a$projections$yield$amount)
                           ,projectedYieldUnits = unlist(a$projections$yield$units))
      }
    } else {
      #Return empty data.frame if no fields have been made
      data <-  data.frame(matrix(ncol = 10, nrow = 0))
      colnames(data) <- c("planting_id", "crop", "field_id", "plantingDate"
                          ,"actualHarvestDate", "yieldAmount", "yieldUnits"
                          ,"projectedHarvestDate", "projectedYieldAmount"
                          ,"projectedYieldUnits")
      
      return(as.data.frame(data))
    }
  }

  if(!is.null(a$statusCode)) {
    stop(a$detailedMessage)
  } else {
    return(as.data.frame(data))
  }

}

#' @title Get Job
#'
#' @description
#' \code{get_job} Gets a job's results when complete.
#'
#' @details
#' Once a batch job is queued you can check on its status with this API. If the job is complete and results are available, they will be included in the response body.
#'
#' @param - job_id: a job ID assigned by an aWhere create job.
#' @param - wait: wait for job to complete before returning
#' @param - keyToUse: aWhere API key to use.  For advanced use only.  Most users will not need to use this parameter (optional)
#' @param - secretToUse: aWhere API secret to use.  For advanced use only.  Most users will not need to use this parameter (optional)
#' @param - tokenToUse: aWhere API token to use.  For advanced use only.  Most users will not need to use this parameter (optional)
#'
#' @return - data.frame containing the requested payload(s).
#'
#' @import httr
#'
#' @references https://developer.awhere.com/api/reference/batch/status-results
#'
#' @examples
#' \dontrun{get_job(job_id='1234')}

#' @export

get_job <- function(job_id
                    ,wait = TRUE
                    ,retry_secs = 60
                    ,num_retries = 60
                    ,keyToUse = awhereEnv75247$uid
                    ,secretToUse = awhereEnv75247$secret
                    ,tokenToUse = awhereEnv75247$token) {

  checkCredentials(keyToUse,secretToUse,tokenToUse)

  ## Create Request
  url <- "https://api.awhere.com/v2/jobs/"

  if(is.na(job_id)) {
    stop("must specify job_id")
  }

  url <- paste0(url, job_id)

  doWeatherGet <- TRUE
  retries <- 0
  while (doWeatherGet == TRUE) {
    request <- httr::GET(url,
                         httr::content_type('application/json'),
                         httr::add_headers(Authorization = paste0("Bearer ", tokenToUse)))

    a <- suppressMessages(httr::content(request))

    if (any(grepl('API Access Expired',a))) {
      if(exists("awhereEnv75247")) {
        if(tokenToUse == awhereEnv75247$token) {
          get_token(keyToUse,secretToUse)
          tokenToUse <- awhereEnv75247$token
        } else {
          stop("The token you passed in has expired. Please request a new one and retry your function call with the new token.")
        }
      } else {
        stop("The token you passed in has expired. Please request a new one and retry your function call with the new token.")
      }
    } else if (a$jobStatus == "Done") {
      doWeatherGet <- FALSE
    } else if (retries >= num_retries) {
      stop(paste("Get job for jobId:", job_id, "timed out after", num_retries, "retries"))
    } else {
      print(sprintf("job %s status: %s, retrying...", job_id, a$jobStatus))
      Sys.sleep(retry_secs)
      retries <- retries + 1
    }
  }
  return(a)
}
