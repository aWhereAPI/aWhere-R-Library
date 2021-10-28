#' @title Check Status Code of Return
#'
#' @description
#' \code{checkStatusCode} Checks to see if valid aWhere API credentials are loaded
#'
#' @param request object returned from HTTR


checkStatusCode<- function(request
                           ,tryCount) {

  #Pause thread if rate exceeded for random interval or if user out of API calls
  if (request$status_code %in% c(429)) { 
    
    if (tryCount >= 5) {
      stop(paste0('\nstatusName: ',a$statusName
                  ,'\nstatusCode: ',request$status_code
                  ,'\n',a$detailedMessage
                  ,'\nErrorID: ',a$errorId))
    }
    
    cat('Pausing thread due to Rate Limit Exceeded\n')
    
    Sys.sleep(runif(n = 1
                    ,min = 15
                    ,max = 45))
  }
  
  if (request$status_code %in% c(500,502,503)) { #Status Code = 500 are problems from API
    cat('Unexpected Error Received from API... Retrying Query after Short Pause\n')
    
    Sys.sleep(runif(n = 1
                    ,min = 30
                    ,max = 60))
  }
  
  if (!(request$status_code %in% c(200,201,204,429,500,502,503))) { # status code = 200's means that the query worked

    a <- suppressMessages(httr::content(request, as = "parsed"))
    
    stop(paste0('\nstatusName: ',a$statusName
                ,'\nstatusCode: ',request$status_code
                ,'\n',a$detailedMessage
                ,'\nErrorID: ',a$errorId))
  }
  
  return(request$status_code)
}
