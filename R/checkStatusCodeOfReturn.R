#' @title Check Status Code of Return
#'
#' @description
#' \code{checkStatusCode} Checks to see if valid aWhere API credentials are loaded
#'
#' @param request object returned from HTTR


checkStatusCode<- function(request) {

  #Pause thread if rate exceeded for random interval
  if (request$status_code %in% c(429)) { #Status Code = 429 means that rate limit exceeded
    cat('Pausing thread due to Rate Limit Exceeded\n')
    
    Sys.sleep(runif(n = 1
                    ,min = 15
                    ,max = 45))
  }
  
  if (!(request$status_code %in% c(200,201,204))) { # status code = 200 means that the query worked

    a <- suppressMessages(httr::content(request, as = "parsed"))
    stop(paste0(a$statusName,'\n',a$detailedMessage,'\nErrorID: ',a$errorId))
  }
  
  return(request$status_code)
}
