#' @title Check Status Code of Return
#'
#' @description
#' \code{checkStatusCode} Checks to see if valid aWhere API credentials are loaded
#'
#' @param request object returned from HTTR

checkStatusCode<- function(request) {

  if (!(request$status_code %in% c(200,201,204))) { # status code = 200 means that the query worked

    a <- suppressMessages(httr::content(request, as = "parsed"))
    stop(paste0(a$statusName,'\n',a$detailedMessage,'\nErrorID: ',a$errorId))
  }
}
