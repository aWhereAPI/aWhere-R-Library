
#' @title Verify User Wants Make API Calls
#'
#' @description
#' \code{verify_api_calls} Tells user how many api calls will be made to pull the data requested
#'
#' @param grid data frame returned from create_awhere_grid

verify_api_calls <- function(grid,bypassNumCallCheck) {
  
  if(nrow(grid) == 0) {
    stop('Polygon is not large enough to contain the centroid of any aWhere Grid cell.  Please enlarge and try again\n')
  }
  
  if (bypassNumCallCheck == FALSE) {
    cat(paste0('This query will require data from ',nrow(grid),' locations \n'))
    makeAPICalls <- readline("Do you wish to proceed? Type yes to begin API calls: ")

    if (tolower(makeAPICalls) != 'yes') {
      stop('User Input indicated they did not want to proceed with making API Calls \n')
    }
  }
}
