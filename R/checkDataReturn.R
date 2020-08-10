#' @title checkDataReturn_daily
#'
#' @description
#' \code{checkDataReturn_daily} Checks to see if data return is complete from API endpoints with daily data
#'
#' @param - dataset: the data.frame to check
#' @param - day_start: character string of the first day for which data was retrieved, in the form: YYYY-MM-DD
#' @param - day_end: character string of the last day for which data was retrieved, in the form: YYYY-MM-DD

checkDataReturn_daily <- function(dataset,day_start,day_end) {
  if (nrow(dataset) != round(difftime(day_end,day_start,units = 'days') +1L)) {
    warning('Incorrect number of rows returned from API call; check returned data to determine issue',immediate. = TRUE)
  }

  columnNames <- colnames(dataset)
  rowsComplete <- complete.cases(dataset)

  if(all(rowsComplete == TRUE) == FALSE) {
    warning(paste0('Missing data from rows ',paste0(which(rowsComplete == FALSE),collapse = ', '),'; check data before continuing'),immediate. = TRUE)
  }
}

#' @title checkDataReturn_norms
#'
#' @description
#' \code{checkDataReturn_norms} Checks to see if data return is complete from norms API
#'
#' @param - dataset: the data.frame to check
#' @param - monthday_start: character string of the first month and day for which you want to retrieve data,
#'                          in the form: MM-DD.  This is the start of your date range. e.g. '07-01' (July 1)
#' @param - monthday_end: character string of the last month and day for which you want to retrieve data,
#'                          in the form: MM-DD.  This is the end of your date range. e.g. '07-01' (July 1)
#' @param - year_start: character string of the starting year (inclusive) of the range of years for which
#'                     you're calculating norms, in the form YYYY. e.g., 2008
#' @param - year_end: character string of the last year (inclusive) of the range of years for which
#'                     you're calculating norms, in the form YYYY. e.g., 2015
#' @param - exclude_year: Year or years which you'd like to exclude from
#'                        your range of years on which to calculate norms. To exclude
#'                        multiple years, provide a vector of years. You must include
#'                       at least three years of data with which to calculate the norms.
#' @param - includeFeb29thData: Whether to keep data from Feb 29th on leap years.

checkDataReturn_norms <- function(dataset,monthday_start,monthday_end,year_start,year_end,exclude_year,includeFeb29thData) {
  yearsToTest <- seq(year_start,year_end,1)

  if(!is.null(exclude_year)) {
    yearsToTest <- yearsToTest[!(yearsToTest %in% exclude_year)]
  }

  maxNumDays <- 0

  for (x in 1:length(yearsToTest)) {
    currentYear <- yearsToTest[x]
    currentMonthDay_start <- paste0(currentYear,'-',monthday_start)
    currentMonthDay_end   <- paste0(currentYear,'-',monthday_end)
    
    currentNumDays <- tryCatch({round(difftime(currentMonthDay_end
                                     ,currentMonthDay_start
                                     ,units = 'days') +1L)
    
    
    }, error = function(e) {
      return(e)
    })

    if (any(class(currentNumDays) == 'simpleError')) {
      next
    }
    
    if (includeFeb29thData == FALSE) {
      if (is.leapyear(currentYear) == TRUE) {
        if (currentMonthDay_start <= paste0(currentYear,'-02-29')) {
          if (currentMonthDay_end >= paste0(currentYear,'-02-29')) {
            currentNumDays <- currentNumDays - 1L
          }
        }
      }
    }

    if (currentNumDays > maxNumDays) {
      maxNumDays <- currentNumDays
    }
  }

  if (nrow(dataset) != maxNumDays) {
    warning('Incorrect number of rows returned from API call; check returned data to determine issue',immediate. = TRUE)
  }

  columnNames <- colnames(dataset)
  rowsComplete <- complete.cases(dataset)

  if(all(rowsComplete == TRUE) == FALSE) {
    warning(paste0('Missing data from rows ',paste0(which(rowsComplete == FALSE),collapse = ', '),'; check data before continuing'),immediate. = TRUE)
  }
}

#' @title checkDataReturn_forecasts
#'
#' @description
#' \code{checkDataReturn_forecasts} Checks to see if data return is complete from forecasts API endpoint
#'
#' @param - dataset: the data.frame to check
#' @param - day_start: character string of the first day for which data was retrieved, in the form: YYYY-MM-DD
#' @param - day_end: character string of the last day for which data was retrieved, in the form: YYYY-MM-DD
#' @param - block_size: Integer value that corresponds to the number of hours to include in each time block.
#'                     Defaults to a 1 hour block.  This value must divide evenly into 24.

checkDataReturn_forecasts <- function(dataset,day_start,day_end,block_size) {
  if (day_end != '') {
    if (nrow(dataset) != round((difftime(day_end,day_start,units = 'days') +1L) * (24 / block_size))) {
      warning('Incorrect number of rows returned from API call; check returned data to determine issue',immediate. = TRUE)
    }
  } else {
    if (nrow(dataset) != round((difftime(day_start,day_start,units = 'days') +1L) * (24 / block_size))) {
      warning('Incorrect number of rows returned from API call; check returned data to determine issue',immediate. = TRUE)
    }
  }
  varNames <- colnames(dataset)
  #The API will always return NA for these columns
  if (block_size == 1) {
    varNames <- grep('relativeHumidity.m',varNames,value = TRUE,invert = TRUE)
    varNames <- grep('wind.m',varNames,value = TRUE,invert = TRUE)
  }
  rowsComplete <- complete.cases(dataset[,varNames,with = FALSE])
  if(all(rowsComplete == TRUE) == FALSE) {
    warning(paste0('Missing data from rows ',paste0(which(rowsComplete == FALSE),collapse = ', '),'; check data before continuing'),immediate. = TRUE)
  }
}
