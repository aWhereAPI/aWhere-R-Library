
#' @title Removes unnecessary columns from API Return
#'
#' @description
#' \code{removeUnnecessaryColumns} Removes unnecessary columns from API Return
#'
#' @param - data: data.table to have columns removed from
#' 
#' @import tidyr
#' @import data.table
#' @import magrittr

removeUnnecessaryColumns <- function(data, returnOnlySoilVars = FALSE) {

  varNames <- colnames(data)

  #This removes the non-data info returned with the JSON object
  suppressWarnings(data[,grep('_links',varNames) := NULL])
  suppressWarnings(data[,grep('.units',varNames) := NULL])
  suppressWarnings(data[,grep('latitude',varNames) := NULL])
  suppressWarnings(data[,grep('longitude',varNames) := NULL])
  suppressWarnings(data[,grep('fieldId',varNames) := NULL])
  
  if (returnOnlySoilVars == FALSE) {
    suppressWarnings(data[,c('soilTemperatures','soilMoisture') := NULL])
  } else{
    columnsToRemove <- colnames(data)
    columnsToRemove <- 
      columnsToRemove[!(columnsToRemove %in% c('startTime'
                                             ,'endTime'
                                             ,'soilTemperatures'
                                             ,'soilMoisture'))]
    
    data[,(columnsToRemove) := NULL]
    
    data <- 
      tidyr::unnest(data,col = c('soilTemperatures','soilMoisture'),names_sep = '_') %>%
      as.data.table(.)
    
    data[,c('soilMoisture_depth','soilTemperatures_units') := NULL]
    setnames(data,'soilTemperatures_depth','soilDepth')
  }

  return(data)
}



#' @title Removes unnecessary columns from API Return
#'
#' @description
#' \code{recalculateAccumulations} manually recalculate accumulations over API calls
#'
#' @param - dataList: list of data.table to have accumulations corrected

recalculateAccumulations <- function(dataList) {
  accumulatedColumns <- grep(pattern = 'accumulated'
                             ,x = unique(unlist(lapply(dataList,colnames)))
                             ,value = TRUE
                             ,fixed = TRUE)
  
  accumulatedColumns <- grep(pattern = '.units'
                             ,x = accumulatedColumns
                             ,invert = TRUE
                             ,value = TRUE
                             ,fixed = TRUE)
  #it is possible for certain columns to be returned from API because of all NULLs.  Need to remove those
  columnsToRemove <- c('accumulatedPet'
                       ,'accumulatedPrecipitation'
                       ,'pet')
  
  accumulatedColumns <- setdiff(accumulatedColumns
                                ,columnsToRemove)
  
  for (x in 1:length(dataList)) {
    #Remove those unneccesary columns
    suppressWarnings(dataList[[x]][,(columnsToRemove) := NULL])
    
    #if any accumulated columns should be present that aren't add them
    suppressWarnings(dataList[[x]][,(setdiff(accumulatedColumns
                                     ,colnames(dataList[[x]]))) := NA,])
    
  }
  
  
  for (x in 1:length(dataList)) {
    if (x > 1) {
      for (y in 1:length(accumulatedColumns)) {
        eval(parse(text = paste0('dataList[[x]][,',accumulatedColumns[y],' := ',accumulatedColumns[y],' + lastValue.accumulatedColumns$',accumulatedColumns[y],']')))
        #eval(parse(text = paste0('dataList[[x]][,',accumulatedColumns[y],' := sum(',accumulatedColumns[y],',lastValue.accumulatedColumns$',accumulatedColumns[y],')]')))
      }  
    }
    
    lastValue.accumulatedColumns <- dataList[[x]][.N,accumulatedColumns,with = FALSE]
    
    #keep NA's from propagating 
    #lastValue.accumulatedColumns[is.na(lastValue.accumulatedColumns)] <- 0
    
  }
  
  return(dataList)
}
