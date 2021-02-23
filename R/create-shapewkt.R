#' @title create_shapewkt
#'
#' @description
#' \code{create_shapewkt} writes the WKT for a grid cell, based on a centroid coordinate
#'
#' @param longitude longitude coordinate for centroid
#' @param latitude latitude coordinate for centroid
#'
#' @export
create_shapewkt <- function(longitude, latitude) {

  upperLeft <- c(longitude - (5/60), latitude + (5/60))
  lowerLeft <- c(longitude - (5/60), latitude - (5/60))
  lowerRight <- c(longitude + (5/60), latitude - (5/60))
  upperRight <- c(longitude + (5/60), latitude + (5/60))

  coordstring <- paste(paste(upperLeft, collapse = " "), paste(lowerLeft, collapse = " "),
                       paste(lowerRight, collapse = " "), paste(upperRight, collapse = " "),
                       paste(upperLeft, collapse = " "), sep = ",")


  shapewkt <- paste0("POLYGON((", coordstring, "))")

  return(shapewkt)
}


#' @title custom_fortify
#'
#' @description
#' \code{custom_fortify} breaks up a shapewkt string into a numeric vector containing the points in the WKT
#'
#' @param shapewkt a well-known text string for a grid cell polygon
#'
#' @export
custom_fortify <- function(shapewkt) {
  shapewkt <- gsub("POLYGON\\(\\(|\\)\\)", "", shapewkt)
  shapewkt <- gsub(", ", ",", shapewkt)
  return(as.numeric(unlist(strsplit(unlist(strsplit(shapewkt, ",")), " "))))
}
