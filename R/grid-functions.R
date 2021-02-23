#' Get aWhere GridX from longitude coordinate(s)
#'
#'
#' @param longitude one more more longitude coordinates
#'
#' @return A vector of GridX values
#'
#' @examples getGridX(-90)
#'
getGridX <- function(longitude) {

  MaxLon <- 180
  MinLon <- -180

  MaxGridX <- 2160
  MinGridX <- -2160

  gridX <- (longitude / ((MaxLon - MinLon) / 2)) * ((MaxGridX - MinGridX) / 2)

  gridX[which(gridX > 0)] <- ceiling(gridX[which(gridX > 0)])
  gridX[which(gridX < 0)] <- floor(gridX[which(gridX < 0)])

  return (gridX)
}


#' Get aWhere GridY from latitude coordinate(s)
#'
#'
#' @param latitude one or more latitudes coordinates
#'
#' @return A vector of GridY values
#'
#' @examples getGridY(-90)
#'
getGridY <- function(latitude) {

  MaxLat <- 90
  MinLat <- -90

  MaxGridY <- 1080
  MinGridY <- -1080

  gridY <- (latitude / ((MaxLat - MinLat) / 2)) * ((MaxGridY - MinGridY) / 2)

  gridY[which(gridY > 0)] <- ceiling(gridY[which(gridY > 0)])
  gridY[which(gridY < 0)] <- floor(gridY[which(gridY < 0)])

  return (gridY)
}

getLongitude <- function(gridX) {

  MaxLon = 180
  MinLon = -180

  MaxGridX = 2160
  MinGridX = -2160

  differenceLon <- (MaxLon - MinLon)/2

  gridX[which(gridX < 0)] <- ((differenceLon/MaxGridX) * gridX[which(gridX < 0)]) + (differenceLon/(MaxGridX*2))

  gridX[which(gridX > 0)] <- ((differenceLon/MaxGridX) * gridX[which(gridX > 0)]) - (differenceLon/(MaxGridX*2))

  return (gridX)
}

getLatitude <- function(gridY) {

  MaxLat = 90
  MinLat = -90

  MaxGridY = 1080
  MinGridY = -1080

  differenceLat <- (MaxLat - MinLat)/2
  
  gridY[which(gridY < 0)] <- ((differenceLat/MaxGridY) * gridY[which(gridY < 0)]) + (differenceLat/(MaxGridY*2))

  gridY[which(gridY > 0)] <- ((differenceLat/MaxGridY) * gridY[which(gridY > 0)]) - (differenceLat/(MaxGridY*2))

  return (gridY)
}
