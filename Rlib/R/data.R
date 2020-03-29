#' Bus stops
#'
#' A dataset containing the bus stops in Dublin
#'
#' @format A large list with 10133 elements.
#' @keywords dataset
#'
"bus_stop_list"

#' Train stations (tidy)
#'
#' A dataset containing all train stations in Ireland
#'
#' @format A list with 167 elements.
#' @keywords dataset
#'
"tidy_train_stations"

#' Train stations
#'
#' A dataset containing all train stations in Ireland
#'
#' @keywords dataset
#' 
#' @format A list with 167 elements.
#' \describe{
#'   \item{StationDesc}{Full station name}
#'   \item{StationAlias}{Alternative name}
#'   \item{StationLatitude}{Latitude}
#'   \item{StationLongitude}{Longitude}
#'   \item{StationCode}{Station code}
#'   \item{StationId}{Station ID}
#'   }
#'
"train_stations"



#' Luas stops
#'
#' A dataset containing all luas stops
#'
#' @keywords dataset
#' 
#' @format A list with 167 elements.
#' \describe{
#'   \item{StopID}{Stop ID}
#'   \item{Name}{Name}
#'   \item{Abbreviation}{Stop name abbreviation}
#'   \item{LineID}{Line ID 1 = Red, 2 = Green}
#'   \item{SortOrder}{Station code}
#'   \item{IsEnabled}{}
#'   \item{IsParkAndRide}{}
#'   \item{IsCycleAndRide}{}
#'   \item{ZoneCountA}{}
#'   \item{ZoneCountB}{}
#'   \item{Latitude}{}
#'   \item{Longitude}{}
#'   \item{IrishName}{Name in Gaeilge}
#'   \item{Line}{Line name, red or green}
#'   }
#'
"luas_stops"
