#' Retrieve GEOID from the Census Geocoder by address
#'
#' Returns GEOID for 2020 geographies.
#'
#' @param address A tibble/data frame with (at a minimum, others can be present)
#'   either character columns street, city, and state OR numeric columns lat and
#'   lon. Lat/lon columns take priority.
#' @param geoid_type GEOID level to return, \code{c('co', 'tr', 'bg', 'bl')}.
#'   Defaults to block.
#' @return the original tibble with GEOIDs appended as a new column called
#'   \code{geoid}.
#'
#' @examples
#' \dontrun{
#' airports <- dplyr::data_frame(
#'   street = "700 Catalina Dr", city = "Daytona Beach", state = "FL"
#' )
#' append_geoid(airports, 'tr')
#' }
#'
#' @details
#' It is possible in some situations for the geolocator API to return a 500 error.
#' Repeating the call should avoid this.
#'
#' @importFrom dplyr mutate
#' @export
append_geoid <- function(address, geoid_type = 'bl') {

  if ("lat" %in% colnames(address) && "lon" %in% colnames(address)) {
    # Call for each row of the data
    geoids <- vector(mode="character", length = nrow(address))
    for (i in 1:nrow(address)) {
      geoids[i] <- call_geolocator_latlon(address$lat[i], address$lon[i])
    }
  } else {
    # If street and zip columns are factors, convert them
    # Call for each row of the data
    geoids <- vector(mode="character", length = nrow(address))
    for (i in 1:nrow(address)) {
      geoids[i] <- call_geolocator(
        as.character(address$street[i]),
        as.character(address$zip[i])
      )
  }

  # Append onto database
  address <- dplyr::mutate(address, geoid = geoids)

  # AABBBCCCCCCDEEE
  if (geoid_type == 'co') {
    end <- 5
  } else if (geoid_type == 'tr') {
    end <- 11
  } else if (geoid_type == 'bg') {
    end <- 12
  } else {
    end <- 15
  }
  address <- dplyr::mutate(address,
                           geoid = ifelse(is.na(geoid), NA_character_, substr(geoid, 1, end)))

  return(address)
}


#' Call gelocator for one address
#'
#' @param street A character string indicating a street name and number
#' @param zip A 5 digit character string as state code
#'
#' @return A character string representing the Census block of the supplied
#'   address.
#'
#' importFrom utils URLencode
#' importFrom httr GET stop_for_status
#'
#' @export
#'
call_geolocator <- function(street, city, state) {
  # Build url
  call_start <- "https://geocoding.geo.census.gov/geocoder/geographies/address?"

  url <- paste0(
    "street=", utils::URLencode(street),
    "&zip=", utils::URLencode(zip)
  )

  call_end <- "&benchmark=Public_AR_Census2020&vintage=Census2020_Census2020&layers=10&format=json"

  url_full <- paste0(call_start, url, call_end)

  # Check response
  r <- httr::GET(url_full)
  httr::stop_for_status(r)
  response <- httr::content(r)
  if (length(response$result$addressMatches) == 0) {
    message(paste0("Address (",
                   street, " ", zip,
                   ") returned no address matches. An NA was returned."))
    return(NA_character_)
  } else {
    if (length(response$result$addressMatches) > 1) {
      message(paste0("Address (",
                     street, " ", zip, 
                     ") returned more than one address match. The first match was returned."))
    }
    return(response$result$addressMatches[[1]]$geographies$`Census Blocks`[[1]]$GEOID)
  }
}


#' Call gelocator for one address with lat/lon
#'
#' @param lat A numeric value
#' @param lon A numeric value
#'
#' @return A character string representing the Census block of the supplied
#'   lat/lon.
#'
#' importFrom utils URLencode
#' importFrom httr GET stop_for_status
#'
#' @export
#'
call_geolocator_latlon <- function(lat, lon) {
  # Build url
  call_start <- "https://geocoding.geo.census.gov/geocoder/geographies/coordinates?"

  url <- paste0(
    "x=", lon,
    "&y=", lat
  )

  call_end <- "&benchmark=Public_AR_Census2020&vintage=Census2020_Census2020&layers=10&format=json"

  url_full <- paste0(call_start, url, call_end)

  # Check response
  r <- httr::GET(url_full)
  httr::stop_for_status(r)
  response <- httr::content(r)
  if (length(response$result$geographies$`Census Blocks`) == 0) {
    message(paste0("Lat/lon (", lat, ", ", lon,
                   ") returned no geocodes. An NA was returned."))
    return(NA_character_)
  } else {
    if (length(response$result$geographies$`Census Blocks`) > 1) {
      message(paste0("Lat/lon (", lat, ", ", lon,
                     ") returned more than geocode. The first match was returned."))
    }
    return(response$result$geographies$`Census Blocks`[[1]]$GEOID)
  }
}
}
