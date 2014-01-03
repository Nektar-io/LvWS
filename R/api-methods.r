library(XML)
library(httr)

#' Get XML
#' 
#' Get XML data from the API.
#' 
#' @param path URL path
#' @param query URL query
get_xml <- function(path, query) {
    url <- modify_url(
        url = .url,
        path = file.path(.path, path),
        query = query
    )
    x <- paste(readLines(url, warn = FALSE), collapse="")
    xmlParse(x)
}

#' Get Street Adresses
#' 
#' Get street adresses from the LvWS API.
#' Returns street numbers, postal codes, coordinates, etc.
#' 
#' @examples
#' \dontrun{
#' GetAddresses(streetName = "Birkagatan")
#' }
#' 
#' @export
GetAddresses <- function(
    apiKey = .api,
    municipalityPattern = "",
    streetName = "",
    streetNumPattern = "",
    postalCodePattern = "",
    postalAreaPattern = "",
    includeAddressConnectionsForTrafficTypes = "0"
) {
    x <- get_xml(
        path = "GetAddresses",
        query = list(
            apiKey = apiKey,
            municipalityPattern = municipalityPattern,
            streetName = streetName,
            streetNumPattern = streetNumPattern,
            postalCodePattern = postalCodePattern,
            postalAreaPattern = postalAreaPattern,
            includeAddressConnectionsForTrafficTypes = includeAddressConnectionsForTrafficTypes
        )
    )
    xmlToDataFrame(x)
}

#' Get Street Names
#' 
#' Get street names from the LvWS API.
#' It's also possible to provide wildcards.
#' 
#' @param apiKey API key
#' @param streetNamePattern Street Name Pattern
#' @param optionalMunicipality Municipality (optional)
#' @param optionalPostalArea Postal Area (optional)
#' @param optionalPostalCode Postal Code (optional)
#' 
#' @examples
#' \dontrun{
#' GetStreetNames(streetNamePattern = "B*")
#' }
#' 
#' @export
GetStreetNames <- function(
    apiKey = .api,
    streetNamePattern = "",
    optionalMunicipality = "",
    optionalPostalArea = "",
    optionalPostalCode = ""
) {
    x <- get_xml(
        path = "GetStreetNames",
        query = list(
            apiKey = apiKey,
            streetNamePattern = streetNamePattern,
            optionalMunicipality = optionalMunicipality,
            optionalPostalArea = optionalPostalArea,
            optionalPostalCode = optionalPostalCode
        )
    )
    
    x <- xmlToList(x, simplify = TRUE)
    x <- x[rownames(x) %in% "StreetName"]
    unlist(x)
}
