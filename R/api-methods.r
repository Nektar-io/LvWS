library(XML)
library(httr)

get_xml <- function(path, query) {
    url <- modify_url(url = .url,
                      path = file.path(.path, path),
                      query = query
    )
    x <- paste(readLines(url, warn = FALSE), collapse="")
    xmlParse(x)
}

# API methods

#' Get Street Adresses
#' 
#' ...
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
# x <- GetAddresses(streetName = "Birkagatan")

#' Get Street Names
#' 
#' ...
#' 
#' @param apiKey API key
#' @param streetNamePattern Street Name Pattern
#' @param optionalMunicipality Municipality (optional)
#' @param optionalPostalArea Postal Area (optional)
#' @param optionalPostalCode Postal Code (optional)
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
# x <- GetStreetNames(streetNamePattern = "B*")
