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

#' Get parking places
#' 
#' TODO: add support for lon, lat instead of street!
#' See http://openstreetgs.stockholm.se/Home/Parking
#' 
#' @examples
#' \dontrun{
#' # Get parking places at Birkagatan
#' get_parking_places(streetName = "Birkagatan")
#' }
#' @export
get_parking_places <- function(
    foreskrift = "ptillaten",
    operation = file.path("street", streetName),
    apiKey = .api,
    streetName = NULL
) {
    if (is.null(operation)) stop("Please provide streetName or operation")
    
    url <- modify_url(
        url = "http://openparking.stockholm.se",
        path = sprintf("LTF-Tolken/v1/%s/%s", foreskrift, operation),
        query = list(
            apiKey = apiKey,
            outputFormat = "json"
        )
    )
    x <- paste0(readLines(url, warn = FALSE))
    x <- fromJSON(x)
    x <- list_to_table(x$features)
    
    # Remove geometry columns (to clean up result table a bit)
    cols <- colnames(x)[grep("geometry.coordinates", colnames(x))]
    new_col <- split(x[, cols], 1:nrow(x))
    x <- x[, -grep("geometry.coordinates", colnames(x))]
    x$geometry.coordinates <- new_col  # store them as a list
    
    return(x)
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
    x <- get_xml(path = "GetAddresses", query = as.list(environment()))
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
    x <- get_xml(path = "GetStreetNames", query = as.list(environment()))
    x <- xmlToList(x, simplify = TRUE)
    x <- x[rownames(x) %in% "StreetName"]
    unlist(x)
}

#' Convert list to table
#' 
#' Flattens out a nested list 
#' 
#' @param x list
list_to_table <- function(x) {
    do.call("rbind.fill",
        lapply(x, function(y) {
            data.frame(t(unlist(y)))
        })
    )
}
