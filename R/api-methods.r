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

    # Replace silly percent encoding with other silly percent encoding
    for(i in 1:6) {
        url <- str_replace_all(
            url, 
            c("%C3%A5","%C3%A4","%C3%B6","%C3%85","%C3%84","%C3%96")[i],
            c("%E5","%E4","%F6","%C5","%C4","%D6")[i]
        )
    }
    
    cat(url,"\n")
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
    xmlToDataFrame(x, stringsAsFactors = FALSE)
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
    do.call(
        "rbind.fill",
        lapply(x, function(y) {
            data.frame(t(unlist(y)))
        })
    )
}

#' Get coords in different coordinate systems
#' 
#' Get coords in different coordinate systems from WKT (e.g. RT90 2.5 from WGS 84)
#' 
#' @param WKT WKT point from GetAddresses()
#' @export

GetCoords <- function(
    WKT,
    apiKey = .api,
    targetCoordSrid = 3021
) {
    # The "WKT" string contains the point coordinates of an address
    strings <- str_extract_all(WKT, "[0-9\\.]*")[[1]]
    strings <- strings[str_length(strings) > 0]
    WKTx <- strings[1]
    WKTy <- strings[2]
    
    coords <- get_xml(path = "TransformGeometry", query = list(
        apikey = apiKey,
        wkt = paste("POINT (", WKTx, " ", WKTy, ")", sep=""),
        fromSrid = 4326,
        toSrid = targetCoordSrid
    ))
    
    
    return_string <- unlist(xpathApply(coords, "//ns:string/text()", xmlValue, namespaces="ns"))
    return_string <- str_extract_all(return_string, "[0-9\\.]*")[[1]]
    return_string <- return_string[str_length(return_string) > 0]
    
    coords <- list(
        RT90 = as.double(return_string[c(2,1)]),
        WGS84 = as.double(strings[c(2,1)])
    )
    
    # We change the order of the values since LvWS returns the values in 
    # [easting, northing] order, which is the opposite of what it should be.
    message("Coordinates generated on [northing, easting] form")
    return(coords)
}