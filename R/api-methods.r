library(XML)
library(httr)

# General methods
fetch_data <- function(path, query) {
    url <- modify_url(url = .url,
                      path = file.path(.path, path),
                      query = query
    )
    get_xml(url)
}

get_xml <- function(url) {
    xml <- paste(readLines(url, warn = FALSE), collapse="")
    xml <- xmlParse(xml)
    xmlToList(xml)
}

# API methods
GetStreetNames <- function(
    apiKey = .api,
    streetNamePattern = "",
    optionalMunicipality = "",
    optionalPostalArea = "",
    optionalPostalCode = ""
) {
    fetch_data(
        path = "GetStreetNames",
        query = list(
            apiKey = apiKey,
            streetNamePattern = streetNamePattern,
            optionalMunicipality = optionalMunicipality,
            optionalPostalArea = optionalPostalArea,
            optionalPostalCode = optionalPostalCode
        )
    )
}
