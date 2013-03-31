library(XML)
library(httr)

# General methods
fetch_data <- function(path, query, nodes) {
    url <- modify_url(url = .url,
                      path = file.path(.path, path),
                      query = query
    )
    get_xml(url, nodes)
}

get_xml <- function(url, nodes) {
    x <- paste(readLines(url, warn = FALSE), collapse="")
    x <- xmlParse(x)
    x <- xmlToList(x, simplify = TRUE)
    x <- x[rownames(x) %in% nodes]
    unlist(x)
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
        ),
        nodes = "StreetName"
    )
}
