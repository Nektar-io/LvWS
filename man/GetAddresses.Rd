\name{GetAddresses}
\alias{GetAddresses}
\title{Get Street Adresses}
\usage{
  GetAddresses(apiKey = .api, municipalityPattern = "",
    streetName = "", streetNumPattern = "",
    postalCodePattern = "", postalAreaPattern = "",
    includeAddressConnectionsForTrafficTypes = "0")
}
\description{
  Get street adresses from the LvWS API. Returns street
  numbers, postal codes, coordinates, etc.
}
\examples{
\dontrun{
GetAddresses(streetName = "Birkagatan")
}
}

