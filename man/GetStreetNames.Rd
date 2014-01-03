\name{GetStreetNames}
\alias{GetStreetNames}
\title{Get Street Names}
\usage{
  GetStreetNames(apiKey = .api, streetNamePattern = "",
    optionalMunicipality = "", optionalPostalArea = "",
    optionalPostalCode = "")
}
\arguments{
  \item{apiKey}{API key}

  \item{streetNamePattern}{Street Name Pattern}

  \item{optionalMunicipality}{Municipality (optional)}

  \item{optionalPostalArea}{Postal Area (optional)}

  \item{optionalPostalCode}{Postal Code (optional)}
}
\description{
  Get street names from the LvWS API. It's also possible to
  provide wildcards.
}
\examples{
\dontrun{
GetStreetNames(streetNamePattern = "B*")
}
}

