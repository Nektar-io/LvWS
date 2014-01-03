\name{get_parking_places}
\alias{get_parking_places}
\title{Get parking places}
\usage{
  get_parking_places(foreskrift = "ptillaten",
    operation = file.path("street", streetName),
    apiKey = .api, streetName = NULL)
}
\description{
  TODO: add support for lon, lat instead of street! See
  http://openstreetgs.stockholm.se/Home/Parking
}
\examples{
\dontrun{
# Get parking places at Birkagatan
get_parking_places(streetName = "Birkagatan")
}
}

