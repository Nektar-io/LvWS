\name{parking_places}
\alias{parking_places}
\title{Count parking places}
\usage{
  parking_places(foreskrift = "ptillaten",
    operation = file.path("street", streetName),
    apiKey = .api, streetName = NULL)
}
\description{
  See http://openstreetgs.stockholm.se/Home/Parking
}
\examples{
\dontrun{
# Number of parking places at Birkagatan
count_parking_places(streetName = "Birkagatan")

# Number of parking places
}
}

