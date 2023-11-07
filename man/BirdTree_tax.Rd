\name{BirdTree_tax}
\alias{BirdTree_tax}
\docType{data}
\title{
Bird Tree taxonomic data
}
\description{
Dataset from Jetz et al. (2012) detailing the BirdTree taxonomy used in the BirdTree backbone phylogenetic trees. 
}
\usage{data("BirdTree_tax")}
\format{
  A data frame with 9993 observations on the following 11 variables.
  \describe{
    \item{\code{Genus}}{a character vector detailing the Bird Tree genus}
    \item{\code{Species}}{a character vector detailing the Bird Tree species name}
    \item{\code{TipLabel}}{a character vector detailing the tip label (genus and species name separated by an underscore)}
    \item{\code{English}}{a character vector detailing the common name for the species}
    \item{\code{Taxonomy}}{a character vector detailing the Bird Tree taxonomy}
    \item{\code{Clade}}{a character vector detailing the Bird Tree clade}
    \item{\code{BLFamilyLatin}}{a character vector detailing the Bird Tree family name (latin)}
    \item{\code{BLFamilyEnglish}}{a character vector detailing the Bird Tree family name (common)}
    \item{\code{FamSequID}}{a numeric vector}
    \item{\code{Order}}{a character vector detailing the Bird Tree order}
    \item{\code{OscSubOsc}}{a character vector}
  }
}
\details{
%%  ~~ If necessary, more details than the __description__ above ~~
}
\source{
Jetz, W., Thomas, G., Joy, J. et al. The global diversity of birds in space and time. Nature 491, 444–448 (2012). https://doi.org/10.1038/nature11631
}
\references{
Jetz, W., Thomas, G., Joy, J. et al. The global diversity of birds in space and time. Nature 491, 444–448 (2012). https://doi.org/10.1038/nature11631
}
\examples{
data(BirdTree_tax)
}
\keyword{datasets}
