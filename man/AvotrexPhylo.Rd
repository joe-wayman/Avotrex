\name{AvotrexPhylo}
\alias{AvotrexPhylo}
\docType{data}
\title{
Avotrex phylo dataset 
}
\description{
The dataset provides instructions to graft extinct bird species from Sayol et al. (IN PREP) on to existing BirdTree phylogenies using the AvoPhylo function from the Avotrex package. Instructions are provided in various columns 
(see format). 
}
\usage{data("AvotrexPhylo")}
\format{
  A data frame with 610 rows and the following variables:
  \describe{
    \item{\code{sp_id}}{a character vector detailing the order in which species are joined}
    \item{\code{phylo_id}}{a character vector detailing the order in which species are joined and whether a species is part of a group (see details)}
    \item{\code{per_fixed}}{a logical vector}
    \item{\code{time_fixed}}{a numeric vector detailing a specific time point for grafting or NA}
    \item{\code{group}}{a numeric vector detailing a group code or NA (see details)}
    \item{\code{species}}{a character vector detailing species name with an underscore}
    \item{\code{order}}{a character vector detailing species order}
    \item{\code{family}}{a character vector detailing species family}
    \item{\code{genus}}{a character vector detailing species genus}
    \item{\code{jetz_order}}{a character vector detailing species order within the Jetz phylogeny}
    \item{\code{jetz_family}}{a character vector detailing species family within the Jetz phylogeny}
    \item{\code{jetz_genus}}{a character vector detailing species genus within the Jetz phylogeny}
    \item{\code{type}}{a character vector of codes. The codes determine how and where the species is grafted to the tree. See details and function "AvoPhylo"}
    \item{\code{sister_order}}{a character vector. Provides the order within the BirdTree backbone tree to join the extinct species to, or randomly within, depending on the code within the column "type"}
    \item{\code{sister_clade}}{a character vector. Provides the clade within the BirdTree backbone tree to join the extinct species to, or randomly within, depending on the code within the column "type"}
    \item{\code{sister_family}}{a character vector. Provides the family within the BirdTree backbone tree to join the extinct species to, or randomly within, depending on the code within the column "type"}
    \item{\code{sister_genus}}{a character vector. Provides the genus within the BirdTree backbone tree to join the extinct species to, or randomly within, depending on the code within the column "type"}
    \item{\code{sister_species}}{a character vector. Provides the species within the BirdTree backbone tree to join the extinct species to depending on the code within the column "type"}
    \item{\code{sister_species_group}}{a character vector. Provides the group of species within the BirdTree backbone tree to join the extinct species to, or randomly within, depending on the code within the column "type"}
  }
}
\details{
As certain species need to be grafted onto the tree before
other species, a number of the species are grafted in a set
order. This ordering is controlled through the “sp_id”,
“phylo_id” and “group” columns in the extinct species
phylogeny database. Before grafting, the database is ordered
by the “sp_id” column, with the “phylo_id” and “group” columns
used to filter out particular groups of species (i.e., those
classified as "xS" in the "phylo_id" column) to be grafted in
different orders (i.e., either a randomised order within
groups, or a fixed order within groups). See the package
vignette for
further details. 

Some species are grafted at specific time points along a branch (column "time_fixed").

As some of the codes within the column "type" (see table below) randomly place the given species within a group of species, a genus, or a family, and some species groups are randomised before grafting (see above), it is useful to run the grafting proceedure over a a number of trees to average out the randomisation (see function "AvoPhylo"). 

See the package vignette, and the help file for AvoPhylo() for more information.

}
\references{
Matthews et al. (IN REVIEW) The global loss of avian functional and phylogenetic diversity from extinctions in the Holocene and Late Pleistocene.

Sayol et al. (IN PREP) The global loss of avian functional and phylogenetic diversity from extinctions in the Holocene and Late Pleistocene
}
\examples{
data(AvotrexPhylo)
}
\keyword{datasets}
