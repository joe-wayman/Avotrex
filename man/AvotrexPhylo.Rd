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
  A data frame with 605 rows and the following variables:
  \describe{
    \item{\code{Id_sps}}{a character vector detailing the order in which species are joined}
    \item{\code{phylo_id2}}{a character vector detailing the order in which species are joined and whether a species is part of a group (see details)}
    \item{\code{per_fixed}}{a logical vector}
    \item{\code{time_fixed}}{a numeric vector detailing a specific time point for grafting or NA}
    \item{\code{Group}}{a numeric vector detailing a group code or NA (see details)}
    \item{\code{species}}{a character vector detailing species name with an underscore}
    \item{\code{Order}}{a character vector detailing species order}
    \item{\code{Family}}{a character vector detailing species family}
    \item{\code{Genus}}{a character vector detailing species genus}
    \item{\code{Jetz_Order}}{a character vector detailing species order within the Jetz phylogeny}
    \item{\code{Jetz_Family}}{a character vector detailing species family within the Jetz phylogeny}
    \item{\code{Jetz_Genus}}{a character vector detailing species genus within the Jetz phylogeny}
    \item{\code{Type}}{a character vector of codes. The codes determine how and where the species is grafted to the tree. See details and function "AvoPhylo"}
    \item{\code{Sister_order}}{a character vector. Provides the order within the BirdTree backbone tree to join the extinct species to, or randomly within, depending on the code within the column "Type"}
    \item{\code{Sister_clade}}{a character vector. Provides the clade within the BirdTree backbone tree to join the extinct species to, or randomly within, depending on the code within the column "Type"}
    \item{\code{Sister_family}}{a character vector. Provides the family within the BirdTree backbone tree to join the extinct species to, or randomly within, depending on the code within the column "Type"}
    \item{\code{Sister_genus}}{a character vector. Provides the genus within the BirdTree backbone tree to join the extinct species to, or randomly within, depending on the code within the column "Type"}
    \item{\code{Sister_species}}{a character vector. Provides the species within the BirdTree backbone tree to join the extinct species to depending on the code within the column "Type"}
    \item{\code{Sister_species_group}}{a character vector. Provides the group of species within the BirdTree backbone tree to join the extinct species to, or randomly within, depending on the code within the column "Type"}
  }
}
\details{
The species are grafted onto the tree in a set order provided in the column " phylo_id", as certain species need to
be grafted onto the tree before other species. Some species are assigned to groups within the data (column "Group"). These species are assigned a code "xS" within the column "phylo_id2". These species groups consist of close relatives, whose exact taxonomic relationships are unknown. Therefore, the order in which they are joined is randomised before they are grafted on to the tree. Some species are grafted at specific time points along a branch (column "time_fixed").

As some of the codes within the column "Type" (see table below) randomly place the given species within a group of species, a genus, or a family, and some species groups are randomised before grafting (see above), it is useful to run the grafting proceedure over a a number of trees to average out the randomisation (see function "AvoPhylo"). 

See the package vignette, and the help file for AvoPhylo() for more information.

}
\source{
Matthews et al. (IN REVIEW) The global loss of avian functional and phylogenetic diversity from extinctions in the Holocene and Late Pleistocene
Sayol et al. (IN PREP) The global loss of avian functional and phylogenetic diversity from extinctions in the Holocene and Late Pleistocene
}
\references{
Matthews et al. (IN REVIEW) The global loss of avian functional and phylogenetic diversity from extinctions in the Holocene and Late Pleistocene
Sayol et al. (IN PREP) The global loss of avian functional and phylogenetic diversity from extinctions in the Holocene and Late Pleistocene
}
\examples{
data(AvotrexPhylo)
}
\keyword{datasets}
