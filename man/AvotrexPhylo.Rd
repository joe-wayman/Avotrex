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
  A data frame with 605 observations on the following * variables.
  \describe{
    \item{\code{Id_sps}}{a character vector detailing the order in which species are joined}
    \item{\code{phylo_id2}}{a character vector detailing the order in which species are joined and whether a species is part of a group (see details)}
    \item{\code{per_fixed}}{a character vector}
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
be grafted onto the tree before other species. Some species are assigned to groups within the data (column "Group"). These species are assigned a code "xS" within the column "phylo_id2". These species groups consist of close relatives, whose exact taxonomic relationships are unknown. Therefore, the order in which they are joined is randomised before they are grafted on to the tree. 

As some of the codes within the column "Type" (see table below) randomly place the given species within a group of species, a genus, or a family, and some species groups are randomised before grafting (see above), it is useful to run the grafting proceedure over a a number of trees to average out the randomisation (see function "AvoPhylo"). 

Codes | Full name                   | Definition                                                                          |
--------------------------------------------------------------------------------------------------------------------------|
S     | Sister                      | Grafted as a sister to a known extant or extinct species already in the tree        |
--------------------------------------------------------------------------------------------------------------------------|
SSG   | Sister species group        | Grafted as a sister to a group of extant and/or extinct species already in the tree |
--------------------------------------------------------------------------------------------------------------------------|
SGG   | Sister genus group          | Grafted as a sister to an entire extant or extinct genus (i.e., for the first       |
      |                             | grafted representative of an extinct genus)                                         |
--------------------------------------------------------------------------------------------------------------------------|      
SGG2  | Sister genus group 2        | Grafted as sister to multiple genera. This was for when a species was sister to a   |
      |                             | subfamily or some other large specific clade                                        |
--------------------------------------------------------------------------------------------------------------------------|    
SFG   | Sister family group         | Grafted as a sister to an entire extant or extinct family already present in the    |
      |                             | tree (i.e., for the first grafted representative of an extinct family)              |
--------------------------------------------------------------------------------------------------------------------------|     
SOG   | Sister order group          | Grafted as a sister to an entire order already present in the tree (i.e., for the   |
      |                             | first grafted representative of an extinct order)                                   |
--------------------------------------------------------------------------------------------------------------------------|     
RSG   | Random species group        | Grafted to a randomly selected species from a pre-defined group of species (i.e.,   | 
      |                             | from which is believed to have close affinities                                     |
--------------------------------------------------------------------------------------------------------------------------|     
RGG   | Random genus group          | Grafted to a randomly selected species from a given genus. For example, if an       |
      |                             | extinct species was believed to be a finch derived from a European finch species,   |
      |                             | but the exact sister species is unknown.                                            | 
--------------------------------------------------------------------------------------------------------------------------|                                     
RGG2  | Random genus group 2        | Grafted to a randomly selected species from a group of genera (e.g. when all that is| 
      |                             | known is that the species is from a specific subfamily). Currently not used in the  |
      |                             | database, but the relevant functionality has been kept in the R script, as it could |
      |                             | be useful for future studies                                                        | 
--------------------------------------------------------------------------------------------------------------------------|                       
RFG   | Random family group         | Grafted to a randomly selected species from a given family                          |  
--------------------------------------------------------------------------------------------------------------------------|                                                                                                
RSGG  | Random sister genus group   | Grafted as sister to a randomly selected genus from a pre-defined group of genera   | 
--------------------------------------------------------------------------------------------------------------------------|                                                                                                   
RSGG2 | Random sister genus group 2 | Grafted as sister to a randomly selected genus from a pre-defined family            |                                                                                                   
--------------------------------------------------------------------------------------------------------------------------|

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
