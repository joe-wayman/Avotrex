## ----setup, include=FALSE-----------------------------------------------------
knitr::opts_chunk$set(
  out.width = "100%"
)
options(cli.unicode = F)

## ----Tree building example, eval=FALSE----------------------------------------
#  
#  # Load package
#  library(avotrex)
#  
#  ## An example of tree building
#  data(BirdTree_trees)
#  data(BirdTree_tax)
#  data(AvotrexPhylo)
#  trees <- AvoPhylo(ctrees = BirdTree_trees,   # BirdTree tree(s)
#                    avotrex = AvotrexPhylo,    # The extinct species phylogeny database
#                    PER = 0.2,                 # Perecentage/fraction for branch truncation
#                    PER_FIXED = 0.75,          # Point along the branch to graft the species
#                    tax = BirdTree_tax,        # Taxonomy
#                    Ntree = 2,                 # Number of trees
#                    n.cores = 2)               # Number of cores
#  

## ----Adding a new species, fig.height=6, fig.width=6, message=FALSE, warning=FALSE----

library(avotrex)
#Create a new species to add to the main database; not the
#values in the vector match the columns in AvotrexPhylo
vaporhams <- c("x644", "x644", "FALSE", NA, "Vapor_hams",
               "Strigiformes", "Strigidae", "Athene",
               "STRIGIFORMES", "Strigidae", "Athene",
               "S", NA, NA, NA, "Athene", "noctua", NA)

#Just for speed, filter the main database just to include owls.
#However, note that in this case, the 12 'AP' species are not
#removed completely as they are already in BirdTree.
AvotrexPhylo2 <- AvotrexPhylo[AvotrexPhylo$Order == 'Strigiformes',]

#Add the new owl species. The species can be added as a final row
#to AvotrexPhylo as adding it makes no changes to other
#taxonomic affinities and therefore, no change to the grafting
#order

AvotrexPhylo3 <- rbind(AvotrexPhylo2, vaporhams)

#Graft species to a Jetz tree
trees <- AvoPhylo(ctrees = BirdTree_trees,
                  avotrex = AvotrexPhylo3, 
                  PER = 0.2, 
                  tax = BirdTree_tax,
                  Ntree = 1)

## ----Plotting the new species example, message=FALSE, warning=FALSE, dpi=300, fig.height=3, fig.width=4----
##single species; 3 levels back
plot(trees[[1]], 
     avotrex = AvotrexPhylo3, 
     tax = BirdTree_tax,
     species = "Vapor_hams",
     tips = "all_dif",
     tips_col = c("red", "darkgreen"),
     lvls = 3,
     type = "phylogram",
     cex = 0.6)


## -----------------------------------------------------------------------------

library(avotrex)

data(BirdTree_trees)
data(BirdTree_tax)
data(AvotrexPhylo)
data(treesEx)

# #all species - no tip names
# plot(treesEx[[1]], 
#      avotrex = AvotrexPhylo, 
#      tax = BirdTree_tax,
#      tips = "none",
#      type = "fan")


## ----dpi=300, fig.height=6, fig.width=6---------------------------------------
#order (owls) - just show extinct tip names (in red) and using
#a fan plot
plot(treesEx[[1]], 
     avotrex = AvotrexPhylo, 
     tax = BirdTree_tax,
     order = "Strigiformes",
     tips = "extinct",
     type = "fan", 
     tip.color = "red", 
     cex = 0.4)

## ----dpi=300, fig.height=5----------------------------------------------------
#genus - cladogram plot
plot(treesEx[[1]], 
     avotrex = AvotrexPhylo, 
     tax = BirdTree_tax,
     genus = "Aplonis", 
     tips = "extinct",
     type = "cladogram",
     tip.color = "red", 
     cex = 0.5)

## ----dpi=300, fig.height=5, fig.width=5---------------------------------------
#family (plot all three trees this time)
plot(treesEx, 
     avotrex = AvotrexPhylo, 
     tax = BirdTree_tax,
     family = "Threskiornithidae", 
     tips = "extinct",
     tip.color = "red", 
     cex = 0.5)


## ----dpi=300------------------------------------------------------------------
#species (& show all tip names in same colour)
species2 <- c("Anas_itchtucknee", "Anas_sp_VitiLevu",
              "Anas_platyrhynchos", "Ara_tricolor")

plot(treesEx[[2]], avotrex = AvotrexPhylo, tax = BirdTree_tax,
     species = species2, tips = "all_same",
     type = "cladogram",
     tip.color = "blue", cex = 0.5)

## ----dpi=300------------------------------------------------------------------
#same as previous, but extinct and extant diff colours
plot(treesEx[[2]], avotrex = AvotrexPhylo, tax = BirdTree_tax,
     species = species2,
     cex = 0.5, tips = "all_dif",
     tips_col = c("red", "darkgreen"),
     type = "cladogram")

## ----dpi=300, message=FALSE, warning=FALSE------------------------------------
##single species only 1 level back
plot(treesEx[[3]], avotrex = AvotrexPhylo, tax = BirdTree_tax,
     species = "Ara_tricolor",
     tips = "all_dif",
     tips_col = c("red", "darkgreen"),
     lvls = 1,
     type = "phylogram",
    cex = 0.6)

## ----dpi=300, message=FALSE, warning=FALSE------------------------------------
#increase levels back
plot(treesEx[[3]], avotrex = AvotrexPhylo, 
     tax = BirdTree_tax,
     species = "Ara_tricolor",
     tips = "all_dif",
     tips_col = c("red", "darkgreen"),
     lvls = 4,
     type = "phylogram",
     cex = 0.5)

