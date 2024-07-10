#' Plot Model Fits for an 'multiAvophylo' Object
#'
#' @description S3 method for class 'multiAvophylo'. See the
#'   \code{\link{plot.avophylo}} documentation for more
#'   information.
#'   
#'   Plots individual trees in turn (with the user required to
#'   press 'enter' to move to the next plot).
#' @param x An object of class 'multiAvophylo'.
#' @param tips What tip labels to present. Can be one of
#'   "extinct" (just tip labels of extinct species), "none" (no
#'   tip labels) or "all_same" (show all labels, with same colour
#'   labels for all species), or "all_dif" (show all labels, with
#'   different colour labels for extant and extinct species).
#'   The latter needs to be used in combination with the
#'   \code{tips_col} argument.
#' @param tips_col Colour of tip labels. If \code{tips =
#'   "all_dif"} this needs to be a vector of length two, with the
#'   first value being the colour for extinct species, and the
#'   second for extant species.
#' @param order Prune the tree to only show a specific order
#'   (should be a character vector of length = 1). Taxonomy
#'   follows BirdTree, see the \code{data(BirdTree_taxa)} file.
#'   For extinct order names, see the \code{data(AvotrexPhylo)}
#'   file.
#' @param family As for \code{order}, but for families.
#' @param genus As for \code{order}, but for genera.
#' @param species Prune the tree to only show a specific set of
#'   species. Should be a vector of at least length = 1. If only
#'   a single species name is provided, the \code{lvls} argument
#'   also needs to be provided.
#' @param lvls If \code{species} is just a single species, how
#'   many levels back (rootward) should be plotted. Uses the
#'   \code{tidytree::tree_subset} function (see help
#'   documentation of that function for further info).
#' @param avotrex The Avotrex phylo dataset used to generate the
#'   trees. For most use cases, this will have been loaded using
#'   \code{data(AvotrexPhylo)}.
#' @param tax The Jetz et al. (2012) BirdTree taxonomy .csv.
#'   Supplied as data within the package.
#' @param \dots Other plotting arguments from the ape package's
#'   \link[ape]{plot.phylo} can be provided.
#' @importFrom grDevices devAskNewPage
#' @return Generates a phylogeny plot for each tree in \code{x}.
#' @examples
#' #See the plot.avophylo documentation for further examples
#' data(treesEx)
#' #family (plot both trees in turn)
#' plot(treesEx, avotrex = AvotrexPhylo, tax = BirdTree_tax,
#'      family = "Threskiornithidae", tips = "extinct",
#'      tip.color = "red", cex = 0.5)
#' @export
 
plot.multiAvophylo <- function(x, 
                          tips = "extinct",
                          tips_col = NULL,
                          order = NULL, 
                          family = NULL,
                          genus = NULL,
                          species = NULL,
                          avotrex,
                          tax,
                          lvls = NULL,
                          ...){
  
  tree <- x
  
  lapply(tree, function(y){
    if (!inherits(y, "avophylo")){
      stop("Tree objects should be of class 'avophylo'")
    }
  })
  
  devAskNewPage(TRUE)
  on.exit(devAskNewPage(FALSE))
  for (i in seq_along(tree)){
    plot.avophylo(x = tree[[i]], tips = tips, 
                  tips_col = tips_col, 
                  order = order, family = family,
                  genus = genus, species = species, 
                  avotrex = avotrex ,
                  tax = tax, lvls = lvls, ...) 
  }
}




#' Plot Model Fits for an 'avophylo' Object
#'
#' @description S3 method for class 'avophylo'.
#'   \code{plot.avophylo} creates plots for objects of class
#'   'avophylo'. The exact plot(s) constructed depends on the
#'   argument values provided (see examples below and also the
#'   package vignette). The function uses the ape package's
#'   \link[ape]{plot.phylo} and can take any argument from that
#'   function (e.g. the \code{type} argument).
#' 
#'   If \code{\link{AvoPhylo}} is used to produce a list of trees
#'   of class 'multiAvophylo', then \code{plot.multiAvophylo} is
#'   first called. This plots individual trees in turn (with the
#'   user required to press 'enter' to move to the next plot).
#'
#'   The user will need to play around with plotting
#'   window size, and/or export the image, particularly if many
#'   tips are included.
#'   
#'   Note - if using the \code{lvls} argument, a warning is
#'   provided. This comes from the \code{tidytree::tree_subset}
#'   function and appears to be a bug (but the plot should be
#'   checked for sense).
#'
#' @param x An object of class 'avophylo'.
#' @param tips What tip labels to present. Can be one of
#'   "extinct" (just tip labels of extinct species), "none" (no
#'   tip labels) or "all_same" (show all labels, with same colour
#'   labels for all species), or "all_dif" (show all labels, with
#'   different colour labels for extant and extinct species).
#'   The latter needs to be used in combination with the
#'   \code{tips_col} argument.
#' @param tips_col Colour of tip labels. If \code{tips =
#'   "all_dif"} this needs to be a vector of length two, with the
#'   first value being the colour for extinct species, and the
#'   second for extant species.
#' @param order Prune the tree to only show a specific order
#'   (should be a character vector of length = 1). Taxonomy
#'   follows BirdTree, see the \code{data(BirdTree_taxa)} file.
#'   For extinct order names, see the \code{data(AvotrexPhylo)}
#'   file.
#' @param family As for \code{order}, but for families.
#' @param genus As for \code{order}, but for genera.
#' @param species Prune the tree to only show a specific set of
#'   species. Should be a vector of at least length = 1. If only
#'   a single species name is provided, the \code{lvls} argument
#'   also needs to be provided.
#' @param lvls If \code{species} is just a single species, how
#'   many levels back (rootward) should be plotted. Uses the
#'   \code{tidytree::tree_subset} function (see help
#'   documentation of that function for further info).
#' @param avotrex The Avotrex phylo dataset used to generate the
#'   trees. For most use cases, this will have been loaded using
#'   \code{data(AvotrexPhylo)}.
#' @param tax The Jetz et al. (2012) BirdTree taxonomy .csv.
#'   Supplied as data within the package.
#' @param \dots Other plotting arguments from the ape package's
#'   \link[ape]{plot.phylo} can be provided.
#' @importFrom ape keep.tip plot.phylo
#' @importFrom tidytree tree_subset
#' @importFrom graphics par
#' @return Generates a phylogeny plot of \code{x}.
#' @examples
#' # Generate a set of trees
#' # data(BirdTree_trees)
#' # data(BirdTree_tax)
#' # data(AvotrexPhylo)
#' # trees <- AvoPhylo(ctrees = BirdTree_trees,
#' # avotrex = AvotrexPhylo, PER = 0.2, PER_FIXED = 0.75,
#' # tax = BirdTree_tax, Ntree = 2, n.cores = 2, 
#' # cluster.ips = NULL)
#' 
#' #For here, we can load in an example set of two trees 
#' #generated using the above code
#' 
#' data(treesEx)
#' 
#' #order (owls) - just show extinct tip names (in red) and using
#' #a fan plot
#' plot(treesEx[[1]], avotrex = AvotrexPhylo, tax = BirdTree_tax,
#'      order = "Strigiformes", tips = "extinct",
#'      type = "fan", tip.color = "red", cex = 0.4)
#' 
#' #family (plot all three trees this time)
#' plot(treesEx, avotrex = AvotrexPhylo, tax = BirdTree_tax,
#'      family = "Threskiornithidae", tips = "extinct",
#'      tip.color = "red", cex = 0.5)
#' 
#' #genus - cladogram plot
#' plot(treesEx[[2]], avotrex = AvotrexPhylo, tax = BirdTree_tax,
#'      genus = "Aplonis", tips = "extinct",
#'      type = "cladogram",
#'      tip.color = "red", cex = 0.5)
#' 
#' #species (& show all tip names in same colour)
#' species2 <- c("Anas_itchtucknee", "Anas_sp_VitiLevu",
#'               "Anas_platyrhynchos", "Ara_tricolor")
#' 
#' plot(treesEx[[2]], avotrex = AvotrexPhylo, tax = BirdTree_tax,
#'      species = species2, tips = "all_same",
#'      type = "cladogram",
#'      tip.color = "blue", cex = 0.5)
#' 
#' #same as previous, but extinct and extant diff colours
#' plot(treesEx[[2]], avotrex = AvotrexPhylo, tax = BirdTree_tax,
#'      species = species2,
#'      cex = 0.5, tips = "all_dif",
#'      tips_col = c("red", "darkgreen"),
#'      type = "cladogram")
#' 
#' ##single species 2 levels back
#' plot(treesEx[[2]], avotrex = AvotrexPhylo, tax = BirdTree_tax,
#'      species = "Ara_tricolor",
#'      tips = "all_dif",
#'      tips_col = c("red", "darkgreen"),
#'      lvls = 2,
#'      type = "phylogram",
#'      cex = 0.6)
#' @export

plot.avophylo <- function(x, 
                          tips = "extinct",
                          tips_col = NULL,
                          order = NULL, 
                          family = NULL,
                          genus = NULL,
                          species = NULL,
                          avotrex,
                          tax,
                          lvls = NULL,
                          ...){
  
  oldpar <- par(no.readonly = TRUE) # code line i
  on.exit(par(oldpar)) # code line i + 1
  
  tree <- x

  if (tips == "all_dif"){
    if (is.null(tips_col) | length(tips_col) != 2){
      stop ("'tips_col' should be a vector of length 2 (i.e., 2 colors)")
    }
  } else {
    if (!is.null(tips_col)){
      warning("tips_col only works with tips == 'all_dif', try 'tip.color'")
    }
  }
  
  class(tree) <- "phylo"
  
  #filter out extinct species in avotrex database that user 
  #does not have in their tree
  wnot2 <- which(!avotrex$species %in% tree$tip.label) 
  if (length(wnot2) == nrow(avotrex)) stop("No extinct species in tree")
  if (length(wnot2) > 0) avotrex <- avotrex[-wnot2,]
  
  #filter out AP species from Jetz (i.e., extinct sp in
  #BirdTree)[if any included - if a user filters AvotrexPhylo,
  #e.g., as we do in the vignette, the AP species may not be
  #included and thus this errors. But note, in this case they 
  #may still be plotted as they won't be removed from Jetz]
  AP_sp <- avotrex[which(avotrex$type == "AP"),]$species
  if (length(AP_sp) > 0){
  if (!all(AP_sp %in% tax$TipLabel)) stop("AP species not in Jetz")
  tax <- tax[-which(tax$TipLabel %in% AP_sp),]
  }
  
  #Select and format Jetz columns
  plot_df1 <- tax[,c("TipLabel", "Genus",
                     "BLFamilyLatin",
                     "Order")]
  colnames(plot_df1) <- c("species", "jetz_genus",
                          "jetz_family", "jetz_order")
  plot_df1$Status <- "Extant"
  
  #In case someone selects an extinct genus, family etc,
  #we need to swap the "Extinct" label in the Jetz columns
  #with the name from the "Birdlife" columns
  wEx <- which(avotrex[,"jetz_order"] == "Extinct")
  avotrex[wEx, "jetz_order"] <- 
    avotrex[wEx, "order"]
  
  plot_df2 <- avotrex[, c("species", "jetz_order", 
                          "jetz_family", "jetz_genus")]
  plot_df2$jetz_order <- toupper(plot_df2$jetz_order) 
  plot_df2$Status <- "Extinct"
  plot_df3 <- rbind(plot_df1, plot_df2)
  
  #################################################
  #If tree is to be subset to a specific clade
  if (!is.null(order) | !is.null(family) |
      !is.null(genus)){
    
    if (is.null(avotrex) | is.null(tax)){
      stop("The 'avotrex' and 'tax' data files have not 
           been provided")
    }
    
    wnull <- c("order" = order, 
               "family" = family, "genus" = genus)
    
    if (length(wnull) > 1){
      stop("Only one of 'order', 'family' or 'genus' should be provided")
    }
    
    level <- names(wnull)
    
    #in case user provides order name in lowercase
    if (level == "order"){
      wnull <- toupper(wnull)
    }
    
    level2 <- switch(level,
                     "genus" = c("genus", "jetz_genus"),
                     "family" = c("family", "jetz_family"),
                     "order" = c("order", "jetz_order"))
    
    if (length(unique(plot_df3$species)) != nrow(plot_df3)){
      stop("Multiple species with same names after filtering out 
           'AP' species")
    }
    
    if (!wnull %in% plot_df3[,level2[2]]){
      stop(paste0("The ", level2[1],
                  " argument provided is not present"))
    }
  
    #Filter out the user's chosen order,family or genus
    wSub <- which(plot_df3[,level2[2]] == wnull)
    plot_df4 <- plot_df3[wSub,]
    
  }# eo if null
  
  if (!is.null(species)){
    wnull <- c("order" = order, 
               "family" = family, "genus" = genus)
    if (length(wnull) > 0){
      stop("Only one of 'species', 'order', 'family' or 'genus' should be provided")
    }
    if (!is.vector(species) | is.list(species)){
      stop("'species' should be a vector")
    }
    if (!all(species %in% tree$tip.label)){
      stop("some 'species' not in tree")
    }
  }#eo if species
  
  
  ##########################################################

  ##PLOTTING CODE
  if(is.null(order) & is.null(family) &
     is.null(genus)){
    plot_df4 <- plot_df3
  }

  
  if(!all(plot_df4$species %in% tree$tip.label)){
    stop("Species selected for plotting not present in provided tree. ")
  }
  
  ### If filtering by order, family, genus or a list of species
  if(!is.null(order) | !is.null(family) |
     !is.null(genus)){
    tree2 <- ape::keep.tip(tree, plot_df4$species)
  }else if(!is.null(species) & length(species) > 1){
    tree2 <- ape::keep.tip(tree, species)
  }else if(!is.null(species) & length(species) == 1){
    
    if(is.null(lvls)){
      stop("If subsetting on one species, provide the number of levels to go back")
    }
    
    tree2 <- tidytree::tree_subset(tree,
                         species,
                         levels_back = lvls)
  }else{
    tree2 <- tree
  }
  
  par(xpd = NA)
  if (tips == "none"){
    ape::plot.phylo(tree2, show.tip.label = FALSE, ...)
  } else if (tips == "extinct"){
    tree2$tip.label[tree2$tip.label %in%
                      plot_df4[plot_df4$Status == "Extant",]$species] <- ""
    ape::plot.phylo(tree2, ...)
    } else if (tips == "all_same"){
    ape::plot.phylo(tree2, ...)
    } else if (tips == "all_dif"){
      tl <- tree2$tip.label
      ml <- match(tl, plot_df4$species)
      tc <- plot_df4$Status[ml]
      tc2 <- ifelse(tc == "Extinct", tips_col[1], tips_col[2])
      ape::plot.phylo(tree2, tip.color = tc2, ...)
    } else {
    stop("'tips should be one of 'none', 'extinct', 'all_same' or 'all_dif'")
    }

}# eo function
