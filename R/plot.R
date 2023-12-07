
#tree can be an individual tree of class avophylo, or a list
#of trees of class multiPhylo (indiv trees of class of avophylo),
#in the latter case, the first tree is used for plotting

#tips = "extinct", "none" or "all"

# data(BirdTree_tax); tax = BirdTree_tax
# data(AvotrexPhylo); avotrex = AvotrexPhylo
# order = "STRIGIFORES"; family = NULL; genus = NULL

#layout = "circular"


#' @importFrom ape keep.tip
#' @importFrom ggtree ggtree geom_tiplab
#' @export 


# library(ggtree)
# library(ggplot2)
# library(treeio)


# tree = trees; tips = "extinct"; order = NULL
# family = NULL;
# genus = NULL;
# species = NULL;
# avotrex = AvotrexPhylo;
# tax = BirdTree_tax;
# lvls = NULL

#plot(trees, avotrex = AvotrexPhylo, tax = BirdTree_tax)

plot.avophylo <- function(tree, 
                          tips = "extinct",
                          order = NULL, 
                          family = NULL,
                          genus = NULL,
                          species = NULL,
                          avotrex,
                          tax,
                          lvls = NULL,
                          ...){
  
  
  if (inherits(tree, "multiPhylo")){
    if (!inherits(tree[[1]], "avophylo")){
      stop("Tree objects should be of class 'avophylo'")
    }
    if (length(tree) == 1){
        tree <- tree[[1]]
    } else {
      tree <- tree[[1]]
      message("A list of multiple trees has been provided: the first has been selected for plotting")
    }
  } else {
    if (!inherits(tree, "avophylo")){
      stop("Tree objects should be of class 'avophylo'")
    }
    }#eo if multiPhylo
  
  
  #Revert class to just phylo
  class(tree) <- "phylo"
  
  #filter out AP species from Jetz (i.e., extinct sp in
  #BirdTree)
  AP_sp <- avotrex[which(avotrex$Type == "AP"),]$species
  if (!all(AP_sp %in% tax$TipLabel)) stop("AP species not in Jetz")
  tax <- tax[-which(tax$TipLabel %in% AP_sp),]
  
  #Select and format Jetz columns
  plot_df1 <- tax[,c("TipLabel", "Genus",
                     "BLFamilyLatin",
                     "Order")]
  colnames(plot_df1) <- c("species", "Jetz_Genus",
                          "Jetz_Family", "Jetz_Order")
  plot_df1$Status <- "Extant"
  
  #In case someone selects an extinct genus, family etc,
  #we need to swap the "Extinct" label in the Jetz columns
  #with the name from the "Birdlife" columns
  wEx <- which(avotrex[,"Jetz_Order"] == "Extinct")
  avotrex[wEx, "Jetz_Order"] <- 
    avotrex[wEx, "Order"]
  
  plot_df2 <- avotrex[, c("species", "Jetz_Order", 
                          "Jetz_Family", "Jetz_Genus")]
  plot_df2$Jetz_Order <- toupper(plot_df2$Jetz_Order) 
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
    
    level2 <- switch(level,
                     "genus" = c("Genus", "Jetz_Genus"),
                     "family" = c("Family", "Jetz_Family"),
                     "order" = c("Order", "Jetz_Order"))
    
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
  ##########################################################

  ##PLOTTING CODE
  if(!is.null(order) | !is.null(family) |
     !is.null(genus)){
  }else{
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
    
    tree2 <- tree_subset(tree,
                         species,
                         levels_back = lvls)
  }else{
    tree2 <- tree
  }
  
  if(tips == "none"){
    f <- ggtree::ggtree(tree2, ...) 
  }
  if(tips == "extinct"){
    tree2$tip.label[tree2$tip.label %in% 
                      plot_df4[plot_df4$Status == "Extant",]$species] <- ""
    f <- ggtree::ggtree(tree2, ...) + 
      ggtree::geom_tiplab()
  }
  if(tips == "all"){
    f <- ggtree::ggtree(tree2, ...) + 
      ggtree::geom_tiplab()
  }
  
  print(f)
  
}# eo function
