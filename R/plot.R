#' @importFrom grDevices devAskNewPage
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
                  tax = tax, lvls = lvls, main = "a", ...) 
  }
}


#tree can be an individual tree of class avophylo, or a list
#of trees of class multiPhylo (indiv trees of class of avophylo),
#in the latter case, the first tree is used for plotting

#tips = "extinct", "none" or "all_same", or "all_dif"

#species should be a vector of species names


#Note - user will need to play around with plotting window
#size, and/or export the image, particularly if many tips


#' @importFrom ape keep.tip plot.phylo
#' @importFrom treeio tree_subset
#' @importFrom graphics par
#' @export 




# tree = trees; tips = "extinct"; order = NULL
# family = NULL;
# genus = NULL;
# species = NULL;
# avotrex = AvotrexPhylo;
# tax = BirdTree_tax;
# lvls = NULL
# 


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
    
    #in case user provides order name in lowercase
    if (level == "order"){
      wnull <- toupper(wnull)
    }
    
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
    
    tree2 <- treeio::tree_subset(tree,
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
