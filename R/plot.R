
#tips = "extinct", "none" or "all"

# data(BirdTree_tax); tax = BirdTree_tax
# data(AvotrexPhylo); avotrex = AvotrexPhylo
# order = "STRIGIFORES"; family = NULL; genus = NULL



plot.avophylo <- function(tree, tips = "extinct",
                          order = NULL, family = NULL,
                          genus = NULL,
                          avotrex = NULL,
                          tax = NULL,
                          ...){
  
  #################################################
  #If tree is to subsetted to a specific clade
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

    plot_df1 <- plot_df1[,c("species", level2[2])]
    
    #check all genera, families, or orders of extinct in 
    #main Jetz taxonomy
    # f <- dplyr::filter(avotrex, Jetz_Order != "Extinct")
    # all(f$Jetz_Order %in% tax$Order)
    # f <- dplyr::filter(avotrex, Jetz_Family != "Extinct")
    # all(f$Jetz_Family %in% tax$BLFamilyLatin)
    # f <- dplyr::filter(avotrex, Jetz_Genus != "Extinct" &
    #                      Jetz_Genus != "NA")
    # all(f$Jetz_Genus %in% tax$Genus)

    #In case someone selects an extinct genus, family etc,
    #we need to swap the "Extinct" label in the Jetz columns
    #with the name from the "Birdlife" columns
    wEx <- which(avotrex[, c(level2[2])] == "Extinct")
    avotrex[wEx, c(level2[2])] <- 
      avotrex[wEx, c(level2[1])]
    
    plot_df2 <- avotrex[, c("species", level2[2])]
    #change swapped values to upper case to match
    if (level2[1] == "Order"){
      plot_df2$Jetz_Order <- toupper(plot_df2$Jetz_Order) 
    }
    
    #Merge the two dataframes
    plot_df1$Status <- "Extant"
    plot_df2$Status <- "Extinct"
    plot_df3 <- rbind(plot_df1, plot_df2)
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
    plot_df3 <- plot_df3[wSub,]
  }# eo if null
  ##########################################################

  ##PLOTTING CODE
  
  ##INCLUDE AN IF CHECK TO check all plot_df3$species are in
  #tree tip labels
  
  

  
}# eo function
