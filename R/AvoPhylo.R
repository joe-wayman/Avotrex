utils::globalVariables(c("phylo_id2", "Group",
                         "BLFamilyLatin", "Order",
                         "Clade"))

#' AvoPhylo: Building phylogenies based on AvoTrex extinct bird trait database and BirdTree backbone
#' 
#' @description
#' AvoPhylo: Building phylogenies based on the AvoTrex database and BirdTree
#' backbone
#' LazyData: true
#' 
#' @usage AvoPhylo(ctrees, avotrex, PER = 0.2, tax, Ntree, n.cores = 1, cluster.ips = NULL)
#' 
#' @details
#' Function to build phylogenies incorporating the extinct species from the
#' AvoTrex extinct birds database. **EXPAND - include info on the grafting 
#' procedure, including the commands, the grafting order, the different codes in the 
#' data file
#' 
#' @param n.cores Number of cores used to build the phylogeny. Default is one
#'   (will run with parallel processing)
#' @param cluster.ips Cluster location. Keep as default. 
#' @param PER Percentage/fraction for branch truncation based on random grafting
#'   (see AvoBind for more details).
#' @param tax The Jetz et al. (2012) BirdTree taxonomy .csv. Supplied as data
#'   within the package.
#' @param Ntree The number of trees to sample from the supplied number of
#'   BirdTree trees. Value must be greater than the number of supplied trees
#'   (ctrees)
#' @param ctrees multiPhylo object containing phylogenies. 
#' @param avotrex The AvoTrex extinct species phylogeny database. This database
#'   contains the information and commands required to graft the extinct species
#'   on to the BirdTree trees.
#' @return The function returns a multiPhylo object consisting of N trees that
#'   were randomly selected from a supplied number. These trees have all had the
#'   extinct species from AvoTrex grafted on. For more details on the grafting,
#'   see: **PAPER**
#' @importFrom parallel makeCluster
#' @importFrom snow makeSOCKcluster
#' @importFrom doParallel registerDoParallel
#' @importFrom doSNOW registerDoSNOW
#' @importFrom utils txtProgressBar setTxtProgressBar
#' @importFrom foreach foreach
#' @importFrom dplyr filter
#' @importFrom stringr str_split
#' @importFrom stats runif
#' @import ape
#' @return The imputed tree(s) with the extinct species grafted on. The object is returned as a class 'multiPhylo', the same as the input tree(s).
#' @examples 
#' # data(BirdTree_trees)
#' # data(BirdTree_tax)
#' # data(AvotrexPhylo)
#' # trees <- AvoPhylo(ctrees = BirdTree_trees, 
#' # avotrex = AvotrexPhylo, PER = 0.2, tax = BirdTree_tax, 
#' # Ntree = 1, n.cores = 1, cluster.ips = NULL)
#' @export 
AvoPhylo <- function(
    ctrees,
    avotrex,
    PER = 0.2,
    tax, 
    Ntree,
    n.cores = 1,
    cluster.ips = NULL
    ){
  
  if(length(ctrees) < Ntree){stop("Error: Number of sampled trees greater than the number of supplied trees.")}
  #subset a number you want to test
  ctrees <- sample(ctrees, Ntree, replace = F)
  
  if (Ntree == 1 & n.cores > 1){
    cat("As Ntree == 1, only 1 core will be used\n")
  }
  
  # Set up the cluster for parallel processing 
  if (is.null(cluster.ips)) {
    if (n.cores == 1) {
      `%dopar%` <- foreach::`%do%`
      on.exit(`%dopar%` <- foreach::`%dopar%`)
      cluster.ips <- NULL
    }
    else {
      temp.cluster <- parallel::makeCluster(n.cores, type = "PSOCK")
    }
  }else {
    Sys.setenv(R_PARALLEL_PORT = cluster.port)
    cluster.spec <- cluster_specification(cluster.ips = cluster.ips, 
                                          cluster.cores = cluster.cores, cluster.user = cluster.user)
    if (verbose == TRUE) {
      outfile <- ""
    }else {
      if (.Platform$OS.type == "windows") {
        outfile <- "nul:"
      }else {
        outfile <- "/dev/null"
      }
    }
    temp.cluster <- snow::makeSOCKcluster(ncores)
  }
  if (exists("temp.cluster")) {
    doParallel::registerDoParallel(cl = temp.cluster)
    doSNOW::registerDoSNOW(temp.cluster)
  }
  #Set a progress bar to return progress of the foreach loop
  pb <- txtProgressBar(min = 0, max = length(Ntree), style = 3)
  progress <- function(n) setTxtProgressBar(pb, n)
  opts <- list(progress=progress)
  
  avotrex$Group <- as.numeric(avotrex$Group)#NA warning fine (just because already NAs in Group)
  
  #Run the parallel dataprep
  ctreesComplete <- foreach(
    i = 1:Ntree,
    .options.snow = opts, 
    .packages = c("phytools")) %dopar%
    {
      
      ctree <- ctrees[[i]]   # Each loop we do one tree
      
      ## Reorder the dataset 
      ex <- as.data.frame(avotrex)
      ex <- ex[order(ex$phylo_id),]
      row.names(ex) <- 1:nrow(ex)
      
      ## Subset the species to randomly shuffle
      shuff <- dplyr::filter(ex, phylo_id2 == "xS")

      ## Remove those species from the initial DB
      ex <- dplyr::filter(ex, !phylo_id2 == "xS")
      ## Set the order
      groups <- as.numeric(unique(shuff$Group))
      groups <- sort(groups)
      
      for(i in 1:length(groups)){
        
        shuff2 <- dplyr::filter(shuff, Group == groups[i])
        shuff2 <-  shuff2[sample(1:nrow(shuff2)), ]
        ex <- rbind(ex, shuff2)
        
      }
      
      row.names(ex) <- 1:nrow(ex)
      
      # For each extinct species find the optimum place to bind 
      for(j in 1:nrow(ex)){
        
        #extract the species' per code and set the PER_VAL and
        #per_fixed values.
        #For the clades with many closely related extinct species,
        #they have a per_fixed value of TRUE in the database. For these,
        #set per to 0.75 to try to avoid really short terminal branches
        #(although sometimes this is forced due to BirdTree typology)
        per_val <- ex$per_fixed[j]
        if (per_val){
          PER_VAL <- 0.75
          pfv <- TRUE
        } else {
          PER_VAL <- PER
          pfv <- FALSE
        }
        
        # Code for checking where individual trees break
        # vec <- paste0(j, ex[j,]$species)
        # write.csv(vec, paste0("Broke/broke_", i, ".csv"), row.names = F)
        
        # AP = Already present - Nothing needs to be done  
        if(ex$Type[j] == "AP"){
          next
        } else if(ex$Type[j] == "S"){
          ## Scenario 1.1: Add target species as a single sister of species X 
          ## - S (SISTER SPECIES)
          
          # Get the tip location for the sister sp.
          nodeX <- which(ctree$tip.label == paste0(ex$Sister_genus[j], 
                                                   "_", ex$Sister_species[j])) 
          
        } else if(ex$Type[j] %in% c("SSG", "SGG", "SGG2", "SFG", "SOG")){
          ## Scenario 1.2: Add species as a sister (outgroup) of a group of
          ## species ## - SSG (SISTER SPECIES GROUP) & SGG (SISTER GENUS GROUP)
          ## & SFG (SISTER FAMILY GROUP) & SOG (SISTER ORDER GROUP)
          
          if(ex$Type[j] == "SSG"){
            
            # Separate the species in the "sister_species_group" column
            sp <- stringr::str_split(ex$Sister_species_group[j], pattern = ";")
            spv <- vector()
            
            for(x in 1:length(sp[[1]])){
              
              spv <- c(spv, paste0(sp[[1]][x]))
              
            } # Make the species group as a vector
            
            # This selects the most recent common ancestor for the group of species
            nodeX <- getMRCA(ctree, spv) 
          }else if(ex$Type[j] == "SGG"){
            
            # If only one species is present within the genus, then make a
            # sister to that species
            if(length(ctree$tip.label[grep(paste0(ex$Sister_genus[j], "_"), 
                                            ctree$tip.label)]) == 1){
              # Get the tip location for the sister sp.
              nodeX <- which(ctree$tip.label == 
                               ctree$tip.label[grep(paste0(ex$Sister_genus[j], 
                                                           "_"), ctree$tip.label)]) 

            }else{

              # Get most recent common ancestor of genus 
              nodeX <- getMRCA(ctree, 
                               ctree$tip.label[grep(paste0(ex$Sister_genus[j], 
                                                           "_"), ctree$tip.label)]) 
 
            }
          }else if(ex$Type[j] == "SGG2"){
            
            sp <- stringr::str_split(ex$Sister_genus[j], pattern = ";")
            spv <- vector()
            
            # Get all the species in the genera
            for(x in 1:length(sp[[1]])){
              
              spv <- c(spv, ctree$tip.label[grep(paste0(sp[[1]][x], "_"),
                                                 ctree$tip.label)])
              
            }
            
            # Get most recent common ancestor of species group
            nodeX <- getMRCA(ctree, spv)  
          }else if(ex$Type[j] == "SFG"){
            
            # Get all species within the family 
            fam <- dplyr::filter(tax, BLFamilyLatin == ex$Sister_family[j])
            spv <- vector()
            
            for(x in 1:nrow(fam)){
              
              spv <- c(spv, paste0(fam$Genus[x], "_", fam$Species[x]))
              
            } # Make the species group as a vector
            
            # This selects the most recent common ancestor for the group of species
            nodeX <- getMRCA(ctree, spv)
          }else if(ex$Type[j] == "SOG"){
            
            # Get all species within the family 
            ord <- dplyr::filter(tax, Order == ex$Sister_order[j])
            spv <- vector()
            
            for(x in 1:nrow(ord)){
              
              spv <- c(spv, paste0(ord$Genus[x], "_", ord$Species[x]))
              
            } # Make the species group as a vector
            
            # This selects the most recent common ancestor for the group of species
            nodeX <- getMRCA(ctree, spv) 
          }
          
        } else if(ex$Type[j] %in% c("RSG", "RGG", "RGG2", "RCG", "RFG", "ROG")){
          ## Scenario 2.1: Add species randomly within of a group of species ##
          ## - RSG (RANDOM SPECIES GROUP) & RGG (RANDOM GENUS GROUP) & RFG
          ## (RANDOM FAMILY GROUP) & ROG (RANDOM ORDER GROUP)
          
          if(ex$Type[j] == "RSG"){
            
            # Separate the species in the "sister_species_group" column
            sp <- stringr::str_split(ex$Sister_species_group[j], pattern = ";")
            spv <- vector()
            
            for(x in 1:length(sp[[1]])){
              
              spv <- c(spv, paste0(sp[[1]][x]))
              
            } 
            
            ## Randomly select one of the species from the listed group. If it
            ## is not present in the tree yet, remove from the list and select
            ## again. Will break if all species have been attempted and there
            ## were none present in the tree.
            repeat{
              #Randomly select one of the species
              spv2 <- sample(spv, 1)
              # Get the tip location for the sister sp.
              nodeX <- which(ctree$tip.label == spv2) 
              # Break if there is a node value
              if(length(nodeX) != 0) break else{
                spv <- spv[!spv == spv2]
              }
              if(length(spv) == 0) break
              
            }
            
            ## Check if the node still is length zero
            if(length(nodeX) == 0){print(paste0("Node is still zero length for ", 
                                                ex$Type[j], " for species ", 
                                                ex$species[j],
                                                " (row ", j, ") after random species selection."))}

          }else if(ex$Type[j] == "RGG"){
            
            # Get all the species in the genus
            sp <- stringr::str_split(ex$Sister_genus[j], pattern = ";")
            spv <- vector()
            
            # Get all the species in the genera
            for(x in 1:length(sp[[1]])){
              
              spv <- c(spv, ctree$tip.label[grep(paste0(sp[[1]][x], "_"), 
                                                 ctree$tip.label)])
              
            }
            
            #Randomly select one of the species
            spv2 <- sample(spv, 1)
            
            # Get the tip location for the sister sp.
            nodeX <- which(ctree$tip.label == spv2) 
          }else if(ex$Type[j] == "RGG2"){
            
            sp <- stringr::str_split(ex$Sister_genus[j], pattern = ";")
            spv <- vector()
            
            # Get all the species in the genera
            for(x in 1:length(sp[[1]])){
              
              spv <- c(spv, ctree$tip.label[grep(paste0(sp[[1]][x], "_"), 
                                                 ctree$tip.label)])
              
            } 
            
            #Randomly select one of the species
            spv2 <- sample(spv, 1)
            
            # Get the tip location for the sister sp.
            nodeX <- which(ctree$tip.label == spv2) 
          }else if(ex$Type[j] == "RFG"){
            
            # Get all species within the family 
            fam <- dplyr::filter(tax, BLFamilyLatin == ex$Sister_family[j])
            spv <- vector()
            
            for(x in 1:nrow(fam)){
              
              spv <- c(spv, paste0(fam$Genus[x], "_", fam$Species[x]))
              
            } # Make the species group as a vector
            
            #Randomly select one of the species
            spv2 <- sample(spv, 1)
            
            # Get the tip location for the sister sp.
            nodeX <- which(ctree$tip.label == spv2) 
          }else if(ex$Type[j] == "RCG"){
            
            # Get all species within the family 
            fam <- dplyr::filter(tax, Clade == ex$Sister_clade[j])
            spv <- vector()
            
            for(x in 1:nrow(fam)){
              
              spv <- c(spv, paste0(fam$Genus[x], "_", fam$Species[x]))
              
            } # Make the species group as a vector
            
            #Randomly select one of the species
            spv2 <- sample(spv, 1)
            
            # Get the tip location for the sister sp.
            nodeX <- which(ctree$tip.label == spv2) 
          }else if(ex$Type[j] == "ROG"){
            
            # Get all species within the family 
            ord <- dplyr::filter(tax, Order == ex$Sister_family[j])
            spv <- vector()
            
            for(x in 1:nrow(fam)){
              
              spv <- c(spv, paste0(ord$Genus[x], "_", ord$Species[x]))
              
            } # Make the species group as a vector
            
            #Randomly select one of the species
            spv2 <- sample(spv, 1)
            
            # Get the tip location for the sister sp.
            nodeX <- which(ctree$tip.label == spv2) 
          }
          
        } else if(ex$Type[j] %in% c("RSGG", "RSGG2")){
          ## Scenario 3.1: Add species as a sister to a genus selected randomly
          ## from a supplied group of genera (RSGG) or a random genus from a
          ## supplied family (RSGG2)
          
          if(ex$Type[j] == "RSGG"){
            
            #Split the supplied genera
            sp <- stringr::str_split(ex$Sister_genus[j], pattern = ";")
            
            #Randomly select one of the genera
            spv2 <- sample(sp[[1]], 1)
            
            if(length(ctree$tip.label[grep(paste0(spv2, "_"), 
                                           ctree$tip.label)]) == 1){
              
              # Get the tip location for single species in the genus
              nodeX <- which(ctree$tip.label == 
                               ctree$tip.label[grep(paste0(spv2, "_"), 
                                                    ctree$tip.label)]) 
              
            }else{
              # Get most recent common ancestor of genus 
              nodeX <- getMRCA(ctree, ctree$tip.label[grep(paste0(spv2, "_"), 
                                                           ctree$tip.label)]) 
            }
          }
          
          if(ex$Type[j] == "RSGG2"){
            
            # Get all genera within the family 
            fam <- dplyr::filter(tax, BLFamilyLatin == ex$Sister_family[j])
            spv <- vector()
            
            for(x in 1:nrow(fam)){
              
              spv <- c(spv, paste0(fam$Genus[x]))
              
            } # Make the species group as a vector
            
            spv <- unique(spv)
            
            #Randomly select one of the genera
            spv2 <- sample(spv, 1)
            
            if(length(ctree$tip.label[grep(paste0(spv2, "_"), 
                                           ctree$tip.label)]) == 1){
              # Get the tip location for single species in the genus
              nodeX <- which(ctree$tip.label == 
                               ctree$tip.label[grep(paste0(spv2, "_"), 
                                                    ctree$tip.label)]) 
            } else{
              # Get most recent common ancestor of genus 
              nodeX <- getMRCA(ctree,ctree$tip.label[grep(paste0(spv2, "_"), 
                                                          ctree$tip.label)]) 
            }
          } #eo if RSGG2
          
        } else {
          stop("Code / Type not recognised")
        }#eo main if statements
        
        # As a final step, Bind the extinct sp. on to tree
        ctree <- AvoBind(tree = ctree, node = nodeX,
                         per = PER_VAL, per_fixed = pfv,
                         sp_name = ex$species[j])

      } #eo for j
      
      return(ctree)          # Return the tree object
      
    }#eo for each
  
  ## Finish Tree ## 
  class(ctreesComplete) <- "multiPhylo"    # Change the class
  return(ctreesComplete)
  
}
