#' AvoBind - Internal function 
#' @param tree Tree object (i.e., phylogeny) 
#' @param node Target node 
#' @param per The fraction (0-1) of total branch length to truncate at either end of the branch for grafting (e.g. 0.2 cuts of 20% of the total branch lenth from either e
#' @param per_fixed whether to graft on at an exact place rather than random; value between 0-1, with larger number meaning grafting happens closer to the root.
#' @param sp_name Name of the grafted species

AvoBind <- function(
    tree, 
    node, 
    per, 
    per_fixed = FALSE, 
    sp_name
    ){
  
  # Get the branch length
  Lx <- tree$edge.length[which(tree$edge[,2]==node)]   
  
  if (!per_fixed){
    #truncate the branch length
    LxTrun <- c((Lx * per), (Lx * (1 - per)))
  } else {
    LxTrun <- rep((Lx * per), 2)
  }
  
  # Bind the extinct sp.
  tree <- bind.tip(tree,                                                                   
                   paste0(sp_name), 
                   where = node, 
                   position = runif(1, min = LxTrun[1],
                                    max = LxTrun[2]))
  return(tree)
  
}