#' AvoBind - Internal function 

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