#' AvoBind - Bind extinct species to the tree
#' @description Binds an extinct species at a randomly selected
#' point along a branch, after truncating either end.
#' @usage AvoBind(tree, node, per, per_fixed = FALSE, sp_name)
#' @param tree Tree object (i.e., phylogeny) 
#' @param node Target node 
#' @param per The fraction (0-1) of total branch length to truncate at either
#'   end of the branch for grafting (e.g. 0.2 cuts of 20% of the total branch
#'   length from either end) if \code{per_fixed == FALSE}. If \code{per_fixed
#'   == TRUE}, then the point along the branch where the grafting occurs: value
#'   between 0-1, with a larger number meaning the grafting occurs closer to the
#'   rootward end of the branch
#' @param per_fixed Logical argument: whether to graft a species on at an exact
#'   point along a branch (TRUE), which is chosen using the \code{per} argument,
#'   rather than random (FALSE; default)
#' @param sp_name Name of the grafted species
#' @importFrom phytools bind.tip
#' @export

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
  tree <- phytools::bind.tip(tree,                                                                   
                   paste0(sp_name), 
                   where = node, 
                   position = runif(1, min = LxTrun[1],
                                    max = LxTrun[2]))
  return(tree)
}
