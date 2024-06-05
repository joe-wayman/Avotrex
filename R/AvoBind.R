#' AvoBind - Bind extinct species to the tree
#' @description Binds an extinct species at either (1) a randomly selected point
#'   along a branch, after truncating either end, (2) a specific fraction along
#'   a branch, or (3) a specific time point along a branch.
#' @usage AvoBind(tree, node, per, per_fixed = FALSE, sp_name,
#'  time_graft = FALSE, terminal = FALSE, mindist = 0.1)
#' @param tree Tree object (i.e., phylogeny) 
#' @param node Target node 
#' @param per The fraction (0-1) of total branch length to truncate at either
#'   end of the branch for grafting (e.g. 0.2 cuts of 20\% of the total branch
#'   length from either end) if \code{per_fixed == FALSE}. If \code{per_fixed
#'   == TRUE}, then the point along the branch where the grafting occurs: value
#'   between 0-1, with a larger number meaning the grafting occurs closer to the
#'   rootward end of the branch. If \code{time_graft = TRUE}, the specific point
#'   (in millions of years, if BirdTree trees are used) for the grafting to
#'   occur.
#' @param per_fixed Logical argument: whether to graft a species on at an exact
#'   point (as a fraction) along a branch (TRUE), which is chosen using the
#'   \code{per} argument, rather than random (FALSE; default). Is ignored if
#'   \code{time_graft = TRUE}.
#' @param sp_name Name of the grafted species.
#' @param time_graft Should the grafting occur at a particular time point (in
#'   millions of years if BirdTree trees provided) along a given branch, using
#'   \code{avotrex:::AgeBind()}. The specific point is provided using the
#'   \code{per} argument.
#' @param terminal Logical value: if \code{time_graft = TRUE}, is the species
#'   being grafted to a terminal branch.
#' @param mindist If \code{time_graft = TRUE}, but the provided grafting time
#'   point (\code{per}) is too old (i.e., older than the parent node) or too
#'   young (i.e., younger than the child node) relative to the focal branch,
#'   grafting will occur \code{mindist} below the parent node or above the child
#'   node.
#' @return Returns a tree of class "phylo", with the extinct species grafted on.
#' @author Joe Wayman, Tom Matthews and Pedro Cardoso (AgeBind)
#' @importFrom phytools bind.tip
#' @export

AvoBind <- function(
    tree, 
    node, 
    per, 
    per_fixed = FALSE, 
    sp_name,
    time_graft = FALSE,
    terminal = FALSE,
    mindist = 0.1
    ){
  
  if (length(time_graft) > 1 | (!is.logical(time_graft))){
    stop("time_graft must be a logical vector of length = 1")
  }
  
  if (!time_graft){
  
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
  } else {
    
    if (length(terminal) > 1 | (!is.logical(terminal))){
      stop("terminal must be a logical vector of length = 1")
    }
    
    tree <- AgeBind(tree = tree, node = node,
                    sp_name = sp_name, 
                    len = per, mindist = mindist, 
                    terminal = terminal)
    
  }#eo if time_graft
  return(tree)
}


#Internal function to graft at specific time point
#len = point to graft (in units of branch lengths);
#mindist = branch length if len is too young/old for
#the branch in question; terminal = is grafting using
#a terminal branch
#' @importFrom TreeTools AddTip
#' @importFrom ape getMRCA branching.times

AgeBind <- function(tree, node, sp_name, 
                    len, mindist, terminal){
  
  if (terminal){
 #   newPlace <- match(tip, tree$tip.label)
    
    #Check if the terminal branch length is < the
    #grafting length (len / edgeLength); if so, 
    #change it
    EN <- which(tree$edge[,2] == node)
    EL <- tree$edge.length[EN]
    if (EL < len){
      #if terminal branch is > 1 in length, just graft it
      #0.1 below the parent node
      if (EL > 1){
        len <- EL - 0.1
        #if shorter than 1, graft it at the 90th percent
        #point (going up from the child node)
      } else {
        len <- EL * 0.9
      }
    } 
    tree <- TreeTools::AddTip(tree, node,
                   label = sp_name,
                   edgeLength = len,
                   lengthBelow = len)
  } else {
 #   ancestor <- ape::getMRCA(tree, tip) #get most recent ancestor
    timeAncestor <- ape::branching.times(tree)[which(names(branching.times(tree)) == 
                                                       node)] #time of MRCA
    above <- tree$edge[which(tree$edge[,2] == node), 1]
    timeAbove <- ape::branching.times(tree)[which(names(branching.times(tree)) ==
                                                    above)] #time of node above MRCA
    #if mindist longer than target branch, change it to
    #10% of branch length
    EL <- timeAbove - timeAncestor
    if (mindist > EL) mindist <- EL * 0.1

    if (len < timeAncestor){
      lenBelow <- mindist
      len <- timeAncestor + mindist
    } else if (len > timeAbove){
      lenBelow <- timeAbove - timeAncestor - mindist
      len <- timeAbove - mindist
    } else {
      lenBelow <- len - timeAncestor
    }
    
    tree <- TreeTools::AddTip(tree, node, 
                   label = sp_name, 
                   edgeLength = len, 
                   lengthBelow = lenBelow)
  }
  return(tree)
}
