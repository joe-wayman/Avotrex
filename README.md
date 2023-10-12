# Avotrex
Repository for R package Avotrex.  

The Avotrex package provides functionality to graft extinct avian species, present in the AvoTrex dataset, to existing BirdTree backbone trees. 

Note that the package imports and loads in the 'ape' R package in its entirety,
as this provides access to its full range of classes and functions.

As this is version 1.0.0 of the package, it is possible that there are some bugs in places. Please report any issues to us via GitHub.

## Installation

You can install the released version of sars from CRAN with:



And the development version from GitHub with:

## Example usage

data(BirdTree_trees) # Load in the example trees 
data(BirdTree_tax)   # Load in the extant BirdTree taxonomy 
data(AvotrexPhylo)   # Load in the extinct grafting database and instructions

trees <- AvoPhylo(ctrees = Trees, 
avotrex = AvotrexPhylo, PER = 0.2, tax = tax, 
Ntree = 1, n.cores = 1, cluster.ips = NULL)