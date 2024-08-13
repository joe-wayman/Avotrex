context("avotrex tests")
library(avotrex)
library(ape)

# ##Manual tests on 1000 trees
# r1 <- ape::read.tree(file.choose())
# r2 <- ape::read.tree(file.choose())
# r3 <- ape::read.tree(file.choose())
# rr <- c(r1, r2, r3)
# data(BirdTree_tax)
# data(AvotrexPhylo)
# trees <- AvoPhylo(ctrees = rr,
# avotrex = AvotrexPhylo, PER = 0.2, PER_FIXED = 0.75,
# mindist = 0.1, ord = FALSE,
# tax = BirdTree_tax, Ntree = 1000,
# n.cores = 10, cluster.ips = NULL)
# 
# saveRDS(trees, file = "trees_1000_610sp.rds")
# 
# all(sapply(trees, is.ultrametric))
# L2 <- sapply(trees,
#            function(x) min(x$edge.length))
# all(L2 > 0)
# L3 <- sapply(trees, function(x) length(x$tip.label))
# all(L3 == (9993 - 12 + nrow(AvotrexPhylo)))
# 
# #Check ages of fixed age grafting species
# YY <- c("Genyornis_newtoni", "Upupa_antaios",
#         "Sylviornis_neocaledoniae", "Aepyornis_maximus",
#         "Nannococcyx_psix", "Dromaius_minor",
#         "Dromaius_baudinianus", "Chaetoptila_sp_Maui",
#         "Psilopterus_sp_Uruguay", "Traversia_lyalli",
#         "Xenicus_longipes", "Pachyplichas_yaldwyni",
#         "Xenicus_gilviventris", "Pachyornis_geranoides")
# 
# 
# nn <- sample(1:1000, 50)
# BLT <- sapply(nn, function(z){
# nodes <- sapply(YY,
#                 function(x,y) which(y==x),
#                 y=trees[[z]]$tip.label)
# 
# setNames(trees[[z]]$edge.length[sapply(nodes,
# function(x,y) which(y==x),y=trees[[z]]$edge[,2])],names(nodes))
# })
# 
# round(BLT, 2)

test_that("Main function errors correctly", {

  skip_on_cran()

  data(BirdTree_trees)
  data(BirdTree_tax)
  data(AvotrexPhylo)

expect_error(AvoPhylo(ctrees = 5,
                      avotrex = AvotrexPhylo,
                      tax = BirdTree_tax))

expect_error(AvoPhylo(ctrees = BirdTree_trees,
                      avotrex = 5,
                      tax = BirdTree_tax))

expect_error(AvoPhylo(ctrees = BirdTree_trees,
                      avotrex = AvotrexPhylo,
                      tax = 5))

expect_error(AvoPhylo(ctrees = BirdTree_trees,
                      avotrex = AvotrexPhylo,
                      PER = 1.2, PER_FIXED = 0.75,
                      tax = BirdTree_tax))

expect_error(AvoPhylo(ctrees = BirdTree_trees,
                      avotrex = AvotrexPhylo,
                      PER = 0.2, PER_FIXED = 0.75,
                      tax = BirdTree_tax, Ntree = 6))


expect_error(AvoPhylo(ctrees = BirdTree_trees,
                      avotrex = AvotrexPhylo,
                      PER = 0.2, PER_FIXED = 0.75,
                      tax = BirdTree_tax,
                      ord = c(FALSE, TRUE),
                      Ntree = 1, n.cores = 1))

expect_error(AvoPhylo(ctrees = BirdTree_trees,
                      avotrex = AvotrexPhylo,
                      PER = 0.2, PER_FIXED = 0.75,
                      tax = BirdTree_tax))

expect_error(AvoPhylo(ctrees = BirdTree_trees,
                      avotrex = AvotrexPhylo,
                      PER = 0.7, PER_FIXED = 0.75,
                      tax = BirdTree_tax, Ntree = 1))

AvotrexPhylo$time_fixed[1] = "A"
expect_error(AvoPhylo(ctrees = BirdTree_trees,
                      avotrex = AvotrexPhylo,
                      PER = 0.2, PER_FIXED = 0.75,
                      tax = BirdTree_tax))
AvotrexPhylo$time_fixed[1] = "NA"


AvotrexPhylo2 <- AvotrexPhylo
AvotrexPhylo2$type[1] <- "P"
expect_error(AvoPhylo(ctrees = BirdTree_trees,
                      avotrex = AvotrexPhylo2,
                      PER = 0.2, PER_FIXED = 0.75,
                      tax = BirdTree_tax, Ntree = 6))

})

test_that("Output makes sense: 1 input tree", {

  skip_on_cran()

  trees <- AvoPhylo(ctrees = BirdTree_trees[[1]],
                    avotrex = AvotrexPhylo,
                    PER = 0.2, PER_FIXED = 0.75,
                    tax = BirdTree_tax,
                    Ntree = 1, n.cores = 1,
                    cluster.ips = NULL)

  expect_true(is.list(trees))
  
  expect_true(is.ultrametric(trees))
  
  expect_false(any(trees[[1]]$edge.length <= 0))

  expect_is(trees, c("multiAvophylo",
                                "multiPhylo"))
  expect_true(length(trees) == 1)

  Ntip <- length(BirdTree_trees[[1]]$tip.label) +
    nrow(AvotrexPhylo) -
    (length(which(AvotrexPhylo$type == "AP")))

  expect_identical(length(trees[[1]]$tip.label), Ntip)

  expect_true(all(AvotrexPhylo$species %in%
                    trees[[1]]$tip.label))

  expect_true(all(BirdTree_trees[[1]]$tip.label %in%
                    trees[[1]]$tip.label))

  expect_true(sum(trees[[1]]$edge.length) >
                sum(BirdTree_trees[[1]]$edge.length))
  
  
  #test_that "time_fixed grafting is working"
  
  YY <- c("Genyornis_newtoni", "Upupa_antaios", 
          "Sylviornis_neocaledoniae", "Aepyornis_maximus",
          "Dromaius_minor",
          "Dromaius_baudinianus",
          "Xenicus_gilviventris")
  
  nodes <- sapply(YY,function(x,y) which(y==x),
                  y=trees[[1]]$tip.label)
  
  SNt1 <- setNames(trees[[1]]$edge.length[sapply(nodes,
                                                 function(x,y) which(y==x),
                                                 y=trees[[1]]$edge[,2])],
                   names(nodes))
  
  SNt1 <- as.vector(round(SNt1, 2))
  
  expect_identical(SNt1, c(70, 14, 32, 3.31,
                           0.01, 0.01, 4))

})


test_that("Output makes sense: > 1 input tree", {

  skip_on_cran()

  trees2 <- AvoPhylo(ctrees = BirdTree_trees,
                    avotrex = AvotrexPhylo,
                    PER = 0.4, PER_FIXED = 0.8,
                    tax = BirdTree_tax,
                    Ntree = 2, n.cores = 1,
                    cluster.ips = NULL)

  expect_true(is.list(trees2))
  
  z <- sapply(trees2, is.ultrametric)
  expect_true(all(z))
  
  expect_false(any(sapply(trees2,
                          function(x) any(x$edge.length <= 0))))
  
  expect_is(trees2, c("multiAvophylo",
                     "multiPhylo"))
  expect_true(length(trees2) == 2)

  Ntip <- length(BirdTree_trees[[2]]$tip.label) +
    nrow(AvotrexPhylo) - (length(which(AvotrexPhylo$type == "AP")))

  expect_identical(length(trees2[[2]]$tip.label), Ntip)

  expect_true(all(AvotrexPhylo$species %in% trees2[[2]]$tip.label))

  expect_true(all(BirdTree_trees[[1]]$tip.label %in% trees2[[1]]$tip.label))

  expect_true(sum(trees2[[2]]$edge.length) > sum(BirdTree_trees[[2]]$edge.length))

  ##Checking plotting functions
  
  expect_no_error(plot(trees2[[1]],
                       avotrex = AvotrexPhylo, 
                       tax = BirdTree_tax,
       genus = "Aplonis", tips = "extinct",
       type = "cladogram",
       tip.color = "red", cex = 0.5))

  expect_no_error(plot(trees2[[1]],
                       avotrex = AvotrexPhylo, 
                       tax = BirdTree_tax,
                       tips = "none"))
  
  species2 <- c("Anas_itchtucknee", "Anas_sp_VitiLevu",
                "Anas_platyrhynchos", "Ara_tricolor")
  
  expect_no_error(plot(trees2[[2]], 
                       avotrex = AvotrexPhylo, 
       tax = BirdTree_tax,
       species = species2, tips = "all_same",
       type = "cladogram",
       tip.color = "blue", cex = 0.5))
  
  expect_no_error(plot(trees2[1:2],
                       avotrex = AvotrexPhylo, 
                       tax = BirdTree_tax,
                       family = "Threskiornithidae", 
                       tips = "extinct",
                       tip.color = "red", 
                       cex = 0.5))
  
  expect_error(plot(trees2[[2]], 
                    avotrex = AvotrexPhylo, 
                    tax = BirdTree_tax,
                    species = species2, 
                    tips = "all_diff"))

})
