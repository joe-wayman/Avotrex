context("avotrex tests")
library(avotrex)
library(ape)

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

AvotrexPhylo2 <- AvotrexPhylo
AvotrexPhylo2$Type[1] <- "P"
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

  expect_is(trees, c("multiAvophylo",
                                "multiPhylo"))
  expect_true(length(trees) == 1)

  Ntip <- length(BirdTree_trees[[1]]$tip.label) +
    nrow(AvotrexPhylo) -
    (length(which(AvotrexPhylo$Type == "AP")))

  expect_identical(length(trees[[1]]$tip.label), Ntip)

  expect_true(all(AvotrexPhylo$species %in%
                    trees[[1]]$tip.label))

  expect_true(all(BirdTree_trees[[1]]$tip.label %in%
                    trees[[1]]$tip.label))

  expect_true(sum(trees[[1]]$edge.length) >
                sum(BirdTree_trees[[1]]$edge.length))

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

  expect_is(trees2, c("multiAvophylo",
                     "multiPhylo"))
  expect_true(length(trees2) == 2)

  Ntip <- length(BirdTree_trees[[2]]$tip.label) +
    nrow(AvotrexPhylo) - (length(which(AvotrexPhylo$Type == "AP")))

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
