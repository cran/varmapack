## ----include = FALSE----------------------------------------------------------
knitr::opts_chunk$set(collapse = TRUE, comment = "#>")

## ----setup--------------------------------------------------------------------
library(varmapack)
library(randompack)

## ----varma--------------------------------------------------------------------
A <- matrix(c(0.5, 0.1,
              0.0, 0.3), 2, 2)
B <- matrix(c(0.2, 0.0,
              0.1, 0.1), 2, 2)
model <- varmapack_model(A = A, B = B, Sig = diag(2))

rng <- randompack_rng()
rng$seed(123)
X <- model$sim(100, nrep = 3, rng = rng)
dim(X)

## ----shocks-------------------------------------------------------------------
out <- model$sim(20, nrep = 2, rng = rng, return_shocks = TRUE)
names(out)
dim(out$E)

## ----testcases----------------------------------------------------------------
varmapack_testcases()
test_model <- varmapack_testcase("smallARMA1")
test_model$specrad()

## ----analysis-----------------------------------------------------------------
Gamma <- model$acvf(10)
Psi <- model$psi(10)
Theta <- model$irf(10)
model$specrad()
model$ma_specrad()

## ----autocov------------------------------------------------------------------
varmapack_autocov(X[, , 1], maxlag = 5)

## ----varmax-------------------------------------------------------------------
C <- array(c(0.3, -0.2), c(2, 1, 1))
varmax <- varmapack_model(A = A, B = B, C = C, Sig = diag(2))
X0 <- matrix(0, 2, 2)
z <- matrix(sin(seq_len(100)/10), 1, 100)
Xmax <- varmax$sim(100, X0 = X0, z = z, rng = rng)

