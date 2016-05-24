library(testr)
library(testthat)

context("Corner cases")

context("unittests")

a <- function(a, b) {
    a + b
}

b <- function() {

}

d <- function() {
    e <- function() {

    }
    e
}


test_that('list_functions', {
    x <- list_functions("test-corner.R")
    expect_true("a" %in% x)
    expect_true("b" %in% x)
    expect_true("d" %in% x)
    expect_true(length(x) == 3)
})

# test_that('Missing argument corner case is fixed', {
#   # was failing before 5fe109579d670a0ebf094215398410ca08c3749b
#   suppressMessages(capture('qr.X'))
#   p <- ncol(x <- LifeCycleSavings[,-1]) # not the `sr'
#   qrstr <- qr(x)   # dim(x) == c(n,p)
#   X <- qr.X(qrstr) # X == x
#   dim(Xc <- qr.X(qrstr, complete=TRUE)) # square: nrow(x) ^ 2
#   dimnames(X) <- NULL
#   expect_true(all.equal(Xc[,1:p], X))
#
#   # was failing before 5fe109579d670a0ebf094215398410ca08c3749b
#   suppressMessages(capture('svd'))
#   Meps <- .Machine$double.eps
#   Eps <- 100 * Meps
#   hilbert <- function(n) { i <- 1:n; 1 / outer(i - 1, i, "+") }
#   X <- hilbert(9)[,1:6]
#   s <- svd(X)
#   D <- diag(s$d)
#   expect_true(all(abs(X - s$u %*% D %*% t(s$v)) < Eps))#  X = U D V'
#   expect_true(all(abs(D - t(s$u) %*% X %*% s$v) < Eps))#  D = U' X V
# })


