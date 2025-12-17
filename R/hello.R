# Hello, world!
#
# This is an example function named 'hello'
# which prints 'Hello, world!'.
#
# You can learn more about package authoring with RStudio at:
#
#   https://r-pkgs.org
#
# Some useful keyboard shortcuts for package authoring:
#
#   Install Package:           'Ctrl + Shift + B'
#   Check Package:             'Ctrl + Shift + E'
#   Test Package:              'Ctrl + Shift + T'

hello <- function() {
  print("Hello, world!")
}


whittaker_henderson <- function(y, lambda = 100, d = 2, w = NULL) {
  n <- length(y)
  if (is.null(w)) w <- rep(1, n)
  W <- diag(w)
  K <- diff(diag(n), differences = d)
  A <- W + lambda * t(K) %*% K
  b <- W %*% y
  y_lisse <- solve(A, b)
  return(as.vector(y_lisse))
}

## Test
