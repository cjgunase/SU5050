install.packages("raster")
library(raster)
## Loading required package: sp
## Warning: package 'sp' was built under R version 3.0.2
install.packages("gstat")
library(gstat)
install.packages("lattice")
library(lattice)
install.packages("ncf")
library(ncf)
set.seed(45)
# Define function to draw random samples from a multivariate normal
# distribution
rmvn <- function(n, mu = 0, V = matrix(1)) {
  p <- length(mu)
  if (any(is.na(match(dim(V), p)))) 
    stop("Dimension problem!")
  D <- chol(V)
  t(matrix(rnorm(n * p), ncol = p) %*% D + rep(mu, rep(n, p)))
}

# Set up a square lattice region
simgrid <- expand.grid(1:50, 1:50)
n <- nrow(simgrid)

# Set up distance matrix
distance <- as.matrix(dist(simgrid))
# Generate random variable
phi <- 0.05
X <- rmvn(1, rep(0, n), exp(-phi * distance))

# Visualize results
Xraster <- rasterFromXYZ(cbind(simgrid[, 1:2] - 0.5, X))
par(mfrow = c(1, 2))
plot(1:100, exp(-phi * 1:100), type = "l", xlab = "Distance", ylab = "Correlation")
plot(Xraster)