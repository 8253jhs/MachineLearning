x1 <- runif(100)
x2 <- runif(100)
x3 <- runif(100)
x4 <- runif(100)
y <- x1 + x2 + x3 + x4 + rnorm(100)
X <- cbind(x1, x2, x3)
w_LS <- solve(t(X) %*% X) %*% t(X) %*% y
w_Ridge <- solve(t(X) %*% X + 10^30*diag(3)) %*% t(X) %*% y
