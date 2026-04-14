x1 <- c(1, 2, 3, 5, 4, 2)
x2 <- c(1, 1, 2, 1, 2, 3)
y <- c(3, 3, 5, 5, 6, 4)

df1 <- data.frame(x1, x2, y)
X <- as.matrix(cbind(1, df1[, 1:2]))
y <- df1$y
w_hat <- solve(t(X) %*% X) %*% t(X) %*% y


reg1 <- lm(y ~ ., data = df1)

new <- data.frame(x1 = c(1, 3), x2 = c(2, 4))
predict(reg1, new)

#############################################

set.seed(1)
x1 <- sample(2:30, 25)
set.seed(2)
x2 <- sample(36:1460, 25)
set.seed(3)
y <- round(2.341 + 1.6159*x1 + 0.015*x2 + rnorm(25,0,3.25),2)
df1 <- data.frame(x1,x2,y)
View(df1)

reg2 <- lm(y ~ ., data = df1)
sum(reg2$residuals)

w <- c(0, 0, 0)
X <- as.matrix(cbind(1, df1[, 1:2]))
y <- df1$y
alpha <- 0.0000001
for (i in 1:100000)
  w <- w - alpha*t(X) %*% (X %*% w - y)/length(y)
w

#################################

state <- as.data.frame(state.x77)

#################################

f <- function(x) x^2 - 10*x + 26
curve(f, 2, 8, lwd=2)
points(5, 1, pch=16, cex=3, col="blue")
points(7, f(7), pch=16, cex=3, col="red")
alpha <- 0.01
x <- 7
library(Deriv)
grad <- Deriv(f, "x")
for (i in 1:200) {
  x <- x - alpha*grad(x)
  points(x, f(x), pch=16, cex=3, col="green")
  #Sys.sleep(1)
  }