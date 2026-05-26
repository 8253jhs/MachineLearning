f <- -(0.75*log2(0.75) + 0.25*log2(0.25))

#########################################

x <- c(1, 2, 4, 5)
y <- c(0, 0, 1, 1)
df <- data.frame(x, y)
plot(df, pch=16, cex=2, col=df$y+2)
logistic1 <- glm(y ~ x, data = df,
                 family = "binomial")
coef(logistic1)
w0_hat <- coef(logistic1)[1]
w1_hat <- coef(logistic1)[2]
curve(1/(1+exp(-(w0_hat+w1_hat*x))),
      col="blue", lwd = 2, add = T)
abline(h=0.5, lty=2)

###################################

set.seed(11)
x <- c(runif(20,0,6), runif(20,5,9))
y <- rep(c(0,1),each=20)
df <- data.frame(x,y)
df <- df[sample(1:40,40),]
row.names(df) <- 1:40

plot(df, pch=16, cex=2, col=df$y+2)
logistic2 <- glm(y ~ x, data = df,
                 family = "binomial")
coef(logistic2)
w0_hat <- coef(logistic2)[1]
w1_hat <- coef(logistic2)[2]
curve(1/(1+exp(-(w0_hat+w1_hat*x))),
      col="blue", lwd = 2, add = T)
abline(h=0.5, lty=2)
library(rootSolve)
eq <- function(x)
  1/(1+exp(-(w0_hat+w1_hat*x)))-0.5
abline(v = uniroot(eq, c(4, 6))$root, lty=2)

df$y_prob <- round(predict(logistic2, df, type = "response"), 5)
df$y_hat <- ifelse(df$y_prob > 0.5, 1, 0)
df[df$y != df$y_hat, ]
