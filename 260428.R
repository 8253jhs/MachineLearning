# 비선형회귀분석

load("nlreg.RData")
plot(p1, pch=16, col="blue", cex=1.5)
lreg1 <- lm(y ~ ., data = p1)
summary(lreg1)
abline(lreg1, col="red", lwd=2)

p1$x2 <- p1$x1^2
nlreg1 <- lm(y ~ ., data = p1)
summary(nlreg1)
nlreg1
curve(33.6109-9.2954*x+0.9337*x^2,
      col="orange", lwd=2, add=T)

predict(nlreg1, data.frame(x1=9, x2=81))
abline(v=9, lty=2)
abline(h=predict(nlreg1, data.frame(x1=9, x2=81)),
       lty=2)

##################################################

plot(p2, pch=16, col="blue", cex=1.5)
lreg2 <- lm(y ~ ., data = p2)
summary(lreg2)
abline(lreg2, col="red", lwd=2)

names(p2)[1] <- "x1"
p2$x2 <- p2$x1^2
nlreg2 <- lm(y ~ ., data = p2)
summary(nlreg2)
nlreg2
curve(-6.6742+11.7640*x-0.6345*x^2,
      col="orange", lwd=2, add=T)

predict(nlreg2, data.frame(x1=15, x2=225))
abline(v=15, lty=2)
abline(h=predict(nlreg2, data.frame(x1=15, x2=225)),
       lty=2)

####################################################

plot(p3, pch=16, col="blue", cex=1.5)
lreg3 <- lm(y ~ ., data = p3)
summary(lreg3)
abline(lreg3, col="red", lwd=2)

p3$x2 <- sin(p3$x1)
nlreg3 <- lm(y ~ ., data = p3)
summary(nlreg3)
nlreg3
curve(0.2892+1.9816*x+3.7738*sin(x),
      col="orange", lwd=2, add=T)

predict(nlreg3, data.frame(x1=14, x2=sin(14)))
abline(v=14, lty=2)
abline(h=predict(nlreg3, data.frame(x1=14, x2=sin(14))),
       lty=2)

########################################################

p4 <- data.frame(x = 1:8,
                 y = c(1.701, 1.153, 0.909, 1.001,
                       0.945, 0.828, 0.861, 0.893))
plot(p4, pch=16, col="blue", cex=1.5)

x_prime <- -1/p4$x
y_prime <- 1/p4$y
p4_prime <- data.frame(x_prime, y_prime)
plot(p4_prime, pch=16, col="green", cex=1.5)
nlreg4 <- lm(y_prime ~ ., data = p4_prime)
summary(nlreg4)
abline(nlreg4, col="red", lwd=2)

nlreg4
coef(nlreg4)
w0_hat <- coef(nlreg4)[1]
w1_hat <- coef(nlreg4)[2]
plot(p4, pch=16, col="blue", cex=1.5)
curve(x/(w0_hat*x-w1_hat),
      col="orange", lwd=2, add=T)
abline(v=2.5, lty=2)
abline(h=2.5/(w0_hat*2.5-w1_hat),lty=2)

#######################################

plot(p5, pch=16, col="blue", cex=1.5)
