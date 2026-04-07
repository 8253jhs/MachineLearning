x <- 10*c(138, 312, 352, 113, 103, 172, 392, 149,
          186, 343, 200, 366, 250, 122, 139)
y <- c(76, 216, 238, 69, 50, 119, 282, 81,
       132, 228, 145, 251, 170, 71, 29)
df1 <- data.frame(x, y)
plot(df1, xlab="주택 면적", ylab="주택 가격",
     pch=16, cex=2)
cov(df1)

reg1 <- lm(y ~ x, data = df1)
abline(reg1, col="red", lwd=2)
x <- 2227
-29.58796 + 0.07794 * x
predict(reg1, data.frame(x = c(2227, 3000)))

reg2 <- lm(x ~ y, data = df1)
predict(reg2, data.frame(y = 150))

df1$y_hat <- reg1$fitted.values
df1$잔차 <- reg1$residuals
df1
sum(df1$잔차)



x <- 1:10
y <- c(3, 3, 3, 6, 6, 9, 9, 9, 10, 11)
plot(x, y)
reg3 <- lm(y ~ x)
abline(reg3, col="blue")
predict(reg3, data.frame(x = 11:20))
sum(reg3$residuals)

x <- c(36.5, 28.0, 42.9, 52.0, 51.5,
       53.8, 25.4, 37.2, 50.9, 29.2)
y <- c(14, 9, 15, 20, 21,
       25, 9, 13, 20, 10)
df1 <- data.frame(x, y)
plot(df1, xlab="광고비", ylab="신규고객",
     pch=10, cex=2)
reg4 <- lm(x ~ y, data = df1)
abline(reg4, col="green", lwd=2)
predict(reg4, data.frame(y = 17))

