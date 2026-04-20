x1 <- c(8, 4, 6, 2, 5, 7, 10)
x3 <- c(0.2, 0.96, 0.99, 0.56, 0.78, 0.98, 0.09)
y <- c(45.42, 44.24, 80.97, 32.87, 17.61, 56.37, 95.66)

df <- data.frame(x1, x3, y)

reg <- lm(y ~ .+0, data = df)

predict(reg, data.frame(x1 = 2, x3 = 0.2))



set.seed(1)
x1 <- sample(2:30, 25)
set.seed(2)
x2 <- sample(36:1460, 25)
set.seed(3)
y <- round(2.341 + 1.6159*x1 + 0.015*x2 + rnorm(25,0,3.25),2)
df1 <- data.frame(x1,x2,y)
View(df1)

reg1 <- lm(y ~ ., data = df1)
y <- df1$y
y_bar <- mean(y)
y_hat <- reg1$fitted.values
SSE <- sum((y - y_hat)^2)
SSR <- sum((y_hat - y_bar)^2)
SST <- sum((y - y_bar)^2) # SSR + SSE
SSE
SSR
SST
r2 <- SSR/SST
summary(reg1)



x1 <- c(14, 16, 13, 10, 18, 17, 16, 15, 11, 10)
x2 <- c(37, 43, 38, 42, 36, 33, 40, 35, 34, 29)
y <- c(850, 970, 730, 940, 920, 830, 940, 900, 760, 710)
df1 <- data.frame(x1,x2,y)
View(df1)

reg1 <- lm(y ~ ., data = df1)
y <- df1$y
y_bar <- mean(y)
y_hat <- reg1$fitted.values
SSE <- sum((y - y_hat)^2)
SSR <- sum((y_hat - y_bar)^2)
SST <- sum((y - y_bar)^2) # SSR + SSE
SSE
SSR
SST
r2 <- SSR/SST
summary(reg1)
