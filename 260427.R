x <- c(2, 4, 6, 8, 10, 12, 14, 16, 18, 20)
y <- c(2, 4, 2, 4, 2, 4, 2, 4, 2, 4)
plot(x,y)
abline(h=3, col='red')
summary(lm(y ~ x)) # p-value가 5%보다 크면 사용 불가
df1 <- data.frame(x,y)
View(df1)

reg1 <- lm(y ~ x, data = df1)
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

#################################

# 유의수준 5%

state <- as.data.frame(state.x77)
x1 <- state["Population"]
x2 <- state["Income"]
x3 <- state["Illiteracy"]
x4 <- state["Life Exp"]
x5 <- state["Frost"]
y <- state$Murder
df <- data.frame(x1, x2, x3, x4, x5, y)
reg1 <- lm(y ~ ., data = df)
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
