install.packages("glmnet")
install.packages("caret")
library("glmnet")
library("caret")

state <- as.data.frame(state.x77)
str(state)
set.seed(1111)
sam <- createDataPartition(state$Murder, p=0.8, list=F)
train_data <- state[sam,]
test_data <- state[-sam,]
View(test_data)


# 일반선형회귀(OLS)
lsreg <- lm(Murder ~ ., data = train_data)
w_hat_OLS <- round(coef(lsreg), 5)
y_hat_OLS <- predict(lsreg, test_data[,-5])
SSE_OLS <- sum((test_data[,5]-y_hat_OLS)^2)


# 능형회귀분석(Ridge)
x_train <- as.matrix(train_data[,-5])
y_train <- as.matrix(train_data[,5])
x_test <- as.matrix(test_data[,-5])
y_test <- as.matrix(test_data[,5])

lambdas <- seq(0, 10, 0.1)
cv_fit <- cv.glmnet(x_train, # 학습데이터 입력변수
                    y_train, # 학습데이터 출력변수
                    alpha = 0, # 0은 능형회귀
                    lambda= lambdas)
best_lambda <- cv_fit$lambda.min
rdreg <- glmnet(x_train,
                y_train,
                alpha = 0,
                lambda = best_lambda)
plot(cv_fit)
w_hat_RD <- round(coef(rdreg), 5)
y_hat_RD <- predict(rdreg, x_test)
SSE_RD <- sum((test_data[,5]-y_hat_RD)^2)


# 라쏘회귀(LASSO)
cv_fit <- cv.glmnet(x_train, # 학습데이터 입력변수
                    y_train, # 학습데이터 출력변수
                    alpha = 1, # 1은 라쏘회귀
                    lambda= lambdas)
best_lambda <- cv_fit$lambda.min
lassoreg <- glmnet(x_train,
                y_train,
                alpha =1,
                lambda = best_lambda)
plot(cv_fit)
w_hat_LASSO <- round(coef(lassoreg), 5)
y_hat_LASSO <- predict(lassoreg, x_test)
SSE_LASSO <- sum((test_data[,5]-y_hat_LASSO)^2)


# 엘라스틱넷 회귀(Elastic Net)
cv_fit <- cv.glmnet(x_train, # 학습데이터 입력변수
                    y_train, # 학습데이터 출력변수
                    alpha = 0.4, # 0과 1 사이는 엘라스틱넷 회귀
                    lambda= lambdas)
best_lambda <- cv_fit$lambda.min
ENreg <- glmnet(x_train,
                y_train,
                alpha = 0.4,
                lambda = best_lambda)
plot(cv_fit)
w_hat_EN <- round(coef(ENreg), 5)
y_hat_EN <- predict(ENreg, x_test)
SSE_EN <- sum((test_data[,5]-y_hat_EN)^2)

# SST는 4개의 모델 전부 동일

#########################################

load("nlreg.RData")
plot(p1)

p1$x2 <- p1$x1^2
p1$x3 <- p1$x1^3
p1$x4 <- p1$x1^4
p1$x5 <- sin(p1$x1)

set.seed(1111)
sam <- createDataPartition(p1$y, p=0.8, list=F)
train_data <- p1[sam,]
test_data <- p1[-sam,]


lsreg <- lm(y ~ ., data = train_data)
w_hat_OLS <- round(coef(lsreg), 5)
y_hat_OLS <- predict(lsreg, test_data[,-2])
SSE_OLS <- sum((test_data[,2]-y_hat_OLS)^2)


# 능형회귀분석(Ridge)
x_train <- as.matrix(train_data[,-2])
y_train <- as.matrix(train_data[,2])
x_test <- as.matrix(test_data[,-2])
y_test <- as.matrix(test_data[,2])

lambdas <- seq(0, 100, 0.1)
cv_fit <- cv.glmnet(x_train, # 학습데이터 입력변수
                    y_train, # 학습데이터 출력변수
                    alpha = 0, # 0은 능형회귀
                    lambda= lambdas)
best_lambda <- cv_fit$lambda.min
rdreg <- glmnet(x_train,
                y_train,
                alpha = 0,
                lambda = best_lambda)
plot(cv_fit)
w_hat_RD <- round(coef(rdreg), 5)
y_hat_RD <- predict(rdreg, x_test)
SSE_RD <- sum((test_data[,2]-y_hat_RD)^2)


# 라쏘회귀(LASSO)
cv_fit <- cv.glmnet(x_train, # 학습데이터 입력변수
                    y_train, # 학습데이터 출력변수
                    alpha = 1, # 1은 라쏘회귀
                    lambda= lambdas)
best_lambda <- cv_fit$lambda.min
lassoreg <- glmnet(x_train,
                   y_train,
                   alpha =1,
                   lambda = best_lambda)
plot(cv_fit)
w_hat_LASSO <- round(coef(lassoreg), 5)
y_hat_LASSO <- predict(lassoreg, x_test)
SSE_LASSO <- sum((test_data[,2]-y_hat_LASSO)^2)


# 엘라스틱넷 회귀(Elastic Net)
cv_fit <- cv.glmnet(x_train, # 학습데이터 입력변수
                    y_train, # 학습데이터 출력변수
                    alpha = 0.4, # 0과 1 사이는 엘라스틱넷 회귀
                    lambda= lambdas)
best_lambda <- cv_fit$lambda.min
ENreg <- glmnet(x_train,
                y_train,
                alpha = 0.4,
                lambda = best_lambda)
plot(cv_fit)
w_hat_EN <- round(coef(ENreg), 5)
y_hat_EN <- predict(ENreg, x_test)
SSE_EN <- sum((test_data[,2]-y_hat_EN)^2)