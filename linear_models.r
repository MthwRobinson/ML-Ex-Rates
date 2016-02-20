
library(class)
library(MASS)
library(pls)
library(lars)
library(lattice)

setwd('/home/matt/MSOR/ISYE7406/ML-Ex-Rates/Data')
ex_rate <- read.csv('oecd.csv')
country <- 'japan'

df <- subset(ex_rate,ex_rate$Country.y==country)
df_set <- subset(df, select = -c(X,Country.x,Country.y,Date,Yield_f,
                                 PrimeRate_f,FDI_f,Exchange_fut))
df_set <- na.omit(df_set)
# Note: Exchange, Exchange_fut and Pct_Change are cols 17 - 19

# Split into training and testing sets
n <- dim(df_set)[1]; n
n1 <- round(n/5); n1
set.seed(19890211)
flag <- sort(sample(1:n,n1))
#train <- df_set[-flag,]; test <- df_set[flag,]
train <- df_set[1:(n-n1),]; test <- df_set[(n-n1+1):n,]

# Linear Regression with all predictors

linreg <- lm(Pct_Chg ~ . , data = train)
summary(linreg)
linreg.test <- predict.lm(linreg,test[,-18])
linreg.TE <- mean( (test[,18] - linreg.test)^2 )
linreg.TE

# Stepwise variable selection
regmodel <- lm(Pct_Chg ~ ., data = train)
min.model = lm(Pct_Chg ~ 1, data = train)
max_model <- formula(regmodel)
linreg_AIC = step(min.model, direction = 'forward', 
                  scope = max_model, trace = FALSE)
# The variables save by stepwise selection were:
# Infl_d, Exchange, CA_d, CA_f, FDI_d, X1Y_Yield_f,
#  GPDP_f, Yield_d, BOT_d, FER_f, FER_d, Infl_f,
#  BOT_f, Int_f
summary(linreg_AIC)
linreg_AIC.test <- predict.lm(linreg_AIC,test[,-18])
linreg_AIC.TE <- mean( (test[,18]-linreg_AIC.test)^2)
linreg_AIC.TE

# Ridge Regression
ridgereg <- lm.ridge(Pct_Chg ~ Infl_d + Exchange + CA_d + CA_f + FDI_d + 
    X1Y_Yield_f + GDPG_f + Yield_d + BOT_d + FER_f + FER_d + 
    Infl_f + BOT_f + Int_f, data = train, lambda = seq(0,200,0.01))
lambdaopt <- which.min(ridgereg$GCV)
ridge_coef <- coef(ridgereg)[lambdaopt,]
test_ridge <- subset(test, select = c(Pct_Chg,Infl_d,Exchange,
                CA_d,CA_f,FDI_d,X1Y_Yield_f,GDPG_f,Yield_d,BOT_d,
                 FER_f,FER_d,Infl_f,BOT_f,Int_f))
ridge_testmat <- as.matrix(cbind(1,test_ridge[,-1]))
ridge.test <- as.matrix(ridge_testmat %*% ridge_coef)
ridge.TE <- mean( (test_ridge[,1] - ridge.test)^2)
ridge.TE

# LASSO Regression
lassotrain <- subset(train, select = c(Infl_d,Exchange,
                CA_d,CA_f,FDI_d,X1Y_Yield_f,GDPG_f,Yield_d,BOT_d,
                 FER_f,FER_d,Infl_f,BOT_f,Int_f))
lassotest <- subset(test, select = c(Infl_d,Exchange,
                CA_d,CA_f,FDI_d,X1Y_Yield_f,GDPG_f,Yield_d,BOT_d,
                 FER_f,FER_d,Infl_f,BOT_f,Int_f))
lassoreg <- lars( x = as.matrix(lassotrain), 
                 y = as.matrix(train[,18]), type = 'lasso')
Cp1 <- summary(lassoreg)$Cp
index1 <- which.min(Cp1)
lasso_coef <- coef(lassoreg)[index1]
lasso_lambda <- lassoreg$lambda[index1]
lassoreg.fit <- predict(lassoreg, as.matrix(lassotest),
                   s = lasso_lambda, type = 'fit', mode = 'lambda')
lassoreg.test <- lassoreg.fit$fit
lassoreg.TE <- mean(( test[,18] - lassoreg.test)^2)
lassoreg.TE

# Principal Component regression
pcreg <- pcr(Pct_Chg ~ . , data = train, validation = 'CV')
pc.ncompopt <- which.min(pcreg$validation$adj)
pcreg.test <- predict(pcreg, ncomp = pc.ncompopt,
                     newdata = test[,-18])
pcreg.TE <- mean ( (test[,18] - pcreg.test)^2)
pcreg.TE

# Partial Least Squares
plsreg <- plsr(Pct_Chg ~ ., data = train, validation = 'CV')
pls.ncompopt <- plsreg$ncomp
plsreg.test <- predict(plsreg, ncomp = pls.ncompopt,
                      newdata = test[,-18])
plsreg.TE <- mean( (test[,18]-plsreg.test)^2)
plsreg.TE

# Naive
naive.TE <- mean ( (test[,18])^2)
naive.TE


