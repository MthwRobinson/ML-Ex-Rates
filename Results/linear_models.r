library(class)
library(MASS)
library(pls)
library(lars)
library(lattice)

setwd('/home/matt/MSOR/ISYE7406/ML-Ex-Rates/Data')
ex_rate <- read.csv('oecd.csv')

test_lin_models('canada',100)
test_lin_models('europe',100)
test_lin_models('mexico',100)
test_lin_models('japan',100)
test_lin_models('korea',100)

test_lin_models <- function(country, B){
        df <- subset(ex_rate,ex_rate$Country.y==country)
        df_set <- subset(df, 
                    select = -c(X,Country.x,Country.y,Date,Yield_f,
                                 PrimeRate_f,FDI_f,Exchange_fut))
        df_set <- na.omit(df_set)
        # Note: Exchange, Exchange_fut and Pct_Change are cols 17 - 19

        # Split into training and testing sets
        n <- dim(df_set)[1]; n
        n1 <- round(n/5); n1
        B <- 100
        set.seed(19890211)
        train <- df_set[1:(n-n1),]; test <- df_set[(n-n1+1):n,]
        
        # AIC Stepwise
        linreg_AIC.TE_list <- vector(length=0)
        for(i in 1:B){
            start <- sample(1:(n-n1-1),1)
            flag <- (start):(start+n1)
            train <- df_set[-flag,]; test <- df_set[flag,]
    
            regmodel <- lm(Pct_Chg ~ ., data = train)
            min.model = lm(Pct_Chg ~ 1, data = train)
            max_model <- formula(regmodel)
            linreg_AIC = step(min.model, direction = 'forward', 
                  scope = max_model, trace = FALSE)
            linreg_AIC.test <- predict.lm(linreg_AIC,test[,-18])
            linreg_AIC.TE <- mean( (test[,18]-linreg_AIC.test)^2)
            linreg_AIC.TE_list <- c(linreg_AIC.TE_list, linreg_AIC.TE)
    
        }    
    
        # Ridge Regression
        ridge.TE_list <- vector(length=0)
        for(i in 1:B){
            start <- sample(1:(n-n1-1),1)
            flag <- (start):(start+n1)
            train <- df_set[-flag,]; test <- df_set[flag,]
    
            ridgereg <- lm.ridge(Pct_Chg ~ Exchange + X1Y_Yield_d + 
                      X1Y_Yield_f + Int_d + Int_f +  Infl_d + Infl_f, 
                    data = train, lambda = seq(0,10,0.001))
            lambdaopt <- which.min(ridgereg$GCV)
            ridge_coef <- coef(ridgereg)[lambdaopt,]
            test_ridge <- subset(test, select = c(Pct_Chg, Exchange,
                   X1Y_Yield_d, X1Y_Yield_f, Int_d, Int_f, 
                    Infl_d, Infl_f))
            ridge_testmat <- as.matrix(cbind(1,test_ridge[,-1]))
            ridge.test <- as.matrix(ridge_testmat %*% ridge_coef)
            ridge.TE <- mean( (test_ridge[,1] - ridge.test)^2)
            ridge.TE_list <- c(ridge.TE_list, ridge.TE)
    
        }
    
        # Naive
        naive.TE_list <- vector(length=0)
        for(i in 1:B){
            start <- sample(1:(n-n1-1),1)
            flag <- (start):(start+n1)
            train <- df_set[-flag,]; test <- df_set[flag,]
            naive.TE <- mean ( (test[,18])^2)
            naive.TE_list <- c(naive.TE_list,naive.TE)
        }
    
        output <- c('Measure','Step','Ridge','Naive')
        output <- rbind(output, c('Mean', mean(linreg_AIC.TE_list),
                             mean(ridge.TE_list),mean(naive.TE_list)))
        output <- rbind(output, c('Variance', var(linreg_AIC.TE_list),
                             var(ridge.TE_list),var(naive.TE_list)))
    
        return(output)
}
