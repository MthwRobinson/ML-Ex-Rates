
setwd('/home/matt/MSOR/ISYE7406/ML-Ex-Rates/Results')
library(class)
library(MASS)
library(pls)
library(lars)
library(lattice)

#setwd('/home/matt/MSOR/ISYE7406/ML-Ex-Rates/Data')
#ex_rate <- read.csv('oecd.csv')
#ex_rate <- subset(ex_rate, ex_rate$Country.y == 'canada' |
#                  ex_rate$Country.y == 'europe' |
#                  ex_rate$Country.y == 'mexico' |
#                  ex_rate$Country.y == 'japan' |
#                  ex_rate$Country.y == 'korea')
#setwd('/home/matt/MSOR/ISYE7406/ML-Ex-Rates/Results')
#write.csv(ex_rate, file = 'topfive.csv')

ex_rates <- read.csv('topfive.csv')
ex_rates <- subset(ex_rates, ex_rate$Country.y=='canada')
    
full <- subset(ex_rates, select = c(Pct_Chg, Exchange, X1Y_Yield_d, 
                      X1Y_Yield_f, Int_d, Int_f,  Infl_d, Infl_f, GDPG_d,
                      GDPG_f, BOT_f, BOT_d, FER_f))
full <- na.omit(full)
struct <- subset(full, select = c(Pct_Chg, Exchange, X1Y_Yield_d, 
                      X1Y_Yield_f, Int_d, Int_f,  Infl_d, Infl_f))
rownames(full) <- 1:nrow(full)

test_lin_models('canada',100)
test_lin_models('europe',100)
test_lin_models('mexico',100)
test_lin_models('japan',100)
test_lin_models('korea',100)

test_lin_models <- function(country, B){
    
        ex_rates <- read.csv('topfive.csv')
        ex_rates <- subset(ex_rates, ex_rate$Country.y==country)
    
        full <- subset(ex_rates, select = c(Pct_Chg, Exchange, X1Y_Yield_d, 
                      X1Y_Yield_f, Int_d, Int_f,  Infl_d, Infl_f, GDPG_d,
                      GDPG_f, BOT_f, BOT_d, FER_f))
        full <- na.omit(full)
        rownames(full) <- 1:nrow(full)
        struct <- subset(full, select = c(Pct_Chg, Exchange, X1Y_Yield_d, 
                      X1Y_Yield_f, Int_d, Int_f,  Infl_d, Infl_f))

        # Split into training and testing sets
        n <- dim(full)[1]
        n1 <- round(n/5)
        B <- 100
    
        # Test Full Model

        # Linear Regression -- Full Model
        set.seed(19890211)
        linreg.TE_list <- vector(length=0)
        for(i in 1:B){
            start <- sample(1:(n-n1-1),1)
            flag <- (start):(start+n1)
            train <- full[-flag,]; test <- full[flag,]
    
            regmodel <- lm(Pct_Chg ~ ., data = train)
            linreg.test <- predict.lm(regmodel,test[,-18])
            linreg.TE <- mean( (test[,1]-linreg.test)^2)
            linreg.TE_list <- c(linreg.TE_list, linreg.TE)
    
        }    
    
        # Ridge Regression -- Full Model
        set.seed(19890211)
        ridge.TE_list <- vector(length=0)
        for(i in 1:B){
            start <- sample(1:(n-n1-1),1)
            flag <- (start):(start+n1)
            train <- full[-flag,]; test <- full[flag,]
    
            ridgereg <- lm.ridge(Pct_Chg ~ ., data = train, 
                        lambda = seq(0,10,0.001))
            lambdaopt <- which.min(ridgereg$GCV)
            ridge_coef <- coef(ridgereg)[lambdaopt,]
            ridge_testmat <- as.matrix(cbind(1,test[,-1]))
            ridge.test <- as.matrix(ridge_testmat %*% ridge_coef)
            ridge.TE <- mean( (test[,1] - ridge.test)^2)
            ridge.TE_list <- c(ridge.TE_list, ridge.TE)
    
        }
    
        # Lasso Regression -- Full Model
        set.seed(19890211)
        lasso.TE_list <- vector(length=0)
        lasso_lambda <- 0
        for(i in 1:B){
            start <- sample(1:(n-n1-1),1)
            flag <- (start):(start+n1)
            train <- full[-flag,]; test <- full[flag,]
    
            lassoreg <- lars( x= as.matrix(train[,-1]),
                            y = train[,1], type = 'lasso')
            Cp1 <- summary(lassoreg)$Cp
            index1 <- which.min(Cp1)
            lasso_coef <- coef(lassoreg)[index1,]
            if (is.na(lassoreg$lambda[index1])==FALSE){
                lasso_lambda <- lassoreg$lambda[index1]
            }
            lassoreg.fit <- predict(lassoreg, as.matrix(test[,-1]),
                                   s = lasso_lambda, type = 'fit',
                                   mode = 'lambda')
            lassoreg.test <- lassoreg.fit$fit
            lassoreg.TE <- mean( (test[,1]-lassoreg.test)^2)
            lasso.TE_list <- c(lasso.TE_list, lassoreg.TE)
                
        }
    
        # PCR -- Full Model
        set.seed(19890211)
        pcreg.TE_list <- vector(length=0)
        for(i in 1:B){
            start <- sample(1:(n-n1-1),1)
            flag <- (start):(start+n1)
            train <- full[-flag,]; test <- full[flag,]
            
            pcreg <- pcr(Pct_Chg ~ ., data = train,
                        validation = 'CV')
            comps <- dim(train)[2]-1
            pcreg.test <- predict(pcreg, ncomp = comps,
                                 newdata = test[,-1])
            pcreg.TE <- mean((test[,1]-pcreg.test)^2)
            pcreg.TE_list <- c(pcreg.TE_list, pcreg.TE)
        }
    
        # Test Structural Model

        # Linear Regression -- Structural Model
        set.seed(19890211)
        linreg2.TE_list <- vector(length=0)
        for(i in 1:B){
            start <- sample(1:(n-n1-1),1)
            flag <- (start):(start+n1)
            train <- struct[-flag,]; test <- struct[flag,]
    
            regmodel <- lm(Pct_Chg ~ ., data = train)
            linreg.test <- predict.lm(regmodel,test[,-18])
            linreg.TE <- mean( (test[,1]-linreg.test)^2)
            linreg2.TE_list <- c(linreg2.TE_list, linreg.TE)
    
        }    
    
        # Ridge Regression -- Structural Model
        set.seed(19890211)
        ridge2.TE_list <- vector(length=0)
        for(i in 1:B){
            start <- sample(1:(n-n1-1),1)
            flag <- (start):(start+n1)
            train <- struct[-flag,]; test <- struct[flag,]
    
            ridgereg <- lm.ridge(Pct_Chg ~ ., data = train, 
                        lambda = seq(0,10,0.001))
            lambdaopt <- which.min(ridgereg$GCV)
            ridge_coef <- coef(ridgereg)[lambdaopt,]
            ridge_testmat <- as.matrix(cbind(1,test[,-1]))
            ridge.test <- as.matrix(ridge_testmat %*% ridge_coef)
            ridge.TE <- mean( (test[,1] - ridge.test)^2)
            ridge2.TE_list <- c(ridge2.TE_list, ridge.TE)
    
        }
    
        # Lasso Regression -- Structural Model
        set.seed(19890211)
        lasso2.TE_list <- vector(length=0)
        lasso_lambda <- 0
        for(i in 1:B){
            start <- sample(1:(n-n1-1),1)
            flag <- (start):(start+n1)
            train <- struct[-flag,]; test <- struct[flag,]
    
            lassoreg <- lars( x= as.matrix(train[,-1]),
                            y = train[,1], type = 'lasso')
            Cp1 <- summary(lassoreg)$Cp
            index1 <- which.min(Cp1)
            lasso_coef <- coef(lassoreg)[index1,]
            if (is.na(lassoreg$lambda[index1])==FALSE){
                lasso_lambda <- lassoreg$lambda[index1]
            }
            lassoreg.fit <- predict(lassoreg, as.matrix(test[,-1]),
                                   s = lasso_lambda, type = 'fit',
                                   mode = 'lambda')
            lassoreg.test <- lassoreg.fit$fit
            lassoreg.TE <- mean( (test[,1]-lassoreg.test)^2)
            lasso2.TE_list <- c(lasso2.TE_list, lassoreg.TE)
                
        }
    
        # PCR -- Structural Model
        set.seed(19890211)
        pcreg2.TE_list <- vector(length=0)
        for(i in 1:B){
            start <- sample(1:(n-n1-1),1)
            flag <- (start):(start+n1)
            train <- struct[-flag,]; test <- struct[flag,]
            
            pcreg <- pcr(Pct_Chg ~ ., data = train,
                        validation = 'CV')
            comps <- dim(train)[2]-1
            pcreg.test <- predict(pcreg, ncomp = comps,
                                 newdata = test[,-1])
            pcreg.TE <- mean((test[,1]-pcreg.test)^2)
            pcreg2.TE_list <- c(pcreg2.TE_list, pcreg.TE)
        }
    
        # Naive
        set.seed(19890211)
        naive.TE_list <- vector(length=0)
        for(i in 1:B){
            start <- sample(1:(n-n1-1),1)
            flag <- (start):(start+n1)
            train <- full[-flag,]; test <- full[flag,]
            naive.TE <- mean ( (test[,1])^2)
            naive.TE_list <- c(naive.TE_list,naive.TE)
        }
    
        output1 <- c('Full','Linear','Ridge',
                     'Lasso','PCR','Naive')
        output1 <- rbind(output1, c('Mean', mean(linreg.TE_list),
                             mean(ridge.TE_list),mean(lasso.TE_list),
                             mean(pcreg.TE_list), mean(naive.TE_list)))
        output1 <- rbind(output1, c('Variance', var(linreg.TE_list),
                             var(ridge.TE_list), var(lasso.TE_list),
                             var(pcreg.TE_list),var(naive.TE_list)))
    
        output2 <- c('Struct','Linear','Ridge',
                     'Lasso','PCR','Naive')
        output2 <- rbind(output2, c('Mean', mean(linreg2.TE_list),
                             mean(ridge2.TE_list),mean(lasso2.TE_list),
                              mean(pcreg2.TE_list), mean(naive.TE_list)))
        output2 <- rbind(output2, c('Variance', var(linreg2.TE_list),
                             var(ridge2.TE_list), var(lasso2.TE_list),
                              var(pcreg2.TE_list),var(naive.TE_list)))
        output <- rbind(output1,output2)
        return(output)
}




