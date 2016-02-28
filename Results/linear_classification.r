
library(ggplot2)
library(gridExtra)
library(MASS)
library(e1071)
library(class)

test_lin_class <- function(country, B){
    ex_rates <- read.csv('topfive.csv')
    ex_rates <- subset(ex_rates, ex_rates$Country.y==country)
    ex_rates$Direction <- I(ex_rates$Pct_Chg >= 0)
    
    full <- subset(ex_rates, select = c(Direction, Exchange, X1Y_Yield_d, 
                X1Y_Yield_f, Int_d, Int_f,  Infl_d, Infl_f, GDPG_d,
                GDPG_f, BOT_f, BOT_d, FER_f))
    full <- na.omit(full)

    rownames(full) <- 1:nrow(full)
    struct <- subset(full, select = c(Direction, Exchange, X1Y_Yield_d, 
                X1Y_Yield_f, Int_d, Int_f,  Infl_d, Infl_f))

    n <- dim(full)[1]
    n1 <- round(n/10)
    
    # LDA -- Full Model
    set.seed(19890211)
    lda.TE_list <- vector(length=0)
    for(i in 1:B){
    
        start <- sample(1:(n-n1-1),1)
        flag <- (start):(start+n1)
        train <- full[-flag,]; test <- full[flag,]
    
        ldamod <- lda(train[,-1],train[,1])
        lda.test <- predict(ldamod,test[,-1])
        lda.TE <- mean(lda.test$class != test[,1])
        lda.TE_list <- c(lda.TE_list, lda.TE)
    }

    # QDA -- Full Model
    set.seed(19890211)
    qda.TE_list <- vector(length=0)
    for(i in 1:B){
    
        start <- sample(1:(n-n1-1),1)
        flag <- (start):(start+n1)
        train <- full[-flag,]; test <- full[flag,]
    
        qdamod <- qda(train[,-1],train[,1])
        qda.test <- predict(qdamod,test[,-1])
        qda.TE <- mean(qda.test$class != test[,1])
        qda.TE_list <- c(qda.TE_list, qda.TE)
    }

    # Naive Bayes -- Full Model
    set.seed(19890211)
    bayes.TE_list <- vector(length=0)
    for(i in 1:B){
    
        start <- sample(1:(n-n1-1),1)
        flag <- (start):(start+n1)
        train <- full[-flag,]; test <- full[flag,]
    
        bayesmod <- naiveBayes(train[,-1],as.factor(train[,1]))
        bayes.test <- predict(bayesmod,test[,-1])
        bayes.TE <- mean(bayes.test != test[,1])
        bayes.TE_list <- c(bayes.TE_list, bayes.TE)
    }

    # Logistic Regression -- Full Model

    set.seed(19890211)
    logreg.TE_list <- vector(length=0)
    for(i in 1:B){
    
        start <- sample(1:(n-n1-1),1)
        flag <- (start):(start+n1)
        train <- full[-flag,]; test <- full[flag,]
    
        logreg <- glm(Direction ~ ., family = binomial(link='logit'),
                 data = train)
        logreg.pred <- predict.glm(logreg, newdata=test[,-1],
                              type='response')
        logreg.pred <- as.vector(logreg.pred)
        logreg.test <- I(logreg.pred >= .50)
        logreg.TE <- mean(logreg.test != test[,1])
        logreg.TE_list <- c(logreg.TE_list, logreg.TE)
    }


    # LDA -- Structural Model
    set.seed(19890211)
    lda2.TE_list <- vector(length=0)
    for(i in 1:B){
    
        start <- sample(1:(n-n1-1),1)
        flag <- (start):(start+n1)
        train <- struct[-flag,]; test <- struct[flag,]
    
        ldamod <- lda(train[,-1],train[,1])
        lda.test <- predict(ldamod,test[,-1])
        lda.TE <- mean(lda.test$class != test[,1])
        lda2.TE_list <- c(lda2.TE_list, lda.TE)
    }

    # QDA -- Structural Model
    set.seed(19890211)
    qda2.TE_list <- vector(length=0)
    for(i in 1:B){
    
        start <- sample(1:(n-n1-1),1)
        flag <- (start):(start+n1)
        train <- struct[-flag,]; test <- struct[flag,]
    
        qdamod <- qda(train[,-1],train[,1])
        qda.test <- predict(qdamod,test[,-1])
        qda.TE <- mean(qda.test$class != test[,1])
        qda2.TE_list <- c(qda2.TE_list, qda.TE)
    }

    # Naive Bayes -- Structural Model
    set.seed(19890211)
    bayes2.TE_list <- vector(length=0)
    for(i in 1:B){
    
        start <- sample(1:(n-n1-1),1)
        flag <- (start):(start+n1)
        train <- struct[-flag,]; test <- struct[flag,]
    
        bayesmod <- naiveBayes(train[,-1],as.factor(train[,1]))
        bayes.test <- predict(bayesmod,test[,-1])
        bayes.TE <- mean(bayes.test != test[,1])
        bayes2.TE_list <- c(bayes2.TE_list, bayes.TE)
    }

    # Logistic Regression -- Structural Model

    set.seed(19890211)
    logreg2.TE_list <- vector(length=0)
    for(i in 1:B){
    
        start <- sample(1:(n-n1-1),1)
        flag <- (start):(start+n1)
        train <- struct[-flag,]; test <- struct[flag,]
    
        logreg <- glm(Direction ~ ., family = binomial(link='logit'),
                 data = train)
        logreg.pred <- predict.glm(logreg, newdata=test[,-1],
                              type='response')
        logreg.pred <- as.vector(logreg.pred)
        logreg.test <- I(logreg.pred >= .50)
        logreg.TE <- mean(logreg.test != test[,1])
        logreg2.TE_list <- c(logreg2.TE_list, logreg.TE)
    }

    # Naive
    set.seed(19890211)
    naive.TE_list <- vector(length=0)
    for(i in 1:B){
    
        start <- sample(1:(n-n1-1),1)
        flag <- (start):(start+n1)
        train <- full[-flag,]; test <- full[flag,]
    
        naive <- sample(c(TRUE,FALSE),size=n1+1,
                    replace=TRUE, prob=c(0.5,0.5))
        naive.TE <- mean ( test[,1] != naive )
        naive.TE_list <- c(naive.TE_list,naive.TE)
    }

        output1 <- c('Full','LDA','QDA','Naive Bayes',
                     'Logistic','Naive')
        output1 <- rbind(output1, c('Mean', mean(lda.TE_list),
                             mean(qda.TE_list),mean(bayes.TE_list),
                             mean(logreg.TE_list), mean(naive.TE_list)))
        output1 <- rbind(output1, c('Variance', var(lda.TE_list),
                             var(qda.TE_list), var(bayes.TE_list),
                             var(logreg.TE_list),var(naive.TE_list)))
    
        output2 <- c('Full','LDA','QDA','Naive Bayes',
                     'Logistic','Naive')
        output2 <- rbind(output2, c('Mean', mean(lda2.TE_list),
                             mean(qda2.TE_list),mean(bayes2.TE_list),
                              mean(logreg2.TE_list), mean(naive.TE_list)))
        output2 <- rbind(output2, c('Variance', var(lda2.TE_list),
                             var(qda2.TE_list), var(bayes2.TE_list),
                              var(logreg2.TE_list),var(naive.TE_list)))
        output <- rbind(output1,output2)
        return(output)
}

test_lin_class('canada',100)
test_lin_class('europe',100)
test_lin_class('mexico',100)
test_lin_class('japan',100)
test_lin_class('korea',100)


