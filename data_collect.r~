
# Takes data from all countries, adds the future exchange rate variables and 
# merges them into one data set

setwd('/home/matt/MSOR/ISYE7406/ML-Ex-Rates/Data')

data <- list.files(getwd())

ex_rate <- data.frame()
usa <- read.csv('usa.csv')
for (file in data){
    if (file != 'usa.csv'){
        df <- read.csv(file)
        future <- rep(NA,12)
        future <- c(future,df$Exchange)
        future <- head(future,-12)
        df$Exchange_fut <- future
        df <- merge(usa, df, by = 'Date')
        ex_rate <- merge(df,ex_rate,all=TRUE)
    }
}
write.csv(ex_rate,file='oecd')

test_set <- subset(ex_rate,as.Date(ex_rate$Date)>"2014-01-01")
train_set <- subset(ex_rate,as.Date(ex_rate$Date)<="2014-01-01")
test_set <- test_set[,c('Exchange_fut', "Exchange", "Infl_d", "Yield_d", "Int_d", "PrimeRate_d", "GDPG_d",
               "Infl_f", "Int_f", "GDPG_f", "Country.y")]
train_set <- train_set[,c('Exchange_fut', "Exchange", "Infl_d", "Yield_d", "Int_d", "PrimeRate_d", "GDPG_d",
               "Infl_f", "Int_f", "GDPG_f", "Country.y")]

regmodel <- lm(Exchange_fut ~ Exchange + Infl_d + Yield_d + Int_d + PrimeRate_d + GDPG_d  +
               Infl_f  + Int_f  + GDPG_f  + Country.y ,data = train_set)

summary(regmodel)

ypred <- predict.lm(regmodel,test_set)
