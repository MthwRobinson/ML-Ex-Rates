
#install.packages("lubridate", repos="http://cran.r-project.org")
# api key sWyMx5Thd3sPhF613nE6
library(Quandl)
library(RCurl)
library(httr)
library(stats)
library(zoo)
library(lubridate)
set_config( config( ssl_verifypeer = 0L ) )
Quandl.api_key('sWyMx5Thd3sPhF613nE6')

trade_econ_data <- function(country, code, excode = 'NULL',
                            monthly, quarterly, col_names ){
    
    data_list <- list()
    
    # Monthly Data
    for (data_set in monthly){
        print(paste('Downloading',data_set,sep=' '))
        link <- paste('SGE/',code,data_set,sep='')
        df <- Quandl(link, start_date = '1971-01-01', collapse = 'monthly')
        colnames(df) <- c('Date',data_set)
        df$Date <- round_date(df$Date, unit = 'month')
        data_list[[length(data_list)+1]] <- df
        
    }
    
    
    # Quarterly Data
    for (data_set in quarterly){
        print(paste('Downloading',data_set,sep=' '))
        link <- paste('SGE/',code,data_set,sep='')
        df <- Quandl(link, start_date = '1971-01-01', collapse = 'quarterly')
        colnames(df) <- c('Date',data_set)
        df$Date <- round_date(df$Date, unit = 'month')
        data_list[[length(data_list)+1]] <- df
    }
    
    
    # Exchange Rate
    if (excode != 'NULL'){
        print(paste('Downloading','exchange rate',sep=' '))
        link <- paste('FRED/',excode,sep='')
        df <- Quandl(link, start_date = '1971-01-01', collapse = 'monthly')
        colnames(df) <- c('Date','Exchange')
        df$Date <- round_date(df$Date, unit = 'month')
        data_list[[length(data_list)+1]] <- df
    }

    
    # Merge all of the data into one date frame by date
    df <- Reduce(function(...) merge(..., by='Date', all=TRUE), data_list)
    df$Country <- rep(country,dim(df)[1])
    colnames(df) <- col_names
    
    # Interpolate intermediate values for quarterly data using splines
    start <- length(monthly)+2
    end <- start + length(quarterly)
    for( i in start:end){
        if( sum(is.na(df[,i])) > 0 ){
            df[,i] <- na.spline(df[,i])
        }
    }
    


    filename <- paste(country,'.csv',sep='')
    setwd('/home/matt/MSOR/ISYE7406/ML-Ex-Rates/Data')
    write.csv(df, file = filename)
    return(df)
}

# USA Data Frame
usa <- trade_econ_data(country = 'usa', code = 'USA', excode = 'NULL',
                            monthly = c('CPIC','BOT','GYLD','FER','IR','BLR'),
                            quarterly = c('GGR','CA','FDI'), 
                    col_names = c('Date','Infl_d','BOT_d',
                                  'Yield_d','FER_d','Int_d','PrimeRate_d','GDPG_d',
                                  'CA_d','FDI_d','Country'))

# Euro-zone Data Frame
# No GYLD or BLR data
euro <- trade_econ_data(country = 'europe', code = 'EUR', excode = 'EXUSEU',
                            monthly = c('CPIC','BOT','FER','IR'),
                            quarterly = c('GGR','CA'), 
                    col_names = c('Date','Infl_f','BOT_f',
                                  'FER_f','Int_f','GDPG_f',
                                  'CA_f','Exchange','Country'))
euro$Exchange <- 1/euro$Exchange
setwd('/home/matt/MSOR/ISYE7406/ML-Ex-Rates/Data')
write.csv(euro, file = 'europe.csv')

# Australia Data Frame
# No BLR data
aus <- trade_econ_data(country = 'australia', code = 'AUS', excode = 'EXUSAL',
                            monthly = c('CPIC','BOT','GYLD','FER','IR'),
                            quarterly = c('GGR','CA','FDI'), 
                    col_names = c('Date','Infl_f','BOT_f',
                                  'Yield_f','FER_f','Int_f','GDPG_f',
                                  'CA_f','FDI_f','Exchange','Country'))
aus$Exchange <- 1/aus$Exchange
setwd('/home/matt/MSOR/ISYE7406/ML-Ex-Rates/Data')
write.csv(euro, file = 'australia.csv')

# Canada Data Frame
can <- trade_econ_data(country = 'canada', code = 'CAN', excode = 'EXCAUS',
                            monthly = c('CPIC','BOT','GYLD','FER','IR','BLR'),
                            quarterly = c('GGR','CA','FDI'), 
                    col_names = c('Date','Infl_f','BOT_f',
                                  'Yield_f','FER_f','Int_f','PrimeRate_f',
                                  'GDPG_f','CA_f','FDI_f','Exchange','Country'))

# Denmark Data Frame
# No BLR, FDI data
den <- trade_econ_data(country = 'denmark', code = 'DNK', excode = 'EXDNUS',
                            monthly = c('CPIC','BOT','GYLD','FER','IR'),
                            quarterly = c('GGR','CA'), 
                    col_names = c('Date','Infl_f','BOT_f',
                                  'Yield_f','FER_f','Int_f',
                                  'GDPG_f','CA_f','Exchange','Country'))

# Japan Data Frame
jap <- trade_econ_data(country = 'japan', code = 'JPN', excode = 'EXJPUS',
                            monthly = c('CPIC','BOT','GYLD','FER','IR','BLR'),
                            quarterly = c('GGR','CA','FDI'), 
                    col_names = c('Date','Infl_f','BOT_f',
                                  'Yield_f','FER_f','Int_f','PrimeRate_f',
                                  'GDPG_f','CA_f','FDI_f','Exchange','Country'))

# Korea Data Frame
korea <- trade_econ_data(country = 'korea', code = 'KOR', excode = 'EXKOUS',
                            monthly = c('CPIC','BOT','GYLD','FER','IR','BLR'),
                            quarterly = c('GGR','CA','FDI'), 
                    col_names = c('Date','Infl_f','BOT_f',
                                  'Yield_f','FER_f','Int_f','PrimeRate_f',
                                  'GDPG_f','CA_f','FDI_f','Exchange','Country'))

# Mexico Data Frame
# No BLR data
mex <- trade_econ_data(country = 'mexico', code = 'MEX', excode = 'EXMXUS',
                            monthly = c('CPIC','BOT','GYLD','FER','IR'),
                            quarterly = c('GGR','CA','FDI'), 
                    col_names = c('Date','Infl_f','BOT_f',
                                  'Yield_f','FER_f','Int_f',
                                  'GDPG_f','CA_f','FDI_f','Exchange','Country'))

# New Zealand Data Frame
nzl <- trade_econ_data(country = 'new_zealand', code = 'NZL', excode = 'EXUSNZ',
                            monthly = c('CPIC','BOT','GYLD','FER','IR','BLR'),
                            quarterly = c('GGR','CA','FDI'), 
                    col_names = c('Date','Infl_f','BOT_f',
                                  'Yield_f','FER_f','Int_f','PrimeRate_f',
                                  'GDPG_f','CA_f','FDI_f','Exchange','Country'))
nzl$Exchange <- 1/nzl$Exchange
setwd('/home/matt/MSOR/ISYE7406/ML-Ex-Rates/Data')
write.csv(nzl, file = 'new_zealand.csv')

# Norway Data Frame
# No BLR or FDI data
nor <- trade_econ_data(country = 'norway', code = 'NOR', excode = 'EXNOUS',
                            monthly = c('CPIC','BOT','GYLD','FER','IR'),
                            quarterly = c('GGR','CA'), 
                    col_names = c('Date','Infl_f','BOT_f',
                                  'Yield_f','FER_f','Int_f',
                                  'GDPG_f','CA_f','Exchange','Country'))

# Sweden Data Frame
# No BLR data
swe <- trade_econ_data(country = 'sweden', code = 'SWE', excode = 'EXSZUS',
                            monthly = c('CPIC','BOT','GYLD','FER','IR'),
                            quarterly = c('GGR','CA','FDI'), 
                    col_names = c('Date','Infl_f','BOT_f',
                                  'Yield_f','FER_f','Int_f',
                                  'GDPG_f','CA_f','FDI_f','Exchange','Country'))

# Switzerland Data Frame
# No BLR data
switz <- trade_econ_data(country = 'switzerland', code = 'CHE', excode = 'EXSZUS',
                            monthly = c('CPIC','BOT','GYLD','FER','IR'),
                            quarterly = c('GGR','CA','FDI'), 
                    col_names = c('Date','Infl_f','BOT_f',
                                  'Yield_f','FER_f','Int_f',
                                  'GDPG_f','CA_f','FDI_f','Exchange','Country'))

# United Kingdom Data Frame
uk <- trade_econ_data(country = 'uk', code = 'GBR', excode = 'NULL',
                            monthly = c('CPIC','BOT','GYLD','FER','IR','BLR'),
                            quarterly = c('GGR','CA','FDI'), 
                    col_names = c('Date','Infl_f','BOT_f',
                                  'Yield_f','FER_f','Int_f','PrimeRate_f',
                                  'GDPG_f','CA_f','FDI_f','Country'))
exchange <- Quandl("FED/RXI_US_N_M_UK", start_date = '1971-01-01')
uk$Exchange <- 1/exchange$Value
setwd('/home/matt/MSOR/ISYE7406/ML-Ex-Rates/Data')
write.csv(uk, file = 'uk.csv')

# Need to find monthly exchange rate data for
# Czech Republic
# Hungary
# Iceland
# Poland
# Turkey



'''
Original ad-hoc code to produce the USA data frame

# US Data Frame

# Monthly Data
# Inflation, Balance of Trade, Prime Lending Rate, Bond Yield,
#  Foreign Exchange Reserves, Interest Rate
infl <- Quandl("SGE/USACPIC", start_date = '1971-01-01', collapse = 'monthly')
colnames(infl) <- c('Date','Infl_d')
infl$Date <- round_date(infl$Date, unit = 'month')

bot <- Quandl("SGE/USABOT", start_date = '1971-01-01', collapse = 'monthly')
colnames(bot) <- c('Date','BOT_d')
bot$Date <- round_date(bot$Date, unit = 'month')

prime_rate <- Quandl("SGE/USABLR", start_date = '1971-01-01', collapse = 'monthly')
colnames(prime_rate) <- c('Date','PrimeRate_d')
prime_rate$Date <- round_date(prime_rate$Date, unit = 'month')

yield <- Quandl("SGE/USAGYLD", start_date = '1971-01-01', collapse = 'monthly')
colnames(yield) <- c('Date','Yield_d')
yield$Date <- round_date(yield$Date, unit = 'month')

fer <- Quandl("SGE/USAFER", start_date = '1971-01-01', collapse = 'monthly')
colnames(fer) <- c('Date','FER_d')
fer$Date <- round_date(fer$Date, unit = 'month')

int <- Quandl("SGE/USAIR", start_date = '1971-01-01', collapse = 'monthly')
colnames(int) <- c('Date','Int_d')
int$Date <- round_date(int$Date, unit = 'month')

# Quarterly Data
# GDP Growth, Current Account
gdpg <- Quandl("SGE/USAGGR", start_date = '1971-01-01', collapse = 'quarterly')
colnames(gdpg) <- c('Date','GDPG_d')
gdpg$Date <- round_date(gdpg$Date, unit = 'month')

ca <- Quandl("SGE/USACA", start_date = '1971-01-01', collapse = 'quarterly')
colnames(ca) <- c('Date','CA_d')
ca$Date <- round_date(ca$Date, unit = 'month')

fdi <- Quandl("SGE/USAFDI", start_date = '1971-01-01', collapse = 'quarterly')
colnames(fdi) <- c('Date','FDI_d')
fdi$Date <- round_date(fdi$Date, unit = 'month')

# Merge all of the data into one date frame by date
usa <- Reduce(function(...) merge(..., by='Date', all=TRUE), 
    list(infl, bot, prime_rate, yield, fer, int, gdpg, ca, fdi))
    
# Interpolate intermediate values for quarterly data using splines
usa$GDPG_d <- na.spline(usa$GDPG_d)
usa$CA_d <- na.spline(usa$CA_d)
usa$FDI_d <- na.spline(usa$FDI_d)

setwd('/home/matt/MSOR/ISYE7406/ML-Ex-Rates/Data')
write.csv(usa, file = 'usa.csv')
'''


