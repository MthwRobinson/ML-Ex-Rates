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

trade_econ_data <- function(country, code, excode = 'NULL', ycode,
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
    
    # Government Yield
    if (ycode != 'NULL'){
        print(paste('Downloading','yield',sep=' '))
        link <- paste('YC/',ycode,sep='')
        df <- Quandl(link, start_date = '1971-01-01', collapse = 'monthly')
        colnames(df) <- c('Date','3M_Yield')
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
usa <- trade_econ_data(country = 'usa', code = 'USA', excode = 'NULL', ycode = 'USA1Y',
                            monthly = c('CPIC','BOT','GYLD','FER','IR','BLR'),
                            quarterly = c('GGR','CA','FDI'), 
                    col_names = c('Date','Infl_d','BOT_d',
                                  'Yield_d','FER_d','Int_d','PrimeRate_d','1Y_Yield',
                                  'GDPG_d','CA_d','FDI_d','Country'))
tail(usa)

# Euro-zone Data Frame
# No GYLD or BLR data
euro <- trade_econ_data(country = 'europe', code = 'EUR', excode = 'EXUSEU', 
                        ycode = 'DEU1Y',
                            monthly = c('CPIC','BOT','FER','IR'),
                            quarterly = c('GGR','CA'), 
                    col_names = c('Date','Infl_f','BOT_f',
                                  'FER_f','Int_f','1Y_Yield','GDPG_f',
                                  'CA_f','Exchange','Country'))
euro$Exchange <- 1/euro$Exchange
setwd('/home/matt/MSOR/ISYE7406/ML-Ex-Rates/Data')
write.csv(euro, file = 'europe.csv')
tail(euro)

# Australia Data Frame
# No BLR data, No 1Y yield
aus <- trade_econ_data(country = 'australia', code = 'AUS', excode = 'EXUSAL',
                       ycode = 'NULL',
                            monthly = c('CPIC','BOT','GYLD','FER','IR'),
                            quarterly = c('GGR','CA','FDI'), 
                    col_names = c('Date','Infl_f','BOT_f',
                                  'Yield_f','FER_f','Int_f',                                  'GDPG_f',
                                  'CA_f','FDI_f','Exchange','Country'))
aus$Exchange <- 1/aus$Exchange
setwd('/home/matt/MSOR/ISYE7406/ML-Ex-Rates/Data')
write.csv(euro, file = 'australia.csv')
tail(aus)

# Canada Data Frame
can <- trade_econ_data(country = 'canada', code = 'CAN', excode = 'EXCAUS',
                       ycode = 'CAN1Y',
                            monthly = c('CPIC','BOT','GYLD','FER','IR','BLR'),
                            quarterly = c('GGR','CA','FDI'), 
                    col_names = c('Date','Infl_f','BOT_f',
                                  'Yield_f','FER_f','Int_f','PrimeRate_f','1Y_Yield',
                                  'GDPG_f','CA_f','FDI_f','Exchange','Country'))

# Denmark Data Frame
# No BLR, FDI data, No 1Y yield
den <- trade_econ_data(country = 'denmark', code = 'DNK', excode = 'EXDNUS',
                       ycode = 'NULL',
                            monthly = c('CPIC','BOT','GYLD','FER','IR'),
                            quarterly = c('GGR','CA'), 
                    col_names = c('Date','Infl_f','BOT_f',
                                  'Yield_f','FER_f','Int_f',
                                  'GDPG_f','CA_f','Exchange','Country'))

# Japan Data Frame
jap <- trade_econ_data(country = 'japan', code = 'JPN', excode = 'EXJPUS', 
                       ycode = 'JPN1Y',
                            monthly = c('CPIC','BOT','GYLD','FER','IR','BLR'),
                            quarterly = c('GGR','CA','FDI'), 
                    col_names = c('Date','Infl_f','BOT_f',
                                  'Yield_f','FER_f','Int_f','PrimeRate_f','1Y_Yield',
                                  'GDPG_f','CA_f','FDI_f','Exchange','Country'))

# Korea Data Frame
korea <- trade_econ_data(country = 'korea', code = 'KOR', excode = 'EXKOUS',
                         ycode = 'KOR1Y',
                            monthly = c('CPIC','BOT','GYLD','FER','IR','BLR'),
                            quarterly = c('GGR','CA','FDI'), 
                    col_names = c('Date','Infl_f','BOT_f',
                                  'Yield_f','FER_f','Int_f','PrimeRate_f','1Y_Yield',
                                  'GDPG_f','CA_f','FDI_f','Exchange','Country'))

# Mexico Data Frame
# No BLR data
mex <- trade_econ_data(country = 'mexico', code = 'MEX', excode = 'EXMXUS',
                       ycode = 'MEX1Y',
                            monthly = c('CPIC','BOT','GYLD','FER','IR'),
                            quarterly = c('GGR','CA','FDI'), 
                    col_names = c('Date','Infl_f','BOT_f',
                                  'Yield_f','FER_f','Int_f','1Y_Yield',
                                  'GDPG_f','CA_f','FDI_f','Exchange','Country'))

# New Zealand Data Frame
nzl <- trade_econ_data(country = 'new_zealand', code = 'NZL', excode = 'EXUSNZ',
                       ycode = 'NZL1Y',
                            monthly = c('CPIC','BOT','GYLD','FER','IR','BLR'),
                            quarterly = c('GGR','CA','FDI'), 
                    col_names = c('Date','Infl_f','BOT_f',
                                  'Yield_f','FER_f','Int_f','PrimeRate_f','1Y_Yield',
                                  'GDPG_f','CA_f','FDI_f','Exchange','Country'))
nzl$Exchange <- 1/nzl$Exchange
setwd('/home/matt/MSOR/ISYE7406/ML-Ex-Rates/Data')
write.csv(nzl, file = 'new_zealand.csv')

# Norway Data Frame
# No BLR or FDI data
nor <- trade_econ_data(country = 'norway', code = 'NOR', excode = 'EXNOUS',
                       ycode = 'NOR12M',
                            monthly = c('CPIC','BOT','GYLD','FER','IR'),
                            quarterly = c('GGR','CA'), 
                    col_names = c('Date','Infl_f','BOT_f',
                                  'Yield_f','FER_f','Int_f','1Y_Yield',
                                  'GDPG_f','CA_f','Exchange','Country'))

# Sweden Data Frame
# No BLR data
swe <- trade_econ_data(country = 'sweden', code = 'SWE', excode = 'EXSZUS',
                       ycode = 'SWE12M',
                            monthly = c('CPIC','BOT','GYLD','FER','IR'),
                            quarterly = c('GGR','CA','FDI'), 
                    col_names = c('Date','Infl_f','BOT_f',
                                  'Yield_f','FER_f','Int_f','1Y_Yield',
                                  'GDPG_f','CA_f','FDI_f','Exchange','Country'))

# Switzerland Data Frame
# No BLR data
switz <- trade_econ_data(country = 'switzerland', code = 'CHE', excode = 'EXSZUS',
                         ycode = 'CHE12M',
                            monthly = c('CPIC','BOT','GYLD','FER','IR'),
                            quarterly = c('GGR','CA','FDI'), 
                    col_names = c('Date','Infl_f','BOT_f',
                                  'Yield_f','FER_f','Int_f','1Y_Yield',
                                  'GDPG_f','CA_f','FDI_f','Exchange','Country'))

# United Kingdom Data Frame
uk <- trade_econ_data(country = 'uk', code = 'GBR', excode = 'NULL',
                      ycode = 'NULL',
                            monthly = c('CPIC','BOT','GYLD','FER','IR','BLR'),
                            quarterly = c('GGR','CA','FDI'), 
                    col_names = c('Date','Infl_f','BOT_f',
                                  'Yield_f','FER_f','Int_f','PrimeRate_f',
                                  'GDPG_f','CA_f','FDI_f','Country'))
exchange <- Quandl("FED/RXI_US_N_M_UK", start_date = '1971-01-01')
uk$Exchange <- 1/exchange$Value
setwd('/home/matt/MSOR/ISYE7406/ML-Ex-Rates/Data')
write.csv(uk, file = 'uk.csv')




