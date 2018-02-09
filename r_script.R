require(dplyr)
require(tidyr)
require(lubridate)
require(markdown)
require(dygraphs)
require(xts)
require(zoo)
require(RcppRoll)
require(quantmod)

# setwd("C:/Users/Ryan Will/Dropbox/Statistics/Stats in R/R meetup talk")
# 
# getSymbols("F")
# getSymbols("GE")
# getSymbols("TSS")
# getSymbols("TSLA")
# 
# df = cbind(GE[,4],TSS[,4],F[,4],TSLA[,4])
# 
# names = unlist(lapply(strsplit(colnames(df), "[.]"), '[[',1))
# colnames(df) =names
# df = data.frame(DATE = index(df), coredata(df))
# df$YEAR = year(df$DATE)
# 
# write.csv(df, 'sample_data2.csv', row.names = FALSE)


stock_selector = function(df, stock){
  #stock = enquo(stock)
  df_out = df%>%filter(YEAR >= year(Sys.Date())-2)%>%
    select(DATE,YEAR, !!stock)
  return(drop_na(df_out))
}

stock_roller = function(df, stock, rolling=14){
  stock = enquo(stock)
  stock_data = stock_selector(df, stock)
  name_var = quo_name(stock)
  
  transform_data = stock_data%>%
    mutate(rolling_close = roll_mean(!!stock, n= rolling, fill =NA , na.rm = TRUE),
           DATE_VAR = as.Date(paste(day(DATE),month(DATE),sep ="/"), '%d/%m'))%>%
    filter(!is.na(DATE_VAR))%>%
    select(DATE_VAR,YEAR,rolling_close)%>%
    spread(YEAR,rolling_close)
  
  time_data = xts(transform_data[,-1],order.by = transform_data[,1])
  return(time_data)
}



