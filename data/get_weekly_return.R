### Get the price, volumes etc. stock trade information 

### If you have not these packages below, please firsly install them
#install.packages("quantmod")
#install.packages("dplyr")
#install.packages("zoo")
#install.packages("xts")
#install.packages("tidyverse")

### import packages
library(quantmod)
library(dplyr)
library(zoo)
library(xts)
library(tidyverse)
library(ISOweek)

# Function: get the open, high, low, close, volumne and adjusted price
# Calculate return
# And merge all research objects as into a whole table 

xts_weekly <- function(symbols,start_year){
  
  getSymbols(symbols)
  i=1
  stock_name <- symbols[i]
  sig_stock <- eval(parse(text = symbols[i]))
  sig_stock <- setNames(sig_stock, c("Open","High","Low","Close","Volumne","Adj"))
  data <- periodReturn(sig_stock$Close, period = 'weekly', subset=paste(start_year, "::", sep = ""), 
                       type = 'arithmetic',leading=TRUE)
  data <- setNames(data,c(stock_name))
  if(length(symbols) != 1){
    for (i in 2:length(symbols))
    {
      stock_name <- symbols[i]
      sig_stock <- eval(parse(text = symbols[i]))
      sig_stock <- setNames(sig_stock, c("Open","High","Low","Close","Volumne","Adj"))
      add <- periodReturn(sig_stock$Close, period = 'weekly', subset=paste(start_year, "::", sep = ""), 
                          type = 'arithmetic',leading=TRUE)
      add <- setNames(add,c(stock_name))
      
      data <- merge.xts(data,add)
      
    }
  }

  return(data)
}


get_weekly_return <- function(data)
{
  
  col <- colnames(data)
  i = 1
  df <- as.data.frame(data[,i])
  df <- setNames(df,"return")
  df$symbol <- col[i]
  df$date <- as.Date(row.names(df)) 
  for (i in 2:length(col)){
    add <- as.data.frame(data[,i]) 
    add <- setNames(add,"return")
    add$symbol <- col[i]
    add$date <- as.Date(row.names(add)) 
    df <- rbind(df,add)
  }
  
  y <- as.Date(unique(df$date))
  week <- data.frame(date = format(y), week = ISOweek(y)) %>% mutate(date = as.Date(date))
  data_week <- df %>% left_join(.,week,by="date")
  data_week <- data_week %>% select(week,symbol,return,date)
  data_week <- setNames(data_week,c("date","symbol","return","day"))
  return(data_week)
}






