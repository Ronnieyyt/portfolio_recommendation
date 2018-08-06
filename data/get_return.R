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


# Function: get the open, high, low, close, volumne and adjusted price
# Calculate return
# And merge all research objects as into a whole table 
get_stock <- function(Stocks_Symbols)
{
  
  getSymbols(Stocks_Symbols)
  
  stock_df <- tribble(~date,~Open,~High,~Low,~close,~Volumne,~Adj, ~symbol,~id,~return)
  i = 0
  
  for (stock_name in Stocks_Symbols)
  {
    i <- i + 1
    sig_stock <- eval(parse(text = Stocks_Symbols[i]))
    sig_stock = setNames(sig_stock, c("Open","High","Low","Close","Volumne","Adj"))
    
    
    # get the return of stock at each time point
    sig_stock_return <- dailyReturn(sig_stock$Close, subset=NULL, type='arithmetic',leading=TRUE)
    sig_stock_return <-  data.frame(date=index(sig_stock_return), coredata(sig_stock_return)) 
    sig_stock_return <- setNames(sig_stock_return,c("date","return"))
    
    
    # add some new variables: stock symbol, id
    sig_stock_add <-  data.frame(date=index(sig_stock), coredata(sig_stock))%>%
      mutate(symbol = stock_name) %>%
      mutate(id = i)
    
    # merge return and other variables
    sig_stock_merg <- left_join(sig_stock_add,sig_stock_return,by="date")
    
    
    # merge all research objects
    stock_df <- rbind(stock_df,sig_stock_merg)
  }
  stock_df <- stock_df %>% arrange(id,date)
  return(stock_df)
}




