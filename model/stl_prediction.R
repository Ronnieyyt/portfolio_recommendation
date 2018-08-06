library(forecast)
library(dplyr)

stl_pre <- function(train,test){
  symbols <- unique(train$symbol)
  
  ncol <- ncol(test)+1
  colname <- union(names(test),"Prediction")
  test_pre <- setNames(data.frame(matrix(ncol = ncol, nrow = 0)), colname)
  
  pre_num <- length(unique(test$date))
  
  for(i in 1:length(symbols)){
    sym = symbols[i]
    train_part <- train[which(train$symbol == sym),]
    data_ts <- ts(train_part$return, frequency = 52)
    data_deseason <- stl(data_ts, t.window=50, s.window='periodic', robust=TRUE) 
    pre <- forecast(data_deseason,h=pre_num)
    #pre <- forecast(data_deseason,h=pre_num,method=c("arima"))
    
    
    pre <- as.tibble(as.numeric(pre$mean))
    pre <- setNames(pre,"Prediction")
    
    test_part <- test[which(test$symbol == sym),]
    test_part <- cbind(as.data.frame(test_part),pre)
    test_part <- test_part %>% mutate(symbol=sym)
    test_pre <- rbind(test_pre,test_part)
  }
  return(test_pre)
}


