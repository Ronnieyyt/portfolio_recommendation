library(forecast)
library(dplyr)

arima_pre <- function(train,test){
  symbols <- unique(train$symbol)
  object <- setdiff(names(train),c("date","symbol"))

  ncol <- ncol(test)+1
  colname <- union(names(test),"Prediction")
  test_pre <- setNames(data.frame(matrix(ncol = ncol, nrow = 0)), colname)
  
  for(i in 1:length(symbols)){
    sym = symbols[i]
    train_part <- train[which(train$symbol == sym),]
    ts <- ts(train_part[object])
    fit<- auto.arima(ts)
    n <- length(unique(test$date))
    pre <- forecast(fit,n)
    pre <- as.tibble(as.numeric(pre$mean))
    pre <- setNames(pre,"Prediction")

    test_part <- test[which(test$symbol == sym),]
    test_part <- cbind(as.data.frame(test_part),pre)
    test_part <- test_part %>% mutate(symbol=sym)
    test_pre <- rbind(test_pre,test_part)
  }
  return(test_pre)
}
