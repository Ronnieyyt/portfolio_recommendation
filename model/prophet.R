library(dplyr)
#install.packages('prophet')
library(prophet)
library(Rcpp)

prophet_pre <- function(train,test){
  symbols <- unique(test$symbol)
  object <- setdiff(names(train),c("date","symbol"))
  
  ncol <- ncol(test)+3
  colname <- union(names(test),c('yhat', 'yhat_lower', 'yhat_upper'))
  test_pre <- setNames(data.frame(matrix(ncol = ncol, nrow = 0)), colname)
  
  for(sym in symbols){
    if (class(train$date) == "Date"){
      ts <- train[c(object,"date")][which(train$symbol == sym),]
      ts <- ts %>% rename(.,ds=date,y=object)
  
    }else{
      ts <- train[c(object,"date")][which(train$symbol == sym),]
      ts <- ts %>% rename(.,ds=date,y=object)
      
      seq_date <- seq(ISOdate(2011,8,1), by = "day", length.out = nrow(ts))
      ts <- cbind(as.data.frame(ts),as.data.frame(seq_date))
      ts <- ts %>% select(seq_date,y) %>% rename(.,ds=seq_date)
    }
    n <- length(unique(test$date))
    fit <- prophet(ts, weekly.seasonality=FALSE)
    fore <- make_future_dataframe(fit, periods = n)
    forecast <- predict(fit, fore)
    pre <-  tail(forecast[c('ds', 'yhat', 'yhat_lower', 'yhat_upper')],n)
    pre <- pre %>% select(-ds)

    test_part <- test[which(test$symbol == sym),]
    test_part <- cbind(as.data.frame(test_part),pre)
    test_part <- test_part %>% mutate(symbol=sym)
    test_pre <- rbind(test_pre,test_part)
  }
  return(test_pre %>% select(symbol,date,object,yhat) %>% rename(.,Prediction=yhat))
}

prophet_plot <- function(train,test){
    symbols <- unique(test$symbol)
    object <- setdiff(names(train),c("date","symbol"))
    number <- length(symbols)
    old.par <- par(mfrow=c(9, number/9))
    for(sym in symbols){
    ts <- train %>% filter(symbol == sym) %>% select(date,object) %>% rename(.,ds=date,y=object)
    n <- length(unique(test$date))
    
    fit <- prophet(ts, weekly.seasonality=FALSE)
    fore <- make_future_dataframe(fit, periods = n)
    forecast <- predict(fit, fore)
    plot(fit, forecast,main=sym)
    }
    return(par(old.par))
}