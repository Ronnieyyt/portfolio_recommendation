# Install the packages
# install.packages("devtools")
# install.packages("xgboost")
# devtools::install_github("ellisp/forecastxgb-r-package/pkg")

library(xgboost)
library(forecast)
library(forecastxgb)
library(ggplot2)
source("./model/XGboost_param.R")

xgboost_pre <- function(train,test,sym,xreg_actual,xreg_future)
{
  
  symbols <- unique(train$symbol)
  
  ncol <- ncol(test)+1
  colname <- union(names(test),"Prediction")
  test_pre <- setNames(data.frame(matrix(ncol = ncol, nrow = 0)), colname)
  
  for(i in 1:length(symbols)){
    sym = symbols[i]
    if (class(train$date) == "Date"){
      xreg_actual <- xreg.actual.week(train,sym)
      xreg_future <- xreg.future.week(test,sym)
      train_ts <- train %>% filter(symbol == sym) 
      model <- log(1 + ts(train_ts$return)) %>% xgbar(xreg = xreg_actual)  
      fc <- forecast(model, xreg = xreg_future)
    }else{
      train_ts <- train[which(train$symbol == sym),]
      model <- log(1 + ts(train_ts$return)) 
      fc <- forecast(model,h=ncol)
    }
    transform.fc <- exp(fc$mean) - 1
    forecast <- as.numeric(transform.fc) 
    pre <- as.tibble(forecast) %>% setNames("Prediction")
    test_part <- test[which(test$symbol == sym),]
    test_part <- cbind(as.data.frame(test_part),pre)
    test_part <- test_part %>% mutate(symbol=sym)
    test_pre <- rbind(test_pre,test_part)
  }
  return(test_pre)
}












