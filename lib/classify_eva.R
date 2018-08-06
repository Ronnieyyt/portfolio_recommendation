library(dplyr)
classify <- function(result){
  if("return" %in% names(result)){
    result <- result %>% mutate(is_correct = ifelse(return * Prediction >= 0,1,0)) %>% 
      mutate(real_label = as.factor(ifelse(return >=0,1,0)), pre_label = as.factor(ifelse(Prediction>=0,1,0)))
  }else{
    result <- result %>% mutate(is_correct = ifelse(Close * Prediction >= 0,1,0)) %>%
    mutate(real_label =as.factor(ifelse(Close>=0,1,0)), pre_label = as.factor(ifelse(Prediction>=0,1,0)))
  }
  return(result)
}


classify_symbol <- function(result){
  if("return" %in% names(result)){
    result <- result %>% mutate(is_correct = ifelse(return * Prediction > 0,1,0))
    accuracy <- result %>% group_by(symbol) %>% summarise(accuracy = sum(is_correct)/nrow(result),mean=mean(return))
  }else{
    result <- result %>% mutate(is_correct = ifelse(Close * Prediction > 0,1,0))
    accuracy <- result %>% group_by(symbol) %>% summarise(accuracy = sum(is_correct)/nrow(result),mean=mean(Close))
  }
  return(accuracy)
}