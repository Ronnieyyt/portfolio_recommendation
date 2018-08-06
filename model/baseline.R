library("dplyr")
repeat_last_day <- function(train,test){
  object <- setdiff(names(train),c("date","symbol"))
  if(class(train$date) == "Date"){
    Close_last_day <- train[c("symbol",object)][which(train$date == max(date)),]
    Close_last_day <- Close_last_day %>% rename(.,Prediction = object)
    
  }else{
    add_id <- data.frame(date = unique(train$date), id = c(1:length(unique(train$date))))
    data <- train %>% 
      left_join(.,add_id,by="date")

    Close_last_day <- data[c("date","symbol",object)][which(data$id == max(data$id)),] 
    Close_last_day <- Close_last_day %>% rename(.,Prediction = object)
    Close_last_day <- Close_last_day[c("symbol","Prediction")]
  }
  return(test %>% left_join(.,Close_last_day,by="symbol")) 
  
}

repeat_previous_days <- function(train,test){
  object <- setdiff(names(train),c("date","symbol"))
  id <- data.frame(symbol = unique(train$symbol),id=c(1:length(unique(train$symbol))))
  id$symbol = as.character(id$symbol)
  train <- train %>% mutate(symbol = as.character(symbol)) %>% left_join(.,id,by="symbol")
  test <- test %>% left_join(.,id,by="symbol") %>% arrange(id) %>% select(-c("id"))
  
  if(class(train$date) == "Date"){
    Close_last_days <- train %>%
      arrange(date) %>% tail(n = length(unique(test$date))*length(unique(train$symbol))) %>% 
      rename(.,Prediction=object) %>% arrange(id) %>% select(-c("date","id","symbol"))
  }else{
    add_id <- data.frame(date = unique(train$date), id_date = c(1:length(unique(train$date))))
    train <- train %>% 
      left_join(.,add_id,by="date")
    Close_last_days <- train %>%
      arrange(id_date) %>% tail(n = length(unique(test$date))*length(unique(train$symbol))) %>% 
      rename(.,Prediction=object) %>% arrange(id) 
    Close_last_days <- Close_last_days[c("Prediction")]
  }
  return(cbind(as.data.frame(test),Close_last_days))
}

mean_last_week <- function(train,test){
  object <- setdiff(names(train),c("date","symbol"))
  id <- data.frame(symbol = unique(train$symbol),id=c(1:length(unique(train$symbol))))
  id$symbol = as.character(id$symbol)
  train <- train %>% left_join(.,id,by="symbol")
  
  if(class(train$date) == "Date"){
    Close_last_days <- train %>%
      arrange(date) %>% tail(n = 5*length(unique(train$symbol))) %>% 
      group_by(symbol) %>% summarise(Prediction=mean(eval(parse(text=object))))
  }else{
    train <- train %>% left_join(.,data.frame(date = unique(train$date), id_date = c(1:length(unique(train$date)))),by="date")
    Close_last_days <- train %>%
      arrange(id_date) %>% tail(n = 5*length(unique(train$symbol))) %>% 
      group_by(symbol) %>% summarise(Prediction=mean(eval(parse(text=object))))
  }
  return(test %>% left_join(.,Close_last_days,by="symbol"))

}
