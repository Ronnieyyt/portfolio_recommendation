#install.packages("ISOweek")
library(ISOweek)

avg_return_week <- function(data){
  y <- as.Date(unique(data$date))
  week <- data.frame(date = format(y), week = ISOweek(y)) %>% mutate(date = as.Date(date))
  data_week <- data %>% left_join(.,week,by="date")
  avg_return <- data_week %>% group_by(week,symbol) %>% summarise(return = mean(return)) %>% rename(date = week)
  return(avg_return)
}


avg_return_month <- function(data){
  data_month <- data %>% mutate(month = format(as.Date(date), "%Y-%m"))
  avg_return <- data_month %>% group_by(month,symbol) %>% summarise(return = mean(return)) %>% rename(date = month)
  return(avg_return)
}