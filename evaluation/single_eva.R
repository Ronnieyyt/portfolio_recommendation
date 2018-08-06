# summary the top n symbols, mean-rank, mean-return and correct rate of each date and each method.

# data is the return data of the function ¡±top_single¡°
# data <- top_single(real_rank,5,"baseline1","baseline2","baseline3","arima","xgboost","prophet","stl")

summary_single <- function(data,n){
  summary <- data %>% group_by(method,date) %>% 
    summarize(mean_rank = mean(rank),mean_return=mean(return),correct_rate = (n - sum(is.na(is_correct)))/n)
  date <- unique(summary["date"]) 
  date <- date %>% mutate(num = as.numeric(substr(date,7,8))) %>% arrange(num) %>% select(date)
  date$period <- c("Period1","Period2","Period3","Period4")
  summary <- merge(summary,date,by="date")
  return(summary %>% arrange(method,date) %>% select(period,date,method,mean_rank,mean_return,correct_rate))
}

#write.xlsx(summary_table,paste0("sum_each_method_",combine_all$date[1],".xlsx"))