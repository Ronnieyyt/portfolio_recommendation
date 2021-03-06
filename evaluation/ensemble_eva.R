# summary the top n symbols, mean-rank, mean-return and correct rate of each date and each method.

# data is the return data of the function ��top_average_ensemble��
# data <- top_average_ensemble(real_rank,5,"baseline1","baseline2","baseline3","arima","xgboost","prophet","stl")

summary_ensemble <- function(data,n){
  summary <- data %>% group_by(date) %>% 
    summarize(mean_rank = mean(rank),mean_return=mean(return),correct_rate = (n - sum(is.na(is_correct)))/n)
  date <- unique(summary["date"]) 
  date <- date %>% mutate(num = as.numeric(substr(date,7,8))) %>% arrange(num) %>% select(date)
  date$period <- c("Period1","Period2","Period3","Period4")
  summary <- merge(summary,date,by="date")
  return(summary %>% arrange(date) %>% select(period,date,mean_rank,mean_return,correct_rate))
}

combine_table <- function(...){
  dots <- list(...)
  df <- eval(parse(text=dots[[1]])) 
  df$method = dots[[1]]
  for(i in 2:length(dots)){
    add <- eval(parse(text=dots[[i]])) 
    add$method = dots[[i]]
    df <- rbind(as.data.frame(df),as.data.frame(add))
  }
  return(df %>% select(period,date,method,mean_rank,mean_return,correct_rate))
}
