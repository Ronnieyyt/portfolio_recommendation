# a <- function(...){
#   dots <- list(...)
#   return(dots)
# }
# dots <- a("baseline1","baseline2","baseline3","arima","xgboost","prophet","stl")


library('scales')
# select the information of top n symbols for each method
top_single <- function(real_rank,n,...)
{
  dots <- list(...)
  # the first iteration
  res <- eval(parse(text=dots[[1]])) 
  top_n <- res %>% group_by(date) %>% top_n(n = n, wt = Prediction) %>% arrange(date,desc(Prediction)) %>% 
    select(date,symbol,Prediction) 
  df <- top_n %>% left_join(.,real_rank,by=c("date","symbol")) %>%  
    mutate(is_correct = ifelse(as.numeric(rank) < (n+1),TRUE,NA),method = dots[[1]])
  
  for(i in 2:length(dots)){
    res <- eval(parse(text=dots[[i]])) 
    top_n <- res %>% group_by(date) %>% top_n(n = n, wt = Prediction) %>% arrange(date,desc(Prediction)) %>% 
      select(date,symbol,Prediction) 
    add <- top_n %>% left_join(.,real_rank,by=c("date","symbol")) %>%  
      mutate(is_correct = ifelse(as.numeric(rank) < (n+1),TRUE,NA),method = dots[[i]])
    df <- rbind(df,add)
    
  }
  return(df %>% select(method,date,symbol,return,Prediction,rank,is_correct))
}  

#summary the top n symbols, mean-rank, mean-return and correct rate of each date and each method.
#data is the return data of the function ¡±top_single¡°
# data <- top_single(real_rank,5,"baseline1","baseline2","baseline3","arima","xgboost","prophet","stl")
sum_top <- function(data,n){
  summary <- data %>% group_by(method,date) %>% 
    summarize(mean_rank = mean(rank),mean_return=mean(return),correct_rate = (n - sum(is.na(is_correct)))/n)
  date <- unique(summary["date"]) 
  date <- date %>% mutate(num = as.numeric(substr(date,7,8))) %>% arrange(num) %>% select(date)
  date$period <- c("Period1","Period2","Period3","Period4")
  summary <- merge(summary,date,by="date")
  return(summary %>% arrange(method,date) %>% select(period,date,method,mean_rank,mean_return,correct_rate))
}
  
#write.xlsx(summary_table,paste0("sum_each_method_",combine_all$date[1],".xlsx"))


