# dots <- a("arima","xgboost")

top_average_ensemble <- function(real_rank,n,...){
  
  dots <- list(...)
  #combine all selected methods into one table, add method column
  res <- eval(parse(text=dots[[1]])) 
  res <- res[c("symbol","date","Prediction")]
  res$method = dots[[1]]
  for(i in 2:length(dots)){
    add <- eval(parse(text=dots[[i]])) 
    add <- add[c("symbol","date","Prediction")]
    add$method = dots[[i]]
    res <- rbind(as.data.frame(res),as.data.frame(add))
  }
  
  res_avg <- res %>% group_by(symbol,date) %>% summarise(Prediction = mean(Prediction)) %>% arrange(date,desc(Prediction))
  
  combine <- res_avg %>% left_join(.,real_rank,by=c("symbol","date"))%>% select(date,symbol,rank,Prediction,return)

  df <- combine %>% group_by(date) %>% top_n(n = n, wt = Prediction) %>% 
    arrange(date,desc(Prediction)) %>% mutate(is_correct = ifelse(rank < n+1,TRUE,NA)) %>%
    select(date,symbol,rank,Prediction,return,is_correct)

  return(df)
}



