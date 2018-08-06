#count the number of selected stocks
# dots <- a("baseline1","baseline2","baseline3","arima","xgboost","prophet","stl")
top_freq_ensemble <- function(top,n,real_rank,Stocks_Symbols,...)
{ 

  dots <- list(...)
  selected_method <- as.table(as.matrix(dots))
  top <- top[which(top$method %in% selected_method),]
 
  date <- unique(top$date)
  # the first value
  i = 1
  top_date <- top[which(top$date ==  date[i]),]
  count_df <- top_date %>% group_by(symbol) %>% summarize(freq = n()) %>% arrange(desc(freq))
  avg_pre_df <- top_date %>% group_by(symbol) %>% summarize(Prediction = mean(Prediction))
  df <- count_df %>% left_join(.,real_rank[which(real_rank$date == date[i]),],by="symbol") %>% left_join(.,avg_pre_df,by="symbol")
  #df <- df[which(df$freq>=threshold),]
  df <- df %>% arrange(desc(freq) ,desc(Prediction)) %>% head(n)
  df$is_correct <- ifelse(df$rank<n+1,TRUE,NA)
  for(i in 2:length(date)){
    top_date <- top[which(top$date ==  date[i]),]
    count_df <- top_date %>% group_by(symbol) %>% summarize(freq = n()) %>% arrange(desc(freq))
    avg_pre_df <- top_date %>% group_by(symbol) %>% summarize(Prediction = mean(Prediction))
    add <- count_df %>% left_join(.,real_rank[which(real_rank$date == date[i]),],by="symbol") %>% left_join(.,avg_pre_df,by="symbol")
    #add <- add[which(add$freq>=threshold),]
    add <- add %>% arrange(desc(freq) ,desc(Prediction)) %>% head(n)
    add$is_correct <- ifelse(add$rank<n+1,TRUE,NA)
    df <- rbind(df,add)
  }
  return(df %>% select(symbol,date,symbol,return,Prediction,rank,freq,is_correct))
}



