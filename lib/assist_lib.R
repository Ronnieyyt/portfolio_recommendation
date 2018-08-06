# tranform 
transform <- function(method_df,his_return){
  symbols <- unique(method_df$symbol)
  combine <- his_return[c("date","return")][which(his_return$symbol == as.name(symbols[1])),]
  colnames(combine) <- c("date",symbols[1])
  for(i in 2:length(symbols)){
    add <- his_return[c("date","return")][which(his_return$symbol == as.name(symbols[i])),]
    colnames(add) <- c("date",symbols[i])
    combine <- merge(combine,add,by="date") %>% arrange(date) 
  }
  combine <- combine %>% select(-date)
  return(combine)
}


# split year+week
split_yw <- function(data){
  data <-  data %>% separate(date, c("year", "week"), "-W",remove = FALSE)
  data$year <- as.numeric(data$year)
  data$week <- as.numeric(data$week)
  data <- data %>% arrange(year,week)
  return(data)
}
