library("readxl")
library(stringr)
library(data.table)
#Summary of all pairs of train and test datasets. - taking average
#sum_name: "avg_ensemble","freq_ensemble","single_method"
summary_tt <- function(num_tests,sum_name){
  str <- str_pad(1:num_tests, pad = 0,width = 2 , "left")
  dir = paste("./result/",sum_name,"/sum_",sum_name,"_2018-W",str[1],".xlsx",sep="") 
  df_used <- as.data.frame(read_excel(dir, 1))
  df  <- as.data.frame(read_excel(dir, 1))
  df <- df %>% select(-date)
  
  for(i in 2:length(str)){
    dir = paste("./result/",sum_name,"/sum_",sum_name,"_2018-W",str[i],".xlsx",sep="") 
    df_add  <- as.data.frame(read_excel(dir, 1))
    df_add <- df_add %>% select(-date)
    #df <- rbindlist(list(df_add,df))[,lapply(.SD,mean,na.rm=TRUE), list(period)]
    df <- rbind(df,df_add)
  }
  df <- df %>% group_by(method,period) %>% summarize(mean_rank = mean(mean_rank),mean_return = mean(mean_return),correct_rate=mean(correct_rate))
  return(df %>% select(period,method,mean_rank,mean_return,correct_rate))
}

