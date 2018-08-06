library(dplyr)
library(xlsx)

# a <- function(...){
#   dots <- list(...)
#   return(dots)
# }

# store in excel
top <- function(...)
{
  wb = createWorkbook()
  dots <- list(...)
  result <- eval(parse(text=dots[[1]]))
  if(class(result$date) == "Date"){
    max = max(result$date)
    min = min(result$date)

  }else{
    data <- result %>%
      left_join(.,data.frame(date = unique(result$date), id = c(1:length(unique(result$date)))),by="date")
    max = unique(data$date[which(data$id==max(data$id))])
    min = unique(data$date[which(data$id==min(data$id))])
  }
  filename = paste(min,"to",max,sep="")
  date <- unique(result$date)
  real_rank <- tribble(~date,~symbol,~return,~Prediction,~rank)
  for(i in 1:length(date)){
     rank_real <- result[which(result$date ==date[i]),]
     rank_real <- rank_real %>% arrange(desc(return)) %>%
       select(date,symbol,return) %>% mutate(rank = row.names(.))
     date_top <- rank_real
    # rank_pre <- result  %>% filter(date == date[i]) %>% arrange(desc(Prediction)) %>%
    #   select(symbol,Prediction,return) %>% left_join(.,rank_real[c("symbol","rank")],by="symbol")
    # rank_pre <- setNames(rank_pre,c(dots[j],"pre","real","rank"))
    # date_top <- cbind(as.data.frame(rank_real),rank_pre)
    #rank_pre <- eval(parse(text=dots[[1]]))  %>% filter(date == date[i]) %>% arrange(desc(Prediction))

    for(j in 1:length(dots)){
      rank_pre <- eval(parse(text=dots[[j]]))
      rank_pre <- rank_pre[which(rank_pre$date == date[i]),]
      rank_pre <- rank_pre  %>% arrange(desc(Prediction))
      rank_pre <- rank_pre[c("symbol","Prediction","return")]  %>% left_join(.,rank_real[c("symbol","rank")],by="symbol")
      rank_pre <- setNames(rank_pre,c(dots[[j]],"pre","real","rank"))
      date_top <- cbind(as.data.frame(date_top),as.data.frame(rank_pre))

    }

    sheet = createSheet(wb, paste(date[i]))
    addDataFrame(date_top, sheet=sheet, startColumn=1, row.names=FALSE)
    real_rank_add <- date_top[1:4]
    real_rank <- rbind(real_rank,real_rank_add)
  }
  real_rank <- real_rank %>% mutate(rank = as.numeric(rank))
  saveWorkbook(wb,paste(filename,".xlsx",sep=""))
  return(real_rank)
}
 
 

success_count <- function(tops){
  col_name <- names(tops %>% select(ends_with("result")))
  tops_sum <- tops %>% select(date,starts_with("is_correct")) %>% group_by(date) %>%
    summarise(is_correct_1 = length(is_correct_1)-sum(is.na(is_correct_1)),
              is_correct_2 = length(is_correct_2)-sum(is.na(is_correct_2)),
              is_correct_3 = length(is_correct_3)-sum(is.na(is_correct_3)),
              is_correct_4 = length(is_correct_4)-sum(is.na(is_correct_4)),
              is_correct_5 = length(is_correct_5)-sum(is.na(is_correct_5)),
              is_correct_6 = length(is_correct_6)-sum(is.na(is_correct_6)),
              is_correct_7 = length(is_correct_7)-sum(is.na(is_correct_7)))
  tops_sum <- setNames(tops_sum,c("date",col_name))
  return(tops_sum)
}

