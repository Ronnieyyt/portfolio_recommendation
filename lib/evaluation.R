source("./model/arima.R")
source("./model/XGboost.R")
source("./model/prophet.R")

evaluation <- function(...)
{
  dots <- list(...)
  object <- setdiff(names(eval(parse(text=as.character(dots[[1]])))),c("date","symbol","Prediction","is_correct","real_label","pre_label"))
  if(length(dots) == 0) return(NULL) 
  eva <- data.frame(matrix(data=NA,nrow=8,ncol=length(dots)),row.names = c("mae","mape","smape","accuracy","recall","precision","TP","TN"))
  
  for(i in 1:length(dots)){
    res <- eval(parse(text=dots[[i]])) 
    res["real_label"] <- as.numeric(res$real_label)
    res["pre_label"] <- as.numeric(res$pre_label)
    res <- eval(parse(text=as.character(dots[[i]])))
    tp <- nrow(res %>% filter(real_label == 0 & pre_label == 0)) 
    fp <- nrow(res %>% filter(real_label == 1 & pre_label == 0)) 
    fn <- nrow(res %>% filter(real_label == 0 & pre_label == 1))
    tn <- nrow(res %>% filter(real_label == 1 & pre_label == 1))
    real <- as.vector(t(res[object]))
    eva[1,i] <- format(round(mae(real, res$Prediction),5))
    eva[2,i] <- format(round(mape(real, res$Prediction),5))
    eva[3,i] <- format(round(smape(real, res$Prediction),5))
    eva[4,i] <- format(round(sum(res$is_correct)/nrow(res),5))
    eva[5,i] <- format(round(tp/(tp+fn),5))
    eva[6,i] <- format(round(tp/(tp+fp),5))
    eva[7,i] <- format(round(as.integer(tp) , 0))
    eva[8,i] <- format(round(as.integer(tn) , 0)) 
    
  }
  return(setNames(eva,dots))
}

