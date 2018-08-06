# parameters of xreg
library(dplyr)
# consider week
xreg.actual.week <- function(train,sym)
{
  train$day <- as.POSIXlt(train$date)$wday
  xreg_actual <- train %>%
    filter(symbol == sym) %>%
    select(date,day) %>%
    unique() %>%
    mutate(Mon = ifelse(day == 1,1,0),
           Tues = ifelse(day == 2,1,0),
           Wed = ifelse(day == 3,1,0),
           Thur = ifelse(day == 4,1,0)) %>%
    select(Mon,Tues,Wed,Thur)
  return(
    as.matrix(xreg_actual)
  )
}


xreg.future.week <- function(test,sym)
{
  test$day <- as.POSIXlt(test$date)$wday
  xreg_future <- test %>%
    filter(symbol == sym) %>%
    select(date,day) %>%
    unique() %>%
    mutate(Mon = ifelse(day == 1,1,0),
           Tues = ifelse(day == 2,1,0),
           Wed = ifelse(day == 3,1,0),
           Thur = ifelse(day == 4,1,0)) %>%
    select(Mon,Tues,Wed,Thur)
  return(
    as.matrix(xreg_future)
  )
}

