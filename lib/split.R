split <- function(dataset,start_time = "2007-07-05",end_time="2017-12-31",object){
  start_time <- as.Date(start_time)
  end_time <- as.Date(end_time)
  stopifnot(end_time>start_time)
  return(dataset %>% filter(date >= start_time & date <= end_time) %>% select(symbol,date,object))
}

# get_test_data <- function(dataset,start_time = "2018-01-01",end_time="2018-02-01",object){
#   start_time <- as.Date(start_time)
#   end_time <- as.Date(end_time)
#   stopifnot(end_time>start_time)
#   return(dataset %>% filter(date >= start_time & date <= end_time) %>% select(symbol,date,object)) 
# }


split_week <- function(data,start_time,end_time)
{
  data <- data %>% left_join(.,data.frame(date = unique(data$date), id = c(1:length(unique(data$date)))),by="date")
  start_id <- unique(data$id[which(data$date==start_time)])
  end_id <- unique(data$id[which(data$date==end_time)])
  return(data[c("symbol",'date',"return")][which(data$id >= start_id & data$id <= end_id),])
}

