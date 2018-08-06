# add branch factor of each symbol
get_branch <- function(dataset)
{
  
  if (!("sector" %in% colnames(dataset)))
  {
    branch_information = read.csv(file="./data/branch_information.csv", header=TRUE, sep=",")
    dataset <- merge(dataset,branch_information,by="symbol")
    
    #set the categories as factor
    #unique(stock_df$sector)
    #num = length(unique(stock_df$sector))
    #stock_df <- stock_df %>% mutate(sector_code = factor(stock_df$sector,levels=c(1:num),labels=c(as.vector(unique(stock_df$sector)))))
  }
  return(dataset)
}
