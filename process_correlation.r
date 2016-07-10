#process_correlation.r
#dependency: process.r

library(dplyr)

# divide dataframes by channel and give them names

  #select meaningful columns from basic data and return column index 
  selected_pattern <- 'HAMD0|HAMD1|HAMD2|HAMD3|HAMD4|HAMD6|response_rate|response|過去發作|sex|age|onset|edu|marriage'
  prepared_columns <- grep(x=colnames(completer),pattern=selected_pattern)
  channels <- channels[1:19]


#get correlation matrix for each channel
  
  # split data by channel
  data_by_channel <- list()

  for (i in channels) {
    data_by_channel[[i]] <- select(completer, c(prepared_columns, grep(x=colnames(completer),pattern=i)))
    
  }

  # create correlation matrices and put into a list
  correlation_matrices_by_channel <-list()
  for (i in channels) {
    correlation_matrices_by_channel[[i]] <- cor(data_by_channel[[i]])
  }  


