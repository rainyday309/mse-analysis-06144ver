#process_correlation.r
#dependency: process.r

library(dplyr)

# divide dataframes by channel and give them names

  #select meaningful columns from basic data and return column index 
  selected_pattern <- 'HAMD0|HAMD1|HAMD2|HAMD3|HAMD4|HAMD6|response_rate|response|過去發作|sex|age|onset|edu|marriage'
  prepared_columns <- grep(x=colnames(completer),pattern=selected_pattern)


  #a code example:
  #FP1_data <- select(completer, c(prepared_columns, grep(x=colnames(completer),pattern='FP1')))

