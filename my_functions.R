#my_functions

#dependency: process.r

library('dplyr')

process_t <- function(input)
{
  
  # perform paired t-test on pre-post values
  variables <- as.vector(outer(channels, items, paste, sep='_'))
  t_test_results <- data.frame(name=character(),p_value=numeric(),difference=numeric(),low_conf_int=numeric(),high_conf_int=numeric(),effect_size=numeric(),stringsAsFactors=FALSE)
  
  for (stuff in variables) {
    name1 <- paste('post',stuff,sep='_')
    name2 <- paste('pre',stuff,sep='_')
    
    res<-t.test(input[,name1],input[,name2],paired=TRUE)
    r <- cor(input[,name1],input[,name2])
    d <- res$statistic * ((2*(1-r)/length(input[,name1]))^0.5)
    
    this_one <- data.frame(name=character(),p_value=numeric(),difference=numeric(),low_conf_int=numeric(),high_conf_int=numeric(),effect_size=numeric(),stringsAsFactors=FALSE)
    this_one[1,1] = stuff
    this_one[1,2] = res$p.value
    this_one[1,3] = res$estimate
    this_one[1,4] = res$conf.int[1]
    this_one[1,5] = res$conf.int[2]
    this_one[1,6] = d
    t_test_results <- rbind(t_test_results, this_one)}
  t_test_results
}

#func_checkplot.R
#dependency: process.R

peekplot <- function(name, data1, data2){
  range <- which(grepl(pattern=name, x=rownames(data1)) %in% TRUE)
  
  print(range)
  print(c(1:length(range)))
  
  plot(x=c(1:length(range)), y=data1$mean[range], col='green', xlab='', ylab=name)
  points(x=c(1:length(range)), y=data2$mean[range], col='red')
}

testpeek <- function(name, data1, data2){
  range <- which(grepl(pattern=name, x=rownames(data)) %in% TRUE)
  
  print(range)
  print(c(1:length(range)))
  print(data1$mean[range])
  print(data2$mean[range])
  
  plot(x=c(1:length(range)),y=range)
}

