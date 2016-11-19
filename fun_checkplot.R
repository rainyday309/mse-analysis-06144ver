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