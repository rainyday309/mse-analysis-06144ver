# process_difference_topograph.r
# provide data for change in effect size in topograph
# hold

library('stringr')

subitems <- c("scale3","scale4","scale5")
mse<-completer[,grepl("scale[3-5]",x=names(completer))]
output<- data.frame(matrix(nrow = 103,ncol=0))

for (i in channels) {
  name <- paste("pre_",i,sep="")
  temp <- mse[,grepl(name,x=colnames(mse))]
  
  output[name] <- apply(temp, MARGIN = 1, FUN = mean)
  
}

for (i in channels) {
  name <- paste("post_",i,sep="")
  temp <- mse[,grepl(name,x=colnames(mse))]
  
  output[name] <- apply(temp, MARGIN = 1, FUN = mean)
  
}


results_for_topo <- data.frame(name=character(),p_value=numeric(),difference=numeric(),low_conf_int=numeric(),high_conf_int=numeric(),effect_size=numeric(),stringsAsFactors=FALSE)


for (i in channels) {
  name1 <- paste('post',i,sep='_')
  name2 <- paste('pre',i,sep='_')
  
  res<-t.test(output[,name1],output[,name2],paired=TRUE)
  r <- cor(output[,name1],output[,name2])
  d <- res$statistic * ((2*(1-r)/length(output[,name1]))^0.5)
  
  this_one <- data.frame(name=character(),p_value=numeric(),difference=numeric(),low_conf_int=numeric(),high_conf_int=numeric(),effect_size=numeric(),stringsAsFactors=FALSE)
  this_one[1,1] = i
  this_one[1,2] = res$p.value
  this_one[1,3] = res$estimate
  this_one[1,4] = res$conf.int[1]
  this_one[1,5] = res$conf.int[2]
  this_one[1,6] = d
  results_for_topo <- rbind(results_for_topo, this_one)
  
}
