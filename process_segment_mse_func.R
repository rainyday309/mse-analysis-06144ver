#process_segment_mse_func.R
#sort into a function then can analyse reponder and nonresponder separately

segment_topo <- function(input)
{
  # process for mean 3-5
  
  scale1_2 <- input[,grepl("scale[1-2]$",x=names(input))]
  scale1_2 <- scale1_2[,grepl("pre|post",x=names(scale1_2))]
  
  scale3_5 <- input[,grepl("scale[3-5]",x=names(input))]
  scale3_5 <- scale3_5[,grepl("pre|post",x=names(scale3_5))]
  
  scale6_10 <- input[,grepl("scale[6-9]|scale10",x=names(input))]
  scale6_10 <- scale6_10[,grepl("pre|post",x=names(scale6_10))]
  
  output1_2<- data.frame(matrix(nrow = nrow(input),ncol=0))
  output3_5<- data.frame(matrix(nrow = nrow(input),ncol=0))
  output6_10<- data.frame(matrix(nrow = nrow(input),ncol=0))
  
  for (i in channels) {
    name <- paste("pre_",i,sep="")
    temp <- scale1_2[,grepl(name,x=colnames(scale1_2))]
    output1_2[name] <- apply(temp, MARGIN = 1, FUN = mean)
    name <- paste("post_",i,sep="")
    temp <- scale1_2[,grepl(name,x=colnames(scale1_2))]
    output1_2[name] <- apply(temp, MARGIN = 1, FUN = mean)
  }
  for (i in channels) {
    name <- paste("pre_",i,sep="")
    temp <- scale3_5[,grepl(name,x=colnames(scale3_5))]
    output3_5[name] <- apply(temp, MARGIN = 1, FUN = mean)
    name <- paste("post_",i,sep="")
    temp <- scale3_5[,grepl(name,x=colnames(scale3_5))]
    output3_5[name] <- apply(temp, MARGIN = 1, FUN = mean)
  }
  for (i in channels) {
    name <- paste("pre_",i,sep="")
    temp <- scale6_10[,grepl(name,x=colnames(scale6_10))]
    output6_10[name] <- apply(temp, MARGIN = 1, FUN = mean)
    name <- paste("post_",i,sep="")
    temp <- scale6_10[,grepl(name,x=colnames(scale6_10))]
    output6_10[name] <- apply(temp, MARGIN = 1, FUN = mean)
  }
  
  results_for_topo_1_2 <- data.frame(name=character(),p_value=numeric(),difference=numeric(),low_conf_int=numeric(),high_conf_int=numeric(),effect_size=numeric(),stringsAsFactors=FALSE)
  results_for_topo_3_5 <- data.frame(name=character(),p_value=numeric(),difference=numeric(),low_conf_int=numeric(),high_conf_int=numeric(),effect_size=numeric(),stringsAsFactors=FALSE)
  results_for_topo_6_10 <- data.frame(name=character(),p_value=numeric(),difference=numeric(),low_conf_int=numeric(),high_conf_int=numeric(),effect_size=numeric(),stringsAsFactors=FALSE)
  
  
  for (i in channels) {
    name1 <- paste('post',i,sep='_')
    name2 <- paste('pre',i,sep='_')
    
    res<-t.test(output1_2[,name1],output1_2[,name2],paired=TRUE)
    r <- cor(output1_2[,name1],output1_2[,name2])
    d <- res$statistic * ((2*(1-r)/length(output1_2[,name1]))^0.5)
    
    this_one <- data.frame(name=character(),p_value=numeric(),difference=numeric(),low_conf_int=numeric(),high_conf_int=numeric(),effect_size=numeric(),stringsAsFactors=FALSE)
    this_one[1,1] = i
    this_one[1,2] = res$p.value
    this_one[1,3] = res$estimate
    this_one[1,4] = res$conf.int[1]
    this_one[1,5] = res$conf.int[2]
    this_one[1,6] = d
    results_for_topo_1_2 <- rbind(results_for_topo_1_2, this_one)
    
  }
  
  for (i in channels) {
    name1 <- paste('post',i,sep='_')
    name2 <- paste('pre',i,sep='_')
    
    res<-t.test(output3_5[,name1],output3_5[,name2],paired=TRUE)
    r <- cor(output3_5[,name1],output3_5[,name2])
    d <- res$statistic * ((2*(1-r)/length(output3_5[,name1]))^0.5)
    
    this_one <- data.frame(name=character(),p_value=numeric(),difference=numeric(),low_conf_int=numeric(),high_conf_int=numeric(),effect_size=numeric(),stringsAsFactors=FALSE)
    this_one[1,1] = i
    this_one[1,2] = res$p.value
    this_one[1,3] = res$estimate
    this_one[1,4] = res$conf.int[1]
    this_one[1,5] = res$conf.int[2]
    this_one[1,6] = d
    results_for_topo_3_5 <- rbind(results_for_topo_3_5, this_one)
    
  }
  
  for (i in channels) {
    name1 <- paste('post',i,sep='_')
    name2 <- paste('pre',i,sep='_')
    
    res<-t.test(output6_10[,name1],output6_10[,name2],paired=TRUE)
    r <- cor(output6_10[,name1],output6_10[,name2])
    d <- res$statistic * ((2*(1-r)/length(output6_10[,name1]))^0.5)
    
    this_one <- data.frame(name=character(),p_value=numeric(),difference=numeric(),low_conf_int=numeric(),high_conf_int=numeric(),effect_size=numeric(),stringsAsFactors=FALSE)
    this_one[1,1] = i
    this_one[1,2] = res$p.value
    this_one[1,3] = res$estimate
    this_one[1,4] = res$conf.int[1]
    this_one[1,5] = res$conf.int[2]
    this_one[1,6] = d
    results_for_topo_6_10 <- rbind(results_for_topo_6_10, this_one)
    
  }
  
  output <- list()
  output[['output1_2']] <- output1_2
  output[['output3_5']] <- output3_5
  output[['output6_10']] <- output6_10
  output[['topo_1_2']] <- results_for_topo_1_2
  output[['topo_3_5']] <- results_for_topo_3_5
  output[['topo_6_10']] <- results_for_topo_6_10
  
  output
}