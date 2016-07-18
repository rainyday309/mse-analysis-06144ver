#process_t_test.r
#dependency: process.r

library(dplyr)

# completer_means and standard error of total
completer_means <- data.frame()


for (i in channels) {
  newname<- paste("pre",i,sep="_")
  
  temp<- select(completer, contains(newname))
  temp<- colMeans(temp)
  
  if (length(completer_means)==0) {
    completer_means <- as.data.frame(temp)
    colnames(completer_means)<- c("pre_FP1")
  } else {
    completer_means[newname] <- temp
  }
}

rownames(completer_means) <- sub(rownames(completer_means),pattern = "pre_FP1_",replacement = "")

for (i in channels) {
  newname<- paste("post",i,sep="_")
  
  temp<- select(completer, contains(newname))
  temp<- colMeans(temp)
  
  completer_means[newname] <- temp
}


# standard deviation
completer_sds <- data.frame()

for (i in channels) {
  newname<- paste("pre",i,sep="_")
  
  temp<- select(completer, contains(newname))
  temp<- apply(temp, MARGIN=2, FUN=sd)
  
  if (length(completer_sds)==0) {
    completer_sds <- as.data.frame(temp)
    colnames(completer_sds)<- c("pre_FP1")
  } else {
    completer_sds[newname] <- temp
  }
}

rownames(completer_sds) <- sub(rownames(completer_sds),pattern = "pre_FP1_",replacement = "")

for (i in channels) {
  newname<- paste("post",i,sep="_")
  
  temp<- select(completer, contains(newname))
  temp<- apply(temp, MARGIN=2, FUN=sd)
  
  completer_sds[newname] <- temp
}

rm(newname)
rm(temp)

# perform paired t-test on pre-post valus
variables <- as.vector(outer(channels, items, paste, sep='_'))
t_test_results <- data.frame(name=character(),p_value=numeric(),difference=numeric(),low_conf_int=numeric(),high_conf_int=numeric(),stringsAsFactors=FALSE)


for (i in variables) {
  name1 <- paste('post',i,sep='_')
  name2 <- paste('pre',i,sep='_')
  
  res<-t.test(completer[,name1],completer[,name2],paired=TRUE)
  
  this_one <- data.frame(name=character(),p_value=numeric(),difference=numeric(),low_conf_int=numeric(),high_conf_int=numeric(),stringsAsFactors=FALSE)
  this_one[1,1] = i
  this_one[1,2] = res$p.value
  this_one[1,3] = res$estimate
  this_one[1,4] = res$conf.int[1]
  this_one[1,5] = res$conf.int[2]
  t_test_results <- rbind(t_test_results, this_one)
  
}

# clean up messes

rm(name1)
rm(name2)
rm(res)
rm(variables)
rm(i)
rm(this_one)
t_test_positive <- filter(t_test_results, p_value<0.05)