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