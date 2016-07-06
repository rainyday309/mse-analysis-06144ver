library(dplyr)

# original trial data obtain
raw <- read.csv('fluoxetin_eeg_with_hamd.csv', header = TRUE, stringsAsFactors=FALSE, fileEncoding="UTF-8")
raw<-raw[,1:26]

# fix typo in original data
raw$post_EEG[95]="FY158"

mse <- read.csv('avgOutput0614.csv', header = TRUE, stringsAsFactors=FALSE, fileEncoding="UTF-8")
premse<-mse[grepl('FX',x=mse$name),]
colnames(premse)<-gsub(colnames(premse),pattern = 'ch',replacement = 'pre_ch')
postmse<-mse[grepl('FY',x=mse$name),]
colnames(postmse)<-gsub(colnames(postmse),pattern = 'ch',replacement = 'post_ch')

# calculate mean mse across scales

for (i in 1:20) {
  channel_name <- paste("_ch",i,"_",sep="")
  output_name <- paste("pre",channel_name,"meanMse",sep="")
  
  premse[output_name] <- rowMeans(select(premse, contains(channel_name)))
}

for (i in 1:20) {
  channel_name <- paste("_ch",i,"_",sep="")
  output_name <- paste("post",channel_name,"meanMse",sep="")
  
  postmse[output_name] <- rowMeans(select(postmse, contains(channel_name)))
}


# join original trial data to mse data
fulltable<-left_join(raw,premse,c("pre_EEG"="name"))
fulltable<-left_join(fulltable,postmse,c("post_EEG"="name"))
fulltable$病歷號碼 = NULL
fulltable$姓名 = NULL
fulltable$收案日期 = NULL
fulltable$生日 = NULL

# rename channels from number to position

channels <- c('FP1', 'FP2', 'F7', 'F3', 'FZ', 'F4', 'F8', 'T3', 'C3', 'Cz', 'C4', 'T4', 'T5', 'P3', 'Pz', 'P4', 'T6', 'O1', 'O2', 'Oz')
newnames <- colnames(fulltable)

for (i in 20:1) {
  newnames <- sub(pattern=paste('_ch',i,'_',sep=''), replacement=paste('_',channels[i],'_',sep=''), x=newnames)
}

colnames(fulltable) <- newnames
rm(newnames)



# completer: remove drop out cases and those without eeg daa
completer <- filter(fulltable,分類=='c')
completer <- filter(completer, !is.na(pre_FP1_scale1))
completer <- filter(completer, !is.na(post_FP1_scale1))

rm('premse')
rm('postmse')


########################
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