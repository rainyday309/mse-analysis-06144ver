library(dplyr)

# original trial data obtain
raw <- read.csv('fluoxetin_eeg_with_hamd.csv', header = TRUE, stringsAsFactors=FALSE, fileEncoding="UTF-8")
raw<-raw[,1:26]

# fix typo in original data
raw$post_EEG[95]="FY158"

# response rate reverse sign, defined as HAMD6-HAMD0/HAMD6
raw['response_rate'] = -raw['response_rate']

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

# rename channels from number to position

channels <- c('FP1', 'FP2', 'F7', 'F3', 'Fz', 'F4', 'F8', 'T3', 'C3', 'Cz', 'C4', 'T4', 'T5', 'P3', 'Pz', 'P4', 'T6', 'O1', 'O2', 'Oz')
newnames <- colnames(fulltable)

for (i in 20:1) {
  newnames <- sub(pattern=paste('_ch',i,'_',sep=''), replacement=paste('_',channels[i],'_',sep=''), x=newnames)
}

colnames(fulltable) <- newnames
rm(newnames)


# remove Oz data
fulltable<-fulltable[!grepl(pattern='Oz',x=colnames(fulltable))]
channels <- channels[1:19]

# completer: remove drop out cases and those without eeg daa
completer <- filter(fulltable,分類=='c')
completer <- filter(completer, !is.na(pre_FP1_scale1))
completer <- filter(completer, !is.na(post_FP1_scale1))

# clear all unused variables
rm('mse')
rm('premse')
rm('postmse')
rm('channel_name')
rm('output_name')
rm('i')

# read spectral analysis data
spectral <- read.csv('spectralOutput.csv', header = TRUE, stringsAsFactors=FALSE, fileEncoding="UTF-8")
prespectral<-spectral[grepl('FX',x=spectral$name),]
colnames(prespectral)<-paste('pre',colnames(prespectral),sep='_')
postspectral<-spectral[grepl('FY',x=spectral$name),]
colnames(postspectral)<-paste('post',colnames(postspectral),sep='_')

# join spectral data to original trial data
fulltable<-left_join(fulltable,prespectral,c("pre_EEG"="pre_name"))
fulltable<-left_join(fulltable,postspectral,c("post_EEG"="post_name"))
completer<-left_join(completer,prespectral,c("pre_EEG"="pre_name"))
completer<-left_join(completer,postspectral,c("post_EEG"="post_name"))

# all items compared list in this object
items <- c('scale1','scale2','scale3','scale4','scale5','scale6','scale7','scale8','scale9','scale10','meanMse','total_power','delta_abs','theta_abs','alpha_abs','beta_abs','delta_rel','theta_rel','alpha_rel','beta_rel')

rm('prespectral')
rm('postspectral')
rm('spectral')