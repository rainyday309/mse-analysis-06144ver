library(dplyr)

# original trial data obtain
raw <- read.csv('fluoxetin_eeg_with_hamd.csv', header = TRUE, stringsAsFactors=FALSE, fileEncoding="UTF-8")
raw<-raw[,1:26]

# fix typo in original data
raw$post_EEG[95]="FY158"

spectral <- read.csv('spectralOutput.csv', header = TRUE, stringsAsFactors=FALSE, fileEncoding="UTF-8")
prespectral<-spectral[grepl('FX',x=spectral$name),]
colnames(prespectral)<-paste('pre',colnames(prespectral),sep='_')
postspectral<-spectral[grepl('FY',x=spectral$name),]
colnames(postspectral)<-paste('post',colnames(postspectral),sep='_')

# join original trial data to spectral data
fulltable<-left_join(raw,prespectral,c("pre_EEG"="pre_name"))
fulltable<-left_join(fulltable,postspectral,c("post_EEG"="post_name"))
fulltable$病歷號碼 = NULL
fulltable$姓名 = NULL
fulltable$收案日期 = NULL
fulltable$生日 = NULL

# completer: remove drop out cases and those without eeg daa
completer <- filter(fulltable,分類=='c')
completer <- filter(completer, !is.na(pre_total_power_FP1))
completer <- filter(completer, !is.na(post_total_power_FP1))
