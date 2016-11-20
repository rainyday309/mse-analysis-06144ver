# get slope coefficient and summarize data
# dependent on process.r

# get pre-post difference for exploration
stuff <- outer(channels,items,paste,sep='_')
for (i in stuff){
  
  completer[(paste('diff',stuff,sep='_'))] = completer[paste('post',stuff,sep='_')] - completer[paste('pre',stuff,sep='_')]
}

rm(stuff)

# get slope coefficient of scale 3-10

slopes <- outer(c("pre","post"),channels,paste,sep='_')

get_scales <- function(x,name,name_list){
  # name: the variable name to be start with. eg pre_FP1_scale3
  # name_list: the name list input for finding location
  start = which(name_list %in% name)
  slope <- as.vector(as.matrix(x[start:(start+7)]))
  slope <- lm(slope~c(1:8))$coefficients[2]
  slope
}

for (i in slopes){
  title = paste(i,'slope',sep='_')
  completer[title] <- apply(completer,MARGIN=1,FUN=get_scales,paste(i,'scale3',sep='_'),names(completer))  
}

# clean_up
rm(slopes)
rm(title)
rm(i)

# summarize data
library('psych')
z_completer_desc <- describe(completer)
z_responder_desc <- describe(responder)
z_non_responder_desc <- describe(non_responder)
z_remission_desc <- describe(remission)
z_non_remission_desc <- describe(non_remission)
non_completer <- fulltable[(!(fulltable$id %in% completer$id)),]
z_non_completer_desc <-describe(non_completer)

