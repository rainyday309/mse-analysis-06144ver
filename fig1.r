# fig1.r
# plot sample entropy vs. scale factor in all scales
# y label: sample entropy
# x label: scale factor
# title: channel and pre - post test

library('ggplot2')
library('grid')
library('gridExtra')

p<-completer[,27:786]
completer_transformed <- as.data.frame(colMeans(p))
completer_transformed['name'] <- rownames(completer_transformed)
completer_transformed['se'] <- as.data.frame(apply(X = p,MARGIN = 2, FUN = sd))/length(completer[,1])
colnames(completer_transformed)[1]='means'

rm(p)

something <-regmatches(completer_transformed$name,regexec(pattern = "(pre|post)_([a-zA-Z1-9]{2,3})_(.*)",text = completer_transformed$name))
something <-unlist(something)
something <-as.data.frame(t(matrix(something,nrow=4,ncol=760)))

transformed <- data.frame(name=completer_transformed$name, means=completer_transformed$means, ses=completer_transformed$se,prepost=something$V2,location=something$V3,measure=something$V4 ,stringsAsFactors = FALSE)
rm(something)
rm(completer_transformed)

df <- data.frame(x=c(1:10),  
                 y=completer_means[1:10,'pre_F7'],
                 ymin=completer_means[1:10,'pre_F7']-completer_ses[1:10,'pre_F7'],
                 ymax=completer_means[1:10,'pre_F7']+completer_ses[1:10,'pre_F7'])
pd <- position_dodge(0.1)
plot <- ggplot(df,aes(x=x,y=y)) + geom_point() + geom_errorbar(aes(ymin=ymin, ymax=ymax)) + ggtitle('pre_F7')
