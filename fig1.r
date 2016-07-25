# fig1.r
# plot sample entropy vs. scale factor in all scales
# y label: sample entropy
# x label: scale factor
# title: channel and pre - post test

library('ggplot2')
library('grid')
library('gridExtra')




df <- data.frame(x=c(1:10),  
                 y=completer_means[1:10,'pre_F7'],
                 ymin=completer_means[1:10,'pre_F7']-completer_ses[1:10,'pre_F7'],
                 ymax=completer_means[1:10,'pre_F7']+completer_ses[1:10,'pre_F7'])
pd <- position_dodge(0.1)
plot <- ggplot(df,aes(x=x,y=y)) + geom_point() + geom_errorbar(aes(ymin=ymin, ymax=ymax)) + ggtitle('pre_F7')
