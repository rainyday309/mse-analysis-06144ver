#myplot.r
library('ggplot2')
library('grid')
library('gridExtra')

plots = list()

for (i in 1:10){  
  df <- data.frame(x=c(1:10),
                   y=total_mean[paste('V',toString(i),sep='')][[1]], 
                   ymin=total_mean[paste('V',toString(i),sep='')][[1]]-total_se[paste('V',toString(i),sep='')][[1]],
                   ymax=total_mean[paste('V',toString(i),sep='')][[1]]+total_se[paste('V',toString(i),sep='')][[1]], 
                   group=total_mean['group'][[1]])
  pd <- position_dodge(0.1)
  plots[[i]] <- ggplot(df,aes(x=x,y=y,colour=group)) + geom_point() + geom_errorbar(aes(ymin=ymin, ymax=ymax)) + ggtitle(paste('channel_',toString(i),sep=''))
}

# put plots in one page

page <- marrangeGrob(plots, ncol = 5, nrow=4, top = 'pre-trial vs post-trial')
ggsave("fig1_pre_post_plot.png", page, width=24, height = 16, dpi = 300)
