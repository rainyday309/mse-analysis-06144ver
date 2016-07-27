# fig1.r
# plot sample entropy vs. scale factor in all scales
# y label: sample entropy
# x label: scale factor
# title: channel and pre - post test

library('ggplot2')
library('grid')
library('gridExtra')


# transform summarized data for plotting
p<-completer[,27:786]
completer_transformed <- as.data.frame(colMeans(p), stringsAsFactors = FALSE)
completer_transformed['name'] <- rownames(completer_transformed)
completer_transformed['se'] <- as.data.frame(apply(X = p,MARGIN = 2, FUN = sd))/sqrt(length(completer[,1]))
colnames(completer_transformed)[1]='means'

rm(p)

something <-regmatches(completer_transformed$name,regexec(pattern = "(pre|post)_([a-zA-Z1-9]{2,3})_(.*)",text = completer_transformed$name))
something <-unlist(something)
something <-as.data.frame(t(matrix(something,nrow=4,ncol=760)),stringsAsFactors = FALSE)

transformed <- data.frame(name=completer_transformed$name, means=completer_transformed$means, ses=completer_transformed$se,prepost=something$V2,location=something$V3,measure=something$V4 ,stringsAsFactors = FALSE)
rm(something)
rm(completer_transformed)

# completed transform

# plot all electrodes
transformed_scales <- filter(transformed, grepl(x=measure, pattern='scale\\d'))
transformed_scales['scale'] <-as.numeric(t(matrix(unlist(regmatches(transformed_scales$measure, regexec(pattern = "scale([0-9]+)",text=transformed_scales$measure))),nrow=2,ncol=380))[,2])

plots<-list()

for (i in 1:19){
  topic <- filter(transformed_scales,location==channels[i])
  
  topic$ymin <- topic$means-topic$ses
  topic$ymax <- topic$means+topic$ses
  
  pd <- position_dodge(0.1)
  plot <- ggplot(topic,aes(x=scale,y=means,color=prepost),xlab='mean entropy') + geom_line() + geom_errorbar(aes(ymin=ymin, ymax=ymax)) + ggtitle(channels[i])
  plots[[i]] <- plot
}

rm(plot)

# arrnage plots in a grid

position <- matrix(c(NA,1,20,2,NA,3,4,5,6,7,8,9,10,11,12,13,14,15,16,17,NA,18,NA,19,NA),ncol=5,nrow=5,byrow = TRUE)

# extracts legend
# shamelessly from https://github.com/hadley/ggplot2/wiki/Share-a-legend-between-two-ggplot2-graphs
g_legend<-function(a.gplot){
  tmp <- ggplot_gtable(ggplot_build(a.gplot))
  leg <- which(sapply(tmp$grobs, function(x) x$name) == "guide-box")
  legend <- tmp$grobs[[leg]]
  return(legend)
}

# add legend back
plots[[20]] <- g_legend(plots[[1]])

for (i in 1:19) {plots[[i]] = plots[[i]] +theme(legend.position='none')}


grid.arrange(grobs=plots, layout_matrix=position)

#save
g <- arrangeGrob(grobs=plots, layout_matrix=position) #generates g
ggsave(file="fig1_pre_post", plot = g ,dpi = 300,width = 5,height = 4) #saves g