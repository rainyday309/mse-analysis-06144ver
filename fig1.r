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

# select only scales
transformed_scales <- filter(transformed, grepl(x=measure, pattern='scale\\d'))
transformed_scales['scale'] <-as.numeric(t(matrix(unlist(regmatches(transformed_scales$measure, regexec(pattern = "scale([0-9]+)",text=transformed_scales$measure))),nrow=2,ncol=380))[,2])


topic <- filter(transformed_scales,location=='F7')

topic$ymin <- topic$means-topic$ses
topic$ymax <- topic$means+topic$ses

pd <- position_dodge(0.1)
plot <- ggplot(topic,aes(x=scale,y=means,color=prepost)) + geom_line() + geom_errorbar(aes(ymin=ymin, ymax=ymax)) + ggtitle('entropy change over scales at F7')
