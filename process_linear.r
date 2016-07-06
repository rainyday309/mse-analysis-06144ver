#process_linear.r
#dependency: process.r
library('stringr')

# linear regression of approximate entropy versus scale
## create picture directory
mainDir <- getwd()
picPath<-file.path(mainDir, 'line_pics')
dir.create(picPath)

# draw pics with mse_means
mse_means <- completer_means[1:10,]
scale <- c(1:10)

for (i in 1:dim(mse_means)[2]) {
  picName <- file.path(picPath, paste('regression_line_',str_pad(i,2,pad='0'),'_',colnames(mse_means)[i],'.png',sep=''))
  png(file=picName)
    mytitle = paste('mean','of',colnames(mse_means)[i],sep=' ')
    plot(scale, mse_means[,i], main=mytitle, xlab='scale factor', ylab='appromixate entropy')
    abline(lm(mse_means[,i]~scale))
  dev.off()
}
