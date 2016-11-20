# dependency: process.r
# get sub items of hamd and cgi into dataframe

subitem <- read.csv('Fluoxetine106UTF.csv', header = TRUE, stringsAsFactors=FALSE, fileEncoding="UTF-8")
subitem <- subitem[,c(4,54:380)]
names(subitem)[1]='name'
completer_sub <-inner_join(completer,subitem,by = c('姓名' = 'name'))

# more exploration