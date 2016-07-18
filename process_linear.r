#process_linear.r
#dependency: process.r
library('stringr')


variables <- as.vector(outer(channels, items, paste, sep='_'))
# model 1: 
# pre_mse ~ sex + age +HAMD0


# model 2:
# (post-mse - pre-mse)/pre-mse ~ age + sex + (HAMD6-HAMD0)/HAMD0