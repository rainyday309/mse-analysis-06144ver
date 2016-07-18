#process_linear.r
#dependency: process.r
library('stringr')

# model 1: 
# pre_mse ~ sex + age +HAMD0


# model 2:
# (post-mse - pre-mse)/pre-mse ~ age + sex + (HAMD6-HAMD0)/HAMD0