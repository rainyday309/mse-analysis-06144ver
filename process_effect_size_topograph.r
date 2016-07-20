# process_difference_topograph.r
# provide data for change in effect size in topograph
# hold

library('stringr')


variables <- as.vector(outer(channels, items, paste, sep='_'))

