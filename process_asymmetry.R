#process_asymmetry

# define pair
pair <- c("FP2-FP1","F8-F7","F4-F3","T4-T3","T6-T5","C4-C3","P4-P3","O2-O1")
electrodes_right <- c("FP2","F8","F4","T4","T6","C4","P4","O2")
electrodes_left <- c("FP1","F7","F3","T3","T5","C3","P3","O1")

pair_items <- as.vector(outer(as.vector(outer(c("pre","post"),pair,paste,sep='_')),items,paste,sep="_"))
rights <- as.vector(outer(as.vector(outer(c("pre","post"),electrodes_right,paste,sep='_')),items,paste,sep="_"))
lefts <- as.vector(outer(as.vector(outer(c("pre","post"),electrodes_left,paste,sep='_')),items,paste,sep="_"))

asymmetries <- data.frame(difference=numeric(nrow(completer)),stringsAsFactors=FALSE)

difference_lr <- data.frame(difference=numeric(nrow(completer)),stringsAsFactors=FALSE)

for (i in 1:length(rights)){
  diff <- ((completer[rights[i]] - completer[lefts[i]]) / (completer[lefts[i]] + completer[rights[i]]))
  asymmetries <- cbind(asymmetries,diff)
}

for (i in 1:length(rights)){
  diff <- ((completer[rights[i]] - completer[lefts[i]]))
  difference_lr <- cbind(difference_lr,diff)
}
asymmetries<-asymmetries[,-1]
difference_lr<-difference_lr[,-1]
names(asymmetries)<-pair_items
names(difference_lr)<-pair_items

# check pre-post difference of asymmetry
# dependency: process_t_test_func.R
checked_variables <- as.vector(outer(pair,items,paste,sep='_'))
index_change <- my_t(asymmetries,checked_variables)
difference_change <- my_t(difference_lr,checked_variables)
asymmetries_t <- t_vs_0(asymmetries)

