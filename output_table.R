# dependency: process.R
# table1 comparation of completer and dropout cases

library('car')

fulltable['dropout'] <- as.factor(!(fulltable$id %in% completer$id))

# chi-square test: age vs dropout

mean(fulltable[fulltable$dropout=='FALSE',]$sex)
mean(fulltable[fulltable$dropout=='TRUE',]$sex)
chisq.test(as.matrix(table(fulltable$dropout,fulltable$sex)))

get_comparation <- function(variable){
  answer <- list()
  answer <- list(answer,mean_completer=list(mean(fulltable[fulltable$dropout=='FALSE',][[variable]])))
  answer <- list(answer,sd_completer=list(sd(fulltable[fulltable$dropout=='FALSE',][[variable]])))
  answer <- list(answer,mean_dropout=list(mean(fulltable[fulltable$dropout=='TRUE',][[variable]])))
  answer <- list(answer,sd_dropout=list(sd(fulltable[fulltable$dropout=='TRUE',][[variable]])))
  answer <- list(answer,t_test=t.test(fulltable[fulltable$dropout=='TRUE',][[variable]], fulltable[fulltable$dropout=='FALSE',][[variable]], paired=FALSE))
  answer
}

# age
get_comparation('age')

# age of onset
get_comparation('onset')

# previous episodes
get_comparation('過去發作')

# HAMD-17 at onset
get_comparation('HAMD0')


