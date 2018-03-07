total_by_validita<-function(stagione){
  library(dplyr)
  library(reshape2)
  
  # 0: take initial dataset
  foo <-passaggi_dat
  
  # 1: filter to keep one season. 
  foo = filter(foo, season %in% c(stagione))
  
  # 2: set up data frame for by-group processing.
  foo = group_by(foo, group_name, bin_code)
  
  # 3: calculate the summary metrics
  foo = summarise(foo,
                                sum_primingressi = sum(QtyPrIngressi),
                                sum_passaggi = sum(QtyPassaggi))
  return(foo)
}
