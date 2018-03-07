# calculates totals and percentages to total of PIR PSG, by season, society, and pools

total_by_validita<-function(stagione, soc, pools){
  library(dplyr)
  library(reshape2)
  
  # 0: assign a dataset
  foo <-passaggi_dat

  
  if(missing(pools)) {
    pools=unique(foo$NrPoolTessera, incomparables = FALSE)}
  
   if (missing(soc)) {
             soc=unique(foo$Societa, incomparables = FALSE)}
      
  
  # 1: filter to keep one season, societa and pool. 
  foo = filter(foo, season %in% c(stagione), Societa %in% c(soc))
  foo =filter (foo, NrPoolTessera %in% c(pools))
  
                   # 1.1: run summary before grouping to create a vector of Grand totals
                   gt<-summarise(foo, sum_primingressi = sum(QtyPrIngressi), sum_passaggi = sum(QtyPassaggi))
                  
     
  # 2: set up data frame for by-group processing.
  foo = group_by(foo, group_name, bin_code)
  
  # 3: calculate the summary metrics
  foo = summarise(foo,
                                sum_pringressi = sum(QtyPrIngressi),
                                percent_pringressi = sprintf("%1.2f%%", 100*(sum_pringressi/gt[1,1])),
                                sum_passaggi = sum(QtyPassaggi),
                                percent_passaggi = sprintf("%1.2f%%", 100*(sum_passaggi/gt[1,2])))
  return(foo)
}
