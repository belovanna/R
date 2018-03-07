# calculates totals and percentages to total of PIR PSG, by season, society, and pools

totals<-function(){
  library(dplyr)
  library(reshape2)
  
  # 0: assign a dataset
  foo <-passaggi_dat
  vend<-Vendite_db
  
  
  # 2: set up data frame for by-group processing.
  foo = group_by(foo, group_name, season)
  vend = group_by(vend, group_name, season)
  
  # 3: calculate the summary metrics
  foo = summarise(foo,
                  sum_pringressi = sum(QtyPrIngressi),
                  sum_passaggi = sum(QtyPassaggi))
  vend =summarise(vend,
                  sum_pringressi = sum(Quantita),
                  sum_passaggi = sum(Importo))                
                  
   totals<-cbind(foo, vend)               
  return(totals)
}
