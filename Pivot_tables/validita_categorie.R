# calculates totals of PIR PSG, by group, bit and category. Inputs (filters): season, societa, pool and category (optional), if missing => takes all unique

validita_categorie<-function(stagione, soc, pools, categ){
  library(dplyr)
  library(reshape2)
  
  # 0: assign a dataset
  foo <-passaggi_dat
  
  if(missing(stagione)){
    stagione=unique(foo$season, incomparables = FALSE)}
  
  if(missing(pools)) {
    pools=unique(foo$NrPoolTessera, incomparables = FALSE)}
  
  if (missing(soc)) {
    soc=unique(foo$Societa, incomparables = FALSE)}
  
  if (missing(categ)) {
    categ=unique(foo$Categ_skimax, incomparables = FALSE)}
  
  # 1: filter to keep a required season, societa and pool. 
  foo = filter(foo, season %in% c(stagione))
  foo = filter(foo, Societa %in% c(soc), NrPoolTessera %in% c(pools), Categ_skimax %in% c(categ))
 
  # 2: group by dimentions
  foo = group_by(foo, group_name, bin_code, Categ_skimax)
  
  # 3: calculate two metrics needed
  foo = summarise(foo,
                  sum_pringressi = sum(QtyPrIngressi),
                  
                  sum_passaggi = sum(QtyPassaggi))
  
  # 4: melt the DF to put all the metrics a single col
  foo<-melt(foo, id.vars =c ("group_name", "bin_code", "Categ_skimax"), measure.vars= c("sum_pringressi", "sum_passaggi"))
  
  # 5: transpose and reshape
  foo = dcast(foo, group_name + bin_code ~ Categ_skimax + variable, value.var = "value")
  
  return(foo)
}

