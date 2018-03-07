# calculates diff and percet diff to benchmark season (base.s)


benchmark_validita <-function(base.s, soc, pools, categ){
  library(dplyr)
  library(reshape2)
  
  # 0: assign a dataset
  foo <-passaggi_dat
  
  if(missing(pools)) {
    pools=unique(foo$NrPoolTessera, incomparables = FALSE)}
  
  if (missing(soc)) {
    soc=unique(foo$Societa, incomparables = FALSE)}
  
  if (missing(categ)) {
    categ=unique(foo$Categ_skimax, incomparables = FALSE)}
  
  # 1: filter to keep a required season, societa and pool. 
  foo = filter(foo, Societa %in% c(soc), NrPoolTessera %in% c(pools), Categ_skimax %in% c(categ))
  
  # 2: group by dimentions
  foo = group_by(foo, group_name, bin_code, season)
  
  # 3: calculate two metrics needed
  foo = summarise(foo,
                  pringressi = sum(QtyPrIngressi),
                  passaggi = sum(QtyPassaggi))

  # 4: melt the DF to put all the metrics a single col
  foo<-melt(foo, id.vars =c ("group_name", "bin_code", "season"), measure.vars= c("pringressi", "passaggi"))
  
  DT<- setDT(foo)[, Differ := value - value[which(season == base.s)], c("group_name", "bin_code", "variable")]
  DT<- setDT(DT)[ , P.Differ := sprintf("%1.2f%%",100*(value - value[which(season == base.s)])/value[which(season == base.s)]), c("group_name", "bin_code", "variable")]
  
  # 5: transpose and reshape
  DT <- dcast(DT, group_name + bin_code ~ season + variable, value.var = c("Differ", "P.Differ"))
  
  return(DT)
}
