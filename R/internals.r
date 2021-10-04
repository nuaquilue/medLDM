############ Used in cohort.establish.r and post.fire.r ############
.count.spp = function(x){
  return(c(sum(x==1), sum(x==2), sum(x==3), sum(x==4), sum(x==5), sum(x==6), sum(x==7),
           sum(x==8), sum(x==9), sum(x==10), sum(x==11), sum(x==12), sum(x==13)))
}

.select.cohort = function(x){
  if(sum(x)==0)
    return(14)
  else
    return(sample(1:14, 1, replace=F, prob=x))
}


############ Used in afforestation.r ############
.select.spp = function(x){
  if(sum(x)==0)
    return(NA)
  else
    return(sample(1:13, 1, replace=F, prob=x))
}

.count.spp.narm = function(x){
  return(c(sum(x==1, na.rm=T), sum(x==2, na.rm=T), sum(x==3, na.rm=T), sum(x==4, na.rm=T), 
           sum(x==5, na.rm=T), sum(x==6, na.rm=T), sum(x==7, na.rm=T), sum(x==8, na.rm=T), 
           sum(x==9, na.rm=T), sum(x==10, na.rm=T), sum(x==11, na.rm=T), sum(x==12, na.rm=T), 
           sum(x==13, na.rm=T)))
}


############ Used in land.dyn.model.r for tracking ############
.forest.areas = function(land, harvest){
  aux = land %>% filter(spp<=13 & spp!=9) %>% left_join(harvest, by="cell.id")  ## Harvestable species  
  dta = data.frame(forest = sum(land$spp<=13),                        # forest area
                   spp.harvestable = sum(land$spp<=13 & land$spp!=9), # harvestable species
                   non.protect = nrow(filter(aux, enpe==0)),          # from harvestable species, non-protected
                   national.park = nrow(filter(aux, enpe==1)),        # from harvestable species, national park  
                   enpe = nrow(filter(aux, enpe>=1)),                 # from harvestable species, protected but not national park  
                   no.park = nrow(filter(aux, enpe!=1)),              # from harvestable species, not national park  
                   slope30.nopark = nrow(filter(aux, enpe!=1, slope.pctg<=30)),    # from harvestable species, not in national.park, slope <=30% 
                   slope30.nopark.distpath1.5 = nrow(filter(aux, enpe!=1, slope.pctg<=30, dist.path<=1500)),  # from harvestable species, not in national.park, slope <=30% , ditance to path 1.5 km
                   slope30.nopark.distpath2.2 = nrow(filter(aux, enpe!=1, slope.pctg<=30, dist.path<=2200))   # from harvestable species, not in national.park, slope <=30% , ditance to path 2.2 km
                   )
  return(dta)
}
