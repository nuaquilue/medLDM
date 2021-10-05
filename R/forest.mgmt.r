#' Forest management
#'
#' Spatially allocation of sawlog and wood for energy demand according to a  land-cover transitions
#' 
#' @param land A \code{landscape} data frame with forest stand records and land-cover types in rows
#' @param clim A data frame with minimum temperature (in ºC), maximum temperature (in ºC), 
#' and precipitation (in mm) per location
#' @param sawlog.demand Sawlog volume to be harvested (in m3)
#' @param wood.demand Wood for energy volume to be harvested (in m3)
#' @param policy A string indicating which regional policy is adopted to allocate the timber demands
#' 
#' @return Data frames still to be polished (under development)!
#' 
#' @export
#' 
#' @examples
#' data(landscape)
#' data(clim)
#' data(orography)
#' land = landscape %>% mutate(typcut = NA, tscut  = NA, tburnt = NA)
#' land = land %>% left_join(sdm.sqi(land, orography, clim), by="cell.id")
#' forest.mgmt(land, clim, 100, 80)
#' 

forest.mgmt = function(land, clim, demand.sawlog=0, demand.wood=0, policy="BAU"){
  
  ## Tracking
  cat("Forest Management", "\n")
  
  ## Function to select items not in a vector
  `%notin%` = Negate(`%in%`)
  
  ## Check if there is demand to alloacte
  if(demand.sawlog + demand.wood == 0){
    return(list(extracted.sawlog=data.frame(cell.id=numeric()), extracted.wood=data.frame(cell.id=numeric())))
  }
  if(demand.sawlog == 0){
    extracted.sawlog=data.frame(cell.id=numeric())
  }
  extracted.wood=data.frame(cell.id=numeric())
  

  ## Track cut cells
  track.cut.cells = data.frame(cut.id=NA, cell.id=NA, spp=NA, cut.sawlog=NA, typcut=NA, 
                               pextract=NA, pwood=NA, vol.sawlog=NA, vol.wood=NA, ba.extract=NA)
  
  ## 8-neighbors neighbourhood used to compute the volume around each target cell
  default.neigh = data.frame(x=c(-1,1,ncol(mask),-ncol(mask))) #,2899,-2901,2901,-2899))
  default.nneigh = nrow(default.neigh)
  
  ## To be sure that non-harvestable covers are cut (no-forest and qsuber, not managed for sawlogs neither wood)
  i = land$spp<=13 & land$spp!=9      # 1.315.162 at t=1
  subland = land[i,]   
  
  
  ### Find cells suitable for management: forest with slope.pctg <=30% are very favorable, and slope.pcth<=50%
  ## are still accessible. Distance to the path network 2000m (including croplands). and dist.path <= 500m
  ## All tree species are harvestable exept Qsuber
  ## This is policy BAU
  lambda = 1/2000
  suit.mgmt = left_join(select(subland, -typdist,-tsdist, -tburnt), harvest, by="cell.id") %>%
              filter(slope.pctg <= 30) %>%            # exclude by slope > 30%
              filter(enpe %notin% c(1)) %>%           # only exclude 'parc nacional', before also exclude 3-'paratge natural interès nacional', 4-'reserva natural fauna salvatge', and 5-reseva natural integral'
              mutate(ppath=ifelse(dist.path>2200, 0, exp(-dist.path*lambda)))  %>%     # 2200=1500(terrestre)+700(cable aeri) dist - ppath: 0 - 1, 100 - 0.95, 500 - 0.77, 1000 - 0.6
              mutate(pslope=ifelse(slope.pctg <= 30, 1, 0.6)) %>% 
              mutate(psuit=pslope*ppath) %>% filter(psuit>0)
  
  ## Count stock (ha and volum) per species that is suitable for managment 
  suit.mgmt = suit.mgmt %>% left_join(ba.volbark, by="spp") %>% mutate(vol=c_ba*biom+c2_ba*biom*biom) 
  # track.stock.area = group_by(suit.mgmt, spp, sqi) %>% summarise(suitable=length(spp)) %>%
  #                    pivot_wider(names_from=sqi, values_from=suitable)
  # track.stock.vol = group_by(suit.mgmt, spp, sqi) %>% summarise(vol=sum(vol)) %>%
  #                   pivot_wider(names_from=sqi, values_from=vol)
  
  ## Plot stock of volume that is accessible (suitable for mgmt)
    # dta = data.frame(cell.id=1:ncell(MASK)) %>% left_join(select(suit.mgmt, cell.id, vol), by="cell.id")
    # MASK[] = dta$vol; plot(MASK, col=viridis(4))
    # writeRaster(MASK, paste0(out.path, "/lyr/suit.mgmt_t", t, ".tif"), format="GTiff", overwrite=T)
  
  ### Now identify cells that can be sustinably harvested. By now, Biomass and time since last intervention
  ## will make the restrictions, omitting age for cuttings (may not for thinnings?): 
  # sustain = left_join(suit.mgmt, harvest.prescrip, by=c("spp", "sqi")) 
  ## Plot biomass as a function of age per species
  # ggplot(suit.mgmt, aes(x=age, y=biom)) + geom_point()+ facet_wrap(~as.factor(spp))
  suit.mgmt$todo = NA
  
  ## 1. Preparatory cut
  to.prep.cut = left_join(filter(harvest.prescrip, prescription=="prep.cut"), suit.mgmt, by=c("spp", "sqi")) %>% 
                mutate(age.allow=(age>=min.age), ba.allow=(biom>=min.ba))
  dispo.prep.cut = group_by(to.prep.cut, spp, sqi) %>% summarise(age.ndispo=sum(age.allow), ba.ndispo=sum(ba.allow), 
                   all=length(spp)) %>% mutate(pct=ba.ndispo/all*100)
  ## Compute the volume that can be extracted in locations where harvesting is sustainable:
  ## Verify BA restriction and cutting cycle
  # vol.from.prep = filter(to.prep.cut, ba.allow, ifelse(!is.na(tscut), tscut>=ts.harvest, T))  %>% 
  #                  mutate(ba.extract=pmin((pctg.extract/100)*(biom/10), biom/10-remain.ba),
  #                         vol.extract=cx*ba.extract+cx2*ba.extract)  
  prep.cut = filter(to.prep.cut, ba.allow, ifelse(!is.na(tscut), tscut>=ts.harvest, T))
  suit.mgmt$todo[suit.mgmt$cell.id %in% unique(prep.cut$cell.id)] = "prep.cut"
  
  
  ## 2. Seed cut
  to.seed.cut = left_join(filter(harvest.prescrip, prescription=="seed.cut"), suit.mgmt, by=c("spp", "sqi")) %>% 
                mutate(age.allow=(age>=min.age), ba.allow=(biom>=min.ba)) %>% 
                filter(cell.id %notin% to.prep.cut$cell.id)
  dispo.seed.cut = group_by(to.seed.cut, spp, sqi) %>% summarise(age.ndispo=sum(age.allow), ba.ndispo=sum(ba.allow), 
                   all=length(spp)) %>% mutate(pct=ba.ndispo/all*100)
  ## Compute the volume that can be extracted in locations where harvesting is sustainable:
  ## Verify BA restriction, cutting cycle, and not cut under Preparatory cut
  # vol.from.seed = filter(to.seed.cut, ba.allow, ifelse(!is.na(tscut), tscut>=ts.harvest, T))  %>% 
  #   mutate(ba.extract=pmin((pctg.extract/100)*(biom/10), biom/10-remain.ba),
  #          vol.extract=cx*ba.extract+cx2*ba.extract) 
  seed.cut = filter(to.seed.cut, ba.allow, ifelse(!is.na(tscut), tscut>=ts.harvest, T))
                     # ifelse(cycle, typcut=="prep.cut", T))
  suit.mgmt$todo[suit.mgmt$cell.id %in% unique(seed.cut$cell.id)] = "seed.cut"
  
  
  ## 3. Removal cut
  to.removal.cut = left_join(filter(harvest.prescrip, prescription=="removal.cut"), suit.mgmt, by=c("spp", "sqi")) %>% 
                   mutate(age.allow=(age>=min.age), ba.allow=((biom)>=min.ba)) %>% 
                   filter(cell.id %notin% to.prep.cut$cell.id) %>%  filter(cell.id %notin% to.seed.cut$cell.id)
  dispo.removal.cut = group_by(to.removal.cut, spp, sqi) %>% summarise(age.ndispo=sum(age.allow), ba.ndispo=sum(ba.allow), 
                      all=length(spp)) %>% mutate(pct=ba.ndispo/all*100)
  ## The 0.90 volum to sawlogs
  # vol.from.removal = filter(to.seed.cut, ba.allow, ifelse(!is.na(tscut), tscut>=ts.harvest, T))  %>% 
  #   mutate(ba.extract=pmin((pctg.extract/100)*(biom/10), biom/10-remain.ba),
  #          vol.extract=cx*ba.extract+cx2*ba.extract) %>% filter(spp %notin% c(8,10,11)) ## quercus no s'extrauen per sawlog
  removal.cut = filter(to.removal.cut, ba.allow, ifelse(!is.na(tscut), tscut>=ts.harvest, T))
                        # ifelse(cycle, typcut=="seed.cut", T))
  suit.mgmt$todo[suit.mgmt$cell.id %in% unique(removal.cut$cell.id)] = "removal.cut"
  
  
  ## 4. Thinning 
  to.thinning = left_join(filter(harvest.prescrip, prescription=="thinning"), suit.mgmt, by=c("spp", "sqi")) %>% 
                mutate(age.min.allow=(age>=min.age), age.max.allow=(age<=max.age)) %>% filter(is.na(todo))
  dispo.thinning = group_by(to.thinning, spp, sqi) %>% summarise(age.min.ndispo=sum(age.min.allow), 
                            age.max.ndispo=sum(age.max.allow), all=sum(age.min.allow&age.max.allow)) 
  thinning = filter(to.thinning, age.min.allow, age.max.allow, ifelse(!is.na(tscut), tscut>=ts.harvest, T))
  suit.mgmt$todo[suit.mgmt$cell.id %in% unique(thinning$cell.id)] = "thinning"
  
  ## Des-allocated
  rm(subland); rm(i)
  rm(to.prep.cut); rm(dispo.prep.cut); rm(prep.cut)
  rm(to.seed.cut); rm(dispo.seed.cut); rm(seed.cut)
  rm(to.removal.cut); rm(dispo.removal.cut); rm(removal.cut)
  rm(to.thinning); rm(dispo.thinning); rm(thinning)
  
  
  ## Volume that can be extract per spp/spi/prescription
  sustain = filter(suit.mgmt, !is.na(todo)) %>% 
            left_join(select(harvest.prescrip, spp, sqi, prescription, pctg.extract, remain.ba),
                      by=c("spp", "sqi", "todo"="prescription")) %>% 
            mutate(ba.extract=pmin((pctg.extract/100)*(biom/10), ifelse(is.na(remain.ba), biom/10, biom/10-remain.ba)),
                   vol.extract=c_ba*ba.extract+c2_ba*ba.extract*ba.extract) 
  
  ## now determine the volume that may go to sawlogs, and the volume that may go to wood
  sustain$vol.extract.sawlog = 0
  aux = sustain$todo=="prep.cut"
  sustain$vol.extract.sawlog[aux] = sustain$vol.extract[aux]*runif(sum(aux),0.65,0.85)  #0.65 - 0.85
  aux = sustain$todo=="seed.cut"
  sustain$vol.extract.sawlog[aux] = sustain$vol.extract[aux]*runif(sum(aux),0.85,1)  #0.65 - 0.85
  aux = sustain$todo=="removal.cut" & sustain$spp %notin% c(8,10,11)
  sustain$vol.extract.sawlog[aux] = sustain$vol.extract[aux]*runif(sum(aux),0.95,1)   #0.90 - 0.95
  aux = sustain$todo=="removal.cut" & sustain$spp %in% c(8,10,11)
  sustain$vol.extract.sawlog[aux] = sustain$vol.extract[aux]*runif(sum(aux),0.1,0.2)

  ## ?thinnigs donen algo per sawlogs??
  sustain$vol.extract.wood = sustain$vol.extract - sustain$vol.extract.sawlog
  
  ## Adjust sawlog and wood demand
  # group_by(sustain, spp) %>% summarise(vol.saw=sum(vol.extract.sawlog), vol.wood=sum(vol.extract.wood))
  demand.sawlog = min(demand.sawlog, sum(sustain$vol.extract.sawlog))
  demand.wood = min(demand.wood, sum(sustain$vol.extract.wood))
  
  
  ## Give a priority rank to those cells that can be sustainably harvested
  ## Set probability of sawlog extraction according to (1) volume available for sawlog extraction, 
  ## (2) volume available for sawlog extraction in the neighborhood, (3) distance to forest industries, and
  ## (4) psuit=function(slope, distance to path, protected areas)
  ## To compute the probability of being harvested first, compute the volume available in the neighborhodd
  neigh.id = data.frame(cell.id=as.integer(rep(sustain$cell.id, each=default.nneigh)+
                                              rep(default.neigh$x, nrow(sustain))),
                        source.id=rep(sustain$cell.id, each=default.nneigh)) %>% 
             left_join(sustain, by="cell.id") %>% group_by(source.id) %>% 
             summarise(vol.neigh=sum(vol.extract.sawlog, na.rm=T))
  ## Weight of the factors
  # w1 = 0.3; w2 = 0.3; w3 = 0.005; w4 =0; w5 = 0.4-w3
  # w1 = 0.25; w2 = 0.25; w3 = 0.005; w4 =0.1; w5 = 0.4-w3
  w1 = 0.275; w2 = 0.25; w3 = 0.005; w4 = 0.075; w5 = 0.4-w3
  sustain = left_join(sustain, neigh.id, by=c("cell.id"="source.id")) %>% 
            mutate(f1=scales::rescale(pmin(vol.extract.sawlog, quantile(vol.extract.sawlog, p=0.9)), to=c(0,1)) ,
                   f2=scales::rescale(pmin(vol.neigh, quantile(vol.neigh, p=0.9)), to=c(0,1)) ,
                   f3=scales::rescale(pmin(1/dist.industry, quantile(1/dist.industry, p=0.9)), to=c(0,1)),
                   f4=ifelse(spp<=7,1,0)) %>% 
            mutate(p=w1*f1+w2*f2+w3*f3+w4*f4+w5*psuit)

  
  ## Harvest sawlogs: filter locations with vol.extract.sawlog>0, sort by "p", and select 
  ## as many loctions as needed to fullfil the demand
  extracted.sawlog = filter(sustain, vol.extract.sawlog>0)
  extracted.sawlog = extracted.sawlog[order(extracted.sawlog$p, decreasing=T),]
  cumulative.vol = cumsum(extracted.sawlog$vol.extract.sawlog)
  extracted.sawlog = extracted.sawlog[1:which(cumulative.vol>demand.sawlog)[1],]
  
  ## Count how much has been extracted of sawlogs and wood
  total.vol.extracted.sawlog = sum(sustain$vol.extract.sawlog[sustain$cell.id %in% extracted.sawlog$cell.id])
  total.vol.extracted.wood = sum(sustain$vol.extract.wood[sustain$cell.id %in% extracted.sawlog$cell.id])
  
  ## If there's still wood to extract, extract from Quercus 
  if(total.vol.extracted.wood<demand.wood){
    extracted.wood = filter(sustain, spp %in% c(8,10,11))
    extracted.wood = extracted.wood[order(extracted.wood$p, decreasing=T),]
    cumulative.vol = cumsum(extracted.wood$vol.extract.sawlog+extracted.wood$vol.extract.wood) # both volums go to wood
    extracted.wood = extracted.wood[1:which(cumulative.vol>demand.wood)[1],]
  }

  return(list(suit.mgmt=suit.mgmt, sustain=sustain, extracted.sawlog=extracted.sawlog, extracted.wood=extracted.wood))
  
}
  
