#' Afforestation 
#'
#' Determines the shrub locations colonized by tree species
#' 
#' @param land A \code{landscape} data frame with forest stand records and land-cover types in rows
#' @param clim A data frame with minimum temperature (in ºC), maximum temperature (in ºC), 
#' and precipitation (in mm) per location
#' @param sdm A data frame with categorical species distribution model (0 or 1) for all tree species
#' (in columns) for all the locations in the study area. It is generated wiht the function \code{sdm.sqi}
#' @param params A list of model parameters, default ones are generated by the function \code{default.params()}  
#' 
#' @return A vector with the new tree species in colonized shurb locations
#' 
#' @export
#' 
#' @examples
#' data(landscape)
#' data(clim)
#' params = default.params()
#' land = dplyr::left_join(landscape, sdm.sqi(landscape, clim), by="cell.id")
#' afforestation(land, clim, params)
#' 

afforestation = function(land, clim, sdm, params){
  
  ## Tracking
  cat("Afforestation", "\n") 
  
  ## Join utm and sdm info to land
  land.utm = land %>% select(cell.id, spp, biom, age, sdm) %>% 
             left_join(select(orography, cell.id, utm), by="cell.id") 
  
  ## Calculate the percentage of old forest within its climatic niche per utm cell
  utm.forest = group_by(land.utm, utm) %>% summarise(nneigh=length(utm), old.neigh=sum(spp<=13 & age>=15 & sdm==1)) %>% 
               mutate(pct=old.neigh/nneigh)
  old.forest = land.utm %>% select(cell.id, utm) %>% left_join(select(utm.forest, utm, pct), by="utm") %>% 
               select(-utm)
  
  ## Put together all explanatory variables of the afforestation model
  dta = select(land.utm, cell.id, spp) %>% filter(spp==14) %>% 
        left_join(select(orography, cell.id, elev, slope), by="cell.id") %>% 
        left_join(select(clim, cell.id, tmin, precip), by="cell.id") %>% 
        left_join(old.forest, by="cell.id")
  
  ## Apply the afforestation model
  dta$z = afforest.mdl$intercept + afforest.mdl$elev*dta$elev + afforest.mdl$slope*dta$slope +
          afforest.mdl$precip*dta$precip + afforest.mdl$tmin*dta$tmin + afforest.mdl$pct*dta$pct +
          afforest.mdl$elev_pct*dta$elev*dta$pct + afforest.mdl$slope_pct*dta$slope*dta$pct + 
          afforest.mdl$precip_pct*dta$precip*dta$pct + afforest.mdl$tmin_pct*dta$tmin*dta$pct
  dta$p = 1/(1+exp(-1*dta$z))
  dta$p = 1-(1-dta$p)^(1/30)  # 30y period of obs, from 1987 to 2017
  dta$z = runif(nrow(dta), 0, 1) <= dta$p
  
  ## For those cells to afforestate, check if actually there are mature forest surrounding them
  dta = dta %>% filter(z)
  
  ## Num of neighbours in a circular neighbourhood according to radius (radius is in pixels)
  ## Assume that the neighbourhood is a star, with the maximum number of pixels in the
  ## east-west or north-south direction is 2*radius + 1 (1 is the center cell).
  ## The num of pixels is sequentially: 3+1*2, 5+3*2+1*2, 7+5*2+3*2+1*2, ...
  nneigh = seq(3,41,2) + cumsum(seq(1,40,2)*2)
  
  ## Coordinates of shruby cells and their closest neighbours
  shrub.coord = dta %>% select(cell.id, z) %>% left_join(coord, by = "cell.id") %>% select(-z)
  if(nrow(shrub.coord)>0){
    
    ## Find neighbours
    neigh.id = nn2(coord[,-1], shrub.coord[,-1],  searchtype="priority", k=nneigh[params$colon.rad]) 
    neigh.id = neigh.id$nn.idx  
    
    ## Identify the species of the mature forest neigbhours 
    ## Do not count for the cell itself, so do not use first colume of neigh.id data.frame
    neigh.spp = matrix(land$spp[neigh.id[,-1]], nrow=nrow(neigh.id), ncol=ncol(neigh.id)-1, byrow=F) # spp of the neighbours
    
    ## Check if species are within their climatic niche in the neighbour cells
    for(i in 1:13){
      neigh.sdm = matrix(sdm[neigh.id[,-1], i+1], nrow=nrow(neigh.id), ncol=ncol(neigh.id)-1, byrow=F) # sdm of the neighbours
      neigh.spp[neigh.spp==i] = neigh.spp[neigh.spp==i] * neigh.sdm[neigh.spp==i] # mask neighbours with sdm 0
    }
    neigh.spp = neigh.spp * matrix(land$age[neigh.id[,-1]]>=15, nrow=nrow(neigh.id), ncol=ncol(neigh.id)-1) # mask young neighbours 
    
    ## Count the presence of each species, mask its presence if the species is out the climatic niche in
    ## the target location, and give double weight to conifers
    count.neigh.spp = t(apply(neigh.spp, 1, .count.spp.narm)) * 
      matrix(species$seed.pressure[species$spp<=13], nrow=nrow(neigh.id), ncol=13, byrow=T) *
      matrix(c(sdm$spp1[neigh.id[,1]], sdm$spp2[neigh.id[,1]], sdm$spp3[neigh.id[,1]],
               sdm$spp4[neigh.id[,1]], sdm$spp5[neigh.id[,1]], sdm$spp6[neigh.id[,1]],
               sdm$spp7[neigh.id[,1]], sdm$spp8[neigh.id[,1]], sdm$spp9[neigh.id[,1]],
               sdm$spp10[neigh.id[,1]], sdm$spp11[neigh.id[,1]], sdm$spp12[neigh.id[,1]],
               sdm$spp13[neigh.id[,1]]), nrow=nrow(neigh.id), ncol=13)
    
    ## Assign new species to those cells to afforestate, if available
    dta$spp = apply(count.neigh.spp, 1, .select.spp) 
    dta = dta %>% filter(!is.na(spp)) 
    
    ## Join climatic and orographic variables to compute sq and then sqi
    res = dta %>% left_join(select(orography, cell.id, aspect, slope.stand), by = "cell.id") %>%
      left_join(sq.tree, by = "spp") %>% 
      mutate(aux=c0+c_mnan*tmin+c2_mnan*tmin*tmin+c_plan*precip+c2_plan*precip*precip+
               c_aspect*ifelse(aspect!=1,0,1)+c_slope*slope.stand) %>%
      mutate(sq=1/(1+exp(-1*aux))) %>% mutate(sqi=ifelse(sq<=p50, 1, ifelse(sq<=p90, 2, 3))) %>%
      select(cell.id, spp, sqi) 
    return(res)
  }
  else{
    return(numeric())
  }
}

