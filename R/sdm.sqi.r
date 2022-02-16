#' Compute SDM and SQI for forest species 
#'
#' Computes the species distribution model (SDM) and the site quality index (SQI) of current forest stands,
#' and the SDM of all forest species in any location
#' 
#' @param land A \code{landscape} data frame with forest stand records and land-cover types in rows
#' @param clim A data frame with default minimum temperature (in ºC), maximum temperature (in ºC), 
#' and precipitation (in mm) per location
#' 
#' @return A list of two data frames, \code{land.sdm.sqi} with the binary SDM (0 or 1) 
#' and the categorical SQI (1-low, 2-high, 3-optimal) for each location in the study area
#' identifed by \code{cell.id} according to the current forest species, and \code{sdm.allspp} with 
#' the binary SDM (0 or 1) of forest tree species (in columns) for all locations in the study area.
#' 
#' @export
#' 
#' @examples
#' data(landscape)
#' data(clim)
#' x = sdm.sqi(landscape, clim)
#' 

sdm.sqi = function(land, clim){
  
  cat(paste("Assign categorical SDM and SQI to forest stands\n"))

  ##################### SDM and SQI of current forest stands #####################
  ## Join species, aspect and slope data
  land.sdm.sqi = clim %>% left_join(select(land, cell.id, spp), by="cell.id") %>% 
    left_join(select(orography, cell.id, slope.stand, aspect), by="cell.id")
  
  ## SDM for single tree species
  clim.sdm.tree = filter(land.sdm.sqi, spp<=12) %>% left_join(sdm.tree, by="spp") %>% 
    mutate(aux=c0+c_mnan*tmin+c2_mnan*tmin*tmin+c_mxan*tmax+c2_mxan*tmax*tmax+
             c_plan*precip+c2_plan*precip*precip+c_pet*pet+c2_pet*pet*pet) %>%
    mutate(aux/(1+exp(-1*aux))) %>% 
    mutate(sdm=ifelse(aux<=th_p10, 0 , 1)) %>% select(cell.id, sdm)
  
  ## SDM for other trees
  clim.sdm.other = filter(land.sdm.sqi, spp==13) %>% left_join(sdm.tree, by="spp") %>% 
    mutate(aux=c0+c_mnan*tmin+c2_mnan*tmin*tmin+c_mxan*tmax+c2_mxan*tmax*tmax+
             c_plan*precip+c2_plan*precip*precip+c_pet*pet+c2_pet*pet*pet) %>%
    mutate(aux/(1+exp(-1*aux))) %>% 
    mutate(sdm=ifelse(aux<=th_p10, 0, 1)) %>% group_by(cell.id) %>% summarise(sdm=max(sdm))
  
  ## Assign SDM 
  land.sdm.sqi = land.sdm.sqi %>% left_join(rbind(clim.sdm.tree, clim.sdm.other), by="cell.id")
  land.sdm.sqi$sdm[land.sdm.sqi$spp==14] = 1  ## SDM of shrub is always 1
  
  ## Compute SQ and SQI
  clim.sqi.tree = filter(land.sdm.sqi, spp<=13) %>% left_join(sq.tree, by="spp") %>% 
                  mutate(aux=c0+c_mnan*tmin+c2_mnan*tmin*tmin+c_plan*precip+c2_plan*precip*precip+
                         c_aspect*ifelse(aspect!=1,0,1)+c_slope*slope.stand) %>%
                  mutate(sq=1/(1+exp(-1*aux))) %>% mutate(sqi=ifelse(sq<=p50, 1, ifelse(sq<=p90, 2, 3))) %>%
                  select(cell.id, sqi)
  
  ## SQI for shrubs
  clim.sqi.shrub = filter(land.sdm.sqi, spp == 14) %>% select(cell.id, spp, tmin, tmax, precip) %>% 
              mutate(aux.brolla=sq.shrub$c0_brolla+sq.shrub$c_temp_brolla*tmin+sq.shrub$c_temp2_brolla*tmin*tmin+sq.shrub$c_precip_brolla*precip+sq.shrub$c_precip2_brolla*precip*precip,
                     aux.maquia=sq.shrub$c0_maquia+sq.shrub$c_temp_maquia*tmin+sq.shrub$c_temp2_maquia*tmin*tmin+sq.shrub$c_precip_maquia*precip+sq.shrub$c_precip2_maquia*precip*precip,
                     aux.boix=sq.shrub$c0_boix+sq.shrub$c_temp_boix*tmin+sq.shrub$c_temp2_boix*tmin*tmin+sq.shrub$c_precip_boix*precip+sq.shrub$c_precip2_boix*precip*precip,
                     sq.brolla=1/(1+exp(-1*aux.brolla)), sq.maquia=1/(1+exp(-1*aux.maquia)), sq.boix=1/(1+exp(-1*aux.boix)))%>% 
              mutate(sqest.brolla=sq.brolla/max(sq.brolla), sqest.maquia=sq.maquia/max(sq.maquia), sqest.boix=sq.boix/max(sq.boix),
              sqi=ifelse(sqest.brolla>=sqest.maquia & sqest.brolla>=sqest.boix, 1,
                    ifelse(sqest.maquia>=sqest.brolla & sqest.maquia>=sqest.boix, 2,
                      ifelse(sqest.boix>=sqest.brolla & sqest.boix>=sqest.maquia, 3, 0)))) %>% 
              select(cell.id, sqi)
  
  ## Assign SQI
  land.sdm.sqi = land.sdm.sqi %>% left_join(rbind(clim.sqi.tree, clim.sqi.shrub), by="cell.id") %>% 
                 select(cell.id, sdm, sqi)
  
  
  ##################### SDM of tree species in the study area #####################
  ## First of all build a matrix with the SDM of all the tree species in all the locations
  sdm = select(coord, cell.id) %>% left_join(clim, by="cell.id") 
  offset = ncol(sdm)
  for(i in 1:13){
    coef = filter(sdm.tree, spp==i)
    if(i<=12){
      sdm$x = coef$c0 + coef$c_mnan*sdm$tmin+ coef$c2_mnan*sdm$tmin*sdm$tmin +   
        coef$c_mxan*sdm$tmax+ coef$c2_mxan*sdm$tmax*sdm$tmax +
        coef$c_plan*sdm$precip+ coef$c2_plan*sdm$precip*sdm$precip +   
        coef$c_pet*sdm$pet+ coef$c2_pet*sdm$pet*sdm$pet
      sdm$x = sdm$x/(1+exp(-1*sdm$x))
      sdm$x = ifelse(sdm$x < coef$th_p10, 0, 1)
    }
    if(i==13){
      sdm$x1 = coef$c0[1] + coef$c_mnan[1]*sdm$tmin+ coef$c2_mnan[1]*sdm$tmin*sdm$tmin +   
        coef$c_mxan[1]*sdm$tmax+ coef$c2_mxan[1]*sdm$tmax*sdm$tmax +
        coef$c_plan[1]*sdm$precip+ coef$c2_plan[1]*sdm$precip*sdm$precip +   
        coef$c_pet[1]*sdm$pet+ coef$c2_pet[1]*sdm$pet*sdm$pet
      sdm$x1 = sdm$x1/(1+exp(-1*sdm$x1))
      sdm$x1 = ifelse(sdm$x1 < coef$th_p10[1], 0, 1)
      sdm$x2 = coef$c0[2] + coef$c_mnan[2]*sdm$tmin+ coef$c2_mnan[2]*sdm$tmin*sdm$tmin +   
        coef$c_mxan[2]*sdm$tmax+ coef$c2_mxan[2]*sdm$tmax*sdm$tmax +
        coef$c_plan[2]*sdm$precip+ coef$c2_plan[2]*sdm$precip*sdm$precip +   
        coef$c_pet[2]*sdm$pet+ coef$c2_pet[2]*sdm$pet*sdm$pet
      sdm$x2 = sdm$x2/(1+exp(-1*sdm$x2))
      sdm$x2 = ifelse(sdm$x2 < coef$th_p10[2], 0, 1)
      sdm$x3 = coef$c0[3] + coef$c_mnan[3]*sdm$tmin+ coef$c2_mnan[3]*sdm$tmin*sdm$tmin +   
        coef$c_mxan[3]*sdm$tmax+ coef$c2_mxan[3]*sdm$tmax*sdm$tmax +
        coef$c_plan[3]*sdm$precip+ coef$c2_plan[3]*sdm$precip*sdm$precip +   
        coef$c_pet[3]*sdm$pet+ coef$c2_pet[3]*sdm$pet*sdm$pet
      sdm$x3 = sdm$x3/(1+exp(-1*sdm$x3))
      sdm$x3 = ifelse(sdm$x3 < coef$th_p10[3], 0, 1)
      sdm$x = pmax(pmax(sdm$x1, sdm$x2), sdm$x3)
      sdm = select(sdm, -x1, -x2, -x3)
    }
    names(sdm)[i+offset] = paste0("spp", i)
  }
  sdm = select(sdm, -tmin, -tmax, -precip, -pet) %>% mutate(spp14=1) ## SDM of shrub is always 1
  
  return(list(land.sdm.sqi = land.sdm.sqi, sdm = sdm))
}



