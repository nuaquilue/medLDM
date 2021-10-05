#' Compute SDM and SQI for forest species 
#'
#' Computes the species distribution model (SDM) and the site quality index (SQI) of forest stands
#' 
#' @param land A \code{landscape} data frame with forest stand records and land-cover types in rows
#' @param clim A data frame with default minimum temperature (in ºC), maximum temperature (in ºC), 
#' and precipitation (in mm) per location
#' 
#' @return A data frame with the binary SDM (0 or 1) and the categorical SQI (1-low, 2-high, 3-optimal) for
#' each location in the study area identifed by \code{cell.id}
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
  
  ## Join land.cover.spp, aspect and slope data
  clim.sdm.sqi = clim %>% left_join(select(land, cell.id, spp), by="cell.id") %>% left_join(orography, by="cell.id")
  
  ## Assign SDM according to current spp distribution 
  ## Oct-2021: It has to be replaced by SDM equations according to climatic variables
  ## By now it reads the sdm data.frame and it is only load for the current climate conditions
  ## that is, for decade = NA
  clim.sdm.sqi$sdm = NA
  clim.sdm.sqi$sdm[clim.sdm.sqi$spp==1] = sdm$sdm.phalepensis[clim.sdm.sqi$spp==1]
  clim.sdm.sqi$sdm[clim.sdm.sqi$spp==2] = sdm$sdm.pnigra[clim.sdm.sqi$spp==2]
  clim.sdm.sqi$sdm[clim.sdm.sqi$spp==3] = sdm$sdm.ppinea[clim.sdm.sqi$spp==3]
  clim.sdm.sqi$sdm[clim.sdm.sqi$spp==4] = sdm$sdm.psylvestris[clim.sdm.sqi$spp==4]
  clim.sdm.sqi$sdm[clim.sdm.sqi$spp==5] = sdm$sdm.ppinaster[clim.sdm.sqi$spp==5]
  clim.sdm.sqi$sdm[clim.sdm.sqi$spp==6] = sdm$sdm.puncinata[clim.sdm.sqi$spp==6]
  clim.sdm.sqi$sdm[clim.sdm.sqi$spp==7] = sdm$sdm.aalba[clim.sdm.sqi$spp==7]
  clim.sdm.sqi$sdm[clim.sdm.sqi$spp==8] = sdm$sdm.qilex[clim.sdm.sqi$spp==8]
  clim.sdm.sqi$sdm[clim.sdm.sqi$spp==9] = sdm$sdm.qsuber[clim.sdm.sqi$spp==9]
  clim.sdm.sqi$sdm[clim.sdm.sqi$spp==10] = sdm$sdm.qfaginea[clim.sdm.sqi$spp==10]
  clim.sdm.sqi$sdm[clim.sdm.sqi$spp==11] = sdm$sdm.qhumilis[clim.sdm.sqi$spp==11]
  clim.sdm.sqi$sdm[clim.sdm.sqi$spp==12] = sdm$sdm.fsylvatica[clim.sdm.sqi$spp==12]
  clim.sdm.sqi$sdm[clim.sdm.sqi$spp==13] = sdm$sdm.other[clim.sdm.sqi$spp==13]
  clim.sdm.sqi$sdm[clim.sdm.sqi$spp==14] = 1  ## SDM of shrub is always 1
  
  ## Compute SQ and SQI
  clim.sdm.sqi = clim.sdm.sqi %>% left_join(sq.tree, by="spp") %>% left_join(sq.index, by="spp") %>% 
                 mutate(aux=c0+c_mnan*tmin+c2_mnan*tmin*tmin+c_plan*precip+c2_plan*precip*precip+
                        c_aspect*ifelse(aspect!=1,0,1)+c_slope*slope.stand) %>%
                 mutate(sq=1/(1+exp(-1*aux))) %>% mutate(sqi=ifelse(sq<=p50, 1, ifelse(sq<=p90, 2, 3))) %>%
                 select(cell.id, spp, tmin, tmax, precip, sdm, sqi)
  
  ## SQI for shrubs
  sqi.shrub = filter(clim.sdm.sqi, spp == 14) %>% select(spp, tmin, tmax, precip) %>% 
              mutate(aux.brolla=sq.shrub$c0_brolla+sq.shrub$c_temp_brolla*tmin+sq.shrub$c_temp2_brolla*tmin*tmin+sq.shrub$c_precip_brolla*precip+sq.shrub$c_precip2_brolla*precip*precip,
                     aux.maquia=sq.shrub$c0_maquia+sq.shrub$c_temp_maquia*tmin+sq.shrub$c_temp2_maquia*tmin*tmin+sq.shrub$c_precip_maquia*precip+sq.shrub$c_precip2_maquia*precip*precip,
                     aux.boix=sq.shrub$c0_boix+sq.shrub$c_temp_boix*tmin+sq.shrub$c_temp2_boix*tmin*tmin+sq.shrub$c_precip_boix*precip+sq.shrub$c_precip2_boix*precip*precip,
                     sq.brolla=1/(1+exp(-1*aux.brolla)), sq.maquia=1/(1+exp(-1*aux.maquia)), sq.boix=1/(1+exp(-1*aux.boix)))%>% 
              mutate(sqest.brolla=sq.brolla/max(sq.brolla), sqest.maquia=sq.maquia/max(sq.maquia), sqest.boix=sq.boix/max(sq.boix),
              sqi=ifelse(sqest.brolla>=sqest.maquia & sqest.brolla>=sqest.boix, 1,
                    ifelse(sqest.maquia>=sqest.brolla & sqest.maquia>=sqest.boix, 2,
                      ifelse(sqest.boix>=sqest.brolla & sqest.boix>=sqest.maquia, 3, 0))))
  clim.sdm.sqi$sqi[clim.sdm.sqi$spp==14] = sqi.shrub$sqi
  
  return(clim.sdm.sqi = select(clim.sdm.sqi, cell.id, sdm, sqi))
}



