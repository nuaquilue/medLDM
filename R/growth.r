#' Vegetation growth
#'
#' Growht of basal area of forest stands and biomass of shrublands
#' 
#' @param land A \code{landscape} data frame with forest stand records and land-cover types in rows
#' @param clim A data frame with minimum temperature (in ºC), maximum temperature (in ºC), 
#' and precipitation (in mm) per location
#' @param who String to display to what type of communitites is growing 
#' 
#' @return A vector with the new basal area or biomass for forest stands or shrublands, respectively
#' 
#' @export
#' 
#' @examples
#' data(landscape)
#' data(clim)
#' land = dplyr::left_join(landscape, sdm.sqi(landscape, clim), by="cell.id")
#' res = growth(land, clim)
#' 

growth = function(land, clim, who = ""){
 
  ## Tracking
  cat(paste(who, "growth", "\n"))

  ## Growth of species when SDM in, species when SDM out, and shrub
  aux.spp = filter(land, spp<=13) 
  aux.spp.sdmin = filter(aux.spp, sdm==1) %>% left_join(growth.tree, by = c("spp", "sqi")) %>% 
                  mutate(increment = c0 + c_ab*biom + c2_ab*biom*biom ) %>%
                  mutate(increment=ifelse(increment<0,0,increment)) %>% select(cell.id, increment)
  aux.spp.sdmout = filter(aux.spp, sdm==0) %>% left_join(filter(growth.tree, sqi==1) %>% select(-sqi), by = "spp") %>% 
                   mutate(increment = c0 + c_ab*biom + c2_ab*biom*biom ) %>%
                   mutate(increment=ifelse(increment<0,0,increment)) %>% select(cell.id, increment)
  aux.shrub0 = filter(land, spp==14 & biom==0) %>% left_join(growth.shrub, by="sqi") %>%
               mutate(increment = b ) %>% select(cell.id, increment)
  aux.shrub = filter(land, spp==14 & biom>0) %>% left_join(growth.shrub, by="sqi") %>%
              mutate(increment = a*log(biom) + b ) %>% 
              mutate(increment = ifelse(increment<=0, b, increment)) %>% 
              select(cell.id, increment)
      
  ## Join increment
  all = rbind(aux.spp.sdmin, aux.spp.sdmout, aux.shrub0, aux.shrub)
  new.biom = left_join(land, all, by = "cell.id") %>% mutate(biom=biom+increment) 
  return(new.biom$biom)
}
  