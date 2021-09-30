######################################################################################
##
######################################################################################

growth <- function(land, clim, what){
 
  ## Tracking
  cat(paste(what, "growth", "\n"))

  ## Growth of species when SDM in, species when SDM out, and shrub
  aux.spp <- filter(land, spp<=13) 
  aux.spp.sdmin <- filter(aux.spp, sdm==1) %>% left_join(growth.tree, by = c("spp", "sqi")) %>% 
                   mutate(increment = c0 + c_ab*biom/10 + c2_ab*(biom/10)^2 ) %>%
                   mutate(increment=ifelse(increment<0,0,increment)) %>% select(cell.id, increment)
  aux.spp.sdmout <- filter(aux.spp, sdm==0) %>% left_join(filter(growth.tree, sqi==1) %>% select(-sqi), by = "spp") %>% 
                    mutate(increment = c0 + c_ab*biom/10 + c2_ab*(biom/10)^2 ) %>%
                    mutate(increment=ifelse(increment<0,0,increment)) %>% select(cell.id, increment)
  aux.shrub <- filter(land, spp==14)  %>% left_join(growth.shrub, by="sqi")  %>%
               mutate(increment = a*log(biom) + b ) %>% 
               mutate(increment = ifelse(increment<=0 | is.infinite(increment), b, increment)/10) %>% 
               select(cell.id, increment)
      ## Divideixo per 10 el increment de creixement dels matollars pq al calcular biomass_t
      ## en functió de biomass_t-1, li sumo increment*10.
      ## Sumo increment*10 pq les unitats de la biomassa dels boscos són m2/ha*10 !! Tocada de pebrots.
  
  ## Join increment
  all <- rbind(aux.spp.sdmin, aux.spp.sdmout, aux.shrub)
  new.biom <- left_join(land, all, by = "cell.id") %>% mutate(biom=biom+increment*10) # %>% select(biom)
  
  return(new.biom$biom)
}
  