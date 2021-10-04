#' Land interfaces
#'
#' Determines the land interface of each location
#' 
#' @param land A \code{landscape} data frame with forest stand records and land-cover types in rows
#' @param orography A data frame with the orographic characteristics of the study area
#' 
#' @return A vector with the code interface of all locations in the study area (1 - urban, 2 - cropland, 3 - natural areas, 4 - other, 
#' 5 - urban & cropland, 6 - urban & natural areas, and 7 - cropland & natural areas)
#' 
#' @export
#' 
#' @examples
#' data(landscape)
#' data(orography)
#' x = interface(landscape, orography)
#' 

interface = function(land, orography){
  
  ## Traking
  cat("Land interfaces", "\n")
  
  ## Join utm info to land
  land.utm = left_join(land, select(orography, cell.id, utm), by="cell.id") 
  
  ## Count each land type per utm, compute percentages
  landtype = aggregate(list(tot=land.utm$spp>0,
                            urb=land.utm$spp==20, 
                            crp=land.utm$spp==16|land.utm$spp==17,
                            nat=land.utm$spp<=15,
                            oth=land.utm$spp==18|land.utm$spp==19), list(utm=land.utm$utm), sum)
  landtype[,3:6] = 100*landtype[,3:6]/landtype[,2]
  
  ## Apply rules to classify
  dta = data.frame(utm=landtype$utm,
                   urb=landtype$urb>=80, crp=landtype$crp>=80,
                   nat=landtype$nat>=80, oth=landtype$oth>=80)
  dta$urbcrp = landtype$urb>=20 & landtype$crp>30 & dta$crp==0
  dta$urbnat = landtype$urb>=20 & landtype$nat>30 & dta$nat==0
  dta$crpnat = landtype$urb<20 & landtype$crp>=20 & landtype$nat>=20 & dta$crp==0 & dta$nat==0
  dta$tot = dta$urb+dta$crp+dta$nat+dta$oth+dta$urbcrp+dta$urbnat+dta$crpnat
  
  ##  Double classification of UrbNat i UrbCrp
  dta$urbnat[dta$tot>1] = ifelse(landtype$crp[dta$tot>1]>landtype$nat[dta$tot>1], F, T)
  dta$urbcrp[dta$tot>1] = ifelse(landtype$crp[dta$tot>1]<=landtype$nat[dta$tot>1], F, T)
  dta$tot = dta$urb+dta$crp+dta$nat+dta$oth+dta$urbcrp+dta$urbnat+dta$crpnat
  
  ## Some UTM has not been classified, so relax conditions
  dta$urb[dta$tot==0] = landtype$urb[dta$tot==0]>=75
  dta$crp[dta$tot==0] = landtype$crp[dta$tot==0]>=75
  dta$nat[dta$tot==0] = landtype$nat[dta$tot==0]>=75
  dta$oth[dta$tot==0] = landtype$oth[dta$tot==0]>=75
  dta$crpnat[dta$tot==0] = landtype$urb[dta$tot==0]<20 & 
                            landtype$crp[dta$tot==0]>=15 & landtype$nat[dta$tot==0]>=15 & 
                              dta$crp[dta$tot==0]==0 & dta$nat[dta$tot==0]==0 &
                                ((landtype$nat[dta$tot==0]>=15 & landtype$crp[dta$tot==0]>=15) | 
                                  (landtype$nat[dta$tot==0]+landtype$crp[dta$tot==0]>=75))
  dta$tot = dta$urb+dta$crp+dta$nat+dta$oth+dta$urbcrp+dta$urbnat+dta$crpnat  
  
  ## Final assignation, the maximal 
  landtype$max = NA
  landtype$max[dta$tot==0] = pmax(landtype$urb[dta$tot==0], landtype$crp[dta$tot==0], 
                                  landtype$nat[dta$tot==0], landtype$oth[dta$tot==0])
  dta$urb[dta$tot==0] = ifelse(landtype$urb[dta$tot==0] == landtype$max[dta$tot==0], T, F)
  dta$tot[dta$tot==0 & dta$urb] = 1
  dta$crp[dta$tot==0] = ifelse(landtype$crp[dta$tot==0] == landtype$max[dta$tot==0], T, F)
  dta$tot[dta$tot==0 & dta$crp] = 1
  dta$nat[dta$tot==0] = ifelse(landtype$nat[dta$tot==0] == landtype$max[dta$tot==0], T, F)
  dta$tot[dta$tot==0 & dta$nat] = 1
  dta$oth[dta$tot==0] = ifelse(landtype$oth[dta$tot==0] == landtype$max[dta$tot==0], T, F)
  dta$tot[dta$tot==0 & dta$oth] = 1
  
  ## Numeric value
  dta = dta[,-ncol(dta)]
  dta$x = apply(dta[,-1] * matrix(1:7, nrow=nrow(dta), ncol=7, byrow=T), 1, sum )
  
  ## Join to the final data.frame
  land.utm = left_join(land.utm, select(dta, utm, x), by="utm") %>% select(x)
  
  return(unlist(land.utm))
    
}
