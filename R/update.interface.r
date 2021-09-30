update.interface <- function(land, orography){
  
  ## Traking
  cat("Update interfaces", "\n")
  
  ## Join utm info to land
  land.utm <- left_join(land, select(orography, cell.id, utm), by="cell.id") 
  
  ## Count each land type per utm, compute percentages
  landtype <- aggregate(list(tot=land.utm$spp>0,
                             urb=land.utm$spp==20, 
                             crp=land.utm$spp==16|land.utm$spp==17,
                             nat=land.utm$spp<=15,
                             oth=land.utm$spp==18|land.utm$spp==19), list(utm=land.utm$utm), sum)
  landtype[,3:6] <- 100*landtype[,3:6]/landtype[,2]
  
  ## Apply rules to classify
  interface <- data.frame(utm=landtype$utm,
                          urb=landtype$urb>=80, crp=landtype$crp>=80,
                          nat=landtype$nat>=80, oth=landtype$oth>=80)
  interface$urbcrp <- landtype$urb>=20 & landtype$crp>30 & interface$crp==0
  interface$urbnat <- landtype$urb>=20 & landtype$nat>30 & interface$nat==0
  interface$crpnat <- landtype$urb<20 & landtype$crp>=20 & landtype$nat>=20 & interface$crp==0 & interface$nat==0
  interface$tot <- interface$urb+interface$crp+interface$nat+interface$oth+
                   interface$urbcrp+interface$urbnat+interface$crpnat
  ##  Double classification of UrbNat i UrbCrp
  interface$urbnat[interface$tot>1] <- ifelse(landtype$crp[interface$tot>1]>landtype$nat[interface$tot>1], F, T)
  interface$urbcrp[interface$tot>1] <- ifelse(landtype$crp[interface$tot>1]<=landtype$nat[interface$tot>1], F, T)
  interface$tot <- interface$urb+interface$crp+interface$nat+interface$oth+
                   interface$urbcrp+interface$urbnat+interface$crpnat
  
  ## Some UTM has not been classified, so relax conditions
  interface$urb[interface$tot==0] <- landtype$urb[interface$tot==0]>=75
  interface$crp[interface$tot==0] <- landtype$crp[interface$tot==0]>=75
  interface$nat[interface$tot==0] <- landtype$nat[interface$tot==0]>=75
  interface$oth[interface$tot==0] <- landtype$oth[interface$tot==0]>=75
  interface$crpnat[interface$tot==0] <- landtype$urb[interface$tot==0]<20 & 
                                        landtype$crp[interface$tot==0]>=15 & landtype$nat[interface$tot==0]>=15 & 
                                        interface$crp[interface$tot==0]==0 & interface$nat[interface$tot==0]==0 &
                                        ((landtype$nat[interface$tot==0]>=15 & landtype$crp[interface$tot==0]>=15) | 
                                           (landtype$nat[interface$tot==0]+landtype$crp[interface$tot==0]>=75))
  interface$tot <- interface$urb+interface$crp+interface$nat+interface$oth+
                   interface$urbcrp+interface$urbnat+interface$crpnat  
  
  ## Final assignation, the maximal 
  landtype$max <- NA
  landtype$max[interface$tot==0] <- pmax(landtype$urb[interface$tot==0], landtype$crp[interface$tot==0], 
                                         landtype$nat[interface$tot==0], landtype$oth[interface$tot==0])
  interface$urb[interface$tot==0] <- ifelse(landtype$urb[interface$tot==0] == landtype$max[interface$tot==0], T, F)
  interface$tot[interface$tot==0 & interface$urb] <- 1
  interface$crp[interface$tot==0] <- ifelse(landtype$crp[interface$tot==0] == landtype$max[interface$tot==0], T, F)
  interface$tot[interface$tot==0 & interface$crp] <- 1
  interface$nat[interface$tot==0] <- ifelse(landtype$nat[interface$tot==0] == landtype$max[interface$tot==0], T, F)
  interface$tot[interface$tot==0 & interface$nat] <- 1
  interface$oth[interface$tot==0] <- ifelse(landtype$oth[interface$tot==0] == landtype$max[interface$tot==0], T, F)
  interface$tot[interface$tot==0 & interface$oth] <- 1
  
  ## Numeric value
  interface <- interface[,-ncol(interface)]
  interface$x <- apply(interface[,-1] * matrix(1:7, nrow=nrow(interface), ncol=7, byrow=T), 1, sum )
  
  ## Join to the final data.frame
  land.utm <- left_join(land.utm, select(interface, utm, x), by="utm") %>% select(x)
  
  return(unlist(land.utm))
    
}
