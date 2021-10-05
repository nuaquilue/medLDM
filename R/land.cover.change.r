#' Land-cover changes
#'
#' Spatially allocation of land-cover transitions
#' 
#' @param land A \code{landscape} data frame with forest stand records and land-cover types in rows
#' @param lc.trans A number indicating the land-cover transition to simulate: 1 - urbanization, 2 - agriculture 
#' conversion, and 3 - rural abandonment
#' @param target.demand Area to be converted to the target land-cover (in ha)
#' @param visit.cells Vector with the \code{cell.id} of already changed cells in the current time step
#' 
#' @return A vector with the \code{cell.id} of the colonized grassland areas
#' 
#' @export
#' 
#' @examples
#' data(landscape)
#' data(coord)
#' land.cover.change(landscape, coord, 1, 10, numeric())
#' 

land.cover.change = function(land, lc.trans, target.demand, visit.cells){

  ## If no demand to allocate
  if(target.demand==0)
    return(numeric())

  ## Function to select items not in a vector
  `%notin%` = Negate(`%in%`)

  ## Land cover transition
  cat(paste0("Land-cover transition: ", 
             ifelse(lc.trans==1, "Urbanization", 
                    ifelse(lc.trans==2, "Agriculture conversion", 
                            ifelse(lc.trans==3, "Rural abandonment", "Undefined")))), "\n")
  

  ## Join land-cover/spp info to coordinates to preselect coordinates of those cells that may
  ## undergo change per each land-cover transition
  coord.land = left_join(coord, select(land, cell.id, spp), by="cell.id")
  
  ## Define per each land-cover transition, the transition potential layer (probability of ignition) 
  ## and the land-cover types that can potentially change to the target land-cover
  ## By now, the transition potential is 1 for interfaces containg the target land-cover
  if(lc.trans==1){
    trans.pot = land$interface %in% c(1,5,6)
    lc.source = land$spp<=17
    coord.land = filter(coord.land, spp<=17) %>% select(-spp)
  }
  else if(lc.trans==2){
    trans.pot = land$interface %in% c(2,5,7)
    lc.source = land$spp<=15 
    coord.land = filter(coord.land, spp<=15) %>% select(-spp)
  }
  else if(lc.trans==3){
    trans.pot = land$interface %in% c(3,6,7)
    lc.source = land$spp>=16 & land$spp<=17
    coord.land = filter(coord.land, spp>=16 & spp<=17) %>% select(-spp)
  }
  
  ## Idem for parameters driving spatial aggregation of change
  ligni = land.cover.change.pattern$ligni[lc.trans]
  lsprd = land.cover.change.pattern$lsprd[lc.trans]
  k = land.cover.change.pattern$k[lc.trans]
  
  
  ## Choose according to trans.pot and lc.source as many cells as target demand to potentially start 
  ## clusters of change. For ignition points, wt is as wt.ini.
  ## Remove cells that have alread been changed by other land-cover transitions
  chg = data.frame(cell.id=sample(land$cell.id, size=target.demand, p=trans.pot*lc.source, replace = F)) %>%
        mutate(wt.ini = rexp(target.demand, rate=ligni)) %>% mutate(wt=wt.ini) %>%
        filter(cell.id %notin% visit.cells)
  
  
  ## Select around 20% of cells to start a cluster (at least one cell) according to wt.ini
  front = sample(chg$cell.id, size=pmax(1,round(nrow(chg)*0.01)), p=1/chg$wt.ini^2, replace=F)
  
  
  ## Make effective the change
  achg = length(front)
  chg.cells = front 
  

  ## Keep spreading the change until area changed is at least as target demand
  while(achg <= target.demand){
  
    ## Look for 8+1 neighbours of front cells
    neighs = nn2(coord.land[,-1], filter(coord.land, cell.id %in% front)[,-1], searchtype="priority", k=9)  
    
    ## Recuperate initial waiting times of the first source cells
    wt.inis = matrix(unlist(filter(chg, cell.id %in% front) %>% select(wt.ini)), 
                      nrow=length(front), ncol=8, byrow=F)
    
    ## Remove front cells from the 'chg' data.frame
    ## Then add new cells that (1) are less than 200m appart from the source cell, 
    ## (2) a waiting time = rexp(lsprd) * w.tini^k (if a cell is visited more than once, 
    ## keep the mininum waitint time), and (3) have not been changed by other land-cover transitions
    chg = rbind(filter(chg, cell.id %notin% front), 
                 data.frame(cell.id=coord.land$cell.id[neighs$nn.idx[,-1][neighs$nn.dists[,-1] <200]], 
                            wt.ini=wt.inis[neighs$nn.dists[,-1] <200]) %>% 
                      mutate(wt = rexp(sum(neighs$nn.dists[,-1] <200), rate=lsprd) * wt.ini^k ) %>%
                      filter(cell.id %notin% visit.cells)) %>%
           group_by(cell.id) %>% summarise(wt.ini=min(wt.ini), wt=min(wt))
                      
    ## Select around 10% of cells to start new clusters or kepp spreading from current ones according to wt
    front = sample(chg$cell.id, size=pmax(1,round(nrow(chg)*0.01)), p=1/chg$wt^2, replace=F)
    
    ## Make effective the change
    achg = achg + length(front)
    chg.cells = c(chg.cells, front)
    visit.cells = c(visit.cells, chg.cells)
  }
  
  return(chg.cells)
}
