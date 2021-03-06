#' Encroachment
#'
#' Determines the grassland locations colonized by shrubs
#' 
#' @param land A \code{landscape} data frame with forest stand records and land-cover types in rows
#' 
#' @return A vector with the \code{cell.id} of the colonized grassland areas
#' 
#' @export
#' 
#' @examples
#' data(landscape)
#' encroachment(landscape)
#' 

encroachment <- function(land){
  
  ## Tracking
  cat("Encroachment", "\n") 

  ## Coordinates of grass cells and their closest neighbours (do not count for the cell itself)
  grass.coord <- land %>% filter(spp==15) %>% select(cell.id) %>% left_join(coord, by = "cell.id") 
  neigh.id <- nn2(coord[,-1], grass.coord[,-1],  searchtype="priority", k=9) 
  neigh.id <- neigh.id$nn.idx  
  
  ## Count the forest and shrub cells in the 8-cells neighbourhood
  count.forest.shrub <- function(x){
    return(sum(x<=14))
  }
  neigh.spp <- matrix(land$spp[neigh.id[,-1]], nrow=nrow(neigh.id), ncol=ncol(neigh.id)-1) # spp of the neighbours
  nneighforest <- apply(neigh.spp, 1, count.forest.shrub)
  
  ## Put together all explanatory variables of the encroachment model
  dta <- land %>% select(cell.id, spp, tsdist) %>% filter(spp==15) %>% 
    left_join(select(orography, cell.id, elev, slope, radsol), by="cell.id") %>% 
    mutate(nneighforest=nneighforest)
  
  ## Apply the encroachment model and decide which pixels transform to shrub
  dta$z <- encroach.mdl$intrc + encroach.mdl$elev*dta$elev + encroach.mdl$slope*dta$slope +
    encroach.mdl$radsol*dta$radsol + encroach.mdl$nneighforest*dta$nneighforest +
    encroach.mdl$grassage*pmin(dta$tsdist,20)
  dta$p <- 1/(1+exp(-1*dta$z))
  dta$z <- runif(nrow(dta), 0, 1) <= dta$p  

  ## cell.id of those transitioning from grass to shrub
  res <- dta %>% filter(z) %>% select(cell.id)
    
  return(res)
}