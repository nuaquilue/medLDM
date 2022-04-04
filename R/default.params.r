#' Default model parameters
#'
#' Returns a list with the medLDM - Mediterranean Landscape Dynamic Model parameters and the default values
#'
#' @return A list with the following items:
#'  \itemize{
#'    \item{\code{species}: Vector with the name of the tree species (sorted as codified in the \code{landscape} data frame)}
#'    \item{\code{spp.distrib.rad}: Radius of the neighborhood (in pixels) to find out if a species is present in a region}
#'    \item{\code{colon.rad}: Radius of the neighborhood (in pixels) to look for mature tree species ready to colonize shrubs
#'     or locations killed by drought}
#'    \item{\code{rpb}: Spread rate}
#'    \item{\code{pb.upper.th}: If spread rate is above this threshold, the cell always burns}
#'    \item{\code{pb.lower.th}: If spread rate is below this threshold, the cell never burns}
#'    \item{\code{fire.intens.th}: high vs. low intensity fire, sr <= fire.intens.th}
#'    \item{\code{pb.target.area}: if NA, then burnt as \code{7*pb.convenient.area}, otherwise annually burnt pb.fix.area}
#'    \item{\code{pb.convenient.area}}
#'    \item{\code{accum.burnt.area}}
#'    \item{\code{pb.mean}}
#'    \item{\code{pb.sd}: Standard deviation of Normal distribution to drawn prescribed burn sizes}
#'    \item{\code{pb.fage.th}: Minimum forest age to apply prescribed burns, in years}
#'  }
#'    
#' @export
#' 
#' @examples
#' # Change the convenient area to be burnt by prescribed burns to 15.000 ha 
#' params = default.params()
#' params$pb.convenient.area = 15000
#' 

default.params = function(){
  return(list(
    
    ## List the name of the forest species
    species = c("phalepensis", "pnigra", "ppinea", "psylvestris", "ppinaster", "puncinata",
                 "aalba", "qilex", "qsuber", "qfaginea", "qhumilis", "fsylvatica", "other"),
    
    # Radius of the neighborhood (in pixels) to find out if a species is present in a region
    spp.distrib.rad = 25 ,	# i.e. 2500 m -> 2.5 km, a ~circular (in fact it's a star) of radius 2.5 km (circle of 5 km diameter)
    # Radius of the neighborhood (in pixels) to look for mature tree species ready to colonize shrubs
    # or locations killed by drought
    colon.rad = 5 ,		# i.e. 500 m
    
    ## Fire parameters (should not change to much): Spread rate, burn probability, prescribed burns
    rpb = 0.2,
    pb.upper.th = 0.8,
    pb.lower.th = -1,
    fire.intens.th = 0.19,  # high vs. low intensity fire, sr <= fire.intens.th
    pb.target.area = NA  ,# if NA, then burnt as 7*pb.convenient.area, otherwise annually burnt pb.fix.area
    pb.convenient.area = 400 ,## should be 15.000 ha
    accum.burnt.area = rep(400,7),
    pb.mean = 1.974,
    pb.sd = 0.683,
    pb.fage.th = 30 ## minimum forest age to apply prescribed burns
  ))
  
}