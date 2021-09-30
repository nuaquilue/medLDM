#' The medLDM 
#'
#' Run the Mediterranean Landscape Dynamic Model medLDM that includes the processes of land-cover changes
#' wilfires, prescribed burns, fire suppression, timber and wood harvesting and vegetation dynamics.
#'
#' @param is.land.cover.change A flag to indicate that land cover changes are simulated
#' @param is.harvest A flag to indicate that harvesting for sawlogs and wood is simulated
#' @param is.widlfire A flag to indicate that wildfires are simualted
#' @param is.prescribed.burnt A flag to indicate that prescribed burns are simulated
#' @param is.drought A flag to indicate that prescribed burns are simulated
#' @param is.postfire A flag to indicate that prescribed burns are simulated
#' @param is.cohort.establish A flag to indicate that prescribed burns are simulated
#' @param is.afforestation A flag to indicate that prescribed burns are simulated
#' @param is.encroachment A flag to indicate that prescribed burns are simulated
#' @param is.growth A flag to indicate that prescribed burns are simulated
#' @param spin.up A flag to indicate if the observed 2010-2019 wildfires and land-cover changes are replicated
#' @param custom.params List with the model paramaters and default and/or user-defined values 
#' @param clim.proj A list of data frame with projections of climatic variables (minimum temperature, annual precipitation) for each \code{clim.step }
#' @param nrun Number of replicates to run the model
#' @param time.step Number of years of each time step
#' @param time.step Number of years of each climatic period
#' @param time.horizon Number of years of the model simulation, it has to be a multiple \code{time.step}
#' @param save.land A flag to save as a RDS file the \code{landscape} data frame at the time step indicated in \code{out.seq}
#' @param out.seq Numeric vector with the time steps the \code{landscape} is saved
#' @param out.path String with the directory path to save the \code{landscape} data frame at each time step indicated in \code{out.seq}
#'
#' @return A list with the following items:
#'  \itemize{
#'    \item{\code{SppByAgeClass}: A data frame of species abundance per age class, with columns:
#'      \itemize{
#'         \item{\code{run}: Number of replicate.}
#'         \item{\code{year}: Year YYYY.}
#'         \item{\code{mgmt.unit}: Code of the forest management unit (FMU).}
#'         \item{\code{spp}: Code of the species.}
#'         \item{\code{age.class}: Code of the age class, \code{C10}, \code{C30}, \code{C50}, \code{C70}, \code{C90}, and \code{OLD}.}
#'         \item{\code{area}: Area in \eqn{km^{2}}.}
#'       }
#'    }
#'    \item{\code{SuitabilityClass}: A data frame of suitability of potential species per bioclimatic domain, with columns:
#'      \itemize{
#'         \item{\code{run}: Number of replicate.}
#'         \item{\code{year}: Year YYYY.}
#'         \item{\code{bioclim.domain}: Code of the bioclimatic domain.}
#'         \item{\code{spp}: Code of the species.}
#'         \item{\code{poor}: Area in \eqn{km^{2}} of poor environmental suitability.}
#'         \item{\code{med}: Area in \eqn{km^{2}} of intermediate environmental suitability.}
#'         \item{\code{good}: Area in \eqn{km^{2}} of good environmental suitability.}
#'       }
#'    }
#'    \item{\code{SppByFireZone}: A data frame of species abundance per fire zone, with columns:
#'      \itemize{
#'         \item{\code{run}: Number of replicate.}
#'         \item{\code{year}: Year YYYY.}
#'         \item{\code{frz}: Code of the fire regime zone.}
#'         \item{\code{spp}: Code of the species.}
#'         \item{\code{area}: Area in \eqn{km^{2}}.}
#'       }
#'    }
#'    \item{\code{FuelByFireZone}: A data frame of fuel type per fire zone, with columns:
#'      \itemize{
#'         \item{\code{run}: Number of replicate.}
#'         \item{\code{year}: Year YYYY.}
#'         \item{\code{frz}: Code of the fire regime zone.}
#'         \item{\code{type}: Code of the fuel type:  \code{low}, \code{med} or \code{high}.}
#'         \item{\code{pct}: Relative abundance of the fuel type in the fire regime zone ([0,1]).}
#'       }
#'    }
#'    \item{\code{Cuts}: A data frame of harvestable area and volume per management unit 
#'    (included if \code{is.harvesting}), with columns:
#'      \itemize{
#'         \item{\code{run}: Number of replicate.}
#'         \item{\code{year}: Year YYYY.}
#'         \item{\code{mgmt.unit}: Code of the forest management unit (FMU).}
#'         \item{\code{a.inc}: Area that can be harvested, i.e. non-protected and FMU informed (in \eqn{km^{2}}).}
#'         \item{\code{a.even.age}: Area within \code{a.inc} of mature stand, i.e.\code{age>age.matu} (in \eqn{km^{2}}).}
#'         \item{\code{a.mat.pc}: Area to be managed under a partial-cut regime (in \eqn{km^{2}}).}
#'         \item{\code{a.inc.burnt}: Area within \code{a.inc} burnt in the current time step (in \eqn{km^{2}}).}
#'         \item{\code{a.inc.mat.burnt}: Area within \code{a.inc} of mature stands burnt in the current time step (in \eqn{km^{2}}).}
#'         \item{\code{a.inc.kill}: Area within \code{a.inc} killed by SBW in the previous or current time step (in \eqn{km^{2}}).}
#'         \item{\code{a.inc.mat.kill}: Area within \code{a.inc} of mature stands killed by SBW in the previous or current time step (in \eqn{km^{2}}).}
#'         \item{\code{a.reg.fail.ex}: Protected area at defforestation risk (in \eqn{km^{2}}).}
#'         \item{\code{a.reg.fail.in}: Non-protected area at defforestation risk (in \eqn{km^{2}}).}
#'         \item{\code{a.salvaged}: Salvaged and clear-cut area (in \eqn{km^{2}}).}
#'         \item{\code{a.unaff}: Clear-cut area unaffected by disturbances (in \eqn{km^{2}}).}
#'         \item{\code{v.salv}: Salvaged and clear-cut volume (in \eqn{m^{3}}).}
#'         \item{\code{v.unaff}: Clear-cut volume unaffected by disturbances (in \eqn{m^{3}}).}
#'         \item{\code{a.pcut}: Area partial cut (in \eqn{km^{2}}).}
#'         \item{\code{v.pcut}: Volume partial cut (in \eqn{m^{3}}).}
#'       }
#'    }
#'    \item{\code{SppCut}: A data frame of area and volum extracted by clear and partial cut per species 
#'    and management unit (included if \code{is.harvesting}), with columns:
#'      \itemize{
#'         \item{\code{run}: Number of replicate.}
#'         \item{\code{year}: Year YYYY.}
#'         \item{\code{mgmt.unit}: Code of the forest management unit (FMU).}
#'         \item{\code{spp}: Code of the species.}
#'         \item{\code{area.ccut}: Clear-cut area (in \eqn{km^{2}}).}
#'         \item{\code{vol.ccut}: Clear-cut volume (in \eqn{m^{3}}).}
#'         \item{\code{area.pcut}: Partial cut area (in \eqn{km^{2}}).}
#'         \item{\code{vol.pcut}: Partial cut volume (in \eqn{m^{3}}).}
#'       }
#'    }
#'    \item{\code{BurntRates}: A data frame of target area to be burnt per fire regime zone 
#'    (included if \code{is.wildfire}), with columns:
#'      \itemize{
#'         \item{\code{run}: Number of replicate.}
#'         \item{\code{year}: Year YYYY.}
#'         \item{\code{frz}: Code of the fire regime zone.}
#'         \item{\code{br}: Baseline area to be burnt derived from MFRI (in \eqn{km^{2}}).}
#'         \item{\code{brvar}: Baseline area \code{br} with inter-period variability added (in \eqn{km^{2}}).}
#'         \item{\code{brfuel}: Variable baseline area \code{brvar} modified according to zone-level fuel if
#'         \code{is.fuel.modifier} (in \eqn{km^{2}}).}
#'         \item{\code{brclima}: ariable baseline area \code{brvar} modified according to zone-level SEP rate if
#'         \code{is.clima.modifier}  (in \eqn{km^{2}}).}
#'         \item{\code{target.area}: Target area to be burnt (in \eqn{km^{2}}).}
#'       }
#'    }
#'    \item{\code{FireRegime}: A data frame of number of fires and burnt area per fire regime zone
#'    (included if \code{is.wildfire}), with columns:
#'      \itemize{
#'         \item{\code{run}: Number of replicate.}
#'         \item{\code{year}: Year YYYY.}
#'         \item{\code{frz}: Code of the fire regime zone.}
#'         \item{\code{target.area}: Target area to be burnt (in \eqn{km^{2}}).}
#'         \item{\code{nfires}: Number of fires burnt.}
#'         \item{\code{burnt.area}: Area effectively burnt (in \eqn{km^{2}}).}
#'         \item{\code{fire.cycle}: Relative fire return interval (in years).}
#'         \item{\code{indx.combust}: Fire-zone mean fuel flammability ([0,1]).}
#'         \item{\code{indx.combust.burnt}: Mean fuel flammability of actual burnt areas ([0,1]).}
#'       }
#'    }
#'    \item{\code{Fires}: A data frame of wildfires
#'    (included if \code{is.wildfire}), with columns:
#'      \itemize{
#'         \item{\code{run}: Number of replicate.}
#'         \item{\code{year}: Year YYYY.}
#'         \item{\code{frz}: Code of the fire regime zone.}
#'         \item{\code{fire.id}: Wildfire identificator.}
#'         \item{\code{wind}: Main wind direction in degrees.}
#'         \item{\code{target.size}: Target area to be burnt (in \eqn{km^{2}}).}
#'         \item{\code{burnt.size}: Area effectively burnt (in \eqn{km^{2}}).}
#'         \item{\code{rem}: Remanent area to be burnt (in \eqn{km^{2}}).}
#'       }
#'    }
#'    \item{\code{BurntFuels}: A data frame of burnt fuel types per fire regime zone
#'    (included if \code{is.wildfire}), with columns:
#'      \itemize{
#'         \item{\code{run}: Number of replicate.}
#'         \item{\code{year}: Year YYYY.}
#'         \item{\code{frz}: Code of the fire regime zone.}
#'         \item{\code{type}: Code of the fuel type:  \code{low}, \code{med} or \code{high}.}
#'         \item{\code{area}: Area burnt of each fuel category (in \eqn{km^{2}}).}
#'       }
#'    }
#'  }
#'  
#' @export
#' 
#' @examples
#'
#' \dontrun{
#' library(medLDM)
#' # Run one single 80-year replicate with forest management and default RCP 4.5 climate projections 
#' ldm(is.harvest = T)
#' }
#'


land.dyn.mdl = function(is.land.cover.change = FALSE, is.harvest = FALSE, is.wildfire = FALSE, 
                        is.prescribed.burn = FALSE, is.drought = TRUE, is.postfire = TRUE, 
                        is.cohort.establish = TRUE, is.afforestation = TRUE, is.encroachment = TRUE, 
                        is.growth = TRUE, spin.up = TRUE, custom.params = NA, clim.proj = NA, 
                        nrun = 1, time.step = 1, clim.step = 10, time.horizon = 90, save.land = FALSE, 
                        out.seq = NA, out.path = NA, ...){
  
  options(dplyr.summarise.inform=F)
  `%notin%` = Negate(`%in%`)
  
  cat("A. Data preparation ...\n")
  
  ## Build the baseline time sequence and the time sequence of the processes (shared for all runs). 
  ## 1. Climate change, 2. Land-cover changes, 3. Forest management
  ## 4. Wildfires, 5. Prescribed burns, 6. Drought, 7. Post-fire regeneration,
  ## 8. Cohort establihsment, 9. Afforestation, 10. Growth
  time.seq = 
    lchg.schedule = 
    mgmt.schedule = 
    fire.schedule = 
    pb.schedule = 
    drought.schedule =
    post.fire.schedule = 
    cohort.schedule = 
    afforest.schedule = 
    encroach.schedule = 
    growth.schedule = seq(1, time.horizon, time.step)
  if(spin.up & time.horizon>10){
    lchg.schedule = seq(11, time.horizon, time.step)
    fire.schedule = seq(11, time.horizon, time.step)
  }
  if(spin.up & time.horizon<=10){
    lchg.schedule = 
      fire.schedule = numeric()
  }
  clim.schedule = seq(1, time.horizon, clim.step*10) 
  
  ## Check the definition of the outputs writing sequence
  if(save.land){
    if(is.na(sum(out.seq))){
      out.seq = time.seq
    } 
    else{
      if(!all(out.seq %in% time.seq)){
        warning("Not all time steps in the output sequence provided are simulated.", call.=F)
      }
    }
    if(is.na(out.path)) stop("Directory path to save outputs not provided")
  }
  
  ## Get the list of default parameters and update user-initialized parameters
  params = default.params()
  if(!is.na(custom.params)){
    # Check class of custom.params
    if((!inherits(customParams, "list"))) {
      stop("'custom.params' must be a named list")
    }
    ## Check that the names of the customized parameters are correct
    if(!all(names(custom.params) %in% names(params)))
      stop("Wrong custom parameters names")
    params = custom.param
  }
  
  ## If provided by the user, load list of data frame with minimum and maximum temperatures 
  ## and precipitation predictions for the whole study area
  ## Check that all time steps are included and columns names is ok
  is.climate.change = FALSE
  if(!is.na(clim.proj)){
    # Check class of clim.proj
    if(!inherits(clim.proj, "data.frame") | !inherits(clim.proj, "list")) {
      stop("'prec.proj' must be a named data frame or a list of data frames")
    }
    # Check that column names of the unique data frame provided are correct
    if(inherits(clim.proj, "data.frame")){
      if(sum(colnames(clim.proj) %in% c("cell.id", "tmin","tmax", "precip"))<ncol(clim.proj))
        stop("Format of the climatic projections data frame is not correct. It has to have four
             columns named 'cell.id', 'tmin', 'tmax', and 'precip'")
    }
    if(inherits(clim.proj, "list")){
      if(time.horizon/clim.step!=lenght(clim.proj)){ 
        stop("The number of elements in the list of climatic projections does not match the 
             number of time steps that climate has to be updated")
      }
      else{
        for(i in 1:length(clim.proj)){
          if(sum(colnames(clim.proj[[i]]) %in% c("cell.id", "tmin","tmax", "precip"))<ncol(clim.proj[[i]]))
            stop("Format of the climatic projections data frame is not correct. It has to have four
             columns named 'cell.id', 'tmin', 'tmax', and 'precip'") 
        }
      }
    }
    is.climate.change = TRUE
  } 
  
  ## Initialize tracking data.frames
  track.harvested = data.frame(run=NA, year=NA, spp=NA, vol.sawlog=NA, vol.wood=NA)
  track.forest.area = data.frame(run=NA, year=NA, forest=NA, spp.harvestable=NA, non.protect=NA,
                                  national.park=NA, enpe=NA, no.park=NA, slope30.nopark=NA, 
                                  slope30.nopark.distpath1.5=NA, slope30.nopark.distpath2.2=NA)
  track.ftype.area = data.frame(run=NA, year=NA, todo=NA, ftype=NA, ha=NA)
  track.ftype.volume = data.frame(run=NA, year=NA, ftype=NA, vol.potential.extract.sawlog=NA, vol.potential.extract.wood=NA,
                             vol.extract.sawlog=NA, vol.extract.wood=NA, pct.sawlog=NA, pct.wood=NA) 
  track.fire = data.frame(run=NA, year=NA, swc=NA, clim.sever=NA, fire.id=NA, fst=NA, wind=NA, atarget=NA, 
                           aburnt.highintens=NA, aburnt.lowintens=NA, asupp.fuel=NA, asupp.sprd=NA, rem=NA)
  track.fire.spp = data.frame(run=NA, year=NA, fire.id=NA, spp=NA, aburnt=NA, bburnt=NA)
  track.pb = data.frame(run=NA, year=NA, clim.sever=NA, fire.id=NA, 
                         wind=NA, atarget=NA, aburnt.lowintens=NA)
  track.drought = data.frame(run=NA, year=NA, spp=NA, ha=NA)
  track.cohort = data.frame(run=NA, year=NA, spp.out=NA, spp.in=NA, ha=NA)
  track.post.fire = data.frame(run=NA, year=NA, spp.out=NA, spp.in=NA, ha=NA)
  track.afforest = data.frame(run=NA, year=NA, spp=NA, ha=NA)
  track.encroach = data.frame(run=NA, year=NA, spp=NA, ha=NA)
  track.land = data.frame(run=NA, year=NA, spp=NA, age.class=NA, area=NA, vol=NA, volbark=NA, carbon=NA)
  track.sqi = data.frame(run=NA, year=NA, spp=NA, sqi=NA, area=NA, vol=NA, volbark=NA)
  
  
  ## Start the simulations
  cat("\nB. Simulations ...\n")
  
  for(irun in 1:nrun){
    
    ## Main landscape data frame 
    land = landscape
    land$typcut = NA
    land$tscut  = NA
    land$tburnt = NA
    ## Initialize times burnt at 0 for burnable covers
    land$tburnt[land$spp<=17] = 0
    land$interface = interface(land, orography)
    
    ## Land at time 0, at the initial stage
    aux.forest = filter(land, spp<=13) %>% select(spp, age, biom) %>% left_join(ba.vol, by="spp") %>% 
                  mutate(vol=c_ba*biom/10+c2_ba*biom*biom/100) %>% select(-c_ba, -c2_ba) %>%
                  left_join(ba.volbark, by="spp") %>% 
                  mutate(volbark=c_ba*biom/10+c2_ba*biom*biom/100) %>% select(-c_ba, -c2_ba) %>% 
                  left_join(ba.carbon, by="spp") %>%  mutate(carbon=c_ba*biom/10) %>% 
                  mutate(age.class=ifelse(spp<=7 & age<=15, "young", ifelse(spp<=7 & age<=50, "mature",
                         ifelse(spp<=7 & age>50, "old", ifelse(spp>7 & spp<=13 & age<=15, "young",
                         ifelse(spp>7 & spp<=13 & age<=50, "mature", "old")))))) %>%       
                  group_by(spp, age.class) %>% select(-c_ba) %>%
                  summarise(area=length(vol), vol=sum(vol), volbark=sum(volbark), carbon=sum(carbon))  
    aux.shrub = filter(land, spp==14) %>% select(spp, biom) %>% group_by(spp) %>%
                 summarise(age.class=NA, area=length(biom), vol=sum(biom), volbark=0, carbon=0)  
    aux.other = filter(land, spp>14) %>% select(spp) %>% group_by(spp) %>%
                 summarise(age.class=NA, area=length(spp), vol=0, volbark=0, carbon=0)  
    track.land = rbind(track.land, data.frame(run=irun, year=0, aux.forest), data.frame(run=irun, year=0, aux.shrub),
                         data.frame(run=irun, year=0, aux.other))
  
    
    ## Simulation of one time step
    for(t in time.seq){
      
      ## Print replicate and time step
      cat(paste0("Replicate ", irun, "/", nrun, " - time: ", t, "/", time.horizon), "\n")
      
      ## Compute decade
      decade = (1+floor((t-1)/10))*10
  
      ## 1. CLIMATE CHANGE  
      if(!is.climate.change & t==1){
        land = land %>% left_join(sdm.sqi(land, orography, clim, decade=NA), by="cell.id")
      }
      else if(is.climate.change & t %in% clim.schedule){
        land = land %>% left_join(sdm.sqi(land, orography, clim, decade=decade), by="cell.id")
      }
      
      
      ## 2. LAND-COVER CHANGE
      if(spin.up & t<=10){
        cat("Observed land-cover changes", "\n")
        ## Select the cells 
        set1 = unlist(filter(land.cover.changes, code==1420) %>% select(cell.id))
        set2 = unlist(filter(land.cover.changes, code==1520) %>% select(cell.id))
        set3 = unlist(filter(land.cover.changes, code==1620) %>% select(cell.id))
        urban.cells = c(sample(set1, min(length(set1), 189), replace=F),
                        sample(set2, min(length(set2), 4), replace=F),
                        sample(set3, min(length(set3), 516), replace=F))
        set1 = unlist(filter(land.cover.changes, code==1419) %>% select(cell.id))
        set2 = unlist(filter(land.cover.changes, code==1519) %>% select(cell.id))
        set3 = unlist(filter(land.cover.changes, code==1619) %>% select(cell.id))
        water.cells = c(sample(set1, min(length(set1), 220), replace=F),
                        sample(set2, min(length(set2), 71), replace=F),
                        sample(set3, min(length(set3), 501), replace=F))
        set1 = unlist(filter(land.cover.changes, code==1415) %>% select(cell.id))
        set2 = unlist(filter(land.cover.changes, code==1615) %>% select(cell.id))
        grass.cells = c(sample(set1, min(length(set1), 84), replace=F),
                        sample(set2, min(length(set2), 119), replace=F))
        set1 = unlist(filter(land.cover.changes, code==1614) %>% select(cell.id))
        shrub.cells = sample(set1, min(length(set1), 6340), replace=F)
        ## Apply the changes in "land" and "clim
        land$spp[land$cell.id %in% urban.cells] = clim$spp[clim$cell.id %in% urban.cells] = 20 
        land$spp[land$cell.id %in% water.cells] = clim$spp[clim$cell.id %in% water.cells] = 19
        land$spp[land$cell.id %in% grass.cells] = clim$spp[clim$cell.id %in% grass.cells] = 15
        land$spp[land$cell.id %in% shrub.cells] = clim$spp[clim$cell.id %in% shrub.cells] = 14
        land$biom[land$cell.id %in% c(urban.cells, water.cells, grass.cells)] = NA
        land$biom[land$cell.id %in% shrub.cells] = 0
        land$age[land$cell.id %in% c(urban.cells, water.cells)] = NA
        land$age[land$cell.id %in% grass.cells] = land$age[land$cell.id %in% shrub.cells] = 0
        land$tsdist[land$cell.id %in% c(urban.cells, water.cells)] = NA
        land$tsdist[land$cell.id %in% c(grass.cells, shrub.cells)] = 0
        land$typdist[land$cell.id %in% grass.cells] = "lchg.agri"
        land$typdist[land$cell.id %in% shrub.cells] = "lchg.rabn"
        land$tburnt[land$cell.id %in% c(urban.cells, water.cells)] = NA
        land$tburnt[land$cell.id %in% c(grass.cells, shrub.cells)] = 0
        land$sdm[land$cell.id %in% c(urban.cells, water.cells, grass.cells)] = NA
        land$sqi[land$cell.id %in% c(urban.cells, water.cells, grass.cells)] = NA
        ## Update sdm and sqi for shrublands
        land$sdm[land$cell.id %in% shrub.cells] = 1
        if(length(shrub.cells)>0){
          sqi.shrub = filter(clim, cell.id %in% shrub.cells) %>% select(spp, tmin, precip) %>% 
                       mutate(aux.brolla=sq.shrub$c0_brolla+sq.shrub$c_temp_brolla*tmin+sq.shrub$c_temp2_brolla*tmin*tmin+sq.shrub$c_precip_brolla*precip+sq.shrub$c_precip2_brolla*precip*precip,
                       aux.maquia=sq.shrub$c0_maquia+sq.shrub$c_temp_maquia*tmin+sq.shrub$c_temp2_maquia*tmin*tmin+sq.shrub$c_precip_maquia*precip+sq.shrub$c_precip2_maquia*precip*precip,
                       aux.boix=sq.shrub$c0_boix+sq.shrub$c_temp_boix*tmin+sq.shrub$c_temp2_boix*tmin*tmin+sq.shrub$c_precip_boix*precip+sq.shrub$c_precip2_boix*precip*precip,
                       sq.brolla=1/(1+exp(-1*aux.brolla)), sq.maquia=1/(1+exp(-1*aux.maquia)), sq.boix=1/(1+exp(-1*aux.boix))) %>% 
            mutate(sqest.brolla=sq.brolla/max(sq.brolla), sqest.maquia=sq.maquia/max(sq.maquia), sqest.boix=sq.boix/max(sq.boix),
                   sqi=ifelse(sqest.brolla>=sqest.maquia & sqest.brolla>=sqest.boix, 1,
                              ifelse(sqest.maquia>=sqest.brolla & sqest.maquia>=sqest.boix, 2,
                                     ifelse(sqest.boix>=sqest.brolla & sqest.boix>=sqest.maquia, 3, 0))))
          clim$sqi[clim$cell.id %in% shrub.cells] = sqi.shrub$sqi
        }
        ## Change in the base dataframe, to not repeat
        land.cover.changes$code[land.cover.changes$cell.id %in% urban.cells] = 2020
        land.cover.changes$code[land.cover.changes$cell.id %in% water.cells] = 1919
        land.cover.changes$code[land.cover.changes$cell.id %in% grass.cells] = 1515
        land.cover.changes$code[land.cover.changes$cell.id %in% shrub.cells] = 1414
        
        if(any(is.infinite(sqi.shrub$sq.brolla)) | any(is.na(sqi.shrub$sq.brolla))){
          write.table(sqi.shrub, paste0(out.path, "/ErrorSQIshrub.txt"), quote=F, row.names=F, sep="\t")
          stop("Error SQI shrub")
        }
        
      }
      if(is.land.cover.change & t %in% lchg.schedule){
        # Urbanization
        chg.cells = land.cover.change(land, coord, 1, lchg.demand$lct.urban[t], numeric())
        land$spp[land$cell.id %in% chg.cells] = clim$spp[clim$cell.id %in% chg.cells] = 20 # urban
        land$typdist[land$cell.id %in% chg.cells] = "lchg.urb"
        land$biom[land$cell.id %in% chg.cells] = 
        land$age[land$cell.id %in% chg.cells] = 
        land$tsdist[land$cell.id %in% chg.cells] =   # don't care the time since it's urban
        land$tburnt[land$cell.id %in% chg.cells] =
        land$sdm[clim$cell.id %in% chg.cells] = 
        land$sqi[clim$cell.id %in% chg.cells] = NA
        # Agriculture conversion
        visit.cells = chg.cells
        chg.cells = land.cover.change(land, coord, 2, lchg.demand$lct.agri[t], visit.cells)
        land$spp[land$cell.id %in% chg.cells]= clim$spp[clim$cell.id %in% chg.cells] = 16 # arableland or 17 - permanent crops
        land$typdist[land$cell.id %in% chg.cells] = "lchg.agri"
        land$tsdist[land$cell.id %in% chg.cells] = 0
        land$tburnt[land$cell.id %in% chg.cells] = 0
        land$biom[land$cell.id %in% chg.cells] = 
        land$age[land$cell.id %in% chg.cells] = 
        land$sdm[clim$cell.id %in% chg.cells] = 
        land$sqi[clim$cell.id %in% chg.cells] = NA
        # Rural abandonment
        visit.cells = c(visit.cells, chg.cells)
        chg.cells = land.cover.change(land, coord, 3, lchg.demand$lct.rabn[t], visit.cells)
        land$spp[land$cell.id %in% chg.cells] = clim$spp[clim$cell.id %in% chg.cells] = 14  # shrub
        land$typdist[land$cell.id %in% chg.cells] = "lchg.rabn"
        land$biom[land$cell.id %in% chg.cells] = 
        land$age[land$cell.id %in% chg.cells] = 
        land$tsdist[land$cell.id %in% chg.cells] = 
        land$tburnt[land$cell.id %in% chg.cells] = 0
        land$sdm[clim$cell.id %in% chg.cells] = 1
        sqi.shrub = filter(clim, cell.id %in% chg.cells) %>% select(spp, tmin, precip) %>% 
                    mutate(aux.brolla=sq.shrub$c0_brolla+sq.shrub$c_temp_brolla*tmin+sq.shrub$c_temp2_brolla*tmin*tmin+sq.shrub$c_precip_brolla*precip+sq.shrub$c_precip2_brolla*precip*precip,
                    aux.maquia=sq.shrub$c0_maquia+sq.shrub$c_temp_maquia*tmin+sq.shrub$c_temp2_maquia*tmin*tmin+sq.shrub$c_precip_maquia*precip+sq.shrub$c_precip2_maquia*precip*precip,
                    aux.boix=sq.shrub$c0_boix+sq.shrub$c_temp_boix*tmin+sq.shrub$c_temp2_boix*tmin*tmin+sq.shrub$c_precip_boix*precip+sq.shrub$c_precip2_boix*precip*precip,
                    sq.brolla=1/(1+exp(-1*aux.brolla)), sq.maquia=1/(1+exp(-1*aux.maquia)), sq.boix=1/(1+exp(-1*aux.boix))) #%>% 
        if(is.infinite(max(sqi.shrub$sq.brolla)) | is.infinite(max(sqi.shrub$sq.maquia))  | is.infinite(max(sqi.shrub$sq.boix)) ){
          write.table(sqi.shrub, paste0(out.path, "/ErrorSQIshrub.txt"), quote=F, row.names=F, sep="\t")
          cat("INF in sqi.shrub - SIM.LC.CHANGE")  
        }
        sqi.shrub = mutate(sqi.shrub, sqest.brolla=sq.brolla/max(sq.brolla), sqest.maquia=sq.maquia/max(sq.maquia), sqest.boix=sq.boix/max(sq.boix),
                           sqi=ifelse(sqest.brolla>=sqest.maquia & sqest.brolla>=sqest.boix, 1,
                                       ifelse(sqest.maquia>=sqest.brolla & sqest.maquia>=sqest.boix, 2,
                                              ifelse(sqest.boix>=sqest.brolla & sqest.boix>=sqest.maquia, 3, 0))))
        land$sqi[land$cell.id %in% chg.cells] = sqi.shrub$sqi
        if(any(is.infinite(sqi.shrub$sq.brolla)) | any(is.na(sqi.shrub$sq.brolla))){
          write.table(sqi.shrub, paste0(out.path, "/ErrorSQIshrub.txt"), quote=F, row.names=F, sep="\t")
          stop("Error SQI shrub")
        }
        # Update interface values
        land$interface = interface(land, orography)
      }
      
      
      ## 3. FOREST MANAGEMENT 
      if(is.harvest & t %in% mgmt.schedule){
        cut.out = forest.mgmt(land, harvest, clim, harvest.demand$sawlog[t], harvest.demand$wood[t])
        extracted.sawlog = cut.out$extracted.sawlog
        if(nrow(extracted.sawlog)>0){
          extracted.sawlog = extracted.sawlog[order(extracted.sawlog$cell.id, decreasing=F),]
        }  
        extracted.wood = cut.out$extracted.wood
        if(nrow(extracted.wood)>0){
          extracted.wood = extracted.wood[order(extracted.wood$cell.id, decreasing=F),]
        }
        # report the cells that have been cut
        land$typdist[land$cell.id %in% c(extracted.sawlog$cell.id, extracted.wood$cell.id)] = "cut"
        land$tsdist[land$cell.id %in% c(extracted.sawlog$cell.id, extracted.wood$cell.id)] = 
          land$tscut[land$cell.id %in% c(extracted.sawlog$cell.id, extracted.wood$cell.id)] = 0
        # report the type of intervention (e.g. thinning, prep.cut, seed.cut, removal.cut)
        land$typcut[land$cell.id %in% extracted.sawlog$cell.id] = extracted.sawlog$todo
        land$typcut[land$cell.id %in% extracted.wood$cell.id] = extracted.wood$todo
        # change the age of the cells after removal.cut.
        # wood is extracted in quercus stands by clear.cut, so age is reset at 0
        land$age[land$cell.id %in% extracted.wood$cell.id[extracted.wood$pctg.extract == 100]] = 0 
        # most of the sawlogs are extracted in conifer stands under a shelterwood sytem.
        # thus, after the removal.cut, the stand is in regeneration and it already has 10 years.
        land$age[land$cell.id %in% extracted.sawlog$cell.id[extracted.sawlog$pctg.extract < 100]] = 9 # sum 1 at the end of the year
        land$age[land$cell.id %in% extracted.sawlog$cell.id[extracted.sawlog$pctg.extract == 100]] = 0 # quercus, conif plantation and other.tress are clear cut
        # change the basal area in harvested stands
        land$biom[land$cell.id %in% extracted.sawlog$cell.id] = 
          land$biom[land$cell.id %in% extracted.sawlog$cell.id]-extracted.sawlog$ba.extract*10
        land$biom[land$cell.id %in% extracted.wood$cell.id] = 
          land$biom[land$cell.id %in% extracted.wood$cell.id]-extracted.wood$ba.extract*10
        # after removal.cut make explicity that basal area is 0
        land$biom[land$cell.id %in% extracted.sawlog$cell.id[extracted.sawlog$todo=="removal.cut"]] = 0
        land$biom[land$cell.id %in% extracted.wood$cell.id[extracted.wood$todo=="removal.cut"]] = 0
        # but there's regeneration of 9 year old in areas harvested under shelterwood
        if(sum(extracted.sawlog$todo=="removal.cut" & extracted.sawlog$pctg.extract < 100)>0){
          for(i in 1:9)
            land$biom[land$cell.id %in% extracted.sawlog$cell.id[extracted.sawlog$todo=="removal.cut" & extracted.sawlog$pctg.extract < 100]] = 
              growth(land[land$cell.id %in% extracted.sawlog$cell.id[extracted.sawlog$todo=="removal.cut" & extracted.sawlog$pctg.extract < 100],], clim, paste("Cohort age", i))
        }
        
        # track the vol extracted per each spp
        if(nrow(extracted.sawlog)>0){
          aux = group_by(extracted.sawlog, spp) %>% summarise(vol.sawlog=sum(vol.extract.sawlog), vol.wood=sum(vol.extract.wood))
          if(nrow(extracted.wood)>0){
            aux = rbind(aux, group_by(extracted.wood,spp) %>% summarise(vol.sawlog=sum(vol.extract.sawlog), vol.wood=sum(vol.extract.wood)))
          }
        }
        else if(nrow(extracted.wood)>0){
          aux = group_by(extracted.wood,spp) %>% summarise(vol.sawlog=sum(vol.extract.sawlog), vol.wood=sum(vol.extract.wood))
        }
        track.harvested = rbind(track.harvested, data.frame(run=irun, year=t, 
                              group_by(aux, spp) %>% summarise(vol.sawlog=round(sum(vol.sawlog),1), vol.wood=round(sum(vol.wood),1))))
        
        ## track the harvestable areas, the sustinably harvestable areas
        track.forest.area = rbind(track.forest.area, data.frame(run=irun, year=t, forest.areas(land, harvest)))
        
        ## track the area and volume extracted per product and forest type
        cut.out$suit.mgmt$ftype = ifelse(cut.out$suit.mgmt$spp<=7, "conif", "decid")
        aux = filter(cut.out$suit.mgmt, !is.na(todo)) %>% group_by(todo, ftype) %>% summarise(ha=length(spp)) 
        track.ftype.area = rbind(track.ftype.area, data.frame(run=irun, year=t, aux))
        cut.out$sustain$ftype = ifelse(cut.out$sustain$spp<=7, "conif", "decid")
        aux2 = group_by(cut.out$sustain, ftype) %>% 
               summarise(vol.potential.extract.sawlog=sum(vol.extract.sawlog), 
                         vol.potential.extract.wood=sum(vol.extract.wood)) 
        extracted.sawlog$ftype = ifelse(extracted.sawlog$spp<=7, "conif", "decid")
        extracted.wood$ftype = ifelse(extracted.wood$spp<=7, "conif", "decid")
        aux3 = group_by(rbind(extracted.sawlog, extracted.wood), ftype) %>% 
               summarise(vol.extract.sawlog=sum(vol.extract.sawlog), vol.extract.wood=sum(vol.extract.wood)) 
        aux3$pct.sawlog = round(100*aux3$vol.extract.sawlog/colSums(aux3[,-1])[1])
        aux3$pct.wood = round(100*aux3$vol.extract.wood/colSums(aux3[,-1])[2])
        aux2 = aux2 %>% left_join(aux3, by="ftype")
        track.ftype.volume = rbind(track.ftype.volume, data.frame(run=irun, year=t, aux2))
      }
      
      
      ## 4. FIRE
      burnt.cells = numeric()
      if(spin.up & t<=10){
        cat("Observed wildfires", "\n")
        burnt = !is.na(wildfires[,t+1])
        if(sum(burnt)>0){
          burnt.cells = data.frame(cell.id=wildfires$cell.id[burnt], fintensity=1)
          land$tsdist[land$cell.id %in% burnt.cells$cell.id] = 0
          land$tburnt[land$cell.id %in% burnt.cells$cell.id] = land$tburnt[land$cell.id %in% burnt.cells$cell.id] + 1
          land$typdist[land$cell.id %in% burnt.cells$cell.id] = "highfire"
          land$biom[land$cell.id %in% burnt.cells$cell.id] = 0  
        }
      }
      else if(is.wildfire & t %in% fire.schedule){
        # Decide climatic severity of the year (default is mild)
        clim.sever = 0
        if(runif(1,0,100) < climatic.severity[climatic.severity$year==t, ncol(climatic.severity)]) # not-mild
          clim.sever = 1
        # Burnt
        fire.out = fire.regime(land, coord, orography, clim, pfst.pwind, 1:3, clim.sever, t, 0, out.path, irun, params)
        # Track fires and Burnt spp & Biomass
        if(nrow(fire.out[[1]])>0)
          track.fire = rbind(track.fire, data.frame(run=irun, fire.out[[1]]))
        burnt.cells = fire.out[[2]] %>% select(-igni)
        if(nrow(burnt.cells)>0){
          aux = left_join(burnt.cells, select(land, cell.id, spp, biom), by="cell.id") %>%
            mutate(bburnt=ifelse(fintensity>params$fire.intens.th, biom, biom*(1-fintensity))) %>%
            group_by(fire.id, spp) %>% summarize(aburnt=length(spp), bburnt=round(sum(bburnt, na.rm=T),1))
          track.fire.spp =  rbind(track.fire.spp, data.frame(run=irun, year=t, aux)) 
          # track.step = rbind(track.step, data.frame(run=irun, fire.out[[3]]))
          # track.sr = rbind(track.sr, data.frame(run=irun, fire.out[[3]]))
        }
        # if(nrow(fire.out[[4]])>0)
        #   track.sr.source = rbind(track.sr.source, data.frame(run=irun, fire.out[[4]]))
        # Done with fires! When high-intensity fire, age = biom = 0 and dominant tree species may change
        # when low-intensity fire, age remains, spp remains and biomass.t = biomass.t-1 * (1-fintensity)
        burnt.cells$intens = burnt.cells$fintensity>params$fire.intens.th
        land$tsdist[land$cell.id %in% burnt.cells$cell.id] = 0
        land$tburnt[land$cell.id %in% burnt.cells$cell.id] = land$tburnt[land$cell.id %in% burnt.cells$cell.id] + 1
        land$typdist[land$cell.id %in% burnt.cells$cell.id[burnt.cells$intens]] = "highfire"
        land$typdist[land$cell.id %in% burnt.cells$cell.id[!burnt.cells$intens]] = "lowfire"
        land$biom[land$cell.id %in% burnt.cells$cell.id[burnt.cells$intens]] = 0
        land$biom[land$cell.id %in% burnt.cells$cell.id[!burnt.cells$intens]] = 
          land$biom[land$cell.id %in% burnt.cells$cell.id[!burnt.cells$intens]]*(1-burnt.cells$fintensity[!burnt.cells$intens])
      }
      
      
      ## 5. PRESCRIBED BURNS
      id.fire = 0
      if(is.prescribed.burn & t %in% pb.schedule){
        if(!exists("clim.sever"))
          clim.sever = 0
        # Annual area burnt for PB
        annual.burnt.area = ifelse(exists("burnt.cells"), nrow(burnt.cells), 0)
        fire.out = fire.regime(land, coord, orography, clim, pfst.pwind, 4, clim.sever, t, 0, out.path, irun, params)
        # Track pb and Done with prescribed burns!
        if(nrow(fire.out[[1]])>0){
          track.pb = rbind(track.pb, data.frame(run=irun, fire.out[[1]][,c(1,3,4,6,7,9)]))
          pb.cells = fire.out[[2]] %>% select(-igni)  
          land$tsdist[land$cell.id %in% pb.cells$cell.id] = 0
          land$tburnt[land$cell.id %in% pb.cells$cell.id] = land$tburnt[land$cell.id %in% pb.cells$cell.id] + 1
          land$typdist[land$cell.id %in% pb.cells$cell.id] = "pb"
          land$biom[land$cell.id %in% pb.cells$cell.id] = land$biom[land$cell.id %in% pb.cells$cell.id]*(1-pb.cells$fintensity)
        }
      }
      
      
      ## 6. DROUGHT
      killed.cells = integer()
      if(is.drought & t %in% drought.schedule){
        killed.cells = drought(land, decade, t)
        land$tsdist[land$cell.id %in% killed.cells] = 0
        land$typdist[land$cell.id %in% killed.cells] = "drght"
        if(length(killed.cells)>0)
          track.drought = rbind(track.drought, data.frame(run=irun, year=t, 
                  filter(land, cell.id %in% killed.cells) %>% group_by(spp) %>% summarise(ha=length(spp))))
      }
      
      
      ## 7. POST-FIRE REGENERATION
      if(is.postfire & t %in% post.fire.schedule & length(burnt.cells)>0){
        ## forest transition of tree species burnt in high intensity
        aux  = post.fire(land, coord, orography, clim, sdm, params)
        if(nrow(aux)>0){
          spp.out = land$spp[land$cell.id %in% aux$cell.id]
          land$spp[land$cell.id %in% aux$cell.id] = aux$spp
          land$sdm[land$cell.id %in% aux$cell.id] = 1
          land$sqi[land$cell.id %in% aux$cell.id] = aux$sqi
          track =  data.frame(table(spp.out, aux$spp))
          names(track) = c("spp.out", "spp.in", "ha")
          track.post.fire = rbind(track.post.fire, data.frame(run=irun, year=t, track))  
        }
        # Reset age of cells burnt in high intensity
        land$age[land$cell.id %in% burnt.cells$cell.id[burnt.cells$intens] & !is.na(land$spp) & land$spp<=14] = 0
        # Reset age of burnt grass
        land$age[land$cell.id %in% burnt.cells$cell.id & !is.na(land$spp) & land$spp==15] = 0
        ## Transition of burnt shublands in high-mountain (>1500m) to grasslands
        burnt.shrub = land %>% filter(spp==14, tsdist==0, typdist %in% c("lowfire", "highfire")) %>% 
                      left_join(select(orography, cell.id, elev), by="cell.id") %>% filter(elev>1500)
        land$spp[land$cell.id %in% burnt.shrub$cell.id] = 15
        land$age[land$cell.id %in% burnt.shrub$cell.id] = 0
        land$sdm[land$cell.id %in% burnt.shrub$cell.id] = NA
        land$sqi[land$cell.id %in% burnt.shrub$cell.id] = NA
      }
      
      
      ## 8. COHORT ESTABLISHMENT
      if(is.cohort.establish & t %in% cohort.schedule & length(killed.cells)>0){
        aux = cohort.establish(land, coord, orography, sdm, params)
        spp.out = land$spp[land$cell.id %in% killed.cells]
        land$spp[land$cell.id %in% killed.cells] = aux$spp
        land$age[land$cell.id %in% killed.cells] = t-(decade-10)-1 ## 0 to 9, so after growth(), age is 1 to 10
        land$biom[land$cell.id %in% killed.cells] = 0  ## biomass at 0,
        if(t-(decade-10)-1 > 0){ ## increase biomass up to cohort.age1, and it will increase in growth() one year more
          for(i in 1:(t-(decade-10)-1))
            land$biom[land$cell.id %in% killed.cells] = 
              growth(land[land$cell.id %in% killed.cells,], clim, paste("Cohort", i))
        }
        land$sdm[clim$cell.id %in% killed.cells] = 1
        land$sqi[clim$cell.id %in% killed.cells] = aux$sqi
        track = data.frame(table(spp.out, aux$spp))
        names(track) = c("spp.out", "spp.in", "ha")
        track.cohort = rbind(track.cohort, data.frame(run=irun, year=t, track))
      }
      
      
      ## 9. AFFORESTATION
      if(is.afforestation & t %in% afforest.schedule){
        aux  = afforestation(land, coord, orography, clim, sdm, params)
        if(length(aux)>0){
          land$spp[land$cell.id %in% aux$cell.id] = aux$spp
          land$biom[land$cell.id %in% aux$cell.id] = 0
          land$age[land$cell.id %in% aux$cell.id] = 0
          land$tsdist[land$cell.id %in% aux$cell.id] = 0
          land$typdist[land$cell.id %in% aux$cell.id] = "afforest"
          land$sdm[clim$cell.id %in% aux$cell.id] = 1
          land$sqi[clim$cell.id %in% aux$cell.id] = aux$sqi
          track = data.frame(table(aux$spp))
          names(track) = c("spp", "ha")
          track.afforest = rbind(track.afforest, data.frame(run=irun, year=t, track))  
        }
      }
      
      
      ## 10. ENCROACHMENT
      if(is.encroachment & t %in% encroach.schedule){
        aux  = encroachment(land, coord, orography)
        land$spp[land$cell.id %in% aux$cell.id] = 14
        land$biom[land$cell.id %in% aux$cell.id] = 0
        land$age[land$cell.id %in% aux$cell.id] = 0
        land$tsdist[land$cell.id %in% aux$cell.id] = 0
        land$typdist[land$cell.id %in% aux$cell.id] = "encroach"
        land$sdm[clim$cell.id %in% aux$cell.id] = 1
        land$sqi[clim$cell.id %in% aux$cell.id] = 1
        track.encroach = rbind(track.encroach, data.frame(run=irun, year=t, spp=14, ha=nrow(aux)))
      }
      
      
      ## 11. GROWTH
      if(is.growth & t %in% growth.schedule){
        land$biom = growth(land, clim, "Species")
        land$age = pmin(land$age+1,600)
        land$tsdist = pmin(land$tsdist+1,600)
        land$tscut = pmin(land$tscut+1,600)
        aux.forest = filter(land, spp<=13) %>% select(spp, age, biom) %>% left_join(ba.vol, by="spp") %>% 
                     mutate(vol=c_ba*biom/10+c2_ba*biom*biom/100) %>% select(-c_ba, -c2_ba) %>%
                     left_join(ba.volbark, by="spp") %>% 
                     mutate(volbark=c_ba*biom/10+c2_ba*biom*biom/100) %>% select(-c_ba, -c2_ba) %>% 
                     left_join(ba.carbon, by="spp") %>% 
                     mutate(carbon=c_ba*biom/10) %>% 
                     mutate(age.class=ifelse(spp<=7 & age<=15, "young", ifelse(spp<=7 & age<=50, "mature",
                                         ifelse(spp<=7 & age>50, "old", ifelse(spp>7 & spp<=13 & age<=15, "young",
                                              ifelse(spp>7 & spp<=13 & age<=50, "mature", "old")))))) %>%       
                     group_by(spp, age.class) %>% select(-c_ba) %>%
                     summarise(area=length(vol), vol=sum(vol), volbark=sum(volbark), carbon=sum(carbon))  
        aux.shrub = filter(land, spp==14) %>% select(spp, biom) %>% group_by(spp) %>%
                    summarise(age.class=NA, area=length(biom), vol=sum(biom), volbark=0, carbon=0)  
        aux.other = filter(land, spp>14) %>% select(spp) %>% group_by(spp) %>%
                    summarise(age.class=NA, area=length(spp), vol=0, volbark=0, carbon=0)  
        track.land = rbind(track.land, data.frame(run=irun, year=t, aux.forest), data.frame(run=irun, year=t, aux.shrub),
                            data.frame(run=irun, year=t, aux.other))
        aux.forest = filter(land, spp<=13) %>% select(cell.id, spp, age, biom, sqi) %>% 
                     left_join(ba.vol, by="spp") %>% 
                     mutate(vol=c_ba*biom/10+c2_ba*biom*biom/100) %>% select(-c_ba, -c2_ba) %>%
                     left_join(ba.volbark, by="spp") %>% 
                     mutate(volbark=c_ba*biom/10+c2_ba*biom*biom/100) %>% select(-c_ba, -c2_ba) %>% 
                     group_by(spp, sqi) %>% summarise(area=length(vol), vol=sum(vol), volbark=sum(volbark))
        track.sqi = rbind(track.sqi, data.frame(run=irun, year=t, aux.forest)) 
      }
      
      
      ## If required, save landscape data frame at the time steps required
      if(save.land & t %in% out.seq){
        if(!file.exists(out.path))
          dir.create(file.path(out.path), showWarnings = FALSE) 
        saveRDS(land, file=paste0(out.path, "landscape_", irun, "t", t, ".rds"))
      }
      
    } # t
  } # irun

  
  cat("\n", "C. Build outputs...\n")
  res = list(Land = track.land[-1,],
             LandSQI = track.sqi[-1,]
  )
  if(is.harvest){
    res = c(res, list(Harvest = track.harvested[-1,],
                      ForestArea = track.forest.area[-1,],
                      HarestArea = track.ftype.area[-1,],
                      HarvestVolume = track.ftype.volume[-1,]))
  }
  if(is.wildfire){
    res = c(res, list(Fires = track.fire[-1,],
                      BurntSpp = track.fire.spp[-1,]))
  }
  if(is.postfire){
    res = c(res, list(PostFire = track.post.fire[-1,]))
  }
  if(is.prescribed.burn){
    res = c(res, list(PrescribedBurns = track.pb[-1,]))
  }
  if(is.drought){
    res = c(res, list(Drought = track.drought[-1,]))
  }
  if(is.cohort.establish){
    track.cohort = filter(track.cohort, ha>0)    
    res = c(res, list(Cohort = track.cohort[-1,]))
  }
  if(is.afforestation){
    res = c(res, list(Afforest = track.afforest[-1,]))
  }
  if(is.encroachment){
    res = c(res, list(Encroach = track.encroach[-1,]))
  }
  
  return(res)  
}
