#' Reference climate
#'
#' Climatic variables of the reference period 1971 - 2000
#'
#' @format A data frame with as many rows as grid cells in the study area and 4 variables:
#' \describe{
#'   \item{cell.id}{Unique grid cell indentificator}
#'   \item{tmin}{Historical (1976-2000) standarized mean minimum temperature, in ºC}
#'   \item{tmax}{Historical (1976-2000) standarized mean maximum temperature, in ºC}
#'   \item{precip}{Historical (1976-2000) standarized accumulated precipitation, in mm}
#'   \item{pet}{Historical (1976-2000) potential evapotranspiration, in mm}
#' }
"clim"

#' UTM coordinates 
#'
#' Coordinates x and y in UTM31N-ETRS89 of the 1 ha cells in the study area.
#'
#' @format A data frame with as many rows as grid cells in the study area and 3 variables:
#' \describe{
#'   \item{cell.id}{Unique grid cell indentificator}
#'   \item{x}{UTM coordinate X}
#'   \item{y}{UTM coordinate Y}
#' }
"coord"

#' Timber harvesting allocation
#'
#' Landscape variables influencing the allocation of sylvicultural prescriptions
#'
#' @format A data frame with as many rows as grid cells in the study area and 6 variables:
#' \describe{
#'   \item{cell.id}{Unique grid cell indentificator}
#'   \item{enpe}{Code of the protection status: 0 - none, 1 - national park, 2 - natural park, 
#'   3 - Natural place of national interest, 4 - Wildlife nature reserve, 5 - Integral natural reserve,
#'   6 - Partial natural reserve, 7 - Peripheral protection zone of a national park, 
#'   8 - Peripheral protection zone of a natural park, 9 - Peripheral protection zone of a natural place}
#'   \item{dist.path}{Distance to the nearest road or forest track, in m}
#'   \item{slope.pctg}{Stand slope, in /100}
#'   \item{dist.industry}{Distance to the nearest wood transformation industry, in km}
#'   \item{dist.biomass}{Distance to the nearest biomass plant, in km}
#' }
#' 
"harvest"

#' 1993 - 2017 Land-cover changes 
#'
#' Codification of the land-cover changes occurred between 1993 and 2017 based on the 
#' 20-categories legend of the Land Cover Map of Catalonia
#'
#' @format A data frame with as many rows as grid cells in the study area and 2 variables:
#' \describe{
#'   \item{cell.id}{Unique grid cell indentificator}
#'   \item{code}{Code made by merging the 2-digits code of the land-cover type in 1993 
#'   to the 2-digits code of the land-cover code in 2017. The 20-categories legend is as \code{landscape$spp}}
#' }
"obs.lcc"

#' Forest landscape features of a region in Catalonia
#'
#' Landscape and forest stands characteristics of the study area in 2010 at 1 ha of spatial resolution 
#'
#' @format A data frame with as many rows as grid cells in the study area and 6 variables:
#' \describe{
#'   \item{cell.id}{Unique grid cell indentificator}
#'   \item{spp}{Tree species or land-cover types: 
#'   1 - Pinus halepensis,
#'   2 - Pinus nigra,
#'   3 - Pinus pinea,
#'   4 - Pinus sylvestris,
#'   5 - Pinus pinaster,
#'   6 - Pinus  uncinata,
#'   7 - Abies alba,
#'   8 - Quercus ilex,
#'   9 - Quercus suber,
#'   10	- Quercus faginea,
#'   11	- Quercus humilis,
#'   12	- Fagus sylvatica,
#'   13	- Other trees,
#'   14	- Shrubland,
#'   15	- Alpine grass,
#'   16	- Arable land,
#'   17	- Permanent crops,
#'   18	- Rock, Sand, Bare soil,
#'   19	- Water,
#'   20	- Urban, Roads}
#'   \item{biom}{Basal area for forest stands (in m2/ha) and biomass for shrublands (in tonne/ha)}
#'   \item{age}{Forest and shrub age, in year}
#'   \item{typdist}{Type of the last disturbance, possible values are \code{highfire} for high-intensity wildfire, 
#'   \code{lowfire} for low-intenstiy wildfire, \code{lchg.urb} for urbanization,
#'   \code{lchg.agri} for agriculture conversion, \code{lchg.rabn} for rural abandonment,
#'   \code{cut} for timber harvesting, \code{pb} for prescribed burn, \code{drght} for drought-induced mortality,
#'   \code{afforest} for afforestation, and \code{encroach} for encroachment}
#'   \item{tsdist}{Time since the last disturbance, in years}
#' }
"landscape"

#' Mask of the study area
#'
#' Binary raster to identify the study area (1 or NA) 
#'
#' @format Raster of 2800 (nrow) x 2009 (ncol)
#' \describe{
#' Raster of the study area (1 or NA) in the UTM31N - ETRS89 projection, at 1 ha of spatial resoltuion. 
#' The unique grid cell identificator \code{cell.id} in the \code{landscape} data frame coincides with 
#' the position of the location in the \code{mask} raster.
#' }
"mask.study.area"

#' Orography 
#'
#' Orographic characteristics of the study area
#'
#' @format A data frame with as many rows as grid cells in the study area and 8 variables:
#' \describe{
#'   \item{cell.id}{Unique grid cell indentificator}
#'   \item{elev}{Elevation, in m}
#'   \item{aspect}{Aspect, 0 - flat, 1 - north, 2 - east, 3 - south, 4 - west}
#'   \item{slope}{Slope, in º}
#'   \item{radsol}{Solar radiation in summer, in kJ/(m2·day·mm)}
#'   \item{road}{Distance to the road network, in m}
#'   \item{utm}{Code of the 1K UTM grid}
#'   \item{slope.stand}{Standarized slope?}
#' }
"orography"

#' Fire spread type and Main wind direction
#'
#' FST & Probability wind directions
#'
#' @format A data frame with as many rows as grid cells in the study area and 6 variables:
#' \describe{
#'   \item{cell.id}{Unique grid cell indentificator}
#'   \item{pfst.wind}{Fire spread type is wind-driven}
#'   \item{pfst.topo}{Fire spread type is topographic}
#'   \item{pwind.n}{Probability (0-100) of north winds dominance}
#'   \item{pwind.nw}{Probability (0-100) of north-west winds dominance}
#'   \item{pwind.w}{Probability (0-100) of west winds dominance}
#' }
"pfst.pwind"

#' 2010 - 2019 Wildfires 
#'
#' Observed wildfires in a region of Catalonia from 2010 to 2019
#'
#' @format A data frame with as many rows as grid cells in the study area and 11 variables:
#' \describe{
#'   \item{cell.id}{Unique grid cell indentificator}
#'   \item{y10}{Locations burnt in 2010}
#'   \item{y11}{Locations burnt in 2011}
#'   \item{y12}{Locations burnt in 2012}
#'   \item{y13}{Locations burnt in 2013}
#'   \item{y14}{Locations burnt in 2014}
#'   \item{y15}{Locations burnt in 2015}
#'   \item{y16}{Locations burnt in 2016}
#'   \item{y17}{Locations burnt in 2017}
#'   \item{y18}{Locations burnt in 2018}
#'   \item{y19}{Locations burnt in 2019}
#' }
"wildfires"
