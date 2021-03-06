% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/cohort.establish.r
\name{cohort.establish}
\alias{cohort.establish}
\title{Cohort establishment}
\usage{
cohort.establish(land, clim, params)
}
\arguments{
\item{land}{A \code{landscape} data frame with forest stand records and land-cover types in rows}

\item{clim}{A data frame with minimum temperature (in ºC), maximum temperature (in ºC), 
and precipitation (in mm) per location}

\item{params}{A list of model parameters, default ones are generated by the function \code{default.params()}}
}
\value{
A data frame with the new species and sqi for after drought-induced mortality
}
\description{
Determines the tree species after forest dieback
}
\examples{
data(landscape)
data(clim)
land = dplyr::left_join(landscape, sdm.sqi(landscape, clim), by="cell.id")
killed.cells = drought(land, 10, 1)
land$tsdist[land$cell.id \%in\% killed.cells] = 0
land$typdist[land$cell.id \%in\% killed.cells] = "drght"
params = default.params()
x = cohort.establish(land, clim, params)

}
