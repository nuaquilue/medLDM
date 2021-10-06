#' medLMD: A package for simulating landscape dynamics in Mediterranean regions
#' 
#' The medLDM package provides a set of functions to simulate landscape-scale processes 
#' that influence the composition and the arrangment of the landscape such as wildfires and 
#' land-cover changes, as well as the structure and composition of forest stands such as 
#' fire and forest management and vegetation dynamics.
#' 
#' @section Author(s):
#' \bold{Maintainer}: Núria Aquilué \email{nuria.aquilue@ctfc.cat}  
#' 
#' \bold{Authors}: Núria Aquilué, Lluís Brotons, Andrea Duane, Quim Canelles
#'
#' @docType package
#' @name medLDM
#' 
#' @importFrom tidyr %>% pivot_wider
#' @importFrom dplyr group_by summarise filter select mutate count left_join 
#' @importFrom RANN nn2
#' @useDynLib medLDM
#' @importFrom Rcpp sourceCpp evalCpp
#' @importFrom stats aggregate dist quantile rexp rlnorm runif
#' @importFrom utils write.table
#' @importFrom scales rescale
NULL
#> NULL

