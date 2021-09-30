# medLDM - Mediterranean Landscape Dynamic Model

## Introduction

The **medLDM** package provides a set of functions to simulate landscape-scale processes that influence the composition and the arrangment of the landscape such as wildfires and land-cover changes, as well as the structure and composition of forest stands such as fire and forest management and vegetation dynamics.

The Mediterranean Landscape Dynamic Model is initialized for the region of Catalonia (NE Spain) in 2010, but includes a spin-up option to replicate most of the landscape-scale processes ocurred in the decade 2010-2020. It is a spatially explicit model currently calibrated to work at 1 ha of spatial resolution and 1-year time step.


## Package installation

Users can download and install the latest stable version of the **medLDM** package from GitHub as follows (required package devtools should be installed/updated first):

```R
devtools::install_github("nuaquilue/medLDM")
```
Additionally, users can have help to run package functions directly as package vignettes, by forcing their inclusion in installation:

```R
devtools::install_github("nuaquilue/medLDM", 
                         build_manual = TRUE,
                         build_vignettes = TRUE)
```

## References

Aquilué, N., Fortin, M.-J., Messier, C., Brotons, L. 2020. The potential of agricultural conversion to shape forest fire regimes in Mediterranean landscapes. Ecosystems. 23-1, 34-51. https://doi.org/10.1007/s10021-019-00385-7

Canelles, Q., Aquilué, N., Duane, A., Brotons, L. 2019. From stand to landscapes: modelling post-fire regeneration and species growth. Ecological Modelling. 404, 103-111. https://doi.org/10.1016/j.ecolmodel.2019.05.001

Duane, A., Aquilué, N., Canelles, Q., Morán-Ordoñez, A., De Cáceres, M., Brotons, L. 2019. Adapting prescribed burns to future climate change in Mediterranean landscapes. Science of the Total Environment. 677, 68-83. https://doi.org/10.1016/j.scitotenv.2019.04.348

Aquilué, N., De Cáceres, M., Fortin, M.-J., Fall, A., Brotons, L. 2017. A spatial allocation procedure to model land-use/land-cover changes: Accounting for occurrence and spread processes. Ecological Modelling. 344, 73-86. https://doi.org/10.1016/j.ecolmodel.2016.11.005

Brotons, L., Aquilué, N., De Cáceres, M., Fortin, M.-J., Fall, A. 2013. How fire history, fire suppression practices and climate change affect wildfire regimes in Mediterranean landscapes. PLOSone 8 (5), e62392. https://doi.org/10.1371/journal.pone.0062392
