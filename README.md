# **Dutch Caribbean Nature Alliance (DCNA)**


## 1. Introduction

The goals of this repository are to standardize benthic cover and fish data gathered by Dutch Caribbean monitoring programs (see below) and to provide R scripts to analyze these standardized data. This repository aims to facilitate the reporting process for the Dutch Caribbean.

* Aruba National Parks Foundation (FPNA)
* Stichting Nationale Parken Bonaire (STINAPA)
* Caribbean Research and Management of Biodiversity (CARMABI)
* Saba Conservation Foundation (SCF)
* Nature Foundation St. Maarten (NFSXM)
* St. Eustatius National Parks (STENAPA)


## 2. Code

This repository contains three folders: `data`, `code`, and `figs`. The `data` folder contains the data, the `code` folder contains the code necessary to standardize and analyse the data, and the `figs` folder contains the figures produced through the analysis codes. 


**Table 1.** Description of variables included in standardized benthic cover dataset. The icons for the variables categories (`Cat.`) represents :memo: = description variables, :globe_with_meridians: = spatial variables, :calendar: = temporal variables, :straight_ruler: = methodological variables, :crab: = taxonomic variables, :chart_with_upwards_trend: = metric variables. Variables names (except *category*, *subcategory*, and *n*) correspond to [DarwinCore terms](https://dwc.tdwg.org/terms).

|  #  | Variable              | Cat.                       | Type      | Description                                 |
|----:|:----------------------|:--------------------------:|:----------|:--------------------------------------------|
| 1   | datasetID             | :memo:                     | Factor    | ID of the dataset                           |  
| 2   | locality              | :globe_with_meridians:     | Character | Site name                                   |  
| 3   | parentEventID         | :globe_with_meridians:     | Integer   | Transect ID                                 |  
| 4   | eventID               | :globe_with_meridians:     | Integer   | Quadrat ID                                  |  
| 5   | decimalLatitude       | :globe_with_meridians:     | Numeric   | Latitude (*decimal, EPSG:4326*)             |  
| 6   | decimalLongitude      | :globe_with_meridians:     | Numeric   | Longitude (*decimal, EPSG:4326*)            |  
| 7   | verbatimDepth         | :globe_with_meridians:     | Numeric   | Depth (*m*)                                 |  
| 8   | year                  | :calendar:                 | Integer   | Four-digit year                             |  
| 9   | category              | :crab:                     | Factor    | Benthic category                            |  
| 10  | subcategory           | :crab:                     | Factor    | Benthic subcategory                         |   
| 11  | n                     | :chart_with_upwards_trend: | Integer   | Number                                      |


## 3. Reproducibility parameters

```
R version 4.2.3 (2023-03-15 ucrt)
Platform: x86_64-w64-mingw32/x64 (64-bit)
Running under: Windows 10 x64 (build 18363)

Matrix products: default

locale:
[1] LC_COLLATE=French_France.utf8  LC_CTYPE=French_France.utf8   
[3] LC_MONETARY=French_France.utf8 LC_NUMERIC=C                  
[5] LC_TIME=French_France.utf8    

attached base packages:
[1] stats     graphics  grDevices utils     datasets  methods   base     

other attached packages:
 [1] lubridate_1.9.2 forcats_1.0.0   stringr_1.5.0   dplyr_1.1.2     purrr_1.0.1    
 [6] readr_2.1.4     tidyr_1.3.0     tibble_3.2.1    ggplot2_3.4.2   tidyverse_2.0.0

loaded via a namespace (and not attached):
 [1] rstudioapi_0.14   magrittr_2.0.3    hms_1.1.3         tidyselect_1.2.0  munsell_0.5.0    
 [6] timechange_0.2.0  colorspace_2.1-0  R6_2.5.1          rlang_1.1.1       fansi_1.0.4      
[11] tools_4.2.3       grid_4.2.3        gtable_0.3.3      sessioninfo_1.2.2 utf8_1.2.3       
[16] cli_3.6.1         withr_2.5.0       lifecycle_1.0.3   tzdb_0.4.0        vctrs_0.6.2      
[21] glue_1.6.2        stringi_1.7.12    compiler_4.2.3    pillar_1.9.0      generics_0.1.3   
[26] scales_1.2.1      pkgconfig_2.0.3 
```