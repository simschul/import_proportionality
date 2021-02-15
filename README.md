
# Import Proportionality Assumption in MRIO

<!-- badges: start -->

<!-- badges: end -->

This repository contains all code to reproduce the results from our
paper on ‘Relaxing the import proportionality assumption in
multi-regional input-output anlysis’ submitted to the Journal of
Economic Structures.

## Abstract

In the absence of data on the destination industry of international
trade flowsmost multi-regional input-output (MRIO) tables are based on
the importproportionality assumption. Under this assumption imported
commodities areproportionally distributed over the target sectors
(individual industries and finaldemand categories) of an importing
region.Here, we quantify the uncertainty arising from the import
proportionalityassumption on the four major environmental footprints of
the different regionsand industries represented in the MRIO database
EXIOBASE. We randomise theglobal import flows by applying an algorithm
that randomly assigns importedcommodities block-wise to the target
sectors of an importing region, whilemaintaining the trade balance.We
find the variability of the national footprints in general below a
coefficientof variation (CV) of 4%, except for the material, water and
land footprints ofhighly trade-dependent and small economies. At the
industry level the variabilityis higher with 25% of the footprints
having a CV above 10% (carbon footprint),and above 30% (land, material
and water footprint), respectively, with maximumCVs up to 394%.We
provide a list of the variability of the national and industry
environmentalfootprints in the online SI so that MRIO scholars can check
if a industry/regionthat is important in their study ranks high, so that
either the database can beimproved through adding more details on
bilateral trade, or the uncertainty canbe calculated and reported.

## How to use the scripts

The individual scripts should be run according to their numbering. Note,
that for running the script you need the 2011 table from EXIOBASE
Version 3.4 in the industry-by-industry version. You can find it on
<https://exiobase.eu/>.

For questions you can contact me via [e-mail](mailto:s.pedro@gmx.de) or
raise an issue on github.

All code was tested with the following setup:

    ## R version 3.6.3 (2020-02-29)
    ## Platform: x86_64-pc-linux-gnu (64-bit)
    ## Running under: Ubuntu 18.04.5 LTS
    ## 
    ## Matrix products: default
    ## BLAS:   /usr/lib/x86_64-linux-gnu/openblas/libblas.so.3
    ## LAPACK: /usr/lib/x86_64-linux-gnu/libopenblasp-r0.2.20.so
    ## 
    ## locale:
    ##  [1] LC_CTYPE=en_US.UTF-8       LC_NUMERIC=C              
    ##  [3] LC_TIME=de_DE.UTF-8        LC_COLLATE=en_US.UTF-8    
    ##  [5] LC_MONETARY=de_DE.UTF-8    LC_MESSAGES=en_US.UTF-8   
    ##  [7] LC_PAPER=de_DE.UTF-8       LC_NAME=C                 
    ##  [9] LC_ADDRESS=C               LC_TELEPHONE=C            
    ## [11] LC_MEASUREMENT=de_DE.UTF-8 LC_IDENTIFICATION=C       
    ## 
    ## attached base packages:
    ## [1] grid      parallel  stats     graphics  grDevices utils     datasets 
    ## [8] methods   base     
    ## 
    ## other attached packages:
    ##  [1] RColorBrewer_1.1-2  tikzDevice_0.12.3.1 ape_5.4-1          
    ##  [4] cowplot_1.1.0       ggalluvial_0.9.1    easyalluvial_0.2.3 
    ##  [7] ggpubr_0.4.0        scales_1.1.1        ggthemes_4.1.0     
    ## [10] ggrepel_0.8.0       gridExtra_2.3       plotly_4.8.0       
    ## [13] viridis_0.5.1       viridisLite_0.3.0   pbmcapply_1.5.0    
    ## [16] my.utils_0.0.0.9000 Rfast_2.0.1         RcppZiggurat_0.1.6 
    ## [19] Rcpp_1.0.6          tictoc_1.0          forcats_0.5.0      
    ## [22] stringr_1.4.0       dplyr_1.0.2         purrr_0.3.4        
    ## [25] readr_1.3.1         tidyr_1.1.2         tibble_3.0.5       
    ## [28] ggplot2_3.3.2       tidyverse_1.3.0     data.table_1.13.6  
    ## 
    ## loaded via a namespace (and not attached):
    ##  [1] colorspace_2.0-0     ggsignif_0.6.0       ggridges_0.5.1      
    ##  [4] ellipsis_0.3.1       class_7.3-18         rio_0.5.16          
    ##  [7] fs_1.5.0             rstudioapi_0.13      prodlim_2019.11.13  
    ## [10] fansi_0.4.2          lubridate_1.7.9.2    xml2_1.3.2          
    ## [13] codetools_0.2-18     splines_3.6.3        knitr_1.31          
    ## [16] jsonlite_1.7.2       caret_6.0-84         broom_0.7.0         
    ## [19] dbplyr_1.4.4         compiler_3.6.3       httr_1.4.2          
    ## [22] backports_1.2.1      assertthat_0.2.1     Matrix_1.3-2        
    ## [25] lazyeval_0.2.2       cli_2.2.0            prettyunits_1.1.1   
    ## [28] htmltools_0.5.1.1    tools_3.6.3          gtable_0.3.0        
    ## [31] glue_1.4.2           reshape2_1.4.4       carData_3.0-4       
    ## [34] cellranger_1.1.0     vctrs_0.3.6          filehash_2.4-2      
    ## [37] nlme_3.1-151         iterators_1.0.13     timeDate_3043.102   
    ## [40] gower_0.2.2          xfun_0.20            openxlsx_4.2.3      
    ## [43] rvest_0.3.6          lifecycle_0.2.0      rstatix_0.6.0       
    ## [46] MASS_7.3-53          ipred_0.9-9          hms_1.0.0           
    ## [49] yaml_2.2.1           curl_4.3             rpart_4.1-15        
    ## [52] stringi_1.5.3        foreach_1.5.1        e1071_1.7-4         
    ## [55] zip_2.1.1            lava_1.6.8.1         rlang_0.4.10        
    ## [58] pkgconfig_2.0.3      evaluate_0.14        lattice_0.20-41     
    ## [61] recipes_0.1.6        htmlwidgets_1.5.3    tidyselect_1.1.0    
    ## [64] plyr_1.8.6           magrittr_2.0.1       R6_2.5.0            
    ## [67] generics_0.1.0       DBI_1.1.1            pillar_1.4.7        
    ## [70] haven_2.3.1          foreign_0.8-71       withr_2.4.1         
    ## [73] survival_3.2-7       abind_1.4-5          nnet_7.3-15         
    ## [76] modelr_0.1.8         crayon_1.4.0         car_3.0-2           
    ## [79] rmarkdown_2.6        progress_1.2.2       readxl_1.3.1        
    ## [82] blob_1.2.1           ModelMetrics_1.2.2.2 reprex_1.0.0        
    ## [85] digest_0.6.27        stats4_3.6.3         munsell_0.5.0
