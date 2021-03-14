Environmental and genetic contributions to eco-geographic rules in house
mice
================
Mallory A. Ballinger
12/19/2020

\[Abstract goes here\] Using metadata from museums, colony data from the
Nachman lab, and morphological data generated from a common garden
experiment, we examine the environmental and genetic contributions of
Bergmann’s Rule and Allen’s Rule in temperate and tropical populations
of house mice.

## \#\#\# Overview:

    BallingerNachman_allenbergmann_XXXX_2021
    |- README          # top level description of content
    |
    |- data            # raw and primary data, are not changed once created
    |  |- raw/         # raw data, will not be altered
    |  +- process/     # cleaned data, will not be altered once created
    |
    |- code/           # any programmatic code
    |
    |- results/        # all output from workflows and analyses
    |  |- tables/      # tables, likely designated for manuscript tables
    |  |- figures/     # graphs, likely designated for manuscript figures
    |
    |- submission      # files for manuscript
    |
    +- Makefile        # executable Makefile for this study

### Dependencies:

  - R version 4.0.3 (2020-10-10)
      - `tidyverse` (v. 1.3.0)  
      - `rmarkdown` (v. 2.7)  
      - `here` (v. 1.0.1)
      - `tinytex` (v. 0.29)

### My computer

    ## R version 4.0.3 (2020-10-10)
    ## Platform: x86_64-apple-darwin17.0 (64-bit)
    ## Running under: macOS Catalina 10.15.4
    ## 
    ## Matrix products: default
    ## BLAS:   /Library/Frameworks/R.framework/Versions/4.0/Resources/lib/libRblas.dylib
    ## LAPACK: /Library/Frameworks/R.framework/Versions/4.0/Resources/lib/libRlapack.dylib
    ## 
    ## locale:
    ## [1] en_US.UTF-8/en_US.UTF-8/en_US.UTF-8/C/en_US.UTF-8/en_US.UTF-8
    ## 
    ## attached base packages:
    ## [1] stats     graphics  grDevices utils     datasets  methods   base     
    ## 
    ## other attached packages:
    ##  [1] here_1.0.1      forcats_0.5.1   stringr_1.4.0   dplyr_1.0.4    
    ##  [5] purrr_0.3.4     readr_1.4.0     tidyr_1.1.2     tibble_3.1.0   
    ##  [9] ggplot2_3.3.3   tidyverse_1.3.0 rmarkdown_2.7  
    ## 
    ## loaded via a namespace (and not attached):
    ##  [1] tidyselect_1.1.0  xfun_0.21         haven_2.3.1       colorspace_2.0-0 
    ##  [5] vctrs_0.3.6       generics_0.1.0    htmltools_0.5.1.1 yaml_2.2.1       
    ##  [9] utf8_1.1.4        rlang_0.4.10      pillar_1.5.0      withr_2.4.1      
    ## [13] glue_1.4.2        DBI_1.1.1         dbplyr_2.1.0      modelr_0.1.8     
    ## [17] readxl_1.3.1      lifecycle_1.0.0   munsell_0.5.0     gtable_0.3.0     
    ## [21] cellranger_1.1.0  rvest_0.3.6       evaluate_0.14     knitr_1.31       
    ## [25] ps_1.5.0          fansi_0.4.2       broom_0.7.5       Rcpp_1.0.6       
    ## [29] scales_1.1.1      backports_1.2.1   jsonlite_1.7.2    fs_1.5.0         
    ## [33] hms_1.0.0         digest_0.6.27     stringi_1.5.3     rprojroot_2.0.2  
    ## [37] grid_4.0.3        cli_2.3.1         tools_4.0.3       magrittr_2.0.1   
    ## [41] crayon_1.4.1      pkgconfig_2.0.3   ellipsis_0.3.1    xml2_1.3.2       
    ## [45] reprex_1.0.0      lubridate_1.7.9.2 assertthat_0.2.1  httr_1.4.2       
    ## [49] rstudioapi_0.13   R6_2.5.0          compiler_4.0.3
