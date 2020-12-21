Environmental and genetic contributions to eco-geographic rules in house mice
================
Mallory A. Ballinger
12/19/2020

Using metadata from museums, colony data from the Nachman lab, and morphological data generated from a common garden experiment, we examine the environmental and genetic contributions of Bergmann's Rule and Allen's Rule in temperate and tropical populations of house mice.

### Dependencies:

-   R version 4.0.3 (2020-10-10)
    -   `tidyverse` (v. 1.3.0)
    -   `rmarkdown` (v. 2.6)
    -   `here` (v. 0.1)

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
    ##  [1] here_0.1        rmarkdown_2.6   forcats_0.5.0   stringr_1.4.0  
    ##  [5] dplyr_1.0.2     purrr_0.3.4     readr_1.4.0     tidyr_1.1.2    
    ##  [9] tibble_3.0.4    ggplot2_3.3.2   tidyverse_1.3.0
    ## 
    ## loaded via a namespace (and not attached):
    ##  [1] tidyselect_1.1.0 xfun_0.19        haven_2.3.1      colorspace_1.4-1
    ##  [5] vctrs_0.3.4      generics_0.1.0   htmltools_0.5.0  yaml_2.2.1      
    ##  [9] blob_1.2.1       rlang_0.4.8      pillar_1.4.6     glue_1.4.2      
    ## [13] withr_2.3.0      DBI_1.1.0        dbplyr_1.4.4     modelr_0.1.8    
    ## [17] readxl_1.3.1     lifecycle_0.2.0  munsell_0.5.0    gtable_0.3.0    
    ## [21] cellranger_1.1.0 rvest_0.3.6      evaluate_0.14    knitr_1.30      
    ## [25] fansi_0.4.1      broom_0.7.2      Rcpp_1.0.5       scales_1.1.1    
    ## [29] backports_1.1.10 jsonlite_1.7.1   fs_1.5.0         hms_0.5.3       
    ## [33] digest_0.6.27    stringi_1.5.3    grid_4.0.3       rprojroot_1.3-2 
    ## [37] cli_2.1.0        tools_4.0.3      magrittr_1.5     crayon_1.3.4    
    ## [41] pkgconfig_2.0.3  ellipsis_0.3.1   xml2_1.3.2       reprex_0.3.0    
    ## [45] lubridate_1.7.9  assertthat_0.2.1 httr_1.4.2       rstudioapi_0.11 
    ## [49] R6_2.5.0         compiler_4.0.3
