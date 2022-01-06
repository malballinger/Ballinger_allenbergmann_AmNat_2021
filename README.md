The contribution of genetic and environmental effects to Bergmann’s rule
and Allen’s rule in house mice
================
Mallory A. Ballinger and Michael W. Nachman
(last updated: January 5, 2022)

See our bioRxiv preprint here:
[![DOI:10.1101/2021.06.14.448454](http://img.shields.io/badge/DOI-10.1101/2021.06.14.448454-B31B1B.svg)](https://doi.org/10.1101/2021.06.14.448454)

Abstract:

Distinguishing between genetic, environmental, and
genotype-by-environment effects is central to understanding geographic
variation in phenotypic clines. Two of the best-documented phenotypic
clines are Bergmann’s rule and Allen’s rule, which describe larger body
sizes and shortened extremities in colder climates, respectively.
Although numerous studies have found inter- and intraspecific evidence
for both ecogeographic patterns, we still have a poor understanding of
the extent to which these patterns are driven by genetics, environment,
or both. Here, we measured the genetic and environmental contributions
to Bergmann’s rule and Allen’s rule across introduced populations of
house mice (*Mus musculus domesticus*) in the Americas. First, we
documented clines for body mass, tail length, and ear length in natural
populations, and found that these conform to both Bergmann’s rule and
Allen’s rule. We then raised descendants of wild-caught mice in the lab
and showed that these differences persisted in a common environment and
are heritable, indicating that they have a genetic basis. Finally, using
a full-sib design, we reared mice under warm and cold conditions. We
found very little plasticity associated with body size, suggesting that
Bergmann’s rule has been shaped by strong directional selection in house
mice. However, extremities showed considerable plasticity, as both tails
and ears grew shorter in cold environments. These results indicate that
adaptive phenotypic plasticity as well as genetic changes underlie major
patterns of clinal variation in house mice and likely facilitated their
rapid expansion into new environments across the Americas.

## Overview:

    Ballinger_allenbergmann_AmNat_2021
    |- README          # top level description of content
    |
    |- data            # raw and processed data, are not changed once created
    |  |- raw/         # raw data, will not be altered
    |  |- processed/   # cleaned data, will not be altered once created
    |
    |- code/           # programmatic code for cleaning, modeling, and plotting
    |
    |- results/        # all output from workflows and analyses
    |  |- tables/      # tables, designated for supplementary material
    |  |- figures/     # graphs, designated for manuscript figures
    |
    |- submission      # files for manuscript
    |
    |- Makefile        # executable Makefile for this study

### Dependencies:

-   R version 4.1.1 (2021-08-10)
    -   `tidyverse` (v. 1.3.1)  
    -   `rmarkdown` (v. 2.11)  
    -   `here` (v. 1.0.1)
    -   `readxl` (v. 1.3.1)
    -   `cowplot` (v. 1.1.1)
    -   `performance` (v. 0.8.0)
    -   `see` (v. 0.6.8)
    -   `car` (v. 3.0.12)
    -   `tinytex` (v. 0.35)

### My computer

    ## R version 4.1.1 (2021-08-10)
    ## Platform: x86_64-apple-darwin17.0 (64-bit)
    ## Running under: macOS Big Sur 10.16
    ## 
    ## Matrix products: default
    ## BLAS:   /Library/Frameworks/R.framework/Versions/4.1/Resources/lib/libRblas.0.dylib
    ## LAPACK: /Library/Frameworks/R.framework/Versions/4.1/Resources/lib/libRlapack.dylib
    ## 
    ## locale:
    ## [1] en_US.UTF-8/en_US.UTF-8/en_US.UTF-8/C/en_US.UTF-8/en_US.UTF-8
    ## 
    ## attached base packages:
    ## [1] stats     graphics  grDevices utils     datasets  methods   base     
    ## 
    ## other attached packages:
    ##  [1] here_1.0.1      rmarkdown_2.11  forcats_0.5.1   stringr_1.4.0  
    ##  [5] dplyr_1.0.7     purrr_0.3.4     readr_2.1.1     tidyr_1.1.4    
    ##  [9] tibble_3.1.6    ggplot2_3.3.5   tidyverse_1.3.1
    ## 
    ## loaded via a namespace (and not attached):
    ##  [1] tidyselect_1.1.1 xfun_0.28        haven_2.4.3      colorspace_2.0-2
    ##  [5] vctrs_0.3.8      generics_0.1.1   htmltools_0.5.2  yaml_2.2.1      
    ##  [9] utf8_1.2.2       rlang_0.4.12     pillar_1.6.4     glue_1.5.1      
    ## [13] withr_2.4.3      DBI_1.1.1        dbplyr_2.1.1     modelr_0.1.8    
    ## [17] readxl_1.3.1     lifecycle_1.0.1  munsell_0.5.0    gtable_0.3.0    
    ## [21] cellranger_1.1.0 rvest_1.0.2      evaluate_0.14    knitr_1.36      
    ## [25] tzdb_0.2.0       fastmap_1.1.0    fansi_0.5.0      broom_0.7.10    
    ## [29] Rcpp_1.0.7       scales_1.1.1     backports_1.4.0  jsonlite_1.7.2  
    ## [33] fs_1.5.2         hms_1.1.1        digest_0.6.29    stringi_1.7.6   
    ## [37] rprojroot_2.0.2  grid_4.1.1       cli_3.1.0        tools_4.1.1     
    ## [41] magrittr_2.0.1   crayon_1.4.2     pkgconfig_2.0.3  ellipsis_0.3.2  
    ## [45] xml2_1.3.3       reprex_2.0.1     lubridate_1.8.0  rstudioapi_0.13 
    ## [49] assertthat_0.2.1 httr_1.4.2       R6_2.5.1         compiler_4.1.1
