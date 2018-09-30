
<!-- README.md is generated from README.Rmd. Please edit that file -->

# DE.AHC

*by **John D. Gagnon*** <br> *University of California, San Francisco*

### Table of Contents

**[Overview](#overview)**<br> **[Installation](#installation)**<br>
**[Usage](#usage)**<br> **[Session info](#session-info)**<br>
**[License](#license)**<br>

## Overview

A shinyapp-based GUI to visualize miR-15/16 differential expression data
along with AGO2 HITS-CLIP data for each gene.

## Installation

1.  If you do not already have R installed, or your version is out of
    date, download and install the latest
    [version](https://cran.r-project.org).

<!-- end list -->

  - Optionally, install the latest version of [RStudio
    Desktop](https://www.rstudio.com/products/rstudio/#Desktop).

<!-- end list -->

2.  Download the package from GitHub

<!-- end list -->

``` r
install.packages("devtools")
devtools::install_github("jdgagnon/DE.AHC")
```

## Usage

1.  Load the package into the R session.

`library(DE.AHC)`

2.  To initialize the shiny app, paste the following code in your R
    console and run it.

`runDEAHCapp()`

3.  Optionally subset genes based on pathways or list specific genes.
    Toggle checkbox to filter genes that contain miR-15/16 seed-matches
    in their 3’UTR

<!-- end list -->

  - Load your own differential expression data file

  - File should be structured
    like:
    
    | Gene l | ogFC  | PValue    |   FDR    | WT\_1 | WT\_2 | WT\_3 c | KO\_1 c | KO\_2 c | KO\_3 |
    | ------ | :---- | :-------- | :------: | :---- | :---- | ------- | ------- | ------- | :---- |
    | Cd28   | 2.4 3 | .42e-05 0 | .0004244 | 50    | 48    | 49      | 62      | 58      | 60    |
    | etc…   | etc…  | etc…      |   etc…   | etc…  | etc…  | etc…    | etc…    | etc…    | etc…  |
    

  - Load your own AGO2 HITS-CLIP data file

  - File should be structured like:
    
    | Gene L | ocation |        miR T | argetscan | Sum  |
    | ------ | :------ | -----------: | :-------- | :--- |
    | Cd28   | 3UTR m  | mu-miR-16-5p | TRUE      | 2899 |
    | etc…   | etc…    |         etc… | etc…      | etc… |
    

## Session info

Here is the output of `sessionInfo()` on the system on which this
package was developed:

``` r
sessionInfo()
#> R version 3.5.1 (2018-07-02)
#> Platform: x86_64-apple-darwin17.6.0 (64-bit)
#> Running under: macOS High Sierra 10.13.6
#> 
#> Matrix products: default
#> BLAS: /System/Library/Frameworks/Accelerate.framework/Versions/A/Frameworks/vecLib.framework/Versions/A/libBLAS.dylib
#> LAPACK: /System/Library/Frameworks/Accelerate.framework/Versions/A/Frameworks/vecLib.framework/Versions/A/libLAPACK.dylib
#> 
#> locale:
#> [1] en_US.UTF-8/en_US.UTF-8/en_US.UTF-8/C/en_US.UTF-8/en_US.UTF-8
#> 
#> attached base packages:
#> [1] stats     graphics  grDevices utils     datasets  methods   base     
#> 
#> loaded via a namespace (and not attached):
#>  [1] compiler_3.5.1  backports_1.1.2 magrittr_1.5    rprojroot_1.3-2
#>  [5] tools_3.5.1     htmltools_0.3.6 yaml_2.2.0      Rcpp_0.12.18   
#>  [9] stringi_1.2.4   rmarkdown_1.10  highr_0.7       knitr_1.20     
#> [13] stringr_1.3.1   digest_0.6.17   evaluate_0.11
```

<br><br>

## License

[GNU GPL-3.0-or-later](https://www.gnu.org/licenses/gpl.txt)
