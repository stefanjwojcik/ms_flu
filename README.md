# Survey Data and Human Computation for Improved Flu Tracking
Submission to Nature Comms

## The R Session Info for the main survey analysis: 
```R version 3.5.2 (2018-12-20)
Platform: x86_64-apple-darwin15.6.0 (64-bit)
Running under: macOS High Sierra 10.13.6

Matrix products: default
BLAS: /System/Library/Frameworks/Accelerate.framework/Versions/A/Frameworks/vecLib.framework/Versions/A/libBLAS.dylib
LAPACK: /Library/Frameworks/R.framework/Versions/3.5/Resources/lib/libRlapack.dylib

locale:
[1] en_US.UTF-8/en_US.UTF-8/en_US.UTF-8/C/en_US.UTF-8/en_US.UTF-8

attached base packages:
[1] stats     graphics  grDevices utils     datasets  methods   base     

other attached packages:
 [1] irr_0.84.1        lpSolve_5.6.13.2  data.table_1.11.8 SnowballC_0.6.0   tidytext_0.2.2   
 [6] dplyr_0.7.8       texreg_1.36.23    noncensus_0.1     Zelig_5.1.6.1     survival_2.43-3  
[11] effects_4.1-0     carData_3.0-2     stargazer_5.2.2   ggplot2_3.1.0    

loaded via a namespace (and not attached):
 [1] Rcpp_1.0.0         lattice_0.20-38    zoo_1.8-5          assertthat_0.2.0   lmtest_0.9-37     
 [6] R6_2.3.0           cellranger_1.1.0   plyr_1.8.4         MatrixModels_0.4-1 stats4_3.5.2      
[11] MatchIt_3.0.2      Amelia_1.7.5       survey_3.35        coda_0.19-3        AER_1.2-7         
[16] pillar_1.3.1       miscTools_0.6-22   geepack_1.2-1      rlang_0.3.1        lazyeval_0.2.1    
[21] curl_3.3           readxl_1.2.0       rstudioapi_0.9.0   minqa_1.2.4        SparseM_1.77      
[26] car_3.0-3          nloptr_1.2.1       Matrix_1.2-15      splines_3.5.2      lme4_1.1-19       
[31] foreign_0.8-71     munsell_0.5.0      janeaustenr_0.1.5  compiler_3.5.2     pkgconfig_2.0.2   
[36] maxLik_1.3-6       mcmc_0.9-6         nnet_7.3-12        tidyselect_0.2.5   tibble_2.0.0      
[41] rio_0.5.16         crayon_1.3.4       withr_2.1.2        MASS_7.3-51.1      grid_3.5.2        
[46] nlme_3.1-137       jsonlite_1.6       gtable_0.2.0       magrittr_1.5       tokenizers_0.2.1  
[51] scales_1.0.0       zip_2.0.3          stringi_1.2.4      bindrcpp_0.2.2     generics_0.0.2    
[56] sandwich_2.5-0     openxlsx_4.1.0.1   Formula_1.2-3      tools_3.5.2        forcats_0.3.0     
[61] glue_1.3.0         purrr_0.2.5        hms_0.4.2          abind_1.4-5        colorspace_1.3-2  
[66] VGAM_1.1-1         bindr_0.1.1        haven_2.0.0        quantreg_5.51      MCMCpack_1.4-4    
```

The version of LME-4 used to generate the MRP estimates: lme4_1.1-12


## Demo:

### The main analysis file 
File name: `Main_Replication_Code.R`

This R file produces figures and images for the main survey analysis - including the main rates of searching in the presence of flu-like symptoms and none. It sources the following supplementary code files:

* `main_flu_dat.rds`: survey data, each row is a survey response, columns describe each respondent, id column is RID
* `queries.rds`: query data from respondents, each row is a query, id column is QID2
* `pages.rds`: page visits from respondents, each row is a page visit, id column is QID2
* `expanded_queries.rds`: queries expanded using Doc2Vec technique, each row is a query, coded by assistants
* `expanded_queries_final.rds`: final set of coded queries, with a single code
* `tau.rds`: the estimated base rate of flu search as a proportion of all searches
* `panel_demographics.csv`: more limited survey data with searches, demographics, and symptoms of users
* `main_flu_dat_sorethroat.rds`: survey data with searches, demographics, and symptoms of users (using alternative definition of flu) 
* `panel_demographics_sorethroat.csv`:more limited survey data with searches, demographics, and symptoms of users (using alternative definition of flu

 

###



