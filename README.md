# Survey Data and Human Computation for Improved Flu Tracking
Paper in Nature Communications - Authors: Wojcik, Bijral, Johnston, Lavista, King, Kennedy, Vespignani, and Lazer

Link: https://www.nature.com/articles/s41467-020-20206-z

**What this replication file includes:**
- Replication of individual-level survey data findings using actual data (including tables, numbers, and figures)
- Replication of MRP smoothing technique using example data
- Replication of Time-series tracking technique using actual data
- Relative rates of flu search by region and by year

**What this replication file does not include:**
- Replication of tables, numbers, and figures based on proprietary vendor panel data (for example, invitees vs. responded vs. whole panel)
- Raw search query data


## The R Session Info for the main survey analysis: (Allow 5-10 minutes for package installation)
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
 [1] randomForest_4.6-12 caret_6.0-73        lattice_0.20-38     SnowballC_0.6.0     tidytext_0.2.2      texreg_1.36.23     
 [7] Zelig_5.1.6.1       survival_2.43-3     effects_4.1-0       carData_3.0-2       stargazer_5.2.2     scales_1.0.0       
[13] reshape2_1.4.3      forecast_8.10       TSA_1.2             lubridate_1.7.4     zoo_1.8-5           pwr_1.3-0          
[19] irr_0.84.1          lpSolve_5.6.13.2    bindrcpp_0.2.2      dplyr_0.7.8         ggplot2_3.1.0       data.table_1.11.8  
[25] reshape_0.8.8       noncensus_0.1      

loaded via a namespace (and not attached):
 [1] VGAM_1.1-1          minqa_1.2.4         colorspace_1.3-2    class_7.3-14        ISOcodes_2019.04.22 rio_0.5.16         
 [7] rstudioapi_0.9.0    MatrixModels_0.4-1  fansi_0.4.0         codetools_0.2-15    splines_3.5.2       leaps_3.0          
[13] Formula_1.2-3       jsonlite_1.6        nloptr_1.2.1        mcmc_0.9-6          geepack_1.2-1       compiler_3.5.2     
[19] assertthat_0.2.0    Matrix_1.2-15       lazyeval_0.2.1      survey_3.35         cli_1.0.1           quantreg_5.51      
[25] tools_3.5.2         coda_0.19-3         gtable_0.2.0        glue_1.3.0          Rcpp_1.0.0          cellranger_1.1.0   
[31] fracdiff_1.5-0      Amelia_1.7.5        urca_1.3-0          nlme_3.1-137        iterators_1.0.10    lmtest_0.9-37      
[37] timeDate_3043.102   stringr_1.3.1       stopwords_1.0       openxlsx_4.1.0.1    lme4_1.1-19         MASS_7.3-51.1      
[43] miscTools_0.6-22    hms_0.4.2           parallel_3.5.2      sandwich_2.5-0      SparseM_1.77        quantmod_0.4-15    
[49] curl_3.3            stringi_1.2.4       tokenizers_0.2.1    tseries_0.10-47     foreach_1.4.4       e1071_1.7-0        
[55] AER_1.2-7           TTR_0.23-6          zip_2.0.3           rlang_0.3.1         pkgconfig_2.0.2     purrr_0.2.5        
[61] bindr_0.1.1         labeling_0.3        tidyselect_0.2.5    plyr_1.8.4          magrittr_1.5        R6_2.3.0           
[67] generics_0.0.2      pillar_1.3.1        haven_2.0.0         foreign_0.8-71      withr_2.1.2         mgcv_1.8-26        
[73] xts_0.11-2          abind_1.4-5         nnet_7.3-12         tibble_2.0.0        janeaustenr_0.1.5   crayon_1.3.4       
[79] car_3.0-3           utf8_1.1.4          maxLik_1.3-6        locfit_1.5-9.1      grid_3.5.2          readxl_1.2.0       
[85] forcats_0.3.0       ModelMetrics_1.2.2  digest_0.6.18       MCMCpack_1.4-4      MatchIt_3.0.2       stats4_3.5.2       
[91] munsell_0.5.0       quadprog_1.5-8      
```

The version of LME-4 used to generate the MRP estimates: lme4_1.1-12
The classification model presented in the supplementary materials uses randomForest version 4.6-12.

## Part 1: Survey and MRP (located in Survey and MRP folder) (Runs in less than five minutes)

Expected result includes numbers, tables, and figures in the survey sections of the main text and supplementary file. 
 
Run `Main_Replication_Code.R` to produce the results of the survey analysis and query coding. This R file produces figures and images for the main survey analysis - including the main rates of searching in the presence of flu-like symptoms and none. It sources the following supplementary code files:

* `main_flu_dat.rds`: survey data, each row is a survey response, columns describe each respondent, id column is RID
* `queries.rds`: query data from respondents, each row is a query, id column is QID2
* `pages.rds`: page visits from respondents, each row is a page visit, id column is QID2
* `expanded_queries.rds`: queries expanded using Doc2Vec technique, each row is a query, coded by assistants
* `expanded_queries_final.rds`: final set of coded queries, with a single code
* `tau.rds`: the estimated base rate of flu search as a proportion of all searches
* `panel_demographics.csv`: more limited survey data with searches, demographics, and symptoms of users
* `main_flu_dat_sorethroat.rds`: survey data with searches, demographics, and symptoms of users (using alternative definition of flu) 
* `panel_demographics_sorethroat.csv`:more limited survey data with searches, demographics, and symptoms of users (using alternative definition of flu
 
### MRP smoothing example file (runs in less than five minutes)
File name: `demo_MRP_smoother.R`

Run `demo_MRP_smoother.R` to replicate the MRP smoothing and reweighting method from the main paper. 

This file does an example smoothing and reweighting procedure, identical to the one we did in the paper, but using a query data file that is not the real query data (we do not provide these data by request of MS). This file was created by taking the actual query data used in the paper, isolating a single short period of time, and sampling roughly 100 zip codes. Then, the actual queries executed in these zipcodes on a given day are re-shuffled with those in other zip codes. As a result, the queries on a given day are real, but they do not acccurately depict the true behavior in that zip code. 

This file opens on the following files:

* `zipcodeCensusData_v2.rds`: Zipcode demographic data from the Census
* `mrp_example_data.csv`: Example query data, reshuffled 


## Part 2: Forecasting (located in 'Forecasting') (runs in 15-25 minutes)



Run `main_State.R` and `USFluPredictionUpdated_ann.R` to produce the main forecasting results. The source data files are located in `Forecasting/Data`. 

`main_State.R`: produces the four state-level forecasts. This file should take between 5 and 10 minutes to run from start to finish. 

`USFluPredictionUpdated_ann.R`: produces main national flu forecast. This file should take between 10 and 20 minutes to run from start to finish, depending on your machine. A test run on a 2014 Macbook Air with 8gb of RAM finishes in less than 15 minutes.  


### The session info for the forecasting analysis is as follows: 

```
R version 3.5.2 (2018-12-20)
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
[1] ggplot2_3.1.0   argo_2.0.0      xts_0.11-2      forecast_8.10   TSA_1.2         lubridate_1.7.4 zoo_1.8-5      

loaded via a namespace (and not attached):
 [1] locfit_1.5-9.1    tidyselect_0.2.5  purrr_0.2.5       urca_1.3-0        lattice_0.20-38   colorspace_1.3-2  mgcv_1.8-26      
 [8] XML_3.98-1.20     rlang_0.3.1       pillar_1.3.1      withr_2.1.2       glue_1.3.0        TTR_0.23-6        bindrcpp_0.2.2   
[15] foreach_1.4.4     bindr_0.1.1       plyr_1.8.4        quantmod_0.4-15   stringr_1.3.1     timeDate_3043.102 munsell_0.5.0    
[22] gtable_0.2.0      codetools_0.2-15  leaps_3.0         labeling_0.3      tseries_0.10-47   lmtest_0.9-37     parallel_3.5.2   
[29] curl_3.3          Rcpp_1.0.0        scales_1.0.0      fracdiff_1.5-0    stringi_1.2.4     dplyr_0.7.8       grid_3.5.2       
[36] quadprog_1.5-8    tools_3.5.2       magrittr_1.5      glmnet_2.0-16     lazyeval_0.2.1    tibble_2.0.0      crayon_1.3.4     
[43] pkgconfig_2.0.2   Matrix_1.2-15     iterators_1.0.10  assertthat_0.2.0  rstudioapi_0.9.0  R6_2.3.0          boot_1.3-20      
[50] nnet_7.3-12       nlme_3.1-137      compiler_3.5.2   
```
