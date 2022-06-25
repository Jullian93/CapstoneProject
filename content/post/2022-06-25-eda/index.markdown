---
title: EDA
author: Jullian Schrup
date: '2022-06-25'
slug: eda
categories:
  - R
tags: []
---


```r
knitr::opts_chunk$set(echo = TRUE, message = FALSE, warning = FALSE)
library(tidyverse)
library(tidytext)
library(caret)
library(fastDummies)
library(randomForest)
library(ggplot2)
library(rpart)
library(rpart.plot)
library(xgboost)
library(MASS)
library(janitor)
library(psych)
library(corrplot)
library(scales)
library(ggthemes)
library(mboost)
library(bst)
library(h2o)
library(reshape2)
library(ShapleyValue)
library(kableExtra)
library(car)
library(readxl)
library(blogdown)


DMM_2022_Data_BST_Used <- read.csv("C:/Users/julli/Downloads/DMM 2022 - Data - BST - Used (10).csv")
DMM_2022_Data_FST_Used <- read.csv("C:/Users/julli/Downloads/DMM 2022 - Data - FST - Used.csv")
HiddenData <- read.csv("C:/Users/julli/Downloads/DMM_CSV_2Weeks.csv")
HiddenFST <- read.csv("C:/Users/julli/Downloads/HiddenFST.csv")
BST <- clean_names(DMM_2022_Data_BST_Used)
BST <- dplyr::select(BST, -c(entry_page_home_page, entry_page_includes_size, entry_page_includes_tire_type, entry_page_includes_vehicles, entry_page_includes_tire_brands, tdp_entry, csc_entry))

FST <- clean_names(DMM_2022_Data_FST_Used)
FST <- dplyr::select(FST, -c(entry_page_home_page, entry_page_includes_size, entry_page_includes_tire_type, entry_page_includes_vehicles, entry_page_includes_tire_brands, tdp_entry, csc_entry))
```

# STEP 1: PCA - FACTOR ANALYSIS" FST


```r
DMM <- FST %>% filter(promo == 0) %>% dplyr::select(-c(brand,region,week,revenue))

## FACTOR

DMM_1 <- prcomp(x = DMM)
screeplot(DMM_1, type = "lines") ## We want to cutoff the screeplot at 4 factors
```

<img src="{{< blogdown/postref >}}index_files/figure-html/unnamed-chunk-1-1.png" width="672" />

```r
prc <- bind_cols(dplyr::select(FST %>% filter(promo == 0),revenue),as.data.frame(DMM_1$x))

# Correlation with Revenue
correlationmatrix <- cor(x = prc, y = prc$revenue) 
correlationmatrix
```

```
##                 [,1]
## revenue  1.000000000
## PC1     -0.037870860
## PC2      0.667603757
## PC3      0.011053967
## PC4      0.080400243
## PC5      0.171860415
## PC6     -0.365726068
## PC7     -0.145078563
## PC8      0.096359975
## PC9     -0.138162578
## PC10     0.338435069
## PC11     0.229646650
## PC12    -0.069284985
## PC13     0.041248049
## PC14    -0.022477271
## PC15     0.116048995
## PC16     0.007423457
## PC17     0.056817004
## PC18     0.075877739
## PC19     0.057369814
## PC20    -0.088283811
## PC21     0.163730997
## PC22    -0.061128014
## PC23    -0.146468645
## PC24    -0.029490017
## PC25     0.081742121
## PC26     0.189676500
## PC27    -0.120111446
## PC28     0.125466767
## PC29    -0.096895146
```

```r
filteredcorrelations <- as.data.frame(apply(correlationmatrix, 2, function(x) ifelse (abs(x) >=0.10, round(x,3), "-")))
filteredcorrelations
```

```
##             V1
## revenue      1
## PC1          -
## PC2      0.668
## PC3          -
## PC4          -
## PC5      0.172
## PC6     -0.366
## PC7     -0.145
## PC8          -
## PC9     -0.138
## PC10     0.338
## PC11      0.23
## PC12         -
## PC13         -
## PC14         -
## PC15     0.116
## PC16         -
## PC17         -
## PC18         -
## PC19         -
## PC20         -
## PC21     0.164
## PC22         -
## PC23    -0.146
## PC24         -
## PC25         -
## PC26      0.19
## PC27     -0.12
## PC28     0.125
## PC29         -
```

```r
melted_cormat <- melt(correlationmatrix)

ggplot(data = melted_cormat, aes(x=Var1, y=Var2, fill=value)) + 
  geom_tile() + theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1))  + 
  scale_fill_gradient2(low = "#075AFF",
                       mid = "#FFFFCC",
                       high = "#FF0000") +
  geom_tile(color = "black") +
  geom_text(aes(label = round(value, 2)), color = "black", size = 3) +
  coord_fixed()
```

<img src="{{< blogdown/postref >}}index_files/figure-html/unnamed-chunk-1-2.png" width="672" />

```r
# Finding most correlated PCA - for FST, it's Component 2.
biplot(DMM_1, choices = c(1,2))
```

<img src="{{< blogdown/postref >}}index_files/figure-html/unnamed-chunk-1-3.png" width="672" />

```r
print(ncol(DMM_1))
```

```
## NULL
```

```r
DMM_1$rotation
```

```
##                                                            PC1          PC2
## csc_catalog_not_entry                            -0.0451394220  0.330377580
## tdp_but_tdp_not_entry                            -0.0181736295  0.146217764
## tire_brand_visit_entry_page_not_tire_brand_visit -0.0059993124  0.032532127
## offers_section_not_financing_visit                0.0057752205  0.041030643
## x90_day_buy_try_visit                            -0.0038688628  0.008660155
## cfna_application_visit                            0.0011012696  0.001475093
## credit_card_vendor_click_through_e61              0.0002886396  0.001465705
## visited_reward_center                             0.0052048124  0.004247821
## page_contains_racing_visit                        0.0025716425 -0.002819555
## catalog_search_complete_e10                      -0.1157802264  0.898684757
## tire_detail_view_e20                              0.9916275721  0.127849713
## locate_tire_visits                               -0.0086109181  0.051691296
## store_dealer_visits                              -0.0023846186  0.051654257
## store_search_experiment                          -0.0094855677  0.094070606
## individual_store_detail_e40                      -0.0065072450  0.061476947
## catalog_sort_change_e110                          0.0037860601  0.003359309
## tire_comparison_e24                              -0.0013405030  0.003901746
## tire_fit_interaction_e46                         -0.0183085100  0.116937135
## store_detail_click_e38                           -0.0008216464  0.014450497
## content_show_hide_click_e35                      -0.0012867564  0.008701897
## purchase_at_retailer_e48                         -0.0013608620  0.005413647
## tab_navigation_click_e17                         -0.0013195160  0.041429742
## csc_interaction                                   0.0026542329  0.006010967
## tdp_interaction                                   0.0007770489  0.032400794
## stepss                                           -0.0072851217  0.059392339
## manual_e4_tdp                                    -0.0017121640  0.016980041
## manual_e4_csc                                    -0.0073106112  0.036535558
## manual_e4                                        -0.0086004633  0.051705611
## video_views_e65                                   0.0046967281  0.005835922
## promo                                             0.0000000000  0.000000000
##                                                           PC3           PC4
## csc_catalog_not_entry                            -0.294813925 -0.3486310097
## tdp_but_tdp_not_entry                            -0.234647285 -0.2606219753
## tire_brand_visit_entry_page_not_tire_brand_visit -0.131294621 -0.3347992798
## offers_section_not_financing_visit               -0.099876465  0.1201428319
## x90_day_buy_try_visit                             0.053208276 -0.0004274080
## cfna_application_visit                           -0.064955233  0.0138614195
## credit_card_vendor_click_through_e61             -0.023866901  0.0061886131
## visited_reward_center                             0.023546795  0.0672432592
## page_contains_racing_visit                       -0.059097150  0.0009094568
## catalog_search_complete_e10                       0.055292328  0.3078968210
## tire_detail_view_e20                              0.002966089 -0.0017899830
## locate_tire_visits                               -0.002483148 -0.0376707818
## store_dealer_visits                              -0.061104699 -0.4687565528
## store_search_experiment                          -0.066620320 -0.4843207894
## individual_store_detail_e40                      -0.006547992 -0.0143926152
## catalog_sort_change_e110                         -0.064531321  0.1038184174
## tire_comparison_e24                              -0.001152644 -0.0176708848
## tire_fit_interaction_e46                          0.865072011 -0.3190985228
## store_detail_click_e38                           -0.017804181 -0.0089217042
## content_show_hide_click_e35                       0.016078292 -0.0098417958
## purchase_at_retailer_e48                         -0.014305553  0.0316990867
## tab_navigation_click_e17                          0.092963692 -0.0318603325
## csc_interaction                                  -0.056187985  0.0626857669
## tdp_interaction                                   0.123836903  0.0183563531
## stepss                                           -0.154765022 -0.0719978109
## manual_e4_tdp                                    -0.008155244 -0.0035663274
## manual_e4_csc                                     0.005478826 -0.0387836598
## manual_e4                                        -0.002148893 -0.0374418280
## video_views_e65                                   0.058087873  0.0323377505
## promo                                             0.000000000  0.0000000000
##                                                          PC5          PC6
## csc_catalog_not_entry                            -0.25830908  0.143108112
## tdp_but_tdp_not_entry                             0.27938683  0.157575303
## tire_brand_visit_entry_page_not_tire_brand_visit -0.03528400  0.074391670
## offers_section_not_financing_visit                0.54132220 -0.338890222
## x90_day_buy_try_visit                            -0.20825163  0.025168525
## cfna_application_visit                           -0.11173529 -0.017094180
## credit_card_vendor_click_through_e61             -0.03948721 -0.005386861
## visited_reward_center                             0.13076426 -0.032858720
## page_contains_racing_visit                        0.06625324 -0.002530265
## catalog_search_complete_e10                       0.03071074  0.023540812
## tire_detail_view_e20                             -0.01389002  0.007004508
## locate_tire_visits                               -0.13174289 -0.008139269
## store_dealer_visits                               0.23045261 -0.150697884
## store_search_experiment                           0.12576070 -0.160783704
## individual_store_detail_e40                      -0.28737759 -0.877380477
## catalog_sort_change_e110                          0.12679054 -0.055476366
## tire_comparison_e24                              -0.01158020 -0.005657740
## tire_fit_interaction_e46                         -0.03032912  0.017472175
## store_detail_click_e38                            0.01292183 -0.019832464
## content_show_hide_click_e35                       0.00140679 -0.022264959
## purchase_at_retailer_e48                          0.01625765 -0.021790365
## tab_navigation_click_e17                          0.32008909 -0.001674517
## csc_interaction                                   0.09513191 -0.051158916
## tdp_interaction                                   0.34242918 -0.034404929
## stepss                                           -0.17842012 -0.042309785
## manual_e4_tdp                                    -0.01825613  0.017858341
## manual_e4_csc                                    -0.12320991 -0.028094228
## manual_e4                                        -0.13190284 -0.008109150
## video_views_e65                                   0.03109501  0.041167507
## promo                                             0.00000000  0.000000000
##                                                           PC7          PC8
## csc_catalog_not_entry                             0.197447445 -0.461617708
## tdp_but_tdp_not_entry                             0.398567797  0.173282662
## tire_brand_visit_entry_page_not_tire_brand_visit  0.228568272 -0.051682658
## offers_section_not_financing_visit               -0.241943806 -0.636779552
## x90_day_buy_try_visit                             0.028317931 -0.131857741
## cfna_application_visit                           -0.131206583  0.095085517
## credit_card_vendor_click_through_e61             -0.032244794  0.022003568
## visited_reward_center                            -0.001127432  0.084095344
## page_contains_racing_visit                       -0.021891156  0.242121297
## catalog_search_complete_e10                      -0.078558733  0.132098901
## tire_detail_view_e20                              0.002555830  0.000417205
## locate_tire_visits                               -0.221510557  0.037930205
## store_dealer_visits                              -0.217133886  0.159781482
## store_search_experiment                          -0.401332622  0.175007721
## individual_store_detail_e40                       0.346361003  0.089102359
## catalog_sort_change_e110                         -0.091267781  0.234757258
## tire_comparison_e24                               0.003588618  0.014268697
## tire_fit_interaction_e46                          0.013571377 -0.113794613
## store_detail_click_e38                           -0.017994613  0.123538026
## content_show_hide_click_e35                       0.022125028  0.002274149
## purchase_at_retailer_e48                         -0.033686185 -0.018086491
## tab_navigation_click_e17                          0.278782036  0.113237541
## csc_interaction                                  -0.077203799  0.187341516
## tdp_interaction                                   0.250791736  0.100469967
## stepss                                           -0.221915492  0.091455557
## manual_e4_tdp                                    -0.060041949  0.081585527
## manual_e4_csc                                    -0.164945792 -0.029697817
## manual_e4                                        -0.220782210  0.038474827
## video_views_e65                                  -0.050597156 -0.117797015
## promo                                             0.000000000  0.000000000
##                                                            PC9          PC10
## csc_catalog_not_entry                             0.1898306097 -0.0920412496
## tdp_but_tdp_not_entry                            -0.3218023315 -0.0599460012
## tire_brand_visit_entry_page_not_tire_brand_visit -0.2320257594 -0.1960198891
## offers_section_not_financing_visit               -0.1920597245 -0.1390140340
## x90_day_buy_try_visit                            -0.1219135276 -0.1643922105
## cfna_application_visit                           -0.0673079918  0.0856702415
## credit_card_vendor_click_through_e61             -0.0409938670  0.0165181842
## visited_reward_center                            -0.0353439686  0.1632885242
## page_contains_racing_visit                       -0.0825056824 -0.5941349844
## catalog_search_complete_e10                       0.0777783802  0.0426024443
## tire_detail_view_e20                             -0.0008635656  0.0014871680
## locate_tire_visits                               -0.4002598291  0.0435569279
## store_dealer_visits                               0.3715982704  0.0982599601
## store_search_experiment                           0.0401183342  0.1190837507
## individual_store_detail_e40                      -0.0150982881 -0.0102787196
## catalog_sort_change_e110                          0.1225311338 -0.4409084925
## tire_comparison_e24                              -0.0303113366 -0.0212720383
## tire_fit_interaction_e46                         -0.0584322414 -0.1838400812
## store_detail_click_e38                           -0.0789052984 -0.0009334067
## content_show_hide_click_e35                      -0.0654725748  0.1161659671
## purchase_at_retailer_e48                         -0.0566838153  0.0707369656
## tab_navigation_click_e17                         -0.1670977241  0.1367101119
## csc_interaction                                   0.0675461863 -0.3492590926
## tdp_interaction                                  -0.1483657755  0.2101083732
## stepss                                           -0.2737741113  0.0826786868
## manual_e4_tdp                                    -0.1208904179  0.1423954267
## manual_e4_csc                                    -0.3100375129 -0.1025984818
## manual_e4                                        -0.4013505773  0.0438757459
## video_views_e65                                   0.0056659129 -0.1292427306
## promo                                             0.0000000000  0.0000000000
##                                                          PC11         PC12
## csc_catalog_not_entry                             0.477568039 -0.140503800
## tdp_but_tdp_not_entry                             0.031837590  0.525028639
## tire_brand_visit_entry_page_not_tire_brand_visit -0.605830699 -0.252364213
## offers_section_not_financing_visit               -0.070553284  0.110106907
## x90_day_buy_try_visit                            -0.097670826  0.035794404
## cfna_application_visit                            0.008547897 -0.052464250
## credit_card_vendor_click_through_e61              0.001315145 -0.041243091
## visited_reward_center                            -0.026896508 -0.009219088
## page_contains_racing_visit                        0.010650558 -0.298080458
## catalog_search_complete_e10                      -0.200388578 -0.012902238
## tire_detail_view_e20                             -0.001239053  0.004067315
## locate_tire_visits                                0.163384546 -0.031854446
## store_dealer_visits                              -0.108931284 -0.019424115
## store_search_experiment                           0.039462094 -0.073925937
## individual_store_detail_e40                       0.015386381  0.014398720
## catalog_sort_change_e110                          0.208425392  0.098684867
## tire_comparison_e24                               0.037150412  0.005760992
## tire_fit_interaction_e46                          0.058866966  0.173086746
## store_detail_click_e38                            0.146981265 -0.079979520
## content_show_hide_click_e35                       0.045521494 -0.054824643
## purchase_at_retailer_e48                          0.156007056  0.044688434
## tab_navigation_click_e17                          0.093137287 -0.266135753
## csc_interaction                                   0.238720484  0.105703545
## tdp_interaction                                   0.306066978 -0.396938210
## stepss                                           -0.085752988  0.030129999
## manual_e4_tdp                                     0.102366811  0.005338872
## manual_e4_csc                                     0.079178631 -0.035133207
## manual_e4                                         0.161367417 -0.030307823
## video_views_e65                                  -0.046506660 -0.482796273
## promo                                             0.000000000  0.000000000
##                                                          PC13         PC14
## csc_catalog_not_entry                            -0.082284957 -0.015789273
## tdp_but_tdp_not_entry                            -0.053224335  0.264550645
## tire_brand_visit_entry_page_not_tire_brand_visit  0.222597202 -0.008145019
## offers_section_not_financing_visit               -0.087088502  0.004722818
## x90_day_buy_try_visit                             0.300311116 -0.434009942
## cfna_application_visit                           -0.196509423  0.198369148
## credit_card_vendor_click_through_e61             -0.084945346  0.062876036
## visited_reward_center                            -0.143603238  0.068641582
## page_contains_racing_visit                       -0.632960502 -0.141297259
## catalog_search_complete_e10                       0.014531869 -0.035721664
## tire_detail_view_e20                             -0.000290606 -0.004003746
## locate_tire_visits                                0.028756221 -0.049550356
## store_dealer_visits                               0.016388373 -0.015978942
## store_search_experiment                           0.057712611 -0.144618554
## individual_store_detail_e40                      -0.009538569  0.074687540
## catalog_sort_change_e110                          0.358108136  0.015439608
## tire_comparison_e24                               0.046465932 -0.059869619
## tire_fit_interaction_e46                         -0.079493450  0.134758652
## store_detail_click_e38                           -0.045689818  0.201257269
## content_show_hide_click_e35                      -0.085959082 -0.066705344
## purchase_at_retailer_e48                          0.063104029  0.093710942
## tab_navigation_click_e17                          0.032216214 -0.310178593
## csc_interaction                                   0.335484043  0.043620383
## tdp_interaction                                   0.179492722 -0.053800779
## stepss                                            0.029102910  0.204198277
## manual_e4_tdp                                    -0.080647899  0.061967748
## manual_e4_csc                                     0.121086732 -0.112185813
## manual_e4                                         0.027676607 -0.049786938
## video_views_e65                                   0.229111630  0.636794206
## promo                                             0.000000000  0.000000000
##                                                          PC15         PC16
## csc_catalog_not_entry                             0.172869705  0.057247596
## tdp_but_tdp_not_entry                            -0.262098829  0.047544490
## tire_brand_visit_entry_page_not_tire_brand_visit  0.281904609 -0.205678668
## offers_section_not_financing_visit                0.042811433  0.060304982
## x90_day_buy_try_visit                            -0.273416820  0.524129128
## cfna_application_visit                            0.222864284  0.165867664
## credit_card_vendor_click_through_e61              0.100974187  0.021176234
## visited_reward_center                             0.374220466  0.311896933
## page_contains_racing_visit                       -0.091367162 -0.003180663
## catalog_search_complete_e10                      -0.049169109 -0.042505773
## tire_detail_view_e20                             -0.002211550 -0.004095682
## locate_tire_visits                               -0.063203126 -0.124848793
## store_dealer_visits                              -0.053281856  0.134537103
## store_search_experiment                          -0.167249306 -0.101535482
## individual_store_detail_e40                      -0.048505360 -0.029460092
## catalog_sort_change_e110                          0.222653191 -0.065805574
## tire_comparison_e24                               0.067766256  0.054687672
## tire_fit_interaction_e46                          0.142098079  0.027750956
## store_detail_click_e38                           -0.159374737  0.333300941
## content_show_hide_click_e35                      -0.002134604 -0.136396869
## purchase_at_retailer_e48                          0.189091632 -0.438655598
## tab_navigation_click_e17                          0.264388807  0.114471089
## csc_interaction                                   0.222936808  0.068214010
## tdp_interaction                                  -0.086349559  0.024314227
## stepss                                            0.362994734  0.347666070
## manual_e4_tdp                                     0.047238524  0.024534923
## manual_e4_csc                                    -0.098305205 -0.108438931
## manual_e4                                        -0.064627821 -0.119649040
## video_views_e65                                  -0.295146059  0.075971935
## promo                                             0.000000000  0.000000000
##                                                           PC17         PC18
## csc_catalog_not_entry                             9.238513e-03  0.073645036
## tdp_but_tdp_not_entry                            -4.081842e-02  0.044113842
## tire_brand_visit_entry_page_not_tire_brand_visit  1.033683e-01  0.009258829
## offers_section_not_financing_visit                8.794391e-02 -0.060413205
## x90_day_buy_try_visit                            -1.268813e-01 -0.188016880
## cfna_application_visit                            3.299039e-01  0.007352043
## credit_card_vendor_click_through_e61              8.618110e-02  0.026268632
## visited_reward_center                            -3.998242e-01  0.533135146
## page_contains_racing_visit                       -1.555244e-01 -0.104896046
## catalog_search_complete_e10                      -1.370956e-04 -0.023485056
## tire_detail_view_e20                             -2.702889e-05 -0.001382613
## locate_tire_visits                                1.077998e-01  0.126154014
## store_dealer_visits                              -4.927013e-02 -0.025445899
## store_search_experiment                          -2.571759e-02  0.017676046
## individual_store_detail_e40                       7.109623e-03  0.031882982
## catalog_sort_change_e110                          2.324466e-01  0.217186043
## tire_comparison_e24                              -1.732475e-03 -0.102460814
## tire_fit_interaction_e46                          2.488526e-02 -0.028753795
## store_detail_click_e38                            5.104967e-02 -0.202780798
## content_show_hide_click_e35                       1.504270e-01 -0.072483680
## purchase_at_retailer_e48                         -5.095313e-01 -0.478099229
## tab_navigation_click_e17                          1.574940e-01 -0.107662423
## csc_interaction                                  -2.044641e-02 -0.154613491
## tdp_interaction                                  -1.070388e-01 -0.002255269
## stepss                                           -2.020221e-01 -0.299491797
## manual_e4_tdp                                     3.904121e-01 -0.223365668
## manual_e4_csc                                    -2.538832e-01  0.335606041
## manual_e4                                         1.119379e-01  0.127463835
## video_views_e65                                  -4.741280e-02  0.041282368
## promo                                             0.000000e+00  0.000000000
##                                                          PC19          PC20
## csc_catalog_not_entry                            -0.002937278 -0.0498787856
## tdp_but_tdp_not_entry                            -0.128922721  0.1018123395
## tire_brand_visit_entry_page_not_tire_brand_visit  0.059067040 -0.1462074375
## offers_section_not_financing_visit                0.009247228 -0.0147557204
## x90_day_buy_try_visit                            -0.411351464  0.0101049804
## cfna_application_visit                           -0.300021375  0.4162295924
## credit_card_vendor_click_through_e61             -0.049653422  0.2652431453
## visited_reward_center                            -0.220894740 -0.1952331874
## page_contains_racing_visit                       -0.069068005 -0.0531098463
## catalog_search_complete_e10                       0.019435086  0.0102041354
## tire_detail_view_e20                              0.002496716  0.0003161587
## locate_tire_visits                                0.019498796 -0.0852471924
## store_dealer_visits                               0.076823689  0.1022176590
## store_search_experiment                          -0.173323396 -0.0565165608
## individual_store_detail_e40                      -0.016243416 -0.0154611924
## catalog_sort_change_e110                         -0.225764648 -0.1646349279
## tire_comparison_e24                               0.195550344  0.3048089093
## tire_fit_interaction_e46                         -0.006125022 -0.0132950943
## store_detail_click_e38                            0.439224819 -0.1694318811
## content_show_hide_click_e35                      -0.314153779  0.3065708444
## purchase_at_retailer_e48                         -0.258287144  0.0036821198
## tab_navigation_click_e17                          0.090869868  0.2370201687
## csc_interaction                                   0.166447271  0.1678190112
## tdp_interaction                                  -0.044720755 -0.1283703132
## stepss                                            0.083104536 -0.0545852217
## manual_e4_tdp                                    -0.142529035 -0.4334577776
## manual_e4_csc                                     0.277011661  0.3175749416
## manual_e4                                         0.010916229 -0.0962794473
## video_views_e65                                  -0.194843265  0.1222846606
## promo                                             0.000000000  0.0000000000
##                                                           PC21          PC22
## csc_catalog_not_entry                             1.289371e-02 -0.0067245268
## tdp_but_tdp_not_entry                             2.812424e-02 -0.0341992555
## tire_brand_visit_entry_page_not_tire_brand_visit -2.099575e-01  0.1270271628
## offers_section_not_financing_visit               -2.804765e-02  0.0481920506
## x90_day_buy_try_visit                            -1.440165e-01 -0.0325184567
## cfna_application_visit                           -2.753727e-01  0.3835892858
## credit_card_vendor_click_through_e61             -1.045569e-01 -0.0960890860
## visited_reward_center                            -2.694304e-01 -0.1475799390
## page_contains_racing_visit                        3.936451e-02 -0.0279324961
## catalog_search_complete_e10                      -2.770966e-02  0.0111166573
## tire_detail_view_e20                              3.943555e-05  0.0008802106
## locate_tire_visits                               -7.511520e-02 -0.2091865623
## store_dealer_visits                              -3.823559e-02 -0.2933106548
## store_search_experiment                          -2.715239e-03  0.2361646135
## individual_store_detail_e40                       2.569171e-03 -0.0508124298
## catalog_sort_change_e110                          1.557868e-01  0.0912149858
## tire_comparison_e24                              -3.566339e-01 -0.1228548260
## tire_fit_interaction_e46                          2.632273e-02  0.0517426119
## store_detail_click_e38                           -3.151640e-01  0.2143752002
## content_show_hide_click_e35                       1.897750e-01  0.0125843869
## purchase_at_retailer_e48                         -2.559663e-01 -0.0568982881
## tab_navigation_click_e17                          1.810424e-01 -0.3614595175
## csc_interaction                                  -1.811390e-01 -0.0839116932
## tdp_interaction                                   1.809846e-02  0.4305389121
## stepss                                            5.538033e-01  0.0836601613
## manual_e4_tdp                                    -1.618833e-01 -0.2711985373
## manual_e4_csc                                     5.018360e-02  0.0827242448
## manual_e4                                        -7.315135e-02 -0.2061597280
## video_views_e65                                   7.029717e-02 -0.2892599357
## promo                                             0.000000e+00  0.0000000000
##                                                           PC23          PC24
## csc_catalog_not_entry                             0.0061265352 -0.0051296633
## tdp_but_tdp_not_entry                             0.0330567347  0.0032055622
## tire_brand_visit_entry_page_not_tire_brand_visit -0.0813378443 -0.0271641507
## offers_section_not_financing_visit               -0.0264074648  0.0045608168
## x90_day_buy_try_visit                            -0.0539298193  0.0822064770
## cfna_application_visit                           -0.0132967696  0.0247494743
## credit_card_vendor_click_through_e61             -0.0404072545  0.6266269124
## visited_reward_center                             0.0498837644 -0.1201530735
## page_contains_racing_visit                       -0.0350711531 -0.0545120998
## catalog_search_complete_e10                      -0.0106759284 -0.0010186237
## tire_detail_view_e20                             -0.0005417172  0.0002150350
## locate_tire_visits                               -0.1381514099  0.0251883899
## store_dealer_visits                              -0.5396476634  0.0698743209
## store_search_experiment                           0.5563314901 -0.0345728613
## individual_store_detail_e40                       0.0342130729 -0.0030510866
## catalog_sort_change_e110                         -0.1057775862  0.2319930940
## tire_comparison_e24                               0.0598864590 -0.3360552281
## tire_fit_interaction_e46                         -0.0099798979  0.0039786474
## store_detail_click_e38                           -0.0319048656  0.2146339636
## content_show_hide_click_e35                      -0.3078535613 -0.3601515343
## purchase_at_retailer_e48                         -0.0351831163  0.2102083163
## tab_navigation_click_e17                          0.3157920264  0.1881297884
## csc_interaction                                   0.0755097588 -0.3283505800
## tdp_interaction                                  -0.2978651094 -0.0727047757
## stepss                                           -0.0484079862 -0.0526405266
## manual_e4_tdp                                     0.0250977049 -0.1825333410
## manual_e4_csc                                    -0.0442023030  0.0006314117
## manual_e4                                        -0.1688379021  0.0387420337
## video_views_e65                                   0.1501249261 -0.0471621693
## promo                                             0.0000000000  0.0000000000
##                                                           PC25          PC26
## csc_catalog_not_entry                             0.0152959403 -1.801366e-03
## tdp_but_tdp_not_entry                            -0.0425860691 -4.850864e-03
## tire_brand_visit_entry_page_not_tire_brand_visit  0.0730433444  1.902295e-02
## offers_section_not_financing_visit                0.0022833321  4.018740e-03
## x90_day_buy_try_visit                             0.0065588911 -4.173812e-02
## cfna_application_visit                           -0.1933276395 -2.938926e-01
## credit_card_vendor_click_through_e61             -0.0848763982  3.837965e-01
## visited_reward_center                             0.1719890107  4.987957e-02
## page_contains_racing_visit                       -0.0733597976  9.948905e-03
## catalog_search_complete_e10                      -0.0012281693 -8.773193e-04
## tire_detail_view_e20                             -0.0000254756 -2.885287e-05
## locate_tire_visits                               -0.0599791851  2.182202e-01
## store_dealer_visits                              -0.0914287281 -1.604386e-01
## store_search_experiment                           0.0793609449  1.392261e-01
## individual_store_detail_e40                      -0.0220321610 -6.070238e-03
## catalog_sort_change_e110                          0.1766001653 -3.162406e-03
## tire_comparison_e24                              -0.1096154284  3.692335e-01
## tire_fit_interaction_e46                         -0.0016267104  1.280110e-02
## store_detail_click_e38                            0.5527283875 -1.725606e-02
## content_show_hide_click_e35                       0.6290106482  1.620268e-01
## purchase_at_retailer_e48                          0.0993539533 -1.557461e-01
## tab_navigation_click_e17                          0.1132414686 -1.894404e-01
## csc_interaction                                  -0.1078772766  3.993197e-02
## tdp_interaction                                  -0.3027455733  1.125616e-01
## stepss                                           -0.0854839090  1.008041e-01
## manual_e4_tdp                                    -0.0871173212 -3.530767e-01
## manual_e4_csc                                     0.0689840767 -5.319038e-01
## manual_e4                                        -0.0640345866  1.146455e-01
## video_views_e65                                   0.0131674689 -2.132241e-02
## promo                                             0.0000000000  0.000000e+00
##                                                           PC27          PC28
## csc_catalog_not_entry                            -0.0060291636  0.0041708963
## tdp_but_tdp_not_entry                            -0.0069934109 -0.0196966116
## tire_brand_visit_entry_page_not_tire_brand_visit  0.0408163118  0.0539074781
## offers_section_not_financing_visit                0.0053664631 -0.0041608940
## x90_day_buy_try_visit                             0.0074837540  0.0037507105
## cfna_application_visit                           -0.2335147708  0.0793738073
## credit_card_vendor_click_through_e61              0.5546683251 -0.0121037068
## visited_reward_center                             0.0528729779  0.0325696453
## page_contains_racing_visit                        0.0068295072 -0.0198631244
## catalog_search_complete_e10                       0.0056693679  0.0034239305
## tire_detail_view_e20                             -0.0003145407 -0.0004743756
## locate_tire_visits                               -0.1074197812  0.0037041093
## store_dealer_visits                              -0.0748989149 -0.0070050263
## store_search_experiment                           0.0763453632  0.0029958363
## individual_store_detail_e40                      -0.0101335728  0.0022556620
## catalog_sort_change_e110                         -0.1659234086 -0.3462872121
## tire_comparison_e24                              -0.1480769098 -0.6235922859
## tire_fit_interaction_e46                          0.0056118852 -0.0056920457
## store_detail_click_e38                           -0.0759177795  0.0176152326
## content_show_hide_click_e35                       0.1961024841  0.0525376291
## purchase_at_retailer_e48                         -0.1191695157 -0.0576771109
## tab_navigation_click_e17                         -0.1940721072  0.0377695242
## csc_interaction                                   0.2691216959  0.5039093224
## tdp_interaction                                   0.1221771987 -0.0397715586
## stepss                                            0.0079546619 -0.1141072789
## manual_e4_tdp                                     0.4312108750 -0.2428451232
## manual_e4_csc                                     0.2917348730 -0.2189118394
## manual_e4                                        -0.3231646347  0.3090098185
## video_views_e65                                  -0.0393053528 -0.0334379473
## promo                                             0.0000000000  0.0000000000
##                                                           PC29
## csc_catalog_not_entry                            -1.211650e-03
## tdp_but_tdp_not_entry                             2.260424e-03
## tire_brand_visit_entry_page_not_tire_brand_visit -6.157358e-03
## offers_section_not_financing_visit                9.741002e-04
## x90_day_buy_try_visit                            -8.892842e-03
## cfna_application_visit                           -7.887443e-02
## credit_card_vendor_click_through_e61              1.144778e-01
## visited_reward_center                             5.122827e-03
## page_contains_racing_visit                        5.302642e-03
## catalog_search_complete_e10                      -1.223509e-04
## tire_detail_view_e20                              4.988407e-05
## locate_tire_visits                               -7.403698e-01
## store_dealer_visits                              -3.373864e-02
## store_search_experiment                           3.381743e-02
## individual_store_detail_e40                      -2.179425e-03
## catalog_sort_change_e110                          4.405624e-02
## tire_comparison_e24                               1.535661e-01
## tire_fit_interaction_e46                          2.290660e-03
## store_detail_click_e38                           -1.834349e-02
## content_show_hide_click_e35                       3.047474e-02
## purchase_at_retailer_e48                         -1.742456e-02
## tab_navigation_click_e17                         -4.554880e-02
## csc_interaction                                  -5.908875e-02
## tdp_interaction                                   2.853427e-02
## stepss                                            3.256531e-02
## manual_e4_tdp                                     9.356168e-02
## manual_e4_csc                                     5.906951e-02
## manual_e4                                         6.190811e-01
## video_views_e65                                   4.219950e-03
## promo                                             0.000000e+00
```

```r
DMM_1$rotation[,2] %>% round(2)
```

```
##                            csc_catalog_not_entry 
##                                             0.33 
##                            tdp_but_tdp_not_entry 
##                                             0.15 
## tire_brand_visit_entry_page_not_tire_brand_visit 
##                                             0.03 
##               offers_section_not_financing_visit 
##                                             0.04 
##                            x90_day_buy_try_visit 
##                                             0.01 
##                           cfna_application_visit 
##                                             0.00 
##             credit_card_vendor_click_through_e61 
##                                             0.00 
##                            visited_reward_center 
##                                             0.00 
##                       page_contains_racing_visit 
##                                             0.00 
##                      catalog_search_complete_e10 
##                                             0.90 
##                             tire_detail_view_e20 
##                                             0.13 
##                               locate_tire_visits 
##                                             0.05 
##                              store_dealer_visits 
##                                             0.05 
##                          store_search_experiment 
##                                             0.09 
##                      individual_store_detail_e40 
##                                             0.06 
##                         catalog_sort_change_e110 
##                                             0.00 
##                              tire_comparison_e24 
##                                             0.00 
##                         tire_fit_interaction_e46 
##                                             0.12 
##                           store_detail_click_e38 
##                                             0.01 
##                      content_show_hide_click_e35 
##                                             0.01 
##                         purchase_at_retailer_e48 
##                                             0.01 
##                         tab_navigation_click_e17 
##                                             0.04 
##                                  csc_interaction 
##                                             0.01 
##                                  tdp_interaction 
##                                             0.03 
##                                           stepss 
##                                             0.06 
##                                    manual_e4_tdp 
##                                             0.02 
##                                    manual_e4_csc 
##                                             0.04 
##                                        manual_e4 
##                                             0.05 
##                                  video_views_e65 
##                                             0.01 
##                                            promo 
##                                             0.00
```

# STEP 2 FST PCA PREDICTION


```r
set.seed(123)
prc_index <- createDataPartition(prc$revenue, p = 0.70, list = FALSE)
train <- prc[ prc_index, ]
test <- prc[-prc_index, ]
tunegrid <- expand.grid(.mtry=1:50)


prcfit <- train(revenue ~ .,
             data = train, 
             method = "rf",
             metric = "RMSE",
             trControl = trainControl(method = "boot"),
             tuneGrid = tunegrid)

prcfit$results
```

```
##    mtry      RMSE  Rsquared      MAE   RMSESD RsquaredSD    MAESD
## 1     1 1066238.6 0.1809250 846818.4 221181.8  0.2018294 192407.1
## 2     2 1048971.5 0.1336456 828861.2 223463.1  0.1792352 197063.3
## 3     3 1044666.5 0.1176574 820642.1 217224.9  0.1570167 190103.6
## 4     4 1030165.6 0.1580477 810552.7 226431.6  0.1930382 193497.2
## 5     5 1032265.1 0.1494778 813209.7 226837.3  0.2125162 197898.6
## 6     6 1011352.2 0.1365856 798109.7 237329.1  0.1763463 197979.1
## 7     7 1005046.9 0.1623259 791618.7 240050.4  0.1996643 200088.1
## 8     8 1007369.2 0.1563456 793846.9 248106.1  0.2045456 206955.9
## 9     9  994286.9 0.1852186 782884.6 253777.7  0.2145714 212422.9
## 10   10  998463.2 0.1592814 788459.5 252980.9  0.2043910 211122.6
## 11   11  988714.6 0.1906909 778173.3 256845.1  0.2024993 211948.1
## 12   12  981749.0 0.1817142 778617.8 255940.9  0.1965362 205151.3
## 13   13  971852.9 0.1812250 769186.0 267958.6  0.1704121 223116.7
## 14   14  969989.4 0.1925547 766150.7 268730.6  0.2015034 221145.5
## 15   15  973904.9 0.1797182 769391.2 268679.4  0.1824367 221333.3
## 16   16  964693.2 0.2149436 762549.6 270591.9  0.2077486 222064.1
## 17   17  962357.8 0.2030619 761579.7 273585.5  0.1895634 221420.7
## 18   18  955362.4 0.2111874 753760.3 274609.2  0.1994147 224172.8
## 19   19  953327.9 0.2239047 753813.5 282286.8  0.2052626 223962.2
## 20   20  949102.1 0.2138467 749046.6 282484.6  0.1826561 228783.4
## 21   21  949672.6 0.2225024 749869.6 279223.8  0.2110505 222037.7
## 22   22  940217.6 0.2354650 746188.1 284306.4  0.2123431 229337.6
## 23   23  948708.2 0.2307882 748459.4 283781.4  0.2240780 226099.2
## 24   24  944400.4 0.2223644 745508.7 286099.7  0.2082096 227492.9
## 25   25  948510.2 0.2182844 751986.1 287158.1  0.1964873 227433.1
## 26   26  939982.8 0.2325568 743661.1 286768.8  0.2210984 231132.5
## 27   27  939442.0 0.2135811 740880.1 288099.4  0.2041236 226685.2
## 28   28  943766.3 0.2287511 746130.1 292567.7  0.2131954 229584.4
## 29   29  941506.3 0.2244620 748013.4 289669.9  0.2173066 225448.8
## 30   30  941493.4 0.2388329 746082.5 291203.1  0.2355351 229500.5
## 31   31  935643.8 0.2178737 742412.3 284727.1  0.2007340 225216.1
## 32   32  943148.7 0.2315746 749972.0 289917.3  0.2210487 227775.5
## 33   33  939889.0 0.2187472 745762.2 287171.9  0.2094089 225736.1
## 34   34  941490.0 0.2403309 747198.5 287962.2  0.2272351 224035.6
## 35   35  938357.2 0.2268642 744179.1 286922.8  0.2226910 224160.6
## 36   36  938932.6 0.2305654 747578.9 285954.3  0.2192829 223771.5
## 37   37  945869.7 0.2172772 750969.0 292190.3  0.1998881 227917.7
## 38   38  938218.7 0.2294263 746859.8 291297.4  0.2169581 225226.7
## 39   39  939414.6 0.2239125 744330.0 286329.9  0.2075972 221932.9
## 40   40  939040.1 0.2216397 746980.5 291409.5  0.1999071 229553.5
## 41   41  941978.4 0.2233233 749157.6 292954.8  0.1995214 230658.9
## 42   42  938108.0 0.2332135 747251.8 294372.2  0.2052218 228957.2
## 43   43  941431.3 0.2253180 746647.5 291264.0  0.2107735 228713.4
## 44   44  940601.5 0.2121776 745730.9 289506.8  0.2044763 226268.9
## 45   45  941708.0 0.2418369 750005.9 286084.6  0.2184963 225403.5
## 46   46  943575.8 0.2181530 748697.9 292256.8  0.2000984 230338.5
## 47   47  942631.9 0.2215105 746097.0 289262.0  0.2046593 227851.6
## 48   48  939078.1 0.2221648 747480.8 287742.5  0.2200216 223597.3
## 49   49  938086.9 0.2369081 745686.4 291778.2  0.2265889 229767.7
## 50   50  943102.5 0.2176802 748468.1 292011.4  0.2064627 229913.6
```

```r
summary(prcfit$results)
```

```
##       mtry            RMSE            Rsquared           MAE        
##  Min.   : 1.00   Min.   : 935644   Min.   :0.1177   Min.   :740880  
##  1st Qu.:13.25   1st Qu.: 940314   1st Qu.:0.1866   1st Qu.:746303  
##  Median :25.50   Median : 944083   Median :0.2182   Median :749102  
##  Mean   :25.50   Mean   : 963158   Mean   :0.2062   Mean   :762510  
##  3rd Qu.:37.75   3rd Qu.: 973392   3rd Qu.:0.2265   3rd Qu.:769340  
##  Max.   :50.00   Max.   :1066239   Max.   :0.2418   Max.   :846818  
##      RMSESD         RsquaredSD         MAESD       
##  Min.   :217225   Min.   :0.1570   Min.   :190104  
##  1st Qu.:268139   1st Qu.:0.1997   1st Qu.:221192  
##  Median :286019   Median :0.2049   Median :224694  
##  Mean   :274182   Mean   :0.2049   Mean   :220284  
##  3rd Qu.:289629   3rd Qu.:0.2142   3rd Qu.:227901  
##  Max.   :294372   Max.   :0.2355   Max.   :231133
```

```r
  plot(varImp(prcfit))
```

<img src="{{< blogdown/postref >}}index_files/figure-html/unnamed-chunk-2-1.png" width="672" />

```r
#### Forecast & MAPE

forecast_test <- data.frame(actual=test, predicted=predict(prcfit,test))
  
    print(forecast_test)
```

```
##    actual.revenue actual.PC1 actual.PC2   actual.PC3 actual.PC4  actual.PC5
## 2        13094660   1420.960  24900.503  2118.975553   200.8313  1386.71013
## 7        13479647  -5800.732  15292.484     5.415213   374.3350 -1248.73445
## 8        10953743  -8310.127  14639.314  1377.718809  1201.4357  -954.81435
## 15       10852992 -18065.875  -9482.812  -758.479644  -144.4223  -112.08487
## 17       11873364 -16901.886  -6675.536 -1093.873442  -551.0028    80.69234
## 19       12675352 -16513.274 -16050.949   516.820916   647.1531   317.23156
## 23       12395256   5453.030 -14947.858   669.034443   566.7766   337.39506
## 28       11626890  35225.294  -5708.971  -723.981929  -178.1582  -220.97383
##     actual.PC6 actual.PC7 actual.PC8 actual.PC9 actual.PC10 actual.PC11
## 2   -298.44173   311.0976  202.87859 -292.17585    14.92794  -319.46421
## 7  -1932.88397   668.1014  120.50743  212.71714   -27.13083   -67.15944
## 8    972.82021  -216.8341 -194.23781  249.19679  -281.40722   -95.32987
## 15   397.08932   397.1331 -179.23071  130.48752   109.78966  -281.64422
## 17   158.67146   150.1205  201.95318  -14.28554  -110.04225   165.82559
## 19  -356.18549  -256.8920   39.82324   23.38595    14.35839   142.90161
## 23   176.07847  -219.4394  -23.64777  -10.85603   -68.86960   -16.82513
## 28   -67.59124   285.0257  313.46940   81.88158    36.77236    67.10601
##    actual.PC12 actual.PC13 actual.PC14 actual.PC15 actual.PC16 actual.PC17
## 2    109.78161    4.272100   36.715738   -2.983480    23.94027   -10.05481
## 7     54.76516  -67.957699   12.616808   -3.015262    12.80753   -40.05349
## 8    109.03593  -36.117526 -175.778440    2.573713    33.24957    27.42647
## 15    -4.03100   -3.625008  -45.460998 -100.141266    49.70115   -50.50822
## 17    49.14614  190.671582    6.079757  -22.018809    88.11886   -19.74844
## 19   270.99949  -18.601544    3.526309  -34.078645   -35.20062    12.73876
## 23  -246.44527 -113.093073  -41.181779  124.324770    61.11645   -69.95636
## 28   -15.68256  174.749121   39.959319  -13.898424   -55.53323   -29.79533
##     actual.PC18 actual.PC19 actual.PC20 actual.PC21 actual.PC22 actual.PC23
## 2   -2.89123157  -14.075267   11.607289 -12.8595447    3.868498   -7.590283
## 7   -8.75660441    3.883838  -25.178200   7.5800050   -5.470787    3.650154
## 8  -21.19085848  -37.821929    6.373633  19.6628078    5.857125    7.669370
## 15  14.73263087    2.603776   18.744595  10.2852650   21.078493  -11.445045
## 17 -55.23180957    6.561485   45.250989  21.1224234  -27.003502   -1.080551
## 19  16.88972551   27.192715    1.555680   5.1117303    5.184595    1.684363
## 23 -20.09711279   13.268679  -13.837145   0.8569894  -11.650488  -17.975818
## 28   0.04751484  -44.305553   -5.519228  36.5965840   11.965808   -2.514031
##    actual.PC24 actual.PC25 actual.PC26 actual.PC27 actual.PC28   actual.PC29
## 2   -0.9103656  -9.6691999 -1.25215458  -1.0625887 -0.34860996 -5.473400e-13
## 7   -6.5277552   2.8923571  0.91829993  -0.4270433  0.06668576 -1.443290e-14
## 8   -1.1103667  -3.1822275  0.34660418   0.2523004 -0.53486275 -5.856426e-13
## 15  10.0116019  10.3886036 -1.62929157   0.5579778  0.95677389  1.887379e-14
## 17   1.7992591   0.9137054 -0.30667123  -1.2415394 -0.40618047 -4.602985e-13
## 19  19.7248695  -4.9113829  2.79265979  -1.0666537  0.71478268  2.797762e-13
## 23  -1.3190813  -5.7069882  2.31711025   0.6715014  0.58766451  4.783951e-13
## 28   5.2345676 -13.6547404 -0.00805663   1.8152165 -0.06144562  5.997980e-13
##    predicted
## 2   12989680
## 7   12871443
## 8   13049316
## 15  11508756
## 17  11437880
## 19  11850572
## 23  11765081
## 28  11488377
```

```r
RMSE(forecast_test$actual.revenue, forecast_test$predicted) 
```

```
## [1] 900597.1
```

```r
MAPE <- mean(abs((forecast_test$actual.revenue-forecast_test$predicted)/forecast_test$actual.revenue)) * 100
    MAPE
```

```
## [1] 5.867136
```

```r
summary(test)
```

```
##     revenue              PC1              PC2                PC3         
##  Min.   :10852992   Min.   :-18066   Min.   :-16050.9   Min.   :-1093.9  
##  1st Qu.:11458603   1st Qu.:-16610   1st Qu.:-10849.1   1st Qu.: -732.6  
##  Median :12134310   Median : -7055   Median : -6192.3   Median :  261.1  
##  Mean   :12118988   Mean   : -2937   Mean   :   245.8   Mean   :  264.0  
##  3rd Qu.:12780179   3rd Qu.:  2429   3rd Qu.: 14802.6   3rd Qu.:  846.2  
##  Max.   :13479647   Max.   : 35225   Max.   : 24900.5   Max.   : 2119.0  
##       PC4              PC5                PC6                PC7        
##  Min.   :-551.0   Min.   :-1248.73   Min.   :-1932.88   Min.   :-256.9  
##  1st Qu.:-152.9   1st Qu.: -404.43   1st Qu.: -312.88   1st Qu.:-217.5  
##  Median : 287.6   Median :  -15.70   Median :   45.54   Median : 217.6  
##  Mean   : 264.6   Mean   :  -51.82   Mean   : -118.81   Mean   : 139.8  
##  3rd Qu.: 586.9   3rd Qu.:  322.27   3rd Qu.:  231.33   3rd Qu.: 332.6  
##  Max.   :1201.4   Max.   : 1386.71   Max.   :  972.82   Max.   : 668.1  
##       PC8               PC9               PC10               PC11        
##  Min.   :-194.24   Min.   :-292.18   Min.   :-281.407   Min.   :-319.46  
##  1st Qu.: -62.54   1st Qu.: -11.71   1st Qu.: -79.163   1st Qu.:-141.91  
##  Median :  80.17   Median :  52.63   Median :  -6.386   Median : -41.99  
##  Mean   :  60.19   Mean   :  47.54   Mean   : -38.950   Mean   : -50.57  
##  3rd Qu.: 202.18   3rd Qu.: 151.04   3rd Qu.:  20.389   3rd Qu.:  86.05  
##  Max.   : 313.47   Max.   : 249.20   Max.   : 109.790   Max.   : 165.83  
##       PC12               PC13              PC14               PC15         
##  Min.   :-246.445   Min.   :-113.09   Min.   :-175.778   Min.   :-100.141  
##  1st Qu.:  -6.944   1st Qu.: -44.08   1st Qu.: -42.252   1st Qu.: -25.034  
##  Median :  51.956   Median : -11.11   Median :   4.803   Median :  -8.457  
##  Mean   :  40.946   Mean   :  16.29   Mean   : -20.440   Mean   :  -6.155  
##  3rd Qu.: 109.222   3rd Qu.:  46.89   3rd Qu.:  18.642   3rd Qu.:  -1.594  
##  Max.   : 270.999   Max.   : 190.67   Max.   :  39.959   Max.   : 124.325  
##       PC16               PC17              PC18              PC19        
##  Min.   :-55.5332   Min.   :-69.956   Min.   :-55.232   Min.   :-44.306  
##  1st Qu.:  0.8055   1st Qu.:-42.667   1st Qu.:-20.371   1st Qu.:-20.012  
##  Median : 28.5949   Median :-24.772   Median : -5.824   Median :  3.244  
##  Mean   : 22.2750   Mean   :-22.494   Mean   : -9.562   Mean   : -5.337  
##  3rd Qu.: 52.5550   3rd Qu.: -4.356   3rd Qu.:  3.719   3rd Qu.:  8.238  
##  Max.   : 88.1189   Max.   : 27.426   Max.   : 16.890   Max.   : 27.193  
##       PC20              PC21              PC22               PC23        
##  Min.   :-25.178   Min.   :-12.860   Min.   :-27.0035   Min.   :-17.976  
##  1st Qu.: -7.599   1st Qu.:  4.048   1st Qu.: -7.0157   1st Qu.: -8.554  
##  Median :  3.965   Median :  8.933   Median :  4.5265   Median : -1.797  
##  Mean   :  4.875   Mean   : 11.045   Mean   :  0.4787   Mean   : -3.450  
##  3rd Qu.: 13.392   3rd Qu.: 20.028   3rd Qu.:  7.3843   3rd Qu.:  2.176  
##  Max.   : 45.251   Max.   : 36.597   Max.   : 21.0785   Max.   :  7.669  
##       PC24              PC25              PC26              PC27         
##  Min.   :-6.5278   Min.   :-13.655   Min.   :-1.6293   Min.   :-1.24154  
##  1st Qu.:-1.1625   1st Qu.: -6.698   1st Qu.:-0.5430   1st Qu.:-1.06360  
##  Median : 0.4444   Median : -4.047   Median : 0.1693   Median :-0.08737  
##  Mean   : 3.3628   Mean   : -2.866   Mean   : 0.3973   Mean   :-0.06260  
##  3rd Qu.: 6.4288   3rd Qu.:  1.408   3rd Qu.: 1.2680   3rd Qu.: 0.58636  
##  Max.   :19.7249   Max.   : 10.389   Max.   : 2.7927   Max.   : 1.81522  
##       PC28               PC29           
##  Min.   :-0.53486   Min.   :-5.856e-13  
##  1st Qu.:-0.36300   1st Qu.:-4.821e-13  
##  Median : 0.00262   Median : 2.220e-15  
##  Mean   : 0.12185   Mean   :-2.886e-14  
##  3rd Qu.: 0.61944   3rd Qu.: 3.294e-13  
##  Max.   : 0.95677   Max.   : 5.998e-13
```

```r
forecast_test <- data.frame(actual=test, predicted=predict(prcfit,test))

forecast_test %>% dplyr::select(c(actual.revenue,predicted)) %>% 
  ggplot(aes(x = as.numeric(row.names(forecast_test)))) +
  theme_fivethirtyeight(base_size = 12, base_family = "sans")+
  geom_point(aes(y = actual.revenue), color = "blue") + 
  geom_point(aes(y = predicted), color = 'red') + 
  geom_line(aes(y = predicted), color = 'red')+ 
  geom_line(aes(y = actual.revenue), color = 'blue') +
    labs(title = "Actual Revenue (Blue) vs. Predicted Revenue (Red)")+ 
    expand_limits(x = 0, y = 8000000)+ 
    scale_y_continuous(labels = scales::dollar_format())+
    scale_x_continuous(labels = number_format(accuracy = 1))
```

<img src="{{< blogdown/postref >}}index_files/figure-html/unnamed-chunk-2-2.png" width="672" />

```r
    print(forecast_test)
```

```
##    actual.revenue actual.PC1 actual.PC2   actual.PC3 actual.PC4  actual.PC5
## 2        13094660   1420.960  24900.503  2118.975553   200.8313  1386.71013
## 7        13479647  -5800.732  15292.484     5.415213   374.3350 -1248.73445
## 8        10953743  -8310.127  14639.314  1377.718809  1201.4357  -954.81435
## 15       10852992 -18065.875  -9482.812  -758.479644  -144.4223  -112.08487
## 17       11873364 -16901.886  -6675.536 -1093.873442  -551.0028    80.69234
## 19       12675352 -16513.274 -16050.949   516.820916   647.1531   317.23156
## 23       12395256   5453.030 -14947.858   669.034443   566.7766   337.39506
## 28       11626890  35225.294  -5708.971  -723.981929  -178.1582  -220.97383
##     actual.PC6 actual.PC7 actual.PC8 actual.PC9 actual.PC10 actual.PC11
## 2   -298.44173   311.0976  202.87859 -292.17585    14.92794  -319.46421
## 7  -1932.88397   668.1014  120.50743  212.71714   -27.13083   -67.15944
## 8    972.82021  -216.8341 -194.23781  249.19679  -281.40722   -95.32987
## 15   397.08932   397.1331 -179.23071  130.48752   109.78966  -281.64422
## 17   158.67146   150.1205  201.95318  -14.28554  -110.04225   165.82559
## 19  -356.18549  -256.8920   39.82324   23.38595    14.35839   142.90161
## 23   176.07847  -219.4394  -23.64777  -10.85603   -68.86960   -16.82513
## 28   -67.59124   285.0257  313.46940   81.88158    36.77236    67.10601
##    actual.PC12 actual.PC13 actual.PC14 actual.PC15 actual.PC16 actual.PC17
## 2    109.78161    4.272100   36.715738   -2.983480    23.94027   -10.05481
## 7     54.76516  -67.957699   12.616808   -3.015262    12.80753   -40.05349
## 8    109.03593  -36.117526 -175.778440    2.573713    33.24957    27.42647
## 15    -4.03100   -3.625008  -45.460998 -100.141266    49.70115   -50.50822
## 17    49.14614  190.671582    6.079757  -22.018809    88.11886   -19.74844
## 19   270.99949  -18.601544    3.526309  -34.078645   -35.20062    12.73876
## 23  -246.44527 -113.093073  -41.181779  124.324770    61.11645   -69.95636
## 28   -15.68256  174.749121   39.959319  -13.898424   -55.53323   -29.79533
##     actual.PC18 actual.PC19 actual.PC20 actual.PC21 actual.PC22 actual.PC23
## 2   -2.89123157  -14.075267   11.607289 -12.8595447    3.868498   -7.590283
## 7   -8.75660441    3.883838  -25.178200   7.5800050   -5.470787    3.650154
## 8  -21.19085848  -37.821929    6.373633  19.6628078    5.857125    7.669370
## 15  14.73263087    2.603776   18.744595  10.2852650   21.078493  -11.445045
## 17 -55.23180957    6.561485   45.250989  21.1224234  -27.003502   -1.080551
## 19  16.88972551   27.192715    1.555680   5.1117303    5.184595    1.684363
## 23 -20.09711279   13.268679  -13.837145   0.8569894  -11.650488  -17.975818
## 28   0.04751484  -44.305553   -5.519228  36.5965840   11.965808   -2.514031
##    actual.PC24 actual.PC25 actual.PC26 actual.PC27 actual.PC28   actual.PC29
## 2   -0.9103656  -9.6691999 -1.25215458  -1.0625887 -0.34860996 -5.473400e-13
## 7   -6.5277552   2.8923571  0.91829993  -0.4270433  0.06668576 -1.443290e-14
## 8   -1.1103667  -3.1822275  0.34660418   0.2523004 -0.53486275 -5.856426e-13
## 15  10.0116019  10.3886036 -1.62929157   0.5579778  0.95677389  1.887379e-14
## 17   1.7992591   0.9137054 -0.30667123  -1.2415394 -0.40618047 -4.602985e-13
## 19  19.7248695  -4.9113829  2.79265979  -1.0666537  0.71478268  2.797762e-13
## 23  -1.3190813  -5.7069882  2.31711025   0.6715014  0.58766451  4.783951e-13
## 28   5.2345676 -13.6547404 -0.00805663   1.8152165 -0.06144562  5.997980e-13
##    predicted
## 2   12989680
## 7   12871443
## 8   13049316
## 15  11508756
## 17  11437880
## 19  11850572
## 23  11765081
## 28  11488377
```

```r
  RMSE(forecast_test$actual.revenue, forecast_test$predicted) 
```

```
## [1] 900597.1
```

```r
  MAPE <- mean(abs((forecast_test$actual.revenue-forecast_test$predicted)/forecast_test$actual.revenue)) * 100
    MAPE
```

```
## [1] 5.867136
```
