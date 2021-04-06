Telco Churn
================

## Updating Libraries

``` r
library(rio)
library(formattable)
library(dplyr)
library(tidyverse)
library(readxl)
library(corrplot)
library(stargazer)
library(car)
library(PerformanceAnalytics)
library(tidyr)
library(tm)
library(MASS)
library(AER)
library(ggplot2)
library(lubridate)
library(lattice)
library(lme4)
library(MuMIn)
library("ggridges")
library("hrbrthemes")
library(ggthemes)
library("maps")
library("mapproj")
library(cowplot)
options(scipen = 999)
```

## Importing Data

``` r
df<-read_xlsx("TelcoChurn.xlsx",sheet = "Data")
```

``` r
str(df)
```

    ## tibble [7,043 x 21] (S3: tbl_df/tbl/data.frame)
    ##  $ customerID      : chr [1:7043] "7590-VHVEG" "5575-GNVDE" "3668-QPYBK" "7795-CFOCW" ...
    ##  $ gender          : chr [1:7043] "Female" "Male" "Male" "Male" ...
    ##  $ SeniorCitizen   : num [1:7043] 0 0 0 0 0 0 0 0 0 0 ...
    ##  $ Partner         : chr [1:7043] "Yes" "No" "No" "No" ...
    ##  $ Dependents      : chr [1:7043] "No" "No" "No" "No" ...
    ##  $ tenure          : num [1:7043] 1 34 2 45 2 8 22 10 28 62 ...
    ##  $ PhoneService    : chr [1:7043] "No" "Yes" "Yes" "No" ...
    ##  $ MultipleLines   : chr [1:7043] "No phone service" "No" "No" "No phone service" ...
    ##  $ InternetService : chr [1:7043] "DSL" "DSL" "DSL" "DSL" ...
    ##  $ OnlineSecurity  : chr [1:7043] "No" "Yes" "Yes" "Yes" ...
    ##  $ OnlineBackup    : chr [1:7043] "Yes" "No" "Yes" "No" ...
    ##  $ DeviceProtection: chr [1:7043] "No" "Yes" "No" "Yes" ...
    ##  $ TechSupport     : chr [1:7043] "No" "No" "No" "Yes" ...
    ##  $ StreamingTV     : chr [1:7043] "No" "No" "No" "No" ...
    ##  $ StreamingMovies : chr [1:7043] "No" "No" "No" "No" ...
    ##  $ Contract        : chr [1:7043] "Month-to-month" "One year" "Month-to-month" "One year" ...
    ##  $ PaperlessBilling: chr [1:7043] "Yes" "No" "Yes" "No" ...
    ##  $ PaymentMethod   : chr [1:7043] "Electronic check" "Mailed check" "Mailed check" "Bank transfer (automatic)" ...
    ##  $ MonthlyCharges  : num [1:7043] 29.9 57 53.9 42.3 70.7 ...
    ##  $ TotalCharges    : num [1:7043] 29.9 1889.5 108.2 1840.8 151.7 ...
    ##  $ Churn           : chr [1:7043] "No" "No" "Yes" "No" ...

## Checking Nulls

``` r
colSums(is.na(df))
```

    ##       customerID           gender    SeniorCitizen          Partner 
    ##                0                0                0                0 
    ##       Dependents           tenure     PhoneService    MultipleLines 
    ##                0                0                0                0 
    ##  InternetService   OnlineSecurity     OnlineBackup DeviceProtection 
    ##                0                0                0                0 
    ##      TechSupport      StreamingTV  StreamingMovies         Contract 
    ##                0                0                0                0 
    ## PaperlessBilling    PaymentMethod   MonthlyCharges     TotalCharges 
    ##                0                0                0               11 
    ##            Churn 
    ##                0

## Dropping Nulls

``` r
df<-na.omit(df)
```

## Converting to factor and releveling

``` r
df$gender<-as.factor(df$gender)
df$SeniorCitizen<-as.factor(df$SeniorCitizen)
df$Partner<-as.factor(df$Partner)
df$Dependents<-as.factor(df$Dependents)
df$Contract<-as.factor(df$Contract)
df$PaperlessBilling<-as.factor(df$PaperlessBilling)
df$Churn<-as.factor(df$Churn)
```

## Making separate datasets for telephone only, Internet only and customers that use both services

``` r
Both<-subset(df, PhoneService=='Yes' & InternetService!='No')
Telephone<-subset(df, PhoneService=='Yes' & InternetService=='No')
Internet<-subset(df, PhoneService=='No' & InternetService!='No')
```

## Preparing data for customers that use both, Telephone and Internet Services

``` r
unique(Both$PhoneService)
```

    ## [1] "Yes"

``` r
Both$MultipleLines<-as.factor(Both$MultipleLines)
Both$OnlineSecurity<-as.factor(Both$OnlineSecurity)
Both$OnlineBackup<-as.factor(Both$OnlineBackup)
Both$DeviceProtection<-as.factor(Both$DeviceProtection)
Both$TechSupport<-as.factor(Both$TechSupport)
Both$StreamingTV<-as.factor(Both$StreamingTV)
Both$StreamingMovies<-as.factor(Both$StreamingMovies)
Both$PaymentMethod<-as.factor(Both$PaymentMethod)
Both$InternetService<-as.factor(Both$InternetService)
Both$PhoneService<-NULL
summary(Both)
```

    ##   customerID           gender     SeniorCitizen Partner    Dependents
    ##  Length:4832        Female:2408   0:3846        No :2481   No :3574  
    ##  Class :character   Male  :2424   1: 986        Yes:2351   Yes:1258  
    ##  Mode  :character                                                    
    ##                                                                      
    ##                                                                      
    ##                                                                      
    ##      tenure      MultipleLines    InternetService OnlineSecurity
    ##  Min.   : 1.00   No :2205      DSL        :1736   No :3098      
    ##  1st Qu.: 9.00   Yes:2627      Fiber optic:3096   Yes:1734      
    ##  Median :30.00                                                  
    ##  Mean   :33.06                                                  
    ##  3rd Qu.:56.00                                                  
    ##  Max.   :72.00                                                  
    ##  OnlineBackup DeviceProtection TechSupport StreamingTV StreamingMovies
    ##  No :2693     No :2717         No :3079    No :2412    No :2399       
    ##  Yes:2139     Yes:2115         Yes:1753    Yes:2420    Yes:2433       
    ##                                                                       
    ##                                                                       
    ##                                                                       
    ##                                                                       
    ##            Contract    PaperlessBilling                   PaymentMethod 
    ##  Month-to-month:2975   No :1495         Bank transfer (automatic):1068  
    ##  One year      : 964   Yes:3337         Credit card (automatic)  :1037  
    ##  Two year      : 893                    Electronic check         :2017  
    ##                                         Mailed check             : 710  
    ##                                                                         
    ##                                                                         
    ##  MonthlyCharges    TotalCharges    Churn     
    ##  Min.   : 42.90   Min.   :  42.9   No :3246  
    ##  1st Qu.: 69.79   1st Qu.: 659.6   Yes:1586  
    ##  Median : 82.50   Median :2350.6             
    ##  Mean   : 81.76   Mean   :2902.5             
    ##  3rd Qu.: 95.70   3rd Qu.:4871.1             
    ##  Max.   :118.75   Max.   :8684.8

## Preparing data for Telephone Only Customers

``` r
Telephone$MultipleLines<-as.factor(Telephone$MultipleLines)
Telephone$PaymentMethod<-as.factor(Telephone$PaymentMethod)

Telephone$PhoneService<-NULL
Telephone$OnlineSecurity<-NULL
Telephone$OnlineBackup<-NULL
Telephone$DeviceProtection<-NULL
Telephone$TechSupport<-NULL
Telephone$StreamingTV<-NULL
Telephone$StreamingMovies<-NULL
Telephone$InternetService<-NULL
summary(Telephone)
```

    ##   customerID           gender    SeniorCitizen Partner   Dependents
    ##  Length:1520        Female:746   0:1468        No :787   No :883   
    ##  Class :character   Male  :774   1:  52        Yes:733   Yes:637   
    ##  Mode  :character                                                  
    ##                                                                    
    ##                                                                    
    ##                                                                    
    ##      tenure      MultipleLines           Contract   PaperlessBilling
    ##  Min.   : 1.00   No :1180      Month-to-month:524   No :1075        
    ##  1st Qu.: 8.00   Yes: 340      One year      :363   Yes: 445        
    ##  Median :25.00                 Two year      :633                   
    ##  Mean   :30.67                                                      
    ##  3rd Qu.:53.00                                                      
    ##  Max.   :72.00                                                      
    ##                    PaymentMethod MonthlyCharges   TotalCharges   
    ##  Bank transfer (automatic):332   Min.   :18.25   Min.   :  18.8  
    ##  Credit card (automatic)  :331   1st Qu.:19.70   1st Qu.: 159.9  
    ##  Electronic check         :122   Median :20.15   Median : 523.7  
    ##  Mailed check             :735   Mean   :21.08   Mean   : 665.2  
    ##                                  3rd Qu.:20.90   3rd Qu.:1110.0  
    ##                                  Max.   :26.90   Max.   :2007.0  
    ##  Churn     
    ##  No :1407  
    ##  Yes: 113  
    ##            
    ##            
    ##            
    ## 

## Preparing data for Internet Only Customers

``` r
Internet$PaymentMethod<-as.factor(Internet$PaymentMethod)
Internet$OnlineSecurity<-as.factor(Internet$OnlineSecurity)
Internet$OnlineBackup<-as.factor(Internet$OnlineBackup)
Internet$DeviceProtection<-as.factor(Internet$DeviceProtection)
Internet$TechSupport<-as.factor(Internet$TechSupport)
Internet$StreamingTV<-as.factor(Internet$StreamingTV)
Internet$StreamingMovies<-as.factor(Internet$StreamingMovies)



Internet$MultipleLines<-NULL
Internet$PhoneService<-NULL
Internet$InternetService<-NULL
summary(Internet)
```

    ##   customerID           gender    SeniorCitizen Partner   Dependents
    ##  Length:680         Female:329   0:576         No :371   No :476   
    ##  Class :character   Male  :351   1:104         Yes:309   Yes:204   
    ##  Mode  :character                                                  
    ##                                                                    
    ##                                                                    
    ##                                                                    
    ##      tenure      OnlineSecurity OnlineBackup DeviceProtection TechSupport
    ##  Min.   : 1.00   No :399        No :394      No :377          No :393    
    ##  1st Qu.: 8.00   Yes:281        Yes:286      Yes:303          Yes:287    
    ##  Median :29.00                                                           
    ##  Mean   :31.83                                                           
    ##  3rd Qu.:53.00                                                           
    ##  Max.   :72.00                                                           
    ##  StreamingTV StreamingMovies           Contract   PaperlessBilling
    ##  No :397     No :382         Month-to-month:376   No :294         
    ##  Yes:283     Yes:298         One year      :145   Yes:386         
    ##                              Two year      :159                   
    ##                                                                   
    ##                                                                   
    ##                                                                   
    ##                    PaymentMethod MonthlyCharges   TotalCharges    
    ##  Bank transfer (automatic):142   Min.   :23.45   Min.   :  23.45  
    ##  Credit card (automatic)  :153   1st Qu.:33.60   1st Qu.: 298.96  
    ##  Electronic check         :226   Median :40.55   Median :1153.80  
    ##  Mailed check             :159   Mean   :41.99   Mean   :1500.43  
    ##                                  3rd Qu.:50.66   3rd Qu.:2465.18  
    ##                                  Max.   :67.20   Max.   :4919.70  
    ##  Churn    
    ##  No :510  
    ##  Yes:170  
    ##           
    ##           
    ##           
    ## 

``` r
colSums(is.na(Internet))
```

    ##       customerID           gender    SeniorCitizen          Partner 
    ##                0                0                0                0 
    ##       Dependents           tenure   OnlineSecurity     OnlineBackup 
    ##                0                0                0                0 
    ## DeviceProtection      TechSupport      StreamingTV  StreamingMovies 
    ##                0                0                0                0 
    ##         Contract PaperlessBilling    PaymentMethod   MonthlyCharges 
    ##                0                0                0                0 
    ##     TotalCharges            Churn 
    ##                0                0
