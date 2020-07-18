#############################Functions#######################################

row_rep <- function(df, n) {
  df[rep(1:nrow(df), times = n),]
}

rm(list=ls())
# install required packages if they are missing:
list.of.packages <- c("tidyverse", "arm","tidymodels","MASS","lfe")
new.packages <- list.of.packages[!(list.of.packages %in% installed.packages()[,"Package"])];
if(length(new.packages)){install.packages(new.packages)}
lapply(list.of.packages, require, character.only = TRUE)

## Load in can_fe dataset
can_fe<-readRDS("can_fe.RDS")

## Load in simulated first stage coefficients. Simulations done
## using arm sim function in AJPSDraft_April42020.R code
ideal_points_CF_sims1000<-readRDS("ideal_points_CF_sims1000.RDS")
ideal_points_DWDIME_sims1000<-readRDS("ideal_points_DWDIME_sims1000.RDS")


## Set number of simulations here, can increase when we decide
## this code works
TotalReps<-100

## Create vectors of intercepts and coefficients which are 
## length of can_fe dataframe for simulation purposes. Note
## that because we are only doing 100 reps, we don't need the entire
## simulated coefficient vector yet, just the first 100 simulated coefficients
SimCF_Intercepts<-rep(ideal_points_CF_sims1000@coef[1:TotalReps,1], each=nrow(can_fe))
SimCF_Slopes<-rep(ideal_points_CF_sims1000@coef[1:TotalReps,2], each=nrow(can_fe))
SimDWDIME_Intercepts<-rep(ideal_points_DWDIME_sims1000@coef[1:TotalReps,1], each=nrow(can_fe))
SimDWDIME_Slopes<-rep(ideal_points_DWDIME_sims1000@coef[1:TotalReps,2], each=nrow(can_fe))


## Create group indicator needed to nest dataframes. Has to be same
## length as stacked dataframes, where each dataframe has all the same
## can_fe values except the coefs from the rogers equation change.
GroupIndicator<-rep(1:TotalReps, each=nrow(can_fe))



## Create TotalReps/N dataframes with simulated first stage coefficients taken from
## original Rogers regression using arm. Each `Group` has 
## a different set of simulated Slopes/Intercepts, but all the can_fe other
## variables are the same.
## Calculate the district ideal point and simulated candidate-district
## distances for each of the TotalReps/N can_fe datasets
## Store these 100 can_fe dataframe as can_feSims.
can_feSims<-readRDS("can_fe.RDS")%>%row_rep(TotalReps)%>%
            mutate(CF_Intercept=SimCF_Intercepts,
                   CF_Slope=SimCF_Slopes,
                   DWDIME_Intercept=SimDWDIME_Intercepts,
                   DWDIME_Slope=SimDWDIME_Slopes,
                   Group=GroupIndicator,
                   ## Calculate the district ideal point and simulated candidate-distrit
                   ## distances for each of the 1000 simulations. 
                   EstimatedIdealPointCF=CF_Intercept+CF_Slope*MRP_Mean,
                   Distance_CFDyn=abs(recipient.cfscore.dyn-EstimatedIdealPointCF),
                   Distance_CFnonDyn=abs(recipient.cfscore-EstimatedIdealPointCF),
                   CFScoreDynAbs=abs(recipient.cfscore.dyn),## Calculate absolute value of dynamicCFScore
                   ## Calculate ideological distance for DWDIME Scores
                   EstimatedIdealPointDWDIME=DWDIME_Intercept+DWDIME_Slope*MRP_Mean,
                   Distance_DWDIME=abs(dwdime-EstimatedIdealPointDWDIME),
                   ## Create indicators for whether data has ideological distance estimate
                   HasDistanceDWDIME=ifelse(is.na(Distance_DWDIME)==TRUE, 0, 1),
                   HasDistanceCFDyn=ifelse(is.na(Distance_CFDyn)==TRUE, 0, 1),
                   HasDistanceCFnonDyn=ifelse(is.na(Distance_CFnonDyn)==TRUE, 0, 1))


## Next rerun TotalReps models using broom setup all in one sweep
## with simulated ideological distances for each Table-Column in the paper, where relevant. 
## Then use the residual standard deviation, clustervcv and mvrnorm to 
## draw coefficients/estimates from the Two-Way FE/T-Test, and store.
## Turn this into a function to be called by each specific model formula function

EP<-function(ModelFormula){
set.seed(02138)
start.time <- Sys.time()## clock function time
## Eventually will want to convert all this to stargazer format, but
## for now want to make sure basic code is doing the right thing.

## Table 2 Column 1 


start.time <- Sys.time()## clock function time
Table1Column1<-can_feSims%>%
             group_by(Group)%>%nest()%>%
             ## Run felm two-way fe model on each simulated group, which each have a slightly
             ## different value for Distance for each candidate because each group also
             mutate(mod=map(data, ModelFormula),
             mutate(mod=map(data, ~ 
                              felm(Distance_CFDyn~CleanYear|UniqueDistrict_CensusGroup+year|0|UniqueDistrict_CensusGroup,
                                   data=.,subset=CensusLines==1 & HasDistanceCFDyn==1)),
            ## Get coefficients, SE, convert to tibble format using unnest
                    reg_results=map(mod,~tidy(.)))%>%unnest(cols = c(reg_results))%>%
             dplyr::select(Group, estimate, std.error, mod)%>%
            ## Pull out summary stats (residual standard error), convert to tibble using unnest
             mutate(detail = map(mod, ~ glance(.)))%>%unnest(cols=c(detail))%>%
            ## Simulate Residual SD from ChiSquare Distribution with df for each group
             mutate(ResidualStandardDeviation=sigma*sqrt(df/rchisq(1, df)),
            ## Get covariance, unscale (divide by sigma^2)
            ## from what I can tell in lfe documentation, estimation covariance matrix
            ## is automatically scaled by sigma2, but we need unscaled to calculate ResidualSD
                    ClusterVCV=unlist(mod)$clustervcv/sigma^2,
            ## Get 1 draw of simulated coefficient using ResidualSD2*Unscaled Covariance Matrix
            ungroup()%>%dplyr::select(-mod)%>%
            ## Get estimate and 95% CI across all draws
             summarize(Estimate=quantile(CoefficientDraw, .5),
               CILow=quantile(CoefficientDraw,.025),
               CIHigh=quantile(CoefficientDraw, .975))
return(EPResults)
            ungroup()%>%
            ## Get estimate and 95% CI across all draws
             summarize(Estimate=quantile(estimate, .5),
               CILow=quantile(estimate,.025),
               CIHigh=quantile(estimate, .975))

## End clock
end.time <- Sys.time()
time.taken <- end.time - start.time
time.taken
## 10 sec for 100 simulations on my computer, so will take a while to do 1000 for each
## column
}


## Create function call for Table 2 Column 1
Table2Column1Fit<-function(df){felm(Distance_CFDyn~CleanYear|UniqueDistrict_CensusGroup+year|0|UniqueDistrict_CensusGroup,
                                data=df,subset=CensusLines==1 & HasDistanceCFDyn==1)}
Table2Column1<-EP(Table2Column1Fit)

## See coefficient draw
Table1Column1$CoefficientDraw[1:100]


## Sources on scaling covariance matrix, simulating regression uncertainty (mostly working from
## Gelman & Hill 2007)
#http://www.clayford.net/statistics/simulation-to-represent-uncertainty-in-regression-coefficients/
#https://stackoverflow.com/questions/27113456/what-is-the-unscaled-variance-in-rs-linear-model-summary