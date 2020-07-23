#########################################################
##                 Mitchell Kilborn                    ##
##                 Arjun Vishwanath                    ##
##    Replication code for tables included in:         ##
##    "Public Money Talks Too: Clean Elections         ##
##      and Representation in State Legislatures"      ##
#########################################################



###################Load Required Libraries#############################

rm(list=ls())
# install required packages if they are missing:
list.of.packages <- c("tidyverse", "stargazer","scales","sjPlot","lfe","stringdist","readstata13",
                      "tidymodels","arm","MASS")
new.packages <- list.of.packages[!(list.of.packages %in% installed.packages()[,"Package"])];
if(length(new.packages)){install.packages(new.packages)}
lapply(list.of.packages, require, character.only = TRUE)


## Package version check
packinfo <- as.data.frame(installed.packages(fields = c("Package", "Version")))
packinfo[which(packinfo$Package %in% list.of.packages),c("Package", "Version")]

## Package                  Version
# arm                 arm   1.10-1
# lfe                 lfe    2.8-5
# MASS               MASS 7.3-51.4
# readstata13 readstata13    0.9.2
# scales           scales    1.1.0
# sjPlot           sjPlot    2.8.2
# stargazer     stargazer    5.2.2
# stringdist   stringdist  0.9.5.5
# tidymodels   tidymodels    0.1.0
# tidyverse     tidyverse    1.3.0
# 

#############################Session Info#######################################

sessionInfo()
# R version 3.6.2 (2019-12-12)
# Platform: x86_64-apple-darwin15.6.0 (64-bit)
# Running under: macOS Catalina 10.15.5
# July 22, 2020


#############################Functions#######################################
'%!in%' <- function(x,y)!('%in%'(x,y))
substrRight <- function(x, n){##Reverse substring function
  substr(x, nchar(x)-n+1, nchar(x))
}
row_rep <- function(df, n) {
  df[rep(1:nrow(df), times = n),]
}
loadRData <- function(fileName){
  #loads an RData file, and returns it
  load(fileName)
  get(ls()[ls() != "fileName"])
}


####Prepare simulated can_fe####
## Load in can_fe dataset
can_fe<-readRDS("can_fe.RDS")

## Load in simulated first stage coefficients. Simulations done
## using arm sim function in AJPSDraft_April42020.R code
ideal_points_CF_sims1000<-readRDS("ideal_points_CF_sims1000.RDS")
ideal_points_DWDIME_sims1000<-readRDS("ideal_points_DWDIME_sims1000.RDS")


## Set number of simulations here, can increase when we decide
## this code works
TotalReps<-10

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




## Calculate IPW for dynamic and stable CFScore estimate subsets
weights_CFDyn<-can_fe%>%filter(CensusLines==1 & HasDistanceCFDyn==1)%>%
  group_by(sab)%>%summarize(IPW_CFDyn=1/(n()/nrow(.)))

weights_CFnonDyn<-can_fe%>%filter(CensusLines==1& HasDistanceCFnonDyn==1)%>%
  group_by(sab)%>%summarize(IPW_CFnonDyn=1/(n()/nrow(.)))




## Create TotalReps/N dataframes with simulated first stage coefficients taken from
## original Rogers regression using arm. Each `Group` has 
## a different set of simulated Slopes/Intercepts, but all the can_fe other
## variables are the same.
## Calculate the district ideal point and simulated candidate-district
## distances for each of the TotalReps/N can_fe datasets
## Store these 100 can_fe dataframe as can_feSims.

can_feSims<-readRDS("can_fe.RDS")%>%
  left_join(weights_CFDyn, by=c("sab"))%>%
  left_join(weights_CFnonDyn, by=c("sab"))%>%
  row_rep(TotalReps)%>%
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

## Have to create dyn and ndyn subsets because lfe
## throws error when trying to use subset and weights in same felm call
## Divide simulated dataframes into list of dataframes.
can_fe_weights_dyn<-can_feSims%>%filter(CensusLines==1 &HasDistanceCFDyn==1)
can_feSims_weights_dyn<-split(can_fe_weights_dyn, f = can_fe_weights_dyn$Group)

can_fe_weights_ndyn<-can_feSims%>%filter(CensusLines==1 & HasDistanceCFnonDyn==1)
can_feSims_weights_ndyn<-split(can_fe_weights_ndyn, f = can_fe_weights_ndyn$Group)


## Divide simulated dataframes into list of dataframes for main analysis
can_feSims<-split( can_feSims, f = can_feSims$Group)



####Prepare T-Test CFScore Dataset####
## Create T-Test dataframe
## Subset to Arizona data only and select relevant variables
aztest<-can_fe%>%filter(sab=="AZ")%>%
  dplyr::select(year, sab, sen, dno, bonica.rid, name, party, seat, recipient.cfscore,
                recipient.cfscore.dyn,ran.general,CleanYear,CleanFirstRun, WonElection, MRP_Mean)%>%
  ## Convert year to numeric from factor to facilitate merging
  mutate(year=as.numeric(as.character(year)),
         DistrictYear=paste0(year,sab,sen,dno))

## Select Clean Elections Candidates who competed in general election
## Add C to front of all columns to make clear which columns
## are clean candidates observations

clean<-aztest%>%filter(CleanYear==1)
colnames(clean)<-paste0("C", colnames(clean))

## Select non-Clean Elections Candidates who competed in general election Add UC
## to front of all columns to make clear which columns are non Clean
## candidates

unclean<-aztest%>%filter(CleanYear==0)
colnames(unclean)<-paste0("UC", colnames(unclean))

## Create versions of clean and unclean dataframes subset to winners
## and general election candidates respectively

cleanGenCan<-clean%>%filter(Cran.general==1)
uncleanGenCan<-unclean%>%filter(UCran.general==1)
cleanWinners<-clean%>%filter(CWonElection==1)
uncleanWinners<-unclean%>%filter(UCWonElection==1)

## Merge Clean Elections candidates to non-Clean Elections candidates, creating
## different observation for each pairing even if Clean Elections candidate is
## used twice to compare with two different non-Clean Elections candidates. We
## decided to use each clean-nonclean pairing as an observation. Do this for
## general election candidates and winners respectively because Table 4 and 6
## use GE candidates only and Table 5 uses winners only

cleanGC<-cleanGenCan%>%left_join(uncleanGenCan, by=c("Cyear"="UCyear", "Cdno"="UCdno", "Cparty"="UCparty"))
cleanW<-cleanWinners%>%left_join(uncleanWinners, by=c("Cyear"="UCyear", "Cdno"="UCdno", "Cparty"="UCparty"))

## Select Democrat and Republican general election candidates and create stacked
## observations for analysis in Table 4 section below

pairedDemsGC<-filter(cleanGC, Cparty=="100")
pairedRepsGC<-filter(cleanGC, Cparty=="200")
pairedGC<-bind_rows(pairedDemsGC,pairedRepsGC)

## Create vectors of simulated intercepts and coefficients which are 
## length of pairedGC dataframe for simulation purposes.
## Note that we can reuse ideal_points_CF_sims1000 here
## because we are studying the error in the Distance_CF/Distance_CFnondyn variables
## Also note that because we are only doing TotalReps reps, we don't need the entire
## simulated coefficient vector yet, just the first TotalRep simulated coefficients
SimCF_Intercepts_TTest<-rep(ideal_points_CF_sims1000@coef[1:TotalReps,1], each=nrow(pairedGC))
SimCF_Slopes_TTest<-rep(ideal_points_CF_sims1000@coef[1:TotalReps,2], each=nrow(pairedGC))


## Create group indicator needed to nest dataframes. Has to be same
## length as pairedGC, where each dataframe has all the same
## pairedGC values except the coefs from the rogers equation change.
GroupIndicatorTTest<-rep(1:TotalReps, each=nrow(pairedGC))

## Create TotalReps paired CFGCSims frames
## Dataframe is in District-Year-Pair format
pairedCFGCSims<-pairedGC%>%
row_rep(TotalReps)%>%
  mutate(CF_Intercept=SimCF_Intercepts_TTest,## Add ith simulated CF intercept to each ith dataframe
         CF_Slope=SimCF_Slopes_TTest,## Add ith simulated CF slope to each ith dataframe
         Group=GroupIndicatorTTest, ## Add ith group indicator to each ith dataframe
         ## Calculate the district ideal point and simulated candidate-distrit
         ## distances for each of the TotalReps simulations. 
         EstimatedIdealPointCF=CF_Intercept+CF_Slope*CMRP_Mean,
         CDistance_CFDyn=abs(Crecipient.cfscore.dyn-EstimatedIdealPointCF),
         CDistance_CFnonDyn=abs(Crecipient.cfscore-EstimatedIdealPointCF),
         UCDistance_CFDyn=abs(UCrecipient.cfscore.dyn-EstimatedIdealPointCF),
         UCDistance_CFnonDyn=abs(UCrecipient.cfscore-EstimatedIdealPointCF))
         
## Divide simulated dataframes into list of dataframes for main analysis
pairedCFGCSims<-split( pairedCFGCSims, f = pairedCFGCSims$Group)


####Prepare NP Score T-Test DataSet####

## Repeat same dataframe shaping for NP Scores

## Read in npscore t-test data from NP_SCORE_TAB7.R
mrp_distcalc<-readRDS("NPResultsBaseFrame.RDS")


## Break mrp_distcalc into clean and nonclean variables
clean_mrpdist<-mrp_distcalc%>%filter( RanClean==1)
colnames(clean_mrpdist)<-paste0("C", colnames(clean_mrpdist))

unclean_mrpdist<-mrp_distcalc%>%filter( RanClean==0)
colnames(unclean_mrpdist)<-paste0("UC", colnames(unclean_mrpdist))

## Merge clean and unclean ideological distance scores by year, district, and party,
mrp_paired_test<-left_join(clean_mrpdist, unclean_mrpdist, by=c("CElection_Year"="UCElection_Year",
                                                                "CDistrict"="UCDistrict",
                                                                "Cparty"="UCparty"))%>%
  filter(is.na(Cnp_score)==FALSE & is.na(UCnp_score)==FALSE)

## Create vectors of simulated intercepts and coefficients which are 
## length of mrp_distcalc dataframe for simulation purposes. 
ideal_points_NP<-lm(np_score~MRP_Mean, data=mrp_distcalc)
summary(ideal_points_NP)

## Simulate using arm
set.seed(02138)
ideal_points_NP_sims1000<-sim(ideal_points_NP, n.sims=1000)

SimNP_Intercepts<-rep(ideal_points_NP_sims1000@coef[1:TotalReps,1], each=nrow(mrp_paired_test))
SimNP_Slopes<-rep(ideal_points_NP_sims1000@coef[1:TotalReps,2], each=nrow(mrp_paired_test))

## Create group indicator needed to nest dataframes. Has to be same
## length as mrp_paired_test, where each dataframe has all the same
## values except the coefs from the rogers equation change.
GroupIndicatorNP<-rep(1:TotalReps, each=nrow(mrp_paired_test))


## Create TotalReps paired NPGCSims frames
## Dataframe is in District-Year-Pair format
## Impute district ideal points on legislator scale
## Calculate NP_Distance for the Clean and unclean legislator in each district-year-pair
pairedNPGCSims<-mrp_paired_test %>%row_rep(TotalReps)%>%
  mutate(Intercept=SimNP_Intercepts,
         phi1=SimNP_Slopes, GroupIndicator=GroupIndicatorNP,
         EstimatedIdealPoint=Intercept+phi1*CMRP_Mean,
         CDistance_NP=abs(Cnp_score-EstimatedIdealPoint),##Calculate difference between district and legislator ideal point
         UCDistance_NP=abs(UCnp_score-EstimatedIdealPoint))##Calculate difference between district and legislator ideal point


## Divide simulated dataframes into list of dataframes for main analysis
pairedNPGCSims<-split(pairedNPGCSims, f = pairedNPGCSims$Group)


#### Error Propagation Multivariate Function####
## Takes arguments:
## frame: List of can_fe simulations
## ModelFormula: felm model formula
## Returns: outMod, felm object which has simulated coefs, SE, and p-value for stargazer
## Covers multi-variate case, which previously broom/tidymodels format did not
EPMV<-function(frame, ModelFormula){
  set.seed(02138)
  
  ## For the specified number of simulations, apply the passed function ModelFormula to the ith simulated
  ## can_fe frame
  for(i in 1:length(frame)){
    subMod<-ModelFormula(frame[[i]])
  ## Collect coefficients, RSD, Coefficient names from ith model
    store<-tibble(Run=rep(i,length(subMod$coefficients)), ## Mark which ith run of the model
                  Coefs=c(unlist(subMod$coefficients)),## Store coefficient values
                  CoefNames= c(row.names(subMod$coefficients)),## Store coefficient names
                  ResidualStandardDeviation=subMod$rse*sqrt(subMod$df/rchisq(1, subMod$df)), ## Calculate RSD
                  N=subMod$N, DF= subMod$df)## Store N and DF
  ## Coefficient draw from multivariate normal distribution
    store$CoefficientDraw<-mvrnorm(1, store$Coefs, 
                                   store$ResidualStandardDeviation^2*(subMod$clustervcv/subMod$rse^2))
  ## Store ith coefficient draw
     if(i==1){finalStore<-store}
     if(i>1){finalStore<-bind_rows(finalStore, store)}
  }
  ## Once all iterations have run, calculate estimate and standard deviation of distribution for SE
  ## calculate P-Value
  finalStore<-finalStore%>%group_by(CoefNames)%>%
    summarize(Estimate=quantile(CoefficientDraw, .5),SE=sd(CoefficientDraw),
              DF=mean(DF))%>%
    mutate(PValue= 2*pt(abs(Estimate/SE),DF,lower.tail=FALSE)) 
  ## Store simulated coefficient, SE, and pval in felm object to return for
  ## easy compatibility with stargazer.
  outMod<-ModelFormula(frame[[i]])
  outMod$beta[1:length(outMod$coefficients)]<-matrix(c(finalStore$Estimate), ncol=1)
  outMod$cse<-finalStore$SE
  outMod$cpval<-finalStore$PValue
  return(outMod)
}


#### Table 2 Function Calls####

## Pooled States: Dynamic Distance Estimate with District and Year Fixed
## Effects, SEs clustered by district
Table2Column1Fit<-function(df){felm(Distance_CFDyn~CleanYear|UniqueDistrict_CensusGroup+year|0|UniqueDistrict_CensusGroup,
                                    data=df,subset=CensusLines==1 & HasDistanceCFDyn==1)}
Table2Column1<-EPMV(can_feSims, Table2Column1Fit)

## Pooled States: Dynamic Distance Estimate with District, Year, and Party Fixed Effects Party Fixed Effects,
## SEs clustered by district
Table2Column2Fit<-function(df){felm(Distance_CFDyn~CleanYear|UniqueDistrict_CensusGroup+year+party|0|UniqueDistrict_CensusGroup,
                                    data=df, subset=CensusLines==1 & HasDistanceCFDyn==1)}
Table2Column2<-EPMV(can_feSims,Table2Column2Fit)

## Pooled States: Dynamic Distance Estimate with District and Party Fixed Effects, District x Time Trend
## SEs clustered by district
Table2Column3Fit<-function(df){felm(Distance_CFDyn~CleanYear|UniqueDistrict_CensusGroup:RedistTime+party|0|UniqueDistrict_CensusGroup,
                                    data=df, subset=CensusLines==1 & HasDistanceCFDyn==1)}
Table2Column3<-EPMV(can_feSims,Table2Column3Fit)

## Pooled States: Non-Dynamic Distance Estimate with District and Year Fixed Effects
## SEs clustered by district
Table2Column4Fit<-function(df){felm(Distance_CFnonDyn~CleanYear|UniqueDistrict_CensusGroup+year|0|UniqueDistrict_CensusGroup,
                                    data=df, subset=CensusLines==1 & HasDistanceCFnonDyn==1)}
Table2Column4<-EPMV(can_feSims,Table2Column4Fit)

## Pooled States: Non-Dynamic Distance Estimate District, Year, and Party Fixed Effects
## SEs clustered by district
Table2Column5Fit<-function(df){felm(Distance_CFnonDyn~CleanYear|UniqueDistrict_CensusGroup+year+party|0|UniqueDistrict_CensusGroup,
                                    data=df, subset=CensusLines==1 & HasDistanceCFnonDyn==1)}
Table2Column5<-EPMV(can_feSims,Table2Column5Fit)

## Pooled States: Non-Dynamic Distance Estimate District, Party Fixed Effects, District x Time Trend
## SEs clustered by district
Table2Column6Fit<-function(df){felm(Distance_CFnonDyn~CleanYear|UniqueDistrict_CensusGroup:RedistTime+party|0|UniqueDistrict_CensusGroup,
                                    data=can_fe, subset=CensusLines==1 & HasDistanceCFnonDyn==1)}
Table2Column6<-EPMV(can_feSims,Table2Column6Fit)



## Effect size interpretation
## Quantile of ideological distance
quantile(can_fe$Distance_CFDyn, na.rm = TRUE, probs=seq(0,1,.1))

## Proportion of SD 
Table2Column1$beta[1]/sd(can_fe$Distance_CFDyn, na.rm = TRUE)


## Create Latex Table
stargazer(Table2Column1, Table2Column2,Table2Column3,Table2Column4,Table2Column5,Table2Column6,
          model.names = FALSE, model.numbers = TRUE,
          dep.var.labels.include = FALSE,
          column.labels   = c("Dynamic CFScore Distance", "Stable CFScore Distance"),
          column.separate = c(3,3),
          star.cutoffs = c(0.05, 0.01, 0.001),
          omit=c("party"),
          style = 'apsr',
          table.placement = "H",
          title=c("Candidate Ideological Distance to District Pooled States 2004-2016 Two Way Fixed Effects"),
          label=c("tab:CoreResults"),
          notes = c("Notes: Standard errors clustered", " by district in parentheses."),
          covariate.labels =c("Public Financing Candidate"),
          add.lines = list(c("District Fixed Effects", "\\checkmark", "\\checkmark", "\\checkmark", "\\checkmark", "\\checkmark", "\\checkmark"),
                           c("Election Year Fixed Effects", "\\checkmark", "\\checkmark", "", "\\checkmark", "\\checkmark", ""),
                           c("Party Fixed Effects", "", "\\checkmark", "\\checkmark", "", "\\checkmark", "\\checkmark"),
                           c("District x Time Effects", "", "", "\\checkmark", "", "", "\\checkmark")),
          multicolumn = FALSE,
          omit.stat = c("rsq","adj.rsq","f","ser"))



####Table 3 Candidate Fixed Effects####

## Count Percentage of clean candidates who switched status at any point in the
## dataset
switchers<-nrow(can_fe %>% filter(CleanYear != CleanFirstRun)%>%
                  distinct(bonica.rid))
all<-can_fe%>%filter(CleanYear==1)%>%distinct(bonica.rid)%>%count()%>%pull()
paste0(round(100*switchers/all, digits=1), "\\%", 
       " candidates switched financing status between 2000 and 2016 ",
       "(", switchers, " of ", all, ")") 


## Add candidate fixed effects to Table 2 with district and year fixed effects.
## SEs clustered by candidate Subset to people who switch public financing at
## some point in their career (PFStatusSwitcher==1)
Table3Column1Fit<-function(df){felm(Distance_CFDyn~CleanYear|bonica.rid+year+UniqueDistrict_CensusGroup|0|bonica.rid,
                                    data=df, subset=CensusLines==1& PFStatusSwitcher==1 & HasDistanceCFDyn==1)}
Table3Column1<-EPMV(can_feSims,Table3Column1Fit)

## Add candidate incumbency variable, SEs clustered by candidate
Table3Column2Fit<-function(df){felm(Distance_CFDyn~CleanYear|bonica.rid+year+UniqueDistrict_CensusGroup+Incumbent|0|bonica.rid,
                                    data=df, subset=CensusLines==1& PFStatusSwitcher==1 & HasDistanceCFDyn==1)}
Table3Column2<-EPMV(can_feSims,Table3Column2Fit)

## Subset by party: Democrats, candidate fixed effects, incumbency indicator, SEs clustered by candidate
Table3Column3Fit<-function(df){felm(recipient.cfscore.dyn ~CleanYear|bonica.rid+year+UniqueDistrict_CensusGroup+Incumbent|0|bonica.rid,
                                    data=df, subset=CensusLines==1& PFStatusSwitcher==1 &party==100 & HasDistanceCFDyn==1)}
Table3Column3<-EPMV(can_feSims,Table3Column3Fit)

## Subset by party: Republican, candidate fixed effects, incumbency indicator, SEs clustered by candidate
Table3Column4Fit<-function(df){felm(recipient.cfscore.dyn ~CleanYear|bonica.rid+year+UniqueDistrict_CensusGroup+Incumbent|0|bonica.rid,
                                    data=df, subset=CensusLines==1& PFStatusSwitcher==1& party==200 & HasDistanceCFDyn==1)}
Table3Column4<-EPMV(can_feSims,Table3Column4Fit)


## Format latex table
stargazer(Table3Column1, Table3Column2, Table3Column3, Table3Column4,
          model.names = FALSE, model.numbers = TRUE,
          dep.var.labels.include = FALSE,
          column.labels   = c("Ideological Distance", "Democrats", "Republicans"),
          column.separate = c(2,1,1),
          star.cutoffs = c(0.05, 0.01, 0.001),
          omit=c("party"),
          style = 'apsr',
          table.placement = "H",
          title=c("Candidate Ideological Distance to District: Arizona, Connecticut, Maine 2004-2016 Two Way Fixed Effects"),
          label=c("tab:CanFE"),
          notes = c("Notes: Standard errors clustered", " by candidate in parentheses.",
                    "Data subset to candidates who","switch public financing status between cycles."),
          covariate.labels =c("Public Financing Candidate", "Incumbent"),
          add.lines = list(c("District Fixed Effects", "\\checkmark", "\\checkmark","\\checkmark", "\\checkmark"),
                           c("Election Year Fixed Effects", "\\checkmark", "\\checkmark","\\checkmark", "\\checkmark"),
                           c("Candidate Fixed Effects", "\\checkmark", "\\checkmark","\\checkmark", "\\checkmark")),
          multicolumn = FALSE,
          omit.stat = c("rsq","adj.rsq","f","ser"))

####Prepare T-Test Data####
## Tables 4, 5, and 6 use Arizona Clean and NonClean observations
## paired by district, party, and year. Need to format data to
## merge these observations while retaining distinct column names


## Load NPResultsTable.RDS from NP_SCORE_TAB.R
NPResultsTable<-readRDS("NPResultsTable.RDS")

## Subset to Arizona data only and select relevant variables
aztest<-can_fe%>%filter(sab=="AZ")%>%
  dplyr::select(year, sab, sen, dno, bonica.rid, name, party, seat, recipient.cfscore,
         recipient.cfscore.dyn,ran.general,CleanYear,CleanFirstRun,
         Distance_CFDyn,Distance_CFnonDyn, WonElection)%>%
  ## Convert year to numeric from factor to facilitate merging
  mutate(year=as.numeric(as.character(year)),
         DistrictYear=paste0(year,sab,sen,dno))

## Select Clean Elections Candidates who competed in general election
## Add C to front of all columns to make clear which columns
## are clean candidates observations

clean<-aztest%>%filter(CleanYear==1)
colnames(clean)<-paste0("C", colnames(clean))

## Select non-Clean Elections Candidates who competed in general election Add UC
## to front of all columns to make clear which columns are non Clean
## candidates

unclean<-aztest%>%filter(CleanYear==0)
colnames(unclean)<-paste0("UC", colnames(unclean))

## Create versions of clean and unclean dataframes subset to winners
## and general election candidates respectively

cleanGenCan<-clean%>%filter(Cran.general==1)
uncleanGenCan<-unclean%>%filter(UCran.general==1)
cleanWinners<-clean%>%filter(CWonElection==1)
uncleanWinners<-unclean%>%filter(UCWonElection==1)

## Merge Clean Elections candidates to non-Clean Elections candidates, creating
## different observation for each pairing even if Clean Elections candidate is
## used twice to compare with two different non-Clean Elections candidates. We
## decided to use each clean-nonclean pairing as an observation. Do this for
## general election candidates and winners respectively because Table 4 and 6
## use GE candidates only and Table 5 uses winners only

cleanGC<-cleanGenCan%>%left_join(uncleanGenCan, by=c("Cyear"="UCyear", "Cdno"="UCdno", "Cparty"="UCparty"))
cleanW<-cleanWinners%>%left_join(uncleanWinners, by=c("Cyear"="UCyear", "Cdno"="UCdno", "Cparty"="UCparty"))

## Select Democrat and Republican general election candidates and create stacked
## observations for analysis in Table 4 section below
## Also create variables marking whether clean or private candidate is more ideologically
## extreme for binomial test below

pairedDemsGC<-filter(cleanGC, Cparty=="100")
pairedRepsGC<-filter(cleanGC, Cparty=="200")
pairedGC<-bind_rows(pairedDemsGC,pairedRepsGC)%>%
  mutate(CLMoreExtremeDyn=ifelse(CDistance_CFDyn > UCDistance_CFDyn,1,0),
         CLMoreExtremeNonDyn=ifelse(CDistance_CFnonDyn> 
                                      UCDistance_CFnonDyn, 1,0))

## Select Democrat and Republican winners and create stacked observations
## for analysis in Table 5 section below

pairedDemsW<-filter(cleanW, Cparty=="100")
pairedRepsW<-filter(cleanW, Cparty=="200")
pairedW<-bind_rows(pairedDemsW,pairedRepsW)


####Table 4 Paired T-Test General Election Candidate Ideology####

## Use pairedDems/pairedRepsGC because Table 4 includes only general election
## candidate same-party, same year, same district pairs
## Calculate difference in ideology using dynamic and stable estimates by party

## Non-dynamic estimates
nonDynD<-t.test(pairedDemsGC$Crecipient.cfscore, pairedDemsGC$UCrecipient.cfscore, paired=TRUE)
nonDynR<-t.test(pairedRepsGC$Crecipient.cfscore, pairedRepsGC$UCrecipient.cfscore, paired=TRUE)

## dynamic estimates
DynD<-t.test(pairedDemsGC$Crecipient.cfscore.dyn, pairedDemsGC$UCrecipient.cfscore.dyn, paired=TRUE)
DynR<-t.test(pairedRepsGC$Crecipient.cfscore.dyn, pairedRepsGC$UCrecipient.cfscore.dyn, paired=TRUE)

## Create lm models to incorporate T-Test results into stargazer
## Can't figure out a simpler way to format these results nicely, any ideas?

modD<-modDDyn<-modR<-modRDyn<-lm(Crecipient.cfscore~Cyear, data=pairedDemsW)

## Store dynamic modD/RDyn and stable CFScore (modD/R) paired t-test results
modD$coefficients[1]<-nonDynD$estimate[[1]]
modDDyn$coefficients[1]<-DynD$estimate[[1]]
modR$coefficients[1]<-nonDynR$estimate[[1]]
modRDyn$coefficients[1]<-DynR$estimate[[1]]

## Create latex table with results
stargazer(modD, modDDyn, modR, modRDyn,
          se=list(nonDynD$stderr, DynD$stderr, nonDynR$stderr, DynR$stderr),
          p=list(nonDynD$p.value, DynD$p.value, nonDynR$p.value, DynR$p.value),
          model.names = FALSE, model.numbers = FALSE,
          dep.var.labels = c("Stable CFscore", "Dynamic CFscore", "Stable CFscore", "Dynamic CFscore"),
          column.labels   = c("Democratic Candidates", "Republican Candidates"),
          column.separate = c(2,2),
          star.cutoffs = c(0.05, 0.01, 0.001),
          omit=c("Cyear"),
          style = 'apsr',
          table.placement = "H",
          title=c("Paired T-Test Estimates: Arizona State Legislative Candidate Ideology, 2000-2016"),
          label=c("tab:CFscoreTTest"),
          covariate.labels =c("CFScore Difference"),
          multicolumn = FALSE,
          omit.stat = c("rsq","adj.rsq","f","ser","n"),
          add.lines = list(c("N",nonDynD$parameter+1,DynD$parameter+1,nonDynR$parameter+1,DynR$parameter+1)))

####Table 5 Paired T-Test Winner Ideology####

## Use pairedDems/pairedRepsW because Table 5 includes only winners
## legislator same-party, same year, same district pairs
## Calculate difference in ideology using dynamic and stable estimates by party

## Non-dynamic estimates
nonDynD_W<-t.test(pairedDemsW$Crecipient.cfscore, pairedDemsW$UCrecipient.cfscore, paired=TRUE)
nonDynR_W<-t.test(pairedRepsW$Crecipient.cfscore, pairedRepsW$UCrecipient.cfscore, paired=TRUE)

## dynamic estimates
DynD_W<-t.test(pairedDemsW$Crecipient.cfscore.dyn, pairedDemsW$UCrecipient.cfscore.dyn, paired=TRUE)
DynR_W<-t.test(pairedRepsW$Crecipient.cfscore.dyn, pairedRepsW$UCrecipient.cfscore.dyn, paired=TRUE)


## Create lm models to incorporate T-Test results into stargazer
modD_W<-modDDyn_W<-modR_W<-modRDyn_W<-modD_NP<-modR_NP<-lm(Crecipient.cfscore~Cyear, data=pairedDemsW)

## Store Estimate
modD_W$coefficients[1]<-nonDynD_W$estimate[[1]]
modDDyn_W$coefficients[1]<-DynD_W$estimate[[1]]
modD_NP$coefficients[1]<-NPResultsTable$Estimate[[1]]
modR_W$coefficients[1]<-nonDynR_W$estimate[[1]]
modRDyn_W$coefficients[1]<-DynR_W$estimate[[1]]
modR_NP$coefficients[1]<-NPResultsTable$Estimate[[2]]

stargazer(modD_W, modDDyn_W,modD_NP, modR_W, modRDyn_W,modR_NP,
          se=list(nonDynD_W$stderr, DynD_W$stderr,NPResultsTable$SE[[1]],
                  nonDynR_W$stderr, DynR_W$stderr,NPResultsTable$SE[[2]]),
          p=list(nonDynD_W$p.value, DynD_W$p.value,NPResultsTable$P[[1]],
                 nonDynR_W$p.value, DynR_W$p.value,NPResultsTable$P[[2]]),
          model.names = FALSE, model.numbers = FALSE,
          dep.var.labels = c("Stable CFscore", "Dynamic CFscore", "SM Score",
                             "Stable CFscore", "Dynamic CFscore", "SM Score"),
          column.labels   = c("Democratic Legislators", "Republican Legislators"),
          column.separate = c(3,3),
          star.cutoffs = c(0.05, 0.01, 0.001),
          omit=c("Cyear"),
          style = 'apsr',
          table.placement = "H",
          title=c("Paired T-Test Estimates: Arizona State Legislator Ideology, 2000-2016"),
          label=c("tab:CFscoreTTestLegislators"),
          covariate.labels =c("CFscore Difference"),
          multicolumn = FALSE,
          omit.stat = c("rsq","adj.rsq","f","ser","n"),
          add.lines = list(c("N",  nonDynD_W$parameter+1, DynD_W$parameter+1, 
                             NPResultsTable$N[[1]],nonDynR_W$parameter+1, 
                             DynR_W$parameter+1,NPResultsTable$N[[2]])))

#### Table 6 Paired T-Test Ideological Distance GE Candidates####

## EP function adapted for T-Test
## Takes arguments:
## frame: list of dataframes with TotalReps perturbed distance values
## Formula: t-test formula
EP_TTest<-function(frame, Formula){
  ## Create tibble to store results
  Diffs<-tibble(SubEstimate=1:length(frame), DF=NA)
  ## Iterate through list of dfs with simulated distance coefficients
  for(i in 1:length(frame)){
    Mod<-Formula(frame[[i]])## Run ith t-test 
    Diffs$SubEstimate[i]<-Mod$estimate[[1]]## Store difference
    Diffs$DF[i]<-Mod$parameter[1]## Store DF
  }
  
  ## Create empty lm object for stargazer output
    outMod<-lm(SubEstimate~DF, data=Diffs)
  
  ## Take center of distribution/SE, and calculate Pval
  Diffs<-Diffs%>%summarize(Estimate=quantile(SubEstimate, .5),SE=sd(SubEstimate),
                          DF=mean(DF))%>%
                 mutate(P=2*pt(abs(Estimate/SE),DF,lower.tail=FALSE))
    outMod$coefficients[1]<-Diffs$Estimate
    outMod$SE<-Diffs$SE## Store SE in rank variable
    outMod$N<-Diffs$DF+1## Store DF, add back 1 for N 
    outMod$PValue<-Diffs$P
    return(outMod)
}


## CF Dynamic
CFDynTTest<-function(df){t.test(df$CDistance_CFDyn, df$UCDistance_CFDyn, paired=TRUE)}
CFDynTT<-EP_TTest(pairedCFGCSims, CFDynTTest)

## CF Stable
CFNonDynTTest<-function(df){t.test(df$CDistance_CFnonDyn, df$UCDistance_CFnonDyn, paired=TRUE)}
CFNonDynTT<-EP_TTest(pairedCFGCSims, CFNonDynTTest)

## NPScore
NPTTest<-function(df){t.test(df$CDistance_NP, df$UCDistance_NP, paired=TRUE)}
NPTT<-EP_TTest(pairedNPGCSims, NPTTest)


## Create line for observations
## Have to add back one due to degree of freedom calculation,
## not sure how to pull out total
## observations from T-test object
stargazer(CFDynTT, CFNonDynTT,NPTT,
          se=list(CFDynTT$SE, CFNonDynTT$SE, NPTT$SE),
          p=list(CFDynTT$PValue, CFNonDynTT$PValue, NPTT$PValue),
          model.names = FALSE, model.numbers = FALSE,
          dep.var.labels.include = FALSE,
          column.labels   = c("Dynamic CFscore", "Stable CFscore", "SM Score"),
          column.separate = c(1,1,1),
          star.cutoffs = c(0.05, 0.01, 0.001),
          omit=c("Cyear"),
          style = 'apsr',
          table.placement = "H",
          title=c("Paired T-Test Estimates: Arizona State Legislative Candidate Ideological Proximity to District, 2000-2016"),
          label=c("tab:DistanceTTest"),
          covariate.labels =c("Ideological Distance Diff."),
          multicolumn = FALSE,
          omit.stat = c("rsq","adj.rsq","f","ser", "n"),
          add.lines = list(c("N",CFDynTT$N, CFNonDynTT$N, NPTT$N)))

#### SI-A9 Binomial Test Results####
## Dynamic CFScore Estimates: For how many cases does the 
## publicly funded candidate have a larger CFScore distance
sum(pairedGC$CLMoreExtremeDyn, na.rm = TRUE)/sum(!is.na(pairedGC$CLMoreExtremeDyn))
binom.test(sum(pairedGC$CLMoreExtremeDyn, na.rm = TRUE),sum(!is.na(pairedGC$CLMoreExtremeDyn)), .5)

## Static CFScore Estimates: For how many cases does the 
## publicly funded candidate have a larger CFScore distance
sum(pairedGC$CLMoreExtremeNonDyn, na.rm = TRUE)/sum(!is.na(pairedGC$CLMoreExtremeNonDyn))
binom.test(sum(pairedGC$CLMoreExtremeNonDyn, na.rm = TRUE), sum(!is.na(pairedGC$CLMoreExtremeNonDyn)), .5)


## NP Score: For how many cases does the publicly funded candidate
## have a larger NP_Score distance
mrp_distcalc<-readRDS("NPResultsBaseFrame.RDS")

ideal_points_NP<-lm(np_score~MRP_Mean, data=mrp_distcalc)
summary(ideal_points_NP)
## Impute district ideal points on legislator scale
mrp_distcalc<-mrp_distcalc %>%mutate(Intercept=ideal_points_NP$coefficients[1],
                                     phi1=ideal_points_NP$coefficients[2],
                                     EstimatedIdealPoint=Intercept+phi1*MRP_Mean,
                                     Distance_NP=abs(np_score-EstimatedIdealPoint),##Calculate difference between district and legislator ideal point
                                     RanClean=as.factor(RanClean))##Create indicator for whether district is marginal

## Merge Clean to unclean data after calculating ideological distances to district 
clean_mrpdist<-mrp_distcalc%>%filter( RanClean==1)%>%
  mutate(CleanDistance=Distance_NP,CleanIdeology=np_score)%>%
  dplyr::select(-np_score)
unclean_mrpdist<-mrp_distcalc%>%filter( RanClean==0)%>%
  mutate(UncleanDistance=Distance_NP,UncleanIdeology=np_score)%>%
  dplyr::select(-np_score)

## Merge clean and unclean ideological distance scores by year, district, and party,

mrp_paired_test<-left_join(clean_mrpdist, unclean_mrpdist, by=c("Election_Year"="Election_Year",
                                                                "District"="District",
                                                                "party"="party"))%>%
  ## Also add indicator for whether clean elections legislator is more ideologically distant 
  ## for binomial test below
  filter(is.na(CleanDistance)==FALSE & is.na(UncleanDistance)==FALSE)%>%
  mutate(CLMoreExtreme=ifelse(CleanDistance>UncleanDistance, 1, 0))


binom.test(sum(mrp_paired_test$CLMoreExtreme, na.rm = TRUE),
           sum(!is.na(mrp_paired_test$CLMoreExtreme)),(1/2))


####SI-A2 Valence Analysis####

## Do public financing candidates receive higher voteshare?

## Need to use election_allres data because this is aggregated at the contest
## level and also contains counts for clean candidates at the district level.

valence<-readRDS("election_allres.RDS")%>%
  
  ## Drop Arizona house districts which are multi-member, hard to map
  ## candidate public financing status onto voteshare in two candidate context and
  ## have results be comparable to single member districts
  
  filter(!(sab=="AZ" & sen==0))%>%
  ## If district-year observation doesn't have any Clean candidates, will have a
  ## NA from count_district, so we need to convert NAs to zero.
  
  mutate(CleanRepub=ifelse(is.na(CleanRepub)==TRUE, 0, CleanRepub),
         CleanDem=ifelse(is.na(CleanDem)==TRUE, 0, CleanDem),
         CleanOth=ifelse(is.na(CleanOth)==TRUE,0,CleanOth),
         ME_2000CENSUS=ifelse(sab=="ME" & year >=2004 & year <=2012, 1, 0),##Create indicator for ME 2000 Census districts
         CT_2000CENSUS=ifelse(sab=="CT" & year>=2008 & year <=2010,1,0),##Create indicator for CT 2000 Census Districits
         AZ_2000CENSUS=ifelse(sab=="AZ" & year>=2004 & year <=2010,1,0),##Create indicator for AZ 2000 Census Districits
         CENSUS2000=ifelse(ME_2000CENSUS==1 | CT_2000CENSUS==1 |AZ_2000CENSUS==1, 1, 0),##Create indicator for 2000 Census districts for combined analysis
         ME_2010CENSUS=ifelse(sab=="ME" & year >=2014, 1, 0),##Create indicator for ME 2010 Census districts
         CT_2010CENSUS=ifelse(sab=="CT" & year>=2012,1,0),##Create indicator for CT 2010 Census Districits
         AZ_2010CENSUS=ifelse(sab=="AZ" & year>=2012,1,0),##Create indicator for AZ 2010 Census Districits
         CENSUS2010=ifelse(ME_2010CENSUS==1 | CT_2010CENSUS==1 |AZ_2010CENSUS==1, 1, 0),##Create indicator for 2000 Census districts for combined analysis
         ## Create indicator for whether observation is in 2000 and 2010 Census district lines, and then indicator
         ## for both (CensusLines)
         Census_Group=ifelse(CENSUS2010==1, "Census2010", NA),
         Census_Group=ifelse(CENSUS2000==1, "Census2000",Census_Group),
         CensusLines=ifelse(is.na(Census_Group)==TRUE,0,1),
         ## Create State-District-Chamber-Redistricting cycle factor level
         UniqueDistrictChamber=ifelse(sen==1, paste0("Senate", dno), paste0("House", dno)),##Create House and Senate District specific indicator variables
         UniqueDistrict=ifelse(sab=="CT", paste0("CT", UniqueDistrictChamber), paste0("ME",UniqueDistrictChamber)),##Create CT and ME district specific indicators
         UniqueDistrict=ifelse(sab=="AZ", paste0("AZ", UniqueDistrictChamber),UniqueDistrict),##Create AZ district specific indicators
         UniqueDistrict_CensusGroup=as.factor(paste0(UniqueDistrict, Census_Group)),
         ## Create chamber indicators
         Chamber=as.factor(ifelse(sen==1, "Senate", "House")),
         ## Create time variable within redistricting cycle for each state
         RedistTime=ifelse(sab=="ME" & year %in%c(2004, 2006, 2008, 2010,2012),year-2002, NA),
         RedistTime=ifelse(sab=="ME" & year %in%c(2014, 2016),year-2012, RedistTime),
         RedistTime=ifelse(sab=="AZ" & year %in%c(2004, 2006, 2008, 2010),year-2002, RedistTime),
         RedistTime=ifelse(sab=="AZ" & year %in%c(2012, 2014, 2016),year-2010, RedistTime),
         RedistTime=ifelse(sab=="CT" & year %in%c(2002, 2004, 2006, 2008, 2010),year-2000, RedistTime),
         RedistTime=ifelse(sab=="CT" & year %in%c(2012, 2014, 2016),year-2010, RedistTime),
         ## Create party two-way voteshare variable
         RepVS=rvote/(dvote+rvote),
         DemVS=dvote/(dvote+rvote),
         ## Create variable with factor levels for CleanRep/CleanDems, set No Clean as base case
         CleanBoth=ifelse(CleanRepub==1 & CleanDem==1,1,0),
         CleanFactor=as.factor(case_when(CleanDem==0 & CleanRepub==0 ~"No Clean",
                                         CleanDem==1 & CleanRepub==0 ~"Clean Dem",
                                         CleanDem==0 & CleanRepub==1 ~"Clean Repub",
                                         CleanDem==1 & CleanRepub==1 ~"BothClean")),
         CleanFactor=fct_relevel(CleanFactor, "No Clean","Clean Repub","Clean Dem", "BothClean"),
         year=as.factor(year), sab=as.factor(sab))


## Valence Analysis: Regress Republican Voteshare on Indicators for party contestation
## and incumbency. Include district and year x state fixed effects. Cluster standard errors
## by redistricting cycle specific standard errors.
valence_mod_rep<-felm(RepVS~CleanFactor+dinc+rinc+oinc+ocand+dcand+rcand|
                        year:sab+UniqueDistrict_CensusGroup|0|UniqueDistrict_CensusGroup, data=valence,
                      subset=CensusLines==1)
summary(valence_mod_rep)

## Valence Analysis: Regress Republican Voteshare on Indicators, add district x
## redistricting cycle time trend to previous specification, remove time fixed
## effect, add state fixed effect

valence_mod_rep_tt<-felm(RepVS~CleanFactor+dinc+rinc+oinc+ocand+dcand+rcand|
                           RedistTime:UniqueDistrict_CensusGroup+sab|0|UniqueDistrict_CensusGroup, data=valence,
                         subset=CensusLines==1)
summary(valence_mod_rep_tt)


## Create latex table for code
stargazer(valence_mod_rep,valence_mod_rep_tt,
          model.names = FALSE, model.numbers = TRUE,
          dep.var.labels.include = FALSE,
          omit=c("rinc","dinc","oinc","rcand","dcand", "ocand"),
          dep.var.labels = c("Dynamic CFScore Distance"),
          column.labels   = c("Republican Voteshare"),
          column.separate = c(2),
          star.cutoffs = c(0.05, 0.01, 0.001),
          style = 'apsr',
          table.placement = "H",
          title=c("Public-Financing Valence Advantage: Two Way Fixed Effects 2004-2016"),
          label=c("tab:Valence"),
          notes = c("Notes: Standard errors clustered", " by district in parentheses."),
          covariate.labels =c("Public Financing Republican","Public Financing Democrat","Both Use Public Financing"),
          add.lines = list(c("District Fixed Effects", "\\checkmark", "\\checkmark"),
                           c("Election Year Fixed Effects", "\\checkmark",""),
                           c("State by Year Fixed Effects", "\\checkmark",""),
                           c("State Fixed Effect","","\\checkmark"),
                           c("District x Time Effects", "","\\checkmark"),
                           c("Party Contestation Controls", "\\checkmark", "\\checkmark"),
                           c("Party Incumbency Controls", "\\checkmark", "\\checkmark")),
          multicolumn = FALSE,
          omit.stat = c("rsq","adj.rsq","f","ser"))



####SI-A3 Evaluate differences between legislators with and without CFScores####
## Regress NP-Score on indicator for whether a legislator had a dynamic CFScore.
## Include party, state, and year fixed effects, cluster SEs by state.
## No significant difference in legislator ideology by CF-Score missingness
Missing<-felm(NP_Score~HasDynamicCF|party+sab+year|0|sab, data=can_fe)
summary(Missing)

stargazer(Missing,
          model.names = FALSE, model.numbers = TRUE,
          dep.var.labels.include = FALSE,
          dep.var.labels = c("NP_Score"),
          column.labels   = c("All States"),
          column.separate = c(1),
          star.cutoffs = c(0.05, 0.01, 0.001),
          style = 'apsr',
          table.placement = "H",
          title=c("Difference in NP-Score Among Legislators with Missing Dynamic CF-Score: Arizona, Connecticut, Maine 2000-2016"),
          label=c("tab:Missing"),
          notes = c("Notes: Standard errors clustered", " by state in parentheses."),
          covariate.labels =c("Has Dynamic CFScore"),
          add.lines = list(c("State Fixed Effects", "\\checkmark"),
                           c("Party Fixed Effects", "\\checkmark"),
                           c("Election Year Fixed Effects", "\\checkmark")),
          multicolumn = FALSE,
          omit.stat = c("rsq","adj.rsq","f","ser"))

####SI-A4 Interaction Effects Analysis####

## Interact candidate incumbency status with clean election status
incumb_cfFit<-function(df){felm(Distance_CFDyn~CleanYear*Incumbent|UniqueDistrict_CensusGroup+year|0|UniqueDistrict_CensusGroup,
                data=df, subset=CensusLines==1 & HasDistanceCFDyn==1)}
incumb_cf<-EPMV(can_feSims, incumb_cfFit)


## Incumbency marginal effects plot
incumb_cf_MarginalEffect<-plot_model(incumb_cf)$data%>%
  mutate(term=recode(term,"Incumbent" ="Privately Funded Incumbent",
                     "CleanYear"="Publicly Funded Challenger",
                     "CleanYear:Incumbent"="Publicly Funded Incumbent"),
         term=as.factor(term),
         term=fct_relevel(term, "Privately Funded Incumbent", "Publicly Funded Incumbent",
                          "Publicly Funded Challenger"))

ggplot(incumb_cf_MarginalEffect, aes(x=term,y=estimate))+theme_classic()+
  geom_point(size=3)+ylim(-.5, .5)+
  geom_errorbar(aes(ymin=conf.low,ymax=conf.high,
                    width=.5), size=1)+ylab("Marginal Effect on Ideological Distance")+
  geom_hline(yintercept=0, color="red", lty=2)+
  scale_x_discrete(labels = wrap_format(10))+
  xlab("")+labs(caption="Marginal effects calculated using R SjPlot package.
                Horizontal lines are 95% confidence intervals")+coord_flip()+
  theme(axis.text =element_text(size=16, face="bold"),axis.title=element_text(size=16,face="bold"),
        text=element_text(size=16, face="bold"))


## Subset to major party and interact party with CleanCandidateIndicator
party_cfFit<-function(df){felm(Distance_CFDyn~CleanYear*RepubIndicator|UniqueDistrict_CensusGroup+year|0|UniqueDistrict_CensusGroup,
               data=df, subset=CensusLines==1 & HasDistanceCFDyn==1 &party %in%c(100,200))}
party_cf<-EPMV(can_feSims, party_cfFit)

party_cf_MarginalEffect<-plot_model(party_cf)$data%>%
  mutate(term=recode(term,"RepubIndicator" ="Privately Funded Republican",
                     "CleanYear"="Publicly Funded Democrat",
                     "CleanYear:RepubIndicator"="Publicly Funded Republican"),
         term=str_wrap(term, width=10))

ggplot(party_cf_MarginalEffect, aes(x=term,y=estimate))+theme_classic()+
  geom_point(size=3)+ylim(-.5, .5)+
  geom_errorbar(aes(ymin=conf.low,ymax=conf.high,
                    width=.5), size=1)+ylab("Marginal Effect on Ideological Distance")+
  geom_hline(yintercept=0, color="red", lty=2)+
  xlab("")+labs(caption="Marginal effects calculated using R SjPlot package.
                Horizontal lines are 95% confidence intervals")+coord_flip()+
  theme(axis.text =element_text(size=16, face="bold"),axis.title=element_text(size=16,face="bold"),
        text=element_text(size=16, face="bold"))

## Competitive Election (+|-10% vote share) Interaction Analysis
# Relevel factor to examine different base cases
# can_fe<-can_fe%>%mutate(CompetitiveInteraction=fct_relevel(CompetitiveInteraction,
#                                        c("Competitive1CleanYear0",
#                                          "Competitive0CleanYear0",
#                                          "Competitive1CleanYear1",
#                                          "Competitive0CleanYear1")))
compet_cfFit<-function(df){felm(Distance_CFDyn~CompetitiveInteraction|UniqueDistrict_CensusGroup+year|0|UniqueDistrict_CensusGroup,
                data=df, subset=CensusLines==1 & HasDistanceCFDyn==1)}
compet_cf<-EPMV(can_feSims, compet_cfFit)
compet_cf_MarginalEffect<-plot_model(compet_cf)$data%>%
  mutate(term=recode(term,"CompetitiveInteractionCompetitive1CleanYear0" ="Privately Funded Candidate Marginal District",
                     "CompetitiveInteractionCompetitive0CleanYear1"="Publicly Funded Candidate Safe District",
                     "CompetitiveInteractionCompetitive1CleanYear1"="Publicly Funded Candidate Marginal District"),
         term=str_wrap(term, width=10))


ggplot(compet_cf_MarginalEffect, aes(x=term,y=estimate))+theme_classic()+
  geom_point(size=3)+ylim(-.5, .5)+
  geom_errorbar(aes(ymin=conf.low,ymax=conf.high,
                    width=.5), size=1)+ylab("Marginal Effect on Ideological Distance")+
  geom_hline(yintercept=0, color="red", lty=2)+
  xlab("")+labs(caption="Marginal effects calculated using R SjPlot package.
                Horizontal lines are 95% confidence intervals")+coord_flip()+
  theme(axis.text =element_text(size=16, face="bold"),axis.title=element_text(size=16,face="bold"),
        text=element_text(size=16, face="bold"))

## Calculate what pct of districts in data exhibit intra-redistricting cycle change
changeDistricts<-can_fe%>%filter(CensusLines==1 & HasDistanceCFDyn==1)%>%
  dplyr::select(UniqueDistrict_CensusGroup,CompetitiveElection)%>%
  distinct()%>%
  group_by(UniqueDistrict_CensusGroup,CompetitiveElection)%>%
  count()%>%
  group_by(UniqueDistrict_CensusGroup)%>%count()%>%filter(n>1)

totalDistricts<-can_fe%>%filter(CensusLines==1 & HasDistanceCFDyn==1)%>%
  dplyr::select(UniqueDistrict_CensusGroup)%>%
  distinct()%>%group_by(UniqueDistrict_CensusGroup)%>%count()

paste(round(nrow(changeDistricts)/nrow(totalDistricts)*100), 
      "% of redistricting specific cycle districts see a change in competitiveness in our sample", sep="")



####Table SI-A5 State by State Analysis####
me_cfFit<-function(df){felm(Distance_CFDyn~CleanYear|UniqueDistrict_CensusGroup+year|0|UniqueDistrict_CensusGroup,
                            data=df, subset=CensusLines==1 & sab=="ME" & HasDistanceCFDyn==1)}
me_cf<-EPMV(can_feSims,me_cfFit)

ct_cfFit<-function(df){felm(Distance_CFDyn~CleanYear|UniqueDistrict_CensusGroup+year|0|UniqueDistrict_CensusGroup,
                            data=df, subset=CensusLines==1 & sab=="CT" & HasDistanceCFDyn==1)}
ct_cf<-EPMV(can_feSims,ct_cfFit)

az_cfFit<-function(df){felm(Distance_CFDyn~CleanYear|UniqueDistrict_CensusGroup+year|0|UniqueDistrict_CensusGroup,
                            data=df, subset=CensusLines==1 & sab=="AZ" & HasDistanceCFDyn==1)}
az_cf<-EPMV(can_feSims,az_cfFit)



## Effect size interpretation
## Quantile of ideological distance
quantile(can_fe$Distance_CFDyn, na.rm = TRUE, probs=seq(0,1,.1))
sd(can_fe$Distance_CFDyn, na.rm = TRUE)

## Calculate pooled variance
## Count total numbers observations for each state
nObs<-can_fe%>%filter(CensusLines==1 & HasDistanceCFDyn==1)%>%
  group_by(sab)%>%count()%>%pull()

## Average coefficients
AvgCoef<-(az_cf$beta[1]+ct_cf$beta[1]+me_cf$beta[1])/3

## Calculate pooled standard errors

pooledSE <- sqrt((((nObs[1] - 1) * az_cf$cse[1] ^ 2) + ((nObs[2] - 1) * ct_cf$cse[1] ^2)
                  + ((nObs[3] - 1) * me_cf$cse[1] ^ 2))
                 /
                   (sum(nObs) - 3))

## Averaged coef T-stat
tstat = AvgCoef/pooledSE
df = (sum(nObs) - 3)
1-pt(tstat,df=df) #p-value

## Create latex table for state by state regression
stargazer(Table2Column1,az_cf, ct_cf, me_cf,
          model.names = FALSE, model.numbers = TRUE,
          dep.var.labels.include = FALSE,
          dep.var.labels = c("Dynamic CFscore Distance"),
          column.labels   = c("All States","Arizona", "Connecticut","Maine"),
          column.separate = c(1,1,1,1),
          star.cutoffs = c(0.05, 0.01, 0.001),
          omit=c("party"),
          style = 'apsr',
          table.placement = "H",
          title=c("Candidate Ideological Distance to District: Arizona, Connecticut, Maine 2004-2016 Two Way Fixed Effects"),
          label=c("tab:CTMEFE"),
          notes = c("Notes: Standard errors clustered", " by district in parentheses."),
          covariate.labels =c("Public Financing Candidate"),
          add.lines = list(c("District Fixed Effects", "\\checkmark", "\\checkmark", "\\checkmark", "\\checkmark"),
                           c("Election Year Fixed Effects", "\\checkmark", "\\checkmark", "\\checkmark", "\\checkmark")),
          multicolumn = FALSE,
          omit.stat = c("rsq","adj.rsq","f","ser"))



####SI-A5 IPW Analysis####

## Calculate IPW so that each state's observations count equally
## and re-estimate Table 2.


## Save weights
weightsPrint<-weights_CFDyn%>%pull()
weightsLabel<-paste("AZ:", round(weightsPrint[1],2), "CT:", round(weightsPrint[2],2), "ME:",
                    round(weightsPrint[3],2))

## Pooled States: Dynamic Distance Estimate
ipw_all_dynFit<-function(df){felm(Distance_CFDyn~CleanYear|UniqueDistrict_CensusGroup+year|0|UniqueDistrict_CensusGroup,
              data=df, weights=df$IPW_CFDyn)}
ipw_all_dyn<-EPMV(can_feSims_weights_dyn, ipw_all_dynFit)

## Pooled States: Dynamic Distance Estimate Party Fixed Effects
ipw_all_dyn_partyFit<-function(df){felm(Distance_CFDyn~CleanYear|UniqueDistrict_CensusGroup+year+party|0|UniqueDistrict_CensusGroup,
                    data=df, weights=df$IPW_CFDyn)}
ipw_all_dyn_party<-EPMV(can_feSims_weights_dyn, ipw_all_dyn_partyFit)

## Pooled States: Dynamic Distance Estimate Party Fixed Effects Time Trend
ipw_all_dyn_party_trendFit<-function(df){felm(Distance_CFDyn~CleanYear|UniqueDistrict_CensusGroup:RedistTime+party|0|UniqueDistrict_CensusGroup,
                          data=df, weights=df$IPW_CFDyn)}
ipw_all_dyn_party_trend<-EPMV(can_feSims_weights_dyn, ipw_all_dyn_party_trendFit)


## Pooled States: Non-Dynamic Distance Estimate
ipw_all_ndynFit<-function(df){felm(Distance_CFnonDyn~CleanYear|UniqueDistrict_CensusGroup+year|0|UniqueDistrict_CensusGroup,
               data=df, weights=df$IPW_CFnonDyn)}
ipw_all_ndyn<-EPMV(can_feSims_weights_ndyn,ipw_all_ndynFit)

## Pooled States: Non-Dynamic Distance Estimate Party Fixed Effects
ipw_all_ndyn_partyFit<-function(df){felm(Distance_CFnonDyn~CleanYear|UniqueDistrict_CensusGroup+year+party|0|UniqueDistrict_CensusGroup,
                     data=df, weights=df$IPW_CFnonDyn)}
ipw_all_ndyn_party<-EPMV(can_feSims_weights_ndyn,ipw_all_ndyn_partyFit)

## Pooled States: Non-Dynamic Distance Estimate Party Fixed Effects Time Trend
ipw_all_ndyn_party_trendFit<-function(df){felm(Distance_CFnonDyn~CleanYear|UniqueDistrict_CensusGroup:RedistTime+party|0|UniqueDistrict_CensusGroup,
                           data=df, weights=df$IPW_CFnonDyn)}
ipw_all_ndyn_party_trend<-EPMV(can_feSims_weights_ndyn,ipw_all_ndyn_party_trendFit)

## Create latex table
stargazer(ipw_all_dyn,ipw_all_dyn_party,ipw_all_dyn_party_trend,ipw_all_ndyn,ipw_all_ndyn_party,
          ipw_all_ndyn_party_trend,
          model.names = FALSE, model.numbers = TRUE,
          dep.var.labels.include = FALSE,
          column.labels   = c("Dynamic CFscore Distance", "Stable CFscore Distance"),
          column.separate = c(3,3),
          star.cutoffs = c(0.05, 0.01, 0.001),
          omit=c("party"),
          style = 'apsr',
          table.placement = "H",
          title=c("Candidate Ideological Distance to District Pooled States 2004-2016 Two Way Fixed Effects, Weighted by State"),
          label=c("tab:WeightedResults"),
          notes = c("Notes: Standard errors clustered", " by district in parentheses.",
                    weightsLabel),
          covariate.labels =c("Public Financing Candidate"),
          add.lines = list(c("District Fixed Effects", "\\checkmark", "\\checkmark", "\\checkmark", "\\checkmark", "\\checkmark", "\\checkmark"),
                           c("Election Year Fixed Effects", "\\checkmark", "\\checkmark", "", "\\checkmark", "\\checkmark", ""),
                           c("Party Fixed Effects", "", "\\checkmark", "\\checkmark", "", "\\checkmark", "\\checkmark"),
                           c("District x Time Effects", "", "", "\\checkmark", "", "", "\\checkmark")),
          
          multicolumn = FALSE,
          omit.stat = c("rsq","adj.rsq","f","ser"))


####SI-A6 Legislator Tenure analysis####

## Add tenure variable to Table 2 models
## Pooled States: Dynamic distance estimate, district and year fixed effects,
## Standard errors clustered on redistricting cycle specific districts
## Subset data to 2000 and 2010 redistricting cycle data and observations
## which have a dynamic CF distance estimate
tenure_all_dynFit<-function(df){felm(Distance_CFDyn~CleanYear+tenure1|UniqueDistrict_CensusGroup+year|0|UniqueDistrict_CensusGroup,
              data=df, subset=CensusLines==1 & HasDistanceCFDyn==1)}
tenure_all_dyn<-EPMV(can_feSims,tenure_all_dynFit)

## Pooled States: Add party fixed effects
tenure_all_dyn_partyFit<-function(df){felm(Distance_CFDyn~CleanYear+tenure1|UniqueDistrict_CensusGroup+year+party|0|UniqueDistrict_CensusGroup,
                    data=df, subset=CensusLines==1 & HasDistanceCFDyn==1)}
tenure_all_dyn_party<-EPMV(can_feSims, tenure_all_dyn_partyFit)

## Pooled States: add Time Trend
tenure_all_dyn_party_trendFit<-function(df){felm(Distance_CFDyn~CleanYear+tenure1|UniqueDistrict_CensusGroup:RedistTime+party|0|UniqueDistrict_CensusGroup,
                          data=df, subset=CensusLines==1 & HasDistanceCFDyn==1)}
tenure_all_dyn_party_trend<-EPMV(can_feSims, tenure_all_dyn_party_trendFit)

## Pooled States: Non-dynamic distance estimate, district and year fixed effects,
## Standard errors clustered on redistricting cycle specific districts
## Subset data to 2000 and 2010 redistricting cycle data and observations
## which have a stable CF distance estimate
tenure_all_ndynFit<-function(df){felm(Distance_CFnonDyn~CleanYear+tenure1|UniqueDistrict_CensusGroup+year|0|UniqueDistrict_CensusGroup,
               data=df, subset=CensusLines==1 & HasDistanceCFnonDyn==1)}
tenure_all_ndyn<-EPMV(can_feSims, tenure_all_ndynFit)

##Pooled States: Add party fixed effects
tenure_all_ndyn_partyFit<-function(df){felm(Distance_CFnonDyn~CleanYear+tenure1|UniqueDistrict_CensusGroup+year+party|0|UniqueDistrict_CensusGroup,
                     data=df, subset=CensusLines==1 & HasDistanceCFnonDyn==1)}
tenure_all_ndyn_party<-EPMV(can_feSims, tenure_all_ndyn_partyFit)

##Pooled States: Add time trend
tenure_all_ndyn_party_trendFit<-function(df){felm(Distance_CFnonDyn~CleanYear+tenure1|UniqueDistrict_CensusGroup:RedistTime+party|0|UniqueDistrict_CensusGroup,
                           data=df, subset=CensusLines==1 & HasDistanceCFnonDyn==1)}
tenure_all_ndyn_party_trend<-EPMV(can_feSims, tenure_all_ndyn_party_trendFit)

## Create latex table
stargazer(tenure_all_dyn, tenure_all_dyn_party, tenure_all_dyn_party_trend,
          tenure_all_ndyn, tenure_all_ndyn_party, tenure_all_ndyn_party_trend,
          model.names = FALSE, model.numbers = TRUE,
          dep.var.labels.include = FALSE,
          column.labels   = c("Dynamic CFscore Distance", "Stable CFscore Distance"),
          column.separate = c(3,3),
          star.cutoffs = c(0.05, 0.01, 0.001),
          omit=c("party"),
          style = 'apsr',
          table.placement = "H",
          title=c("Candidate Ideological Distance to District Pooled States 2004-2016 Two Way Fixed Effects"),
          label=c("tab:Tenure"),
          notes = c("Notes: Standard errors clustered", " by district in parentheses."),
          covariate.labels =c("Public Financing Candidate", "Candidate Tenure"),
          add.lines = list(c("District Fixed Effects", "\\checkmark", "\\checkmark", "\\checkmark", "\\checkmark", "\\checkmark", "\\checkmark"),
                           c("Election Year Fixed Effects", "\\checkmark", "\\checkmark", "", "\\checkmark", "\\checkmark", ""),
                           c("Party Fixed Effects", "", "\\checkmark", "\\checkmark", "", "\\checkmark", "\\checkmark"),
                           c("District x Time Effects", "", "", "\\checkmark", "", "", "\\checkmark")),
          multicolumn = FALSE,
          omit.stat = c("rsq","adj.rsq","f","ser"))



####SI-A7 DW-DIME Analysis####

## Repeat Table 2 analysis substituting DW-DIME distance estimate for dynamic cf
## estimate Subset data to observations which have both a DW-DIME estimate and
## DistanceCFDyn estimate

##Pooled States: DW-DIME Distance Estimate
all_dwdimeFit<-function(df){felm(Distance_DWDIME~CleanYear|UniqueDistrict_CensusGroup+year|0|UniqueDistrict_CensusGroup,
                 data=df, subset=CensusLines==1 & HasDistanceDWDIME==1 & HasDistanceCFDyn==1)}
all_dwdime<-EPMV(can_feSims, all_dwdimeFit)


## Pooled States: Repeat Table 2 column 1 analysis, subsetting to data which is
## used in DW-DIME estimate for comparison.
all_dwdime_subsetFit<-function(df){felm(Distance_CFDyn~CleanYear|UniqueDistrict_CensusGroup+year|0|UniqueDistrict_CensusGroup,
                     data=df, subset=CensusLines==1& HasDistanceDWDIME==1& HasDistanceCFDyn==1)}
all_dwdime_subset<-EPMV(can_feSims, all_dwdime_subsetFit)


## Create latex table
stargazer(all_dwdime,all_dwdime_subset,
          model.names = FALSE, model.numbers = TRUE,
          dep.var.labels.include = FALSE,
          dep.var.labels = c("Dynamic CFScore Distance"),
          column.labels   = c("DW-DIME Distance", "Dynamic CFScore Distance"),
          column.separate = c(1,1),
          star.cutoffs = c(0.05, 0.01, 0.001),
          style = 'apsr',
          table.placement = "H",
          title=c("Candidate Ideological Distance to District: DW-DIME and CFScore Two Way Fixed Effects"),
          label=c("tab:DWDIME"),
          notes = c("Notes: Standard errors clustered", " by district in parentheses."),
          covariate.labels =c("Public Financing Candidate"),
          add.lines = list(c("District Fixed Effects", "\\checkmark", "\\checkmark"),
                           c("Election Year Fixed Effects", "\\checkmark", "\\checkmark")),
          multicolumn = FALSE,
          omit.stat = c("rsq","adj.rsq","f","ser"))

## Comparison of Effect sizes
sd(can_fe$Distance_DWDIME, na.rm=TRUE)
sd(can_fe$Distance_CFDyn, na.rm=TRUE)




####SI-A8 Clean From First Run Analysis####

## Repeat Table 2 analysis using public financing status in first run as
## independent variable. All candidates elected prior to 2000 are coded as
## non-CleanFirstRun even if they took public financing in 2000 and in
## subsequent elections.

## Pooled States: Dynamic Distance Estimate
cfr_all_dynFit<-function(df){felm(Distance_CFDyn~CleanFirstRun|UniqueDistrict_CensusGroup+year|0|UniqueDistrict_CensusGroup,
              data=df, subset=CensusLines==1 & HasDistanceCFDyn==1)}
cfr_all_dyn<-EPMV(can_feSims, cfr_all_dynFit)

## Pooled States: Dynamic Distance Estimate Party Fixed Effects
cfr_all_dyn_partyFit<-function(df){felm(Distance_CFDyn~CleanFirstRun|UniqueDistrict_CensusGroup+year+party|0|UniqueDistrict_CensusGroup,
                    data=df, subset=CensusLines==1 & HasDistanceCFDyn==1)}
cfr_all_dyn_party<-EPMV(can_feSims, cfr_all_dyn_partyFit)

## Pooled States: Dynamic Distance Estimate Party Fixed Effects Time Trend
cfr_all_dyn_party_trendFit<-function(df){felm(Distance_CFDyn~CleanFirstRun|UniqueDistrict_CensusGroup:RedistTime+party|0|UniqueDistrict_CensusGroup,
                          data=df, subset=CensusLines==1 & HasDistanceCFDyn==1)}
cfr_all_dyn_party_trend<-EPMV(can_feSims, cfr_all_dyn_party_trendFit)

## Pooled States: Non-Dynamic Distance Estimate
cfr_all_ndynFit<-function(df){felm(Distance_CFnonDyn~CleanFirstRun|UniqueDistrict_CensusGroup+year|0|UniqueDistrict_CensusGroup,
               data=df, subset=CensusLines==1 & HasDistanceCFnonDyn==1)}
cfr_all_ndyn<-EPMV(can_feSims, cfr_all_ndynFit)

## Pooled States: Non-Dynamic Distance Estimate Party Fixed Effects
cfr_all_ndyn_partyFit<-function(df){felm(Distance_CFnonDyn~CleanFirstRun|UniqueDistrict_CensusGroup+year+party|0|UniqueDistrict_CensusGroup,
                     data=df, subset=CensusLines==1 & HasDistanceCFnonDyn==1)}
cfr_all_ndyn_party<-EPMV(can_feSims, cfr_all_ndyn_partyFit)

## Pooled States: Non-Dynamic Distance Estimate Party Fixed Effects Time Trend
cfr_all_ndyn_party_trendFit<-function(df){felm(Distance_CFnonDyn~CleanFirstRun|UniqueDistrict_CensusGroup:RedistTime+party|0|UniqueDistrict_CensusGroup,
                           data=df, subset=CensusLines==1 & HasDistanceCFnonDyn==1)}
cfr_all_ndyn_party_trend<-EPMV(can_feSims, cfr_all_ndyn_party_trendFit)

## Create latex table
stargazer(cfr_all_dyn, cfr_all_dyn_party, cfr_all_dyn_party_trend,
          cfr_all_ndyn, cfr_all_ndyn_party, cfr_all_ndyn_party_trend,
          model.names = FALSE, model.numbers = TRUE,
          dep.var.labels.include = FALSE,
          column.labels   = c("Dynamic CFScore Distance", "Stable CFScore Distance"),
          column.separate = c(3,3),
          star.cutoffs = c(0.05, 0.01, 0.001),
          omit=c("party"),
          style = 'apsr',
          table.placement = "H",
          title=c("Candidate Ideological Distance to District Pooled States 2004-2016 Two Way Fixed Effects"),
          label=c("tab:CoreResults_alt"),
          notes = c("Notes: Standard errors clustered", " by district in parentheses."),
          covariate.labels =c("Publicly Financed First Campaign"),
          add.lines = list(c("District Fixed Effects", "\\checkmark", "\\checkmark", "\\checkmark", "\\checkmark", "\\checkmark", "\\checkmark"),
                           c("Election Year Fixed Effects", "\\checkmark", "\\checkmark", "", "\\checkmark", "\\checkmark", ""),
                           c("Party Fixed Effects", "", "\\checkmark", "\\checkmark", "", "\\checkmark", "\\checkmark"),
                           c("District x Time Effects", "", "", "\\checkmark", "", "", "\\checkmark")),
          
          multicolumn = FALSE,
          omit.stat = c("rsq","adj.rsq","f","ser"))





####FIGURE 1 & FIGURE 2 CODE####

## Read in contest level election results.
election<-readRDS("102slersuoacontest20181024-1.RDS")

## Select Relevant years
election_all<-election %>%
  filter( (year>=2000 &  sab == "ME") | (year>=2008 & sab=="CT") |
            (year>=2000 &  sab=="AZ") )%>%##Select relevant observations
  select(year, sab, sen, dno, dcand, eseats,
         rcand, ocand, rinc, dinc, oinc)%>%
  ## Calculate total numbers of candidates in each contest.
  ## Klarner divides multi-member district rcand/dcand/ocand by the number of seats, so
  ## have to multiply Arizona house seats by 2 to get total candidates
  mutate(rcand=ifelse(sab=="AZ"& sen==0, rcand*2, rcand),
         ocand=ifelse(sab=="AZ"& sen==0, ocand*2, ocand),
         dcand=ifelse(sab=="AZ"& sen==0, dcand*2, dcand),
         ## Use eseats variable to calculate number of incumbents in all districts
         ## because of multi-member problem noted above
         IncumbentInRace=rinc*eseats+dinc*eseats+oinc*eseats,
         ## Calculate total candidates and number of challengers within district-year observations
         TotalCan=rcand+ocand+dcand, Challengers=TotalCan-IncumbentInRace)



## Summarize candidate counts by party
election_all<-election_all%>%group_by(sab, year, sen, dno)%>%
  summarize(TotalRepCan=sum(rcand, na.rm = TRUE),
            TotalDemCan=sum(dcand, na.rm = TRUE))%>%
  gather("Party", "TotalCandidates", 5:6)%>%
  mutate(Party=recode(Party, "TotalRepCan"="REPUBLICAN",
                      "TotalDemCan"="DEMOCRATIC"))%>%
  group_by(sab, year, Party)%>%
  summarize(TotalCandidates=sum(TotalCandidates))

## Get public financing candidate counts
ftm<-read.csv("PublicFundingFTM.csv", stringsAsFactors = FALSE)


count_district<-ftm %>%
  mutate(House=ifelse(str_detect(Office, "HOUSE"), 1, 0),
         Senate=ifelse(str_detect(Office, "SENATE"),1,0))%>%
  filter( ( (State=="ME") | (State=="CT") | (State=="AZ") ) & ( House==1 | Senate==1)
          & Year <=2016 &  Year %!in% c(2011, 2013, 2015) &
            ElectionStatus %!in% c("LOST-PRIMARY RUNOFF",
                                   "LOST-PRIMARY", "DISQUALIFIED-GENERAL",
                                   "WITHDREW-GENERAL"))%>%
  mutate(District=substr(Office, 17,18), District=ifelse(Senate==1,substr(Office, 18,19),District),
         District=as.numeric(as.character(District)),
         CleanWinner=ifelse(ElectionStatus=="WON-GENERAL", 1,0),
         CleanDem=ifelse(GeneralParty=="DEMOCRATIC", 1,0),
         CleanChallenger=ifelse(IncumbencyStatus!="INCUMBENT",1,0),
         CleanIncumbent=ifelse(IncumbencyStatus=="INCUMBENT",1,0),
         CleanRepub=ifelse(GeneralParty=="REPUBLICAN", 1,0))%>%
  arrange(Senate, District, Year)%>%
  group_by(State, Year, GeneralParty)%>%
  summarize(TotalCleanCan=n(), TotalCleanWinners=sum(CleanWinner), TotalCleanIncumbents=sum(CleanIncumbent),
            TotalCleanChallengers=sum(CleanChallenger))%>%
  filter(GeneralParty %in% c("DEMOCRATIC", "REPUBLICAN"))

## Create seats dataframe to calculate percentage of legislators using public financing
## below
seats<-data_frame(State=c("AZ", "CT", "ME"), Seats=c(90, 187, 186 ))

## Join election total candidates to clean candidate data
partplot<-left_join(election_all, count_district, by=c("sab"="State", "year"="Year", "Party"="GeneralParty"))

## Calculate Party Pct, Pct of legislators using Clean Elections
party_pct<-partplot%>%mutate(PartyCanPart=100*TotalCleanCan/TotalCandidates)%>%
  spread(Party, PartyCanPart)
dem_year_pct<-party_pct%>%mutate(DemocratPct=DEMOCRATIC)%>%select(sab, year, DemocratPct)%>%
  filter(is.na(DemocratPct)==FALSE)
rep_year_pct<-party_pct%>%mutate(RepublicanPct=REPUBLICAN)%>%select(sab, year, RepublicanPct)%>%
  filter(is.na(RepublicanPct)==FALSE)
leg_pct<-partplot%>%group_by(sab, year)%>%summarize(TotalCleanWinners=sum(TotalCleanWinners))%>%
  left_join(seats, by=c("sab"="State"))%>%mutate(LegislatorPct=100*TotalCleanWinners/Seats)%>%
  select(sab, year, LegislatorPct)

## Combine for plotting
plotframe<-dem_year_pct%>%left_join(rep_year_pct, by=c("sab"="sab", "year"="year"))%>%
  left_join(leg_pct, by=c("sab"="sab", "year"="year"))%>%
  gather("Category", "Pct", 3:5)%>%
  ungroup()%>%
  mutate(Category=recode(Category,"DemocratPct"="Democratic Candidates",
                         "RepublicanPct"="Republican Candidates",
                         "LegislatorPct"="Legislators"),
         sab=trimws(sab),
         sab=recode(sab, "CT"="Connecticut", "AZ"="Arizona", "ME"="Maine"))

## Plot Republican, Democrat, and Legislator participation
ggplot(plotframe, aes(x=year, y=Pct/100, group=Category)) +
  geom_line(aes(color=Category))+ylab("Pct. Participation in Public Campaign Financing")+
  geom_point(aes(color=Category))+xlab("Election Year")+
  scale_color_manual(values=c("DodgerBlue", "Black","IndianRed"))+xlim(2000,2016)+
  scale_x_continuous(breaks = seq(2000, 2016, by=2))+theme_classic()+ylim(0,100)+
  scale_y_continuous(labels=percent)+
  theme(axis.text=element_text(size=24),axis.title=element_text(size=20,face="bold"),
        axis.text.x = element_text(angle = 90, hjust = 1),legend.position = c(.75, .25),
        legend.title=element_blank(),
        text=element_text(size=24))+facet_wrap(~sab, nrow=2)

## Calculate program participation among challengers and incumbents
IC_plot<-count_district%>%group_by(State, Year)%>%
  summarize(ChallengerShare=100*sum(TotalCleanChallengers)/sum(TotalCleanCan),
            IncumbentShare=100*sum(TotalCleanIncumbents)/sum(TotalCleanCan))%>%
  ungroup()%>%
  gather("Category", "Pct", 3:4)%>%mutate(Category=recode(Category, "ChallengerShare"="Challenger Share",
                                                          "IncumbentShare" ="Incumbent Share"),
                                          State=recode(State, "ME"="Maine", "AZ"="Arizona", "CT"="Connecticut"))

## Plot program participation among challengers and incumbents
ggplot(IC_plot, aes(x=Year, y=Pct/100, group=Category)) +
  geom_line(aes(color=Category))+ylab("Pct. of All Public Financing Candidates")+
  geom_point(aes(color=Category))+xlab("Election Year")+
  scale_color_manual(values=c("DodgerBlue","IndianRed"))+xlim(2000,2016)+
  scale_x_continuous(breaks = seq(2000, 2016, by=2))+theme_classic()+ylim(0,100)+
  scale_y_continuous(labels=percent)+
  theme(axis.text=element_text(size=24),axis.title=element_text(size=20,face="bold"),
        axis.text.x = element_text(angle = 90, hjust = 1),legend.position = c(.75, .25),
        legend.title=element_blank(),
        text=element_text(size=24))+facet_wrap(~State, nrow=2)







####Code scratchpad####




## EP univariate using broom. Had to stop using broom
## because couldn't figure out tidy way to simulate
## from multivariate model in broom
## Next rerun TotalReps models using broom setup all in one sweep
## with simulated ideological distances for each Table-Column in the paper, where relevant. 
## Then use the residual standard deviation, clustervcv and mvrnorm to 
## draw coefficients/estimates from the Two-Way FE/T-Test, and store.
## Turn this into a function to be called by each specific model formula function

EP<-function(ModelFormula){
  set.seed(02138)
  
  EPResults<-can_feSims%>%
    group_by(Group)%>%nest()%>%
    ## Run felm two-way fe model on each simulated group, which each have a slightly
    ## different value for Distance for each candidate because each group also
    mutate(mod=map(data, ModelFormula),
           ## has different simulated Rogers Regression Slopes/Intercepts
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
           CoefficientDraw=mvrnorm(1, estimate, ResidualStandardDeviation^2*ClusterVCV),
           N=unlist(mod)$N)%>%
    ungroup()%>%dplyr::select(-mod)%>%
    ## Get estimate and 95% CI across all draws
    summarize(Estimate=quantile(CoefficientDraw, .5),
              CILow=quantile(CoefficientDraw,.025),
              CIHigh=quantile(CoefficientDraw, .975),
              SE=sd(CoefficientDraw),
              DF=mean(df), N=mean(N))
  
  return(EPResults)
  
}







frame<-split( can_feSims , f = can_feSims$Group)
## Try to go parallel, can't figure out why this isn't faster
EP<-function(frame, ModelFormula){
foreach(i = 1:1000, .combine=rbind, .packages = c("lfe"),.inorder = FALSE) %dopar% {                                      
                   subMod<-ModelFormula(frame[[i]])
                   store<-data.frame(Run=rep(i,2), Coefs=subMod$coefficients,CoefNames= c(row.names(subMod$coefficients)),
                   ResidualStandardDeviation=subMod$rse*sqrt(subMod$df/rchisq(1, subMod$df)),
                   ClusterVCV=subMod$clustervcv/subMod$rse^2, N=subMod$N, DF= subMod$df)
                   store$CoefficientDraw<-mvrnorm(1, store$Coefs, store$ResidualStandardDeviation^2*store$ClusterVCV)
    }
}

ModelFormula<-function(df){felm(Distance_CFDyn~CleanYear+Incumbent|bonica.rid+year+UniqueDistrict_CensusGroup|0|bonica.rid,
                  data=df, subset=CensusLines==1& PFStatusSwitcher==1 & HasDistanceCFDyn==1)}
store$CoefficientDraw[i]<-mvrnorm(1, subMod$coefficients, ResidualStandardDeviation^2*ClusterVCV)
i<-1
library(foreach)
library(doParallel)
frame<-split( can_feSims , f = can_feSims$Group)
cl <- makeCluster(detectCores())
registerDoParallel(cl)
time = proc.time()
out<-EP(frame, ModelFormula)
print(proc.time()-time)
stopCluster(cl)
beep()

## Sources on scaling covariance matrix, simulating regression uncertainty (mostly working from
## Gelman & Hill 2007)
#http://www.clayford.net/statistics/simulation-to-represent-uncertainty-in-regression-coefficients/
#https://stackoverflow.com/questions/27113456/what-is-the-unscaled-variance-in-rs-linear-model-summary