#########################################################
##                 Mitchell Kilborn                    ##
##                 Arjun Vishwanath                    ##
##    Replication code for Error Propagation Method in ##
##    "Public Money Talks Too: Clean Elections         ##
##      and Representation in State Legislatures"      ##
#########################################################



###################Load Required Libraries#############################
# install required packages if they are missing:
list.of.packages <- c("tidyverse", "stargazer","scales","sjPlot","lfe","stringdist","readstata13",
                      "arm","MASS")
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
# tidyverse     tidyverse    1.3.0


#############################Session Info#######################################

sessionInfo()
# R version 4.0.2 (2020-06-22)
# Platform: x86_64-apple-darwin17.0 (64-bit)
# Running under: macOS Catalina 10.15.6


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
## using arm sim function in Create_can_fe.R code
ideal_points_CF_sims1000<-readRDS("ideal_points_CF_sims1000.RDS")
ideal_points_DWDIME_sims1000<-readRDS("ideal_points_DWDIME_sims1000.RDS")


## Set number of simulations here, can increase when we decide
## this code works
#TotalReps<-1000

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

## Read in npscore t-test data from CreatePairedAZData.R
mrp_distcalc<-readRDS("shor_az.RDS")%>%
  filter(Election_Year>=2004 & party !="X")



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
                  ## Store N, p (total coefs, including those projected out) and DF
                  N=subMod$N, CoefTotal= subMod$p, DF=N-CoefTotal,
                  ResidualStandardDeviation=subMod$rse*sqrt(DF/rchisq(1, DF))) ## Calculate RSD
    
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




#### EP function adapted for T-Test###
## Takes arguments:
## frame: list of dataframes with TotalReps perturbed distance values
## Formula: t-test formula
EP_TTest<-function(frame, Formula){
  ## Create tibble to store results
  Diffs<-tibble(SubEstimate=1:length(frame), DF=NA, Draw=NA)
  ## Iterate through list of dfs with simulated distance coefficients
  for(i in 1:length(frame)){
    Mod<-Formula(frame[[i]])## Run ith t-test 
    Diffs$SubEstimate[i]<-Mod$estimate[[1]]## Store difference
    Diffs$DF[i]<-Mod$parameter[1]## Store DF
    Diffs$Draw[i]<-rnorm(1, Mod$estimate[[1]], Mod$stderr)## Use estimate and SE to draw rnorm
  }
  
  ## Create empty lm object for stargazer output
  outMod<-lm(SubEstimate~DF, data=Diffs)
  
  ## Take center of distribution/SE, and calculate Pval
  Diffs<-Diffs%>%summarize(Estimate=quantile(Draw, .5),SE=sd(Draw),
                           DF=mean(DF))%>%
    mutate(P=2*pt(abs(Estimate/SE),DF,lower.tail=FALSE))
  outMod$coefficients[1]<-Diffs$Estimate
  outMod$SE<-Diffs$SE## Store SE 
  outMod$N<-Diffs$DF+1## Store DF, add back 1 for N 
  outMod$PValue<-Diffs$P## Store p-value calculated from t-distribution
  return(outMod)
}

