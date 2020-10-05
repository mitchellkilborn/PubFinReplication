#########################################################
##                 Mitchell Kilborn                    ##
##                 Arjun Vishwanath                    ##
##    Code for Tables Tables 2, 3, G.1, G.2,           ##
##                  G.3, G.4, G.5.                     ##
##    "Public Money Talks Too: Clean Elections         ##
##      and Representation in State Legislatures"      ##
#########################################################



###################Load Required Libraries#############################

rm(list=ls())
# install required packages if they are missing:
list.of.packages <- c("tidyverse", "stargazer","scales","sjPlot","lfe","stringdist","readstata13")
new.packages <- list.of.packages[!(list.of.packages %in% installed.packages()[,"Package"])];
if(length(new.packages)){install.packages(new.packages)}
lapply(list.of.packages, require, character.only = TRUE)


## Package version check
packinfo <- as.data.frame(installed.packages(fields = c("Package", "Version")))
packinfo[which(packinfo$Package %in% list.of.packages),c("Package", "Version")]

## Package                      Version
## lfe                          2.8-5.1
## readstata13                  0.9.2
## scales                       1.1.1
## sjPlot                       2.8.5
## stargazer                    5.2.2
## stringdist                   0.9.6
## tidyverse                    1.3.0


#############################Session Info#######################################

sessionInfo()
## R version 4.0.2 (2020-06-22)
## Platform: x86_64-apple-darwin17.0 (64-bit)
## Running under: macOS Catalina 10.15.6



####Set up Error Propagation Functions####

## PrepEPFunctions.R prepares the can_fe.RDS and NPResultsBaseFrame.RDS datasets
## for error propagation method described in Appendix F. 
## To run the analyses below, the user must have run this script.
## As the EP functions are computationally intensive, we allow
## the user to set the number of simulations with the TotalReps variable below
## before running PrepEPFunctions.R.
## The manuscript uses 1000 simulations.

TotalReps<-10
source("PrepEPFunctions.R")



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
          notes = c("\\textit{Note}: $\\beta$ \\& SEs reflect error propagation",
                    "method described in SI-A11"),
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
Table3Column2Fit<-function(df){felm(Distance_CFDyn~CleanYear+Incumbent|bonica.rid+year+UniqueDistrict_CensusGroup|0|bonica.rid,
                                    data=df, subset=CensusLines==1& PFStatusSwitcher==1 & HasDistanceCFDyn==1)}
Table3Column2<-EPMV(can_feSims,Table3Column2Fit)

## Subset by party: Democrats, candidate fixed effects, incumbency indicator, SEs clustered by candidate
Table3Column3Fit<-function(df){felm(recipient.cfscore.dyn ~CleanYear+Incumbent|bonica.rid+year+UniqueDistrict_CensusGroup|0|bonica.rid,
                                    data=df, subset=CensusLines==1& PFStatusSwitcher==1 &party==100 & HasDistanceCFDyn==1)}
Table3Column3<-EPMV(can_feSims,Table3Column3Fit)

## Subset by party: Republican, candidate fixed effects, incumbency indicator, SEs clustered by candidate
Table3Column4Fit<-function(df){felm(recipient.cfscore.dyn ~CleanYear+Incumbent|bonica.rid+year+UniqueDistrict_CensusGroup|0|bonica.rid,
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
          notes = c("\\textit{Note}: $\\beta$ \\& SEs reflect error propagation",
                    "method described in SI-A11"),         
          covariate.labels =c("Public Financing Candidate", "Incumbent"),
          add.lines = list(c("District Fixed Effects", "\\checkmark", "\\checkmark","\\checkmark", "\\checkmark"),
                           c("Election Year Fixed Effects", "\\checkmark", "\\checkmark","\\checkmark", "\\checkmark"),
                           c("Candidate Fixed Effects", "\\checkmark", "\\checkmark","\\checkmark", "\\checkmark")),
          multicolumn = FALSE,
          omit.stat = c("rsq","adj.rsq","f","ser"))

####Prepare T-Test Data####
## Tables 4, 5, and 6 use Arizona publicly financed
## and publicly financed observations
## paired by district, party, and year. Need to format data to
## merge these observations while retaining distinct column names


## Subset can_fe dataset to Arizona data only and select relevant variables
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


## Load shor_az.RDS analysis dataset created in CreatePairedAZData.R
shor_az<-readRDS("shor_az.RDS")

## Conduct Paired T-Test for AZ Republican State Legislators
## Positive value of Clean/Unclean ideology T-Test indicates that Clean
## Republicans are more conservative than Unclean Republicans

## Select republicans, create clean and unclean 
## ideology variables, merge variables by district and election year so
## each clean legislator is paired with a privately funded legislator
## or NA if none available in their district-year combination
reps<-shor_az  %>%filter(party=="R")
cleanReps<-reps %>%filter( RanClean==1)%>%mutate(CleanIdeology=np_score)%>%dplyr::select(-np_score)
uncleanReps<-reps %>%filter(RanClean==0)%>%mutate(UncleanIdeology=np_score)%>%dplyr::select(-np_score)
rep_test<-left_join(cleanReps, uncleanReps, by=c("Election_Year"="Election_Year", "District"="District"))

## Run Paired T-Test and save as rtest object
rtest<-t.test(rep_test$CleanIdeology, rep_test$UncleanIdeology, na.rm=TRUE, paired = TRUE)


## Conduct Paired T-Test for AZ Democrat State Legislators
## Negative value of Clean/Unclean ideology T-Test indicates 
## that Clean Democrats are more conservative than 
## Unclean Democrats

## Select Democrats, create clean and unclean 
## ideology variables, merge variables by district and election year
dems<-shor_az  %>%filter(party=="D")
cleanDems<-dems %>%filter( RanClean==1)%>%mutate(CleanIdeology=np_score)%>%dplyr::select(-np_score)
uncleanDems<-dems %>%filter(RanClean==0)%>%mutate(UncleanIdeology=np_score)%>%dplyr::select(-np_score)
dem_test<-left_join(cleanDems, uncleanDems, by=c("Election_Year"="Election_Year", "District"="District"))

## Run Paired T-Test and save as dtest object
dtest<-t.test(dem_test$CleanIdeology, dem_test$UncleanIdeology, na.rm=TRUE, paired = TRUE)

## Create lm models to incorporate T-Test results into stargazer
modR_NP<-modD_NP<-lm(np_score~RanClean, data=dems)

## Store Estimates for stargazer presentation below
modR_NP$coefficients[1]<-rtest$estimate[[1]]
modD_NP$coefficients[1]<-dtest$estimate[[1]]


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
modR_W$coefficients[1]<-nonDynR_W$estimate[[1]]
modRDyn_W$coefficients[1]<-DynR_W$estimate[[1]]

stargazer(modD_W, modDDyn_W,modD_NP, modR_W, modRDyn_W,modR_NP,
          se=list(nonDynD_W$stderr, DynD_W$stderr,dtest$stderr,
                  nonDynR_W$stderr, DynR_W$stderr,rtest$stderr,
          p=list(nonDynD_W$p.value, DynD_W$p.value,dtest$p.value,
                 nonDynR_W$p.value, DynR_W$p.value,rtest$p.value,
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
          p=list(CFDynTT$PValue[[1]], CFNonDynTT$PValue[[1]], NPTT$PValue[[1]]),
          model.names = FALSE, model.numbers = FALSE,
          dep.var.labels.include = TRUE,
          dep.var.labels = c("Dynamic CFscore", "Stable CFscore", "SM Score"),
          column.separate = c(2,1),
          column.labels   = c("Candidates", "Legislators"),
          star.cutoffs = c(0.05, 0.01, 0.001),
          omit=c("DF"),
          style = 'apsr',
          table.placement = "H",
          title=c("Paired T-Test Estimates: Arizona State Legislative Candidate Ideological Proximity to District, 2000-2016"),
          label=c("tab:DistanceTTest"),
          covariate.labels =c("Ideological Distance Diff."),
          multicolumn = FALSE,
          omit.stat = c("rsq","adj.rsq","f","ser", "n"),
          add.lines = list(c("N",CFDynTT$N, CFNonDynTT$N, NPTT$N)))



####SI Section C Valence Analysis####

## Do public financing candidates receive higher voteshare?

## Load Valence.RDS produced in CreateValence.R

valence<-readRDS("Valence.RDS")
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
          notes = c("\\textit{Note}: Standard errors clustered", " by district in parentheses."),
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



####SI Section E Evaluate differences between legislators with and without CFScores####
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
          notes = c("\textit{Note}: Standard errors clustered", " by state in parentheses."),
          covariate.labels =c("Has Dynamic CFScore"),
          add.lines = list(c("State Fixed Effects", "\\checkmark"),
                           c("Party Fixed Effects", "\\checkmark"),
                           c("Election Year Fixed Effects", "\\checkmark")),
          multicolumn = FALSE,
          omit.stat = c("rsq","adj.rsq","f","ser"))

####SI Section G.1 Interaction Effects Analysis####

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



####SI Section G.2 State By State Analysis####
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
          notes = c("\\textit{Note}: $\\beta$ \\& SEs reflect error propagation",
                    "method described in SI-A11"),  
          covariate.labels =c("Public Financing Candidate"),
          add.lines = list(c("District Fixed Effects", "\\checkmark", "\\checkmark", "\\checkmark", "\\checkmark"),
                           c("Election Year Fixed Effects", "\\checkmark", "\\checkmark", "\\checkmark", "\\checkmark")),
          multicolumn = FALSE,
          omit.stat = c("rsq","adj.rsq","f","ser"))



####SI Section G.2 IPW Analysis####

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
          notes = c("\\textit{Note}: $\\beta$ \\& SEs reflect error propagation",
                    "method described in SI-A11",
                    weightsLabel),
          covariate.labels =c("Public Financing Candidate"),
          add.lines = list(c("District Fixed Effects", "\\checkmark", "\\checkmark", "\\checkmark", "\\checkmark", "\\checkmark", "\\checkmark"),
                           c("Election Year Fixed Effects", "\\checkmark", "\\checkmark", "", "\\checkmark", "\\checkmark", ""),
                           c("Party Fixed Effects", "", "\\checkmark", "\\checkmark", "", "\\checkmark", "\\checkmark"),
                           c("District x Time Effects", "", "", "\\checkmark", "", "", "\\checkmark")),
          
          multicolumn = FALSE,
          omit.stat = c("rsq","adj.rsq","f","ser"))


####SI Section G.3 Legislator Tenure analysis####

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
          notes = c("\\textit{Note}: $\\beta$ \\& SEs reflect error propagation",
                    "method described in SI-A11"),  
          covariate.labels =c("Public Financing Candidate", "Candidate Tenure"),
          add.lines = list(c("District Fixed Effects", "\\checkmark", "\\checkmark", "\\checkmark", "\\checkmark", "\\checkmark", "\\checkmark"),
                           c("Election Year Fixed Effects", "\\checkmark", "\\checkmark", "", "\\checkmark", "\\checkmark", ""),
                           c("Party Fixed Effects", "", "\\checkmark", "\\checkmark", "", "\\checkmark", "\\checkmark"),
                           c("District x Time Effects", "", "", "\\checkmark", "", "", "\\checkmark")),
          multicolumn = FALSE,
          omit.stat = c("rsq","adj.rsq","f","ser"))



####SI Section G.4 DW-DIME Analysis####

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
          notes = c("\textit{Note}: $\\beta$ \\& SEs reflect error propagation",
                    "method described in SI-A11"),  
          covariate.labels =c("Public Financing Candidate"),
          add.lines = list(c("District Fixed Effects", "\\checkmark", "\\checkmark"),
                           c("Election Year Fixed Effects", "\\checkmark", "\\checkmark")),
          multicolumn = FALSE,
          omit.stat = c("rsq","adj.rsq","f","ser"))

## Comparison of Effect sizes
all_dwdime$beta[1]/sd(can_fe$Distance_DWDIME, na.rm=TRUE)
sd(can_fe$Distance_CFDyn, na.rm=TRUE)




####SI Section G.5 Alternative Specification of PF Participation####

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
          notes = c("\\textit{Note}: $\\beta$ \\& SEs reflect error propagation",
                    "method described in SI-A11"),  
          covariate.labels =c("Publicly Financed First Campaign"),
          add.lines = list(c("District Fixed Effects", "\\checkmark", "\\checkmark", "\\checkmark", "\\checkmark", "\\checkmark", "\\checkmark"),
                           c("Election Year Fixed Effects", "\\checkmark", "\\checkmark", "", "\\checkmark", "\\checkmark", ""),
                           c("Party Fixed Effects", "", "\\checkmark", "\\checkmark", "", "\\checkmark", "\\checkmark"),
                           c("District x Time Effects", "", "", "\\checkmark", "", "", "\\checkmark")),
          
          multicolumn = FALSE,
          omit.stat = c("rsq","adj.rsq","f","ser"))


#### SI Section H Exact Binomial Test####
## Dynamic CFScore Estimates: For how many cases does the 
## publicly funded candidate have a larger CFScore distance
sum(pairedGC$CLMoreExtremeDyn, na.rm = TRUE)/sum(!is.na(pairedGC$CLMoreExtremeDyn))
binom.test(sum(pairedGC$CLMoreExtremeDyn, na.rm = TRUE),sum(!is.na(pairedGC$CLMoreExtremeDyn)), .5)

## Static CFScore Estimates: For how many cases does the 
## publicly funded candidate have a larger CFScore distance
sum(pairedGC$CLMoreExtremeNonDyn, na.rm = TRUE)/sum(!is.na(pairedGC$CLMoreExtremeNonDyn))
binom.test(sum(pairedGC$CLMoreExtremeNonDyn, na.rm = TRUE), sum(!is.na(pairedGC$CLMoreExtremeNonDyn)), .5)


## NP Score: For how many cases does the publicly funded candidate
## have a larger NP_Score distance.
## Data created in CreatePairedAZData.R
## Subset observations to Democratic and Republican legislators after 2004
## because MRP lines reflect Arizona state legislative map starting in
## 2004. Arizona redistricted in 2002 and then again in 2004. 
mrp_distcalc<-readRDS("shor_az.RDS")%>%
  filter(Election_Year>=2004 & party !="X")

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



