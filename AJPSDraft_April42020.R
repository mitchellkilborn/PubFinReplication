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
list.of.packages <- c("tidyverse", "stargazer","scales","sjPlot","lfe","stringdist","readstata13")
new.packages <- list.of.packages[!(list.of.packages %in% installed.packages()[,"Package"])];
if(length(new.packages)){install.packages(new.packages)}
lapply(list.of.packages, require, character.only = TRUE)


## Package version check
packinfo <- as.data.frame(installed.packages(fields = c("Package", "Version")))
packinfo[which(packinfo$Package %in% list.of.packages),c("Package", "Version")]

## Package                      Version
## lfe                          2.8-5
## scales                       1.1.0
## sjPlot                       2.8.2
## stargazer                    5.2.2
## stringdist                   0.9.5.5
## tidyverse                    1.3.0
## readstata13                  0.9.2


#############################Session Info#######################################

sessionInfo()
# R version 3.6.2 (2019-12-12)
# Platform: x86_64-apple-darwin15.6.0 (64-bit)
# Running under: macOS Catalina 10.15.3
# April 30, 2020


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

#############################CLEAN DATA#######################################

####Clean MRP Data####

## MRP: http://www.americanideologyproject.com/
## Stack MRP Estimates for each redistricting cycle for upper and lower chambers to merge with
## candidate ideology estimates below
## Create vector of years with same length as one year's worth of observations of
## the MRP dataset. (ie 2000, 2000...2000, 2002)
## Select relevant states
## Mark Sen=0 for lower chamber,  Sen=1 for upper chamber.
## Filter for each state and relevant years for each state given redistricting
## Select relevant variables and repeat for each chamber

## 2000 Cycle Lower Chamber
Years2000House<-rep(c("2002","2004","2006","2008","2010","2012"), each=332)
## Read in Merged District and NPAT Scores
SH02<-read.csv("SH02_2015UPDATE.csv", stringsAsFactors = FALSE)%>%
  filter(abb %in% c("ME","AZ","CT"))%>%
  mutate(Sen=0,
         District=as.numeric(shd_fips)-as.numeric(state_fips)*1000)%>%
  row_rep(6)%>%
  mutate(Election_Year=Years2000House)%>%
  filter((abb=="AZ" & Election_Year %in% c(2004:2010)) |
           (abb=="ME" & Election_Year %in% c(2004:2012)) |
           (abb=="CT" & Election_Year %in% c(2002:2010)) )%>%
  dplyr::select(abb, Sen, District, Election_Year, mrp_mean)

## 2000 Cycle Upper Chamber

Years2000Senate<-rep(c("2002","2004","2006","2008","2010","2012"), each=101)

SS02<-read.csv("SS02_2015UPDATE.csv",stringsAsFactors = FALSE)%>%
  filter(abb %in% c("ME","AZ","CT"))%>%
  mutate(Sen=1,
         District=as.numeric(ssd_fips)-as.numeric(state_fips)*1000)%>%
  row_rep(6)%>%
  mutate(Election_Year=Years2000Senate)%>%
  filter((abb=="AZ" & Election_Year %in% c(2004:2010)) |
           (abb=="ME" & Election_Year %in% c(2004:2012))|
           (abb=="CT" & Election_Year %in% c(2002:2010)))%>%
  dplyr::select(abb, Sen, District, Election_Year, mrp_mean)

## 2010 Cycle Lower Chamber

Years2010House<-rep(c("2012","2014","2016"), each=332)
SH12<-read.csv("SH12_2015UPDATE.csv",stringsAsFactors = FALSE)%>%
  filter(abb %in% c("ME","AZ","CT"))%>%
  mutate(Sen=0,
         District=as.numeric(shd_fips)-as.numeric(state_fips)*1000)%>%
  row_rep(3)%>%
  mutate(Election_Year=Years2010House)%>%
  filter((abb=="AZ" & Election_Year %in% c(2012:2016)) |
           (abb=="ME" & Election_Year %in% c(2014:2016))|
           (abb=="CT" & Election_Year %in% c(2012:2016)))%>%
  dplyr::select(abb, Sen, District, Election_Year, mrp_mean)

## 2010 Cycle Upper Chamber

Years2010Senate<-rep(c("2012","2014","2016"), each=101)
SS12<-read.csv("SS12_2015UPDATE.csv",stringsAsFactors = FALSE)%>%
  filter(abb %in% c("ME","AZ","CT"))%>%
  mutate(Sen=1,
         District=as.numeric(ssd_fips)-as.numeric(state_fips)*1000)%>%
  row_rep(3)%>%
  mutate(Election_Year=Years2010Senate)%>%
  filter((abb=="AZ" & Election_Year %in% c(2012:2016)) |
           (abb=="ME" & Election_Year %in% c(2014:2016))|
           (abb=="CT" & Election_Year %in% c(2012:2016)))%>%
  dplyr::select(abb, Sen, District, Election_Year, mrp_mean)

## Bind together all observations, rename columns for clarity
## and ease of merging with other data.

MRP<-bind_rows(SH02, SH12, SS02, SS12)%>%
  mutate(State=abb,Election_Year=as.numeric(Election_Year),
         MRP_Mean=mrp_mean)%>%
  dplyr::select(-c(abb, mrp_mean))







#### Read in Klarner Contest level election results####

## Need this data for valence analysis and
## competitiveness interaction analysis.

## Had to convert Klarner's original dta object to RDS
## format to be able to upload project to github
## while observing 100mb file limit.
#election<-read.dta13("102slersuoacontest20181024-1.dta")
#saveRDS(election, file="102slersuoacontest20181024-1.RDS")
election<-readRDS("102slersuoacontest20181024-1.RDS")
## Select relevant observations

election_all<-election %>%
  filter( (year>=2000 &  sab == "ME") | (year>=2008 & sab=="CT") |
            (year>=2000 &  sab == "AZ") ) %>%

  ## Select relevant variables

  dplyr::select(year, sab, sen, dno, dvote, rvote, ovote, dcand,
         rcand, ocand, dinc, rinc, oinc, dwin, rwin, owin, dinc2, rinc2, oinc2, dinc3,rinc3,oinc3)%>%

  ## Calculate top 2 vote share based on possible combinations of candidates
  ## (1 Rep, 1 Dem, 1 Oth or 1 Rep and 1 Dem) (Need this for competitiveness analysis)
  mutate(Top2Votes=ifelse(dvote>0 & rvote>0 & dvote>ovote & rvote>ovote, rvote+dvote, 0),##RDO/DRO
         Top2Votes=ifelse(dvote>0 & rvote==0 & ovote==0, dvote, Top2Votes),##D
         Top2Votes=ifelse(dvote==0 & rvote>0 & ovote==0, rvote, Top2Votes),##R
         Top2Votes=ifelse(dvote==0 & rvote==0 & ovote>0, ovote, Top2Votes),##O
         Top2Votes=ifelse(dvote>0 & rvote==0 & ovote>0, dvote+ovote, Top2Votes),##DO
         Top2Votes=ifelse(dvote==0 & rvote>0 & ovote>0, rvote+ovote, Top2Votes),##RO
         Top2Votes=ifelse(dvote>0 &ovote>0 & dvote > rvote & ovote >rvote, dvote+ovote, Top2Votes),##DOR
         Top2Votes=ifelse(rvote>0 & ovote>0 & ovote>dvote & rvote>dvote , rvote+ovote, Top2Votes),##ROD
         Rep_T2VoteShare=rvote/Top2Votes, Dem_T2VoteShare=dvote/Top2Votes, Oth_T2VoteShare=ovote/Top2Votes,##Calculate party share of top 2 votes
         RepInT2=ifelse(rvote>ovote, 1, 0), DemInT2=ifelse(dvote>ovote,1,0), OthInTop2=ifelse(ovote>dvote | ovote>rvote,1,0),##Indicator for whether party is in top 2
         RepWin=ifelse(rvote> ovote & rvote>dvote,1,0), DemWin=ifelse(dvote>rvote& dvote>ovote,1,0),
         OthWin=ifelse(ovote>dvote & ovote>rvote,1,0),##Create indicators for which party candidate won
  ## Calculate winner top 2 voteshare (Use Rep, Dem, Oth)
         WinnerT2VoteShare=ifelse(RepWin==1, Rep_T2VoteShare, 0),
         WinnerT2VoteShare=ifelse(DemWin==1, Dem_T2VoteShare, WinnerT2VoteShare),
         WinnerT2VoteShare=ifelse(OthWin==1, Oth_T2VoteShare, WinnerT2VoteShare))

####Read in Public Financing Status List####

## Public financing  list gathered from https://www.followthemoney.org/
ftm<-read.csv("PublicFundingFTM.csv", stringsAsFactors = FALSE)

## Fix miscoded party from FTM list
ftm$GeneralParty[which(ftm$Name=="MCDOUGAL, VALERIE M")]<-"REPUBLICAN"

## Calculate how many clean candidates ran in each district-year combination
## need this quantity for valence analysis

count_district<-ftm %>%
  ## Create indicators for House and Senate observations
  mutate(House=ifelse(str_detect(Office, "HOUSE"), 1, 0),
         Senate=ifelse(str_detect(Office, "SENATE"), 1, 0))%>%
  filter( (State=="ME" | State=="CT" | State=="AZ") & ( House==1 | Senate==1) & ## Select relevant states and chambers
            ElectionStatus %!in% c("LOST-PRIMARY RUNOFF",#Grab self-financing GE contestants only
                                   "LOST-PRIMARY", "DISQUALIFIED-GENERAL",
                                   "WITHDREW-GENERAL") & ElectionType %!in% c("SPECIAL"))%>%##Drop special elections
  mutate(District=substr(Office, 16,18), District=ifelse(Senate==1,substr(Office, 17,19),District),##Grab district id numbers
         District=as.numeric(as.character(District)),
         Senate=ifelse(Senate==1, 1, 0),## Create Senate indicator
         Incumbent=ifelse(IncumbencyStatus=="INCUMBENT",1,0),## Create indicator for incumbent public financing
         CleanWinner=ifelse(ElectionStatus=="WON-GENERAL", 1,0),## Create indicator for victorious clean candidate
         CleanDem=ifelse(GeneralParty=="DEMOCRATIC", 1,0),## Create indicator for clean Democrat
         CleanRepub=ifelse(GeneralParty=="REPUBLICAN", 1,0),## Create indicator for clean republican
         CleanThirdParty=ifelse(GeneralParty=="THIRD-PARTY",1,0))%>%## Create indicator for clean third party
  arrange(State, Senate, Year, District)%>%
  group_by(State, Senate, Year, District)%>%
  summarize(TotalCleanCan=n(), CleanWinner=sum(CleanWinner,na.rm = TRUE), CleanRepub=sum(CleanRepub, na.rm = TRUE),
            CleanDem=sum(CleanDem, na.rm = TRUE), CleanOth=sum(CleanThirdParty, na.rm = TRUE),
            CleanIncumbent=sum(Incumbent, na.rm = TRUE))##Count how many of each type in GE contest

## Join the election contest data with the district counts of clean candidates by
## year, state, chamber, and district. Note, this generates NA counts
## of public financing candidates in contests
## which saw no clean candidates

election_allres<-left_join(election_all, count_district, by=c("year"="Year", "sab"="State",
                                                              "sen"="Senate",
                                                              "dno"="District"))

## Join election results+clean candidate count to MRP scores for each district-chamber-state-year

election_allres<-left_join(election_allres, MRP, by=c("year"="Election_Year", "sab"="State",
                                                      "sen"="Sen",
                                                      "dno"="District"))


## Because we extended the bonica ideology data to include 2014 and 2016, had to
## fill in missing ran.general field for 2014, missing party for 2014 and 2016,
## and missing incumbency for 2016 by hand
## Note that there are quite a few missing observations for 2014 and 2016
## because we use Bonica's 2018 update which hadn't gone through
## full cleaning yet.

## Missing ran.general for 2014
fixed2014<-read.csv("MissingGeneralElectionAZCTMEFixed.csv", stringsAsFactors = FALSE)%>%
  distinct(bonica.rid, .keep_all=TRUE)%>%
  dplyr::select(-c(name,district, seat,state, X, MissingCandidates_2014))%>%
  mutate(ran.general_2014=ifelse(is.na(ran.general_2014),0, ran.general_2014))## Convert empty cells to 0
## Missing party for 2014-2016

fixedMissingParty<-read.csv("Bonica_2014_2016MissingParty_Fixed.csv", stringsAsFactors = FALSE)%>%
  dplyr::select(bonica.rid, cycle, party_fixed)
## Missing incumbency variable 2016
missingIncumbents2016<-read.csv("MissingIncumbency2016_fixed.csv", stringsAsFactors = FALSE)%>%
  dplyr::select(bonica.rid, cycle, Incum.Chall_2016)%>%distinct(bonica.rid, cycle, .keep_all = TRUE)


#### Read in Bonica Data####
## Read in Bonica candidate data matched to public financing data in CombineDimeFTM_All.R Files
## and merge with fixed datasets

bonica_ftm<-readRDS("FTM_BONICA_MERGE_AddedThru2016_V5.RDS")%>%
  mutate(cycle=as.numeric(cycle))%>%
  left_join(fixed2014,by=c("cycle"="cycle", "bonica.rid"="bonica.rid"))%>%
  left_join(fixedMissingParty,by=c("cycle"="cycle", "bonica.rid"="bonica.rid"))%>%
  left_join(missingIncumbents2016,by=c("cycle"="cycle", "bonica.rid"="bonica.rid"))%>%

  ## Replace missing fields in bonica_ftm with hand-coded data for relevant years.

  mutate(ran.general=ifelse(cycle==2014 & is.na(ran.general),ran.general_2014, ran.general),
         winner=ifelse(cycle==2014 & is.na(ran.general), winner_2014, winner),
         party=ifelse(is.na(party)==TRUE, party_fixed, party),
         Incum.Chall=ifelse(cycle==2014 & is.na(Incum.Chall), Incum.Chall_2014, Incum.Chall),
         Incum.Chall=ifelse(cycle==2016& is.na(Incum.Chall), Incum.Chall_2016, Incum.Chall),
         ## Create empty tenure variables
         tenure1=0, tenure2=0)%>%
  ## drop un-needed variables from fixed data
  dplyr::select(-c(ran.general_2014,Incum.Chall_2014,party_fixed, Incum.Chall_2016))

#### Read in Klarner candidate specific election result data####

## This is a different dataset from the previous Klarner data because it uses
## candidate-years as the unit rather than the district-year contest. We need the
## candidate specific data because it contains the tenure variable for
## incumbents. However, there is no common ID between the Klarner data and the
## Bonica data, so we have to match by name.

load("196slers1967to2016_20180908.RData")

## Grab tenures for incumbents for Klarner data for each state for relevant years
## Subset to relevant years and states.

table<-table %>%
  filter( ( (year>=2000 &  sab == "me") | (year>=2008 & sab=="ct") | (year>=2000 &  sab == "az") ) &
            (tenure1>0 | tenure2>0))%>%## select only observations which have any tenure
  mutate(state=toupper(sab), Senate=sen, District=dno, election=as.character(year),
         Partyk=party,## Rename party variable for merging purposes
         combName=paste0(last,first), ## Combine the first and last name for string distance match method
         NameMatchDistance=NA)%>% ## Create holder variable for Name Match Distance

  ## Select relevant variables

  dplyr::select(state, Senate, District,election, tenure1, tenure2,combName, Partyk,outcome,NameMatchDistance)%>%
  arrange(state,Senate,District,election)

## Get tenure variables for incumbent candidates in BonicaFTM data by matching
## the string distance between the name from the bonica dataset and the Klarner
## dataset. We have to use name matching rather than assigning the incumbent in
## a given year the tenure of the incumbent from the Klarner dataset because of
## the Arizona multi-member districts.

for(i in 1:nrow(bonica_ftm)){## Iterate through rows of bonica dataset
  subBonica<-bonica_ftm[i,]## Select ith row
  ## Non-incumbents don't have tenure, so skip
  if(subBonica$Incum.Chall!="I" | is.na(subBonica$Incum.Chall)==TRUE){next}

  ## Subset Klarner data to district, chamber, state, and year of candidate from bonica data

  subKlarner<-table%>%filter(state==subBonica$state & District==subBonica$District &
                               election==subBonica$election & Senate==subBonica$Senate)

  ## If no observations in the bonica data
  ## cell for Klarner, enter 0 for tenure
  if(nrow(subKlarner)==0){
    bonica_ftm$tenure1[i]<-bonica_ftm$tenure2[i]<-0
    next
  }
  ## If there is only one observation in that cell for Klarner
  ## (Ie every non AZ house district) assign tenure values for bonica data

  if(nrow(subKlarner)==1){
    bonica_ftm$tenure1[i]<-subKlarner$tenure1
    bonica_ftm$tenure2[i]<-subKlarner$tenure2
    next
  }
  ## If there are multiple incumbents, i.e. Arizona house districts
  ## use minimum of name distance between Bonica and Klarner name
  ## to get tenure information

  if(nrow(subKlarner)>1){
    for(j in 1:nrow(subKlarner)){

      ## Calculate distance between Bonica and Klarner name and store in subKlarner
      subKlarner$NameMatchDistance[j]<-stringdist(subBonica$name, subKlarner$combName[j])
    }
    ## Subset Klarner to string which is best match for name
    subKlarner<-subKlarner[which.min(subKlarner$NameMatchDistance),]

    ## Assign tenure values for bonica data based on best match
    bonica_ftm$tenure1[i]<-subKlarner$tenure1
    bonica_ftm$tenure2[i]<-subKlarner$tenure2
    next
  }
}


#### Merge Bonica Data to all datasets and prep final dataset####

## election_allres contains mrp scores, candidate and contest level result variables
## bonica_ftm contains state leg candidates, their public financing status and bonica ideology

can_fe<-left_join(election_allres, bonica_ftm,
                  by=c("year"="cycle", "sab"="state", "sen"="Senate", "dno"="District"))





## Merge NP-Scores to Bonica data via bonica.rid variable, which were matched to np_scores
## in CombineDimeFTM_All.R script. Once again, this had to be done using string distance
## method in Arizona multi-members districts.
## 2014 is the last election cycle for which Shor-McCarty scores are available
## for legislators.

npscores<-readRDS("NP_ScoresMatchedToBonicaRIDS2000_2014_AZMECT.RDS")%>%
  mutate(cycle=as.numeric(cycle))%>%distinct(bonica.rid,cycle,.keep_all = TRUE)
can_fe<-left_join(can_fe, npscores, by=c("bonica.rid"="bonica.rid","year"="cycle"))
## Calculate coefficients for Rogers (2017) district ideal point imputation

## Regress dynamic CF Score on district ideology
ideal_points_CF<-lm(recipient.cfscore.dyn~MRP_Mean, data=can_fe)
summary(ideal_points_CF)

## Regress DWDIME on district ideology

ideal_points_DWDIME<-lm(dwdime~MRP_Mean, data=can_fe)
summary(ideal_points_DWDIME)


## Create simulation objects and save
set.seed(02138)
library(arm)
ideal_points_CF_sims1000<-sim(ideal_points_CF, n.sims=1000)
ideal_points_DWDIME_sims1000<-sim(ideal_points_DWDIME, n.sims=1000)
saveRDS(ideal_points_CF_sims1000, file="ideal_points_CF_sims1000.RDS")
saveRDS(ideal_points_DWDIME_sims1000, file="ideal_points_DWDIME_sims1000.RDS")


## Construct needed variables in combined dataset can_fe, which at this point consists of
## candidate-year observations of Bonica ideology data, Klarner contest level stats,
## Klarner candidate level stats, Shor McCarty NP_Scores, Tausanovitch-Warshaw MRP estimates,
## and all candidates are coded by year whether they used public financing in the general election

can_fe<-can_fe%>%mutate(
  ## Fix Clean Winner where FTM had no clean cans running in a district,
  ## so binding FTM to election results produces NAs
  CleanWinner=ifelse(is.na(CleanWinner)==TRUE,0,CleanWinner),
  ## Similarly, if candidate is not listed in public financing list,
  ## they get an NA on CleanYear, so mark as 0
  CleanYear=ifelse(is.na(CleanYear)==TRUE,0,CleanYear),
  ## Create indicator for whether candidate won race
  WonElection=ifelse(winner=="W",1,0),
  ## Calculate ideological distance using dynamic CF Score and non dynamic CFScore estimate
  Intercept=ideal_points_CF$coefficients[1], phi1=ideal_points_CF$coefficients[2],
  EstimatedIdealPointCF=Intercept+phi1*MRP_Mean,
  Distance_CFDyn=abs(recipient.cfscore.dyn-EstimatedIdealPointCF),
  Distance_CFnonDyn=abs(recipient.cfscore-EstimatedIdealPointCF),
  CFScoreDynAbs=abs(recipient.cfscore.dyn),## Calculate absolute value of dynamicCFScore
  ## Calculate ideological distance for DWDIME Scores
  Intercept=ideal_points_DWDIME$coefficients[1], phi1=ideal_points_DWDIME$coefficients[2],
  EstimatedIdealPointDWDIME=Intercept+phi1*MRP_Mean,
  Distance_DWDIME=abs(dwdime-EstimatedIdealPointDWDIME),
  ## Create indicators for whether data has ideological distance estimate
  HasDistanceDWDIME=ifelse(is.na(Distance_DWDIME)==TRUE, 0, 1),
  HasDistanceCFDyn=ifelse(is.na(Distance_CFDyn)==TRUE, 0, 1),
  HasDistanceCFnonDyn=ifelse(is.na(Distance_CFnonDyn)==TRUE, 0, 1),
  ## Create Redistricting Time variable
  RedistTime=ifelse(sab=="ME" & year %in%c(2004, 2006, 2008, 2010,2012),year-2002, NA),
  RedistTime=ifelse(sab=="ME" & year %in%c(2014, 2016),year-2012, RedistTime),
  RedistTime=ifelse(sab=="AZ" & year %in%c(2004, 2006, 2008, 2010),year-2002, RedistTime),
  RedistTime=ifelse(sab=="AZ" & year %in%c(2012, 2014, 2016),year-2010, RedistTime),
  RedistTime=ifelse(sab=="CT" & year %in%c(2002, 2004, 2006, 2008, 2010),year-2000, RedistTime),
  RedistTime=ifelse(sab=="CT" & year %in%c(2012, 2014, 2016),year-2010, RedistTime),
  Incumbent=ifelse(Incum.Chall=="I",1,0),## Create indicator for whether candidate was incumbent
  RepubIndicator=ifelse(party==200, 1, 0), ## Create indicator for whether candidate is Republican
  ## Create competitive interaction term factor
  CompetitiveElection=ifelse(WinnerT2VoteShare<=.55,1,0),
  CompetitiveInteraction=as.factor(paste0("Competitive",CompetitiveElection, "CleanYear",CleanYear)),
  ME_2000CENSUS=ifelse(sab=="ME" & year >=2004 & year <=2012, 1, 0),##Create indicator for ME 2000 Census districts
  CT_2000CENSUS=ifelse(sab=="CT" & year>=2008 & year <=2010,1,0),##Create indicator for CT 2000 Census Districits
  AZ_2000CENSUS=ifelse(sab=="AZ" & year>=2004 & year <=2010,1,0),##Create indicator for AZ 2000 Census Districits
  CENSUS2000=ifelse(ME_2000CENSUS==1 | CT_2000CENSUS==1 |AZ_2000CENSUS==1, 1, 0),##Create indicator for 2000 Census districts for combined analysis
  ME_2010CENSUS=ifelse(sab=="ME" & year >=2014, 1, 0),##Create indicator for ME 2010 Census districts
  CT_2010CENSUS=ifelse(sab=="CT" & year>=2012,1,0),##Create indicator for CT 2010 Census Districits
  AZ_2010CENSUS=ifelse(sab=="AZ" & year>=2012,1,0),##Create indicator for AZ 2010 Census Districits
  CENSUS2010=ifelse(ME_2010CENSUS==1 | CT_2010CENSUS==1 |AZ_2010CENSUS==1, 1, 0),##Create indicator for 2000 Census districts for combined analysis
  UniqueDistrictChamber=ifelse(sen==1, paste0("Senate", dno), paste0("House", dno)),##Create House and Senate District specific indicator variables
  Census_Group=ifelse(CENSUS2010==1, "Census2010", NA),## Create Census group indicators
  Census_Group=ifelse(CENSUS2000==1, "Census2000",Census_Group),
  ## Create subset variable for when data is in Census 2000 or Census 2010 district lines
  ## for each state
  CensusLines=ifelse(is.na(Census_Group)==TRUE,0,1),
  ## Create Chamber factor
  Chamber=as.factor(ifelse(sen==1, "Senate", "House")),
  ## Create state chamber
  StateChamber=as.factor(paste0(sab, Chamber)),
  ## Create state-chamber-district factor levels
  UniqueDistrict=ifelse(sab=="CT", paste0("CT", UniqueDistrictChamber), paste0("ME",UniqueDistrictChamber)),##Create CT and ME district specific indicators
  UniqueDistrict=ifelse(sab=="AZ", paste0("AZ", UniqueDistrictChamber),UniqueDistrict),##Create AZ district specific indicators
  UniqueDistrict_CensusGroup=as.factor(paste0(UniqueDistrict, Census_Group)),
  ## Impute non-dynamic CF score if dynamic CF Score is missing
  Distance_CFAll=ifelse(is.na(Distance_CFDyn)==TRUE,
                        abs(recipient.cfscore-EstimatedIdealPointCF),Distance_CFDyn),
  ## Create indicator for whether candidate has dynamic/stable cfscores
  HasDynamicCF=ifelse(is.na(recipient.cfscore.dyn)==FALSE,1,0),
  HasStableCF=ifelse(is.na(recipient.cfscore)==FALSE,1,0),
  ## Create empty variables for whether candidate ran clean first campaign or switched PF status
  ## these will be marked in the next section
  CleanFirstRun=0,
  PFStatusSwitcher=0,
  ## Make year a factor class to create state-year effects
  ## (lfe requires one variable in fixed effects
  ## interaction to be a factor variable)
  year=as.factor(year))%>%
  filter(ran.general==1 )## Select general election candidates only

####Code Candidate Clean First Run####

## To do SI-A7, we need an indicator for whether a candidate used public
## financing in their first campaign. We also need an indicator for whether a
## candidate switched public financing status between campaign cycles

for(i in 1:nrow(can_fe)){

  ## Subset to ith row/candidate and get their bonica.rid
  id_sub<-can_fe$bonica.rid[i]

  ## Grab all instances of candidate and arrange by year ascending
  dat_sub<-can_fe%>%filter(bonica.rid==id_sub)%>%arrange(year)

  ## If they are an incumbent in 2000, they couldn't have entered
  ## legislature with clean elections, so mark them as a 0 for clean from start
  if(dat_sub$year[1]==2000 & dat_sub$Incumbent[1]==1){
    can_fe$CleanFirstRun[i]<-0
    next
  }
  ## Put the public financing status of a candidate's first run into the
  ## the ith row, which may be first or subsequent runs.
  can_fe$CleanFirstRun[i]<-dat_sub$CleanYear[1]

  ## If they switch status at any time, they will have a 1 and 0
  ## in the CleanYear column
  ## so the unique values in dat_sub$CleanYear
  ## will have more than 1 value. Mark these people as switchers.

  if(length(unique(dat_sub$CleanYear))>1){
    can_fe$PFStatusSwitcher[i]<-1
  }
}

## QA variables for matches between Public Financing List and Bonica data name
## are Candidate's Name from Bonica data (name) and name from FTM List
## (MatchName). Whenever a match isn't found (MatchName = NA), the candidate gets
## a zero for CleanYear After matching the bonica data and FTM data I checked to
## see which names in FTM weren't in the matched data and added those in to the
## Bonica data, about 40 cases. So all names on the public financing list which
## have a matching candidate in the bonica data are in the matched data, bonica_ftm
## I checked visually the first 200 matches and the last 100 matches
## to ensure they are correct and the match
## rate is 100%, so I feel good about the quality of this procedure.
## look object allows you to compare matches
## look<-can_fe%>%select(name,MatchName,CleanYear)%>%filter(CleanYear==1)


## Select variables used in below analysis
can_fe<-can_fe%>%
  dplyr::select(year, sab, sen, dno,name,party,
         bonica.rid,Distance_CFnonDyn, Distance_CFDyn,
         UniqueDistrict_CensusGroup,MRP_Mean,
         Census_Group, #need to include this for calcs below
         CensusLines,CleanYear, RedistTime,WonElection,
         CleanFirstRun,HasDistanceCFDyn, HasDistanceCFnonDyn,
         PFStatusSwitcher,Incumbent, seat,
         recipient.cfscore,NP_Score,dwdime,
         recipient.cfscore.dyn,ran.general,
         HasDynamicCF,RepubIndicator,CompetitiveInteraction,CompetitiveElection,
         tenure1, Distance_DWDIME, HasDistanceDWDIME)

#### Load can_fe from here####
#saveRDS(can_fe, file="can_fe.RDS")
can_fe<-readRDS("can_fe.RDS")






####Calculate Public/Private Financing Changeovers####
## The look object below calculates how many unique
## legislators and unique never succesful legislative candidates
## exist within each district for the Census 2000 and Census2010 data
## Note that the bonica data is missing about 20 legislators from
## 2014 and 2016 for CT and ME respectively. This is due to
## missingness in the bonica update data we received.
## Because CT is limited to 2 cycles in the Census 2000 and 3 cycles
## in the Census2010 data, we rarely see districts change hands
## and especially rarely see a district go from being represented
## by a clean candidate to a non-clean candidate and vice versa
## (see Changes object below). Perhaps worth
## discussing in the memo?
## Because of term limits in Maine,
## there are on average more than 2 candidates per redistricting cycle in the
## Census2000 lines, but not in the Census2010 lines.

## Subset to relevant district lines data
look<-can_fe%>%filter(CensusLines==1)%>%
  ## Sort within district and candidates years in which they won and lost
  group_by(sab,sen,Census_Group,UniqueDistrict_CensusGroup,bonica.rid,WonElection)%>%
  ## Count number of observations
  ## (creates rows for each candidates wins and losses if they have
  ## wins and losses in the data)
  summarize(count=n())%>%
  ## Group within bonica.rid so we can use top_n function to select 1 candidate-district
  ## observation. top_n returns the row with the highest value in WonElection within district,
  ## so will return row where a bonica.rid won (WonElection==1) or
  ## if the bonica.rid never won, the observation they have for a loss where WonElection==0
  group_by(sab,sen,Census_Group,UniqueDistrict_CensusGroup,bonica.rid)%>%
  top_n(1,WonElection)%>%
  ## Count up the numbers of winners and never winner candidates within each district
  group_by(sab,sen,Census_Group,UniqueDistrict_CensusGroup,WonElection)%>%
  summarize(count=n())%>%
  ## Average these counts by state
  group_by(sab, sen,Census_Group,WonElection)%>%
  summarize(AverageCandidateTotal=sum(count,na.rm=TRUE)/n()) %>%
  ## Create variable with number of eligible race within each census group
  ## then divide count of winners and never winners in each district by
  ## number of races per redistricting cycles we have data for
  mutate(RacesPerRedist=case_when(
    sab == "ME" & Census_Group== "Census2000"~5,
    sab == "ME" & Census_Group==  "Census2010"~2,
    sab == "CT" & Census_Group==  "Census2000"~2,
    sab == "CT" & Census_Group==  "Census2010"~3,
    sab == "AZ" & Census_Group==  "Census2000"~4,
    sab == "AZ" & Census_Group==  "Census2010"~3),
    AverageCandidateTotal=AverageCandidateTotal/RacesPerRedist)

## Count how many district seats change hands between clean and not clean legislators
Changes<-can_fe%>%
  ## Select legislators and CensusLines
  filter(WonElection==1 &CensusLines==1)%>%
  ## Group by state, census redistricting cycle, and district
  group_by(sab,Census_Group,UniqueDistrict_CensusGroup)%>%
  ## Pare data to observation of candidate by district
  distinct(UniqueDistrict_CensusGroup, bonica.rid, .keep_all = TRUE)%>%
  ## Count how many public financing statuses exist in a redistricting cycle
  ## (does the district change from clean to not clean legislator)
  summarize(ChangeDistricts=n_distinct(CleanFirstRun,na.rm=TRUE))%>%
  ## Calculate if there is contrast in public financing status for a district
  mutate(Contrast=ifelse(ChangeDistricts>1,1,0))%>%
  ## Calculate what combination of districts ever change hands (i.e. Contrast is 1)
  ## for each state.
  count(sab,Contrast)%>%
  mutate(Prop=prop.table(n))

## Calculate total for ME and CE
ChangesCTME<-Changes%>%filter(sab!="AZ")%>%group_by(Contrast)%>%
  summarize(Total=sum(n))%>%mutate(Prop=prop.table(Total))%>%pull(Prop)


####Table 2 Core Results####

## All models in this section are subset to Census Lines to include only years
## within 2000 and 2010 Census Cycles and for observations that have an
## ideological distance metric. Observations might not have an ideological
## distance metric due to missing recipient.cfscore.dyn or missing
## recipient.cfscore.

## Pooled States: Dynamic Distance Estimate with District and Year Fixed
## Effects, SEs clustered by district
all_dyn<-felm(Distance_CFDyn~CleanYear|UniqueDistrict_CensusGroup+year|0|UniqueDistrict_CensusGroup,
              data=can_fe, subset=CensusLines==1 & HasDistanceCFDyn==1)
summary(all_dyn)

## Pooled States: Dynamic Distance Estimate with District, Year, and Party Fixed Effects Party Fixed Effects,
## SEs clustered by district
all_dyn_party<-felm(Distance_CFDyn~CleanYear|UniqueDistrict_CensusGroup+year+party|0|UniqueDistrict_CensusGroup,
                    data=can_fe, subset=CensusLines==1 & HasDistanceCFDyn==1)
summary(all_dyn_party)

## Pooled States: Dynamic Distance Estimate with District and Party Fixed Effects, District x Time Trend
## SEs clustered by district
all_dyn_party_trend<-felm(Distance_CFDyn~CleanYear|UniqueDistrict_CensusGroup:RedistTime+party|0|UniqueDistrict_CensusGroup,
                          data=can_fe, subset=CensusLines==1 & HasDistanceCFDyn==1)
summary(all_dyn_party_trend)


## Pooled States: Non-Dynamic Distance Estimate with District and Year Fixed Effects
## SEs clustered by district
all_ndyn<-felm(Distance_CFnonDyn~CleanYear|UniqueDistrict_CensusGroup+year|0|UniqueDistrict_CensusGroup,
               data=can_fe, subset=CensusLines==1 & HasDistanceCFnonDyn==1)
summary(all_ndyn)

## Pooled States: Non-Dynamic Distance Estimate District, Year, and Party Fixed Effects
## SEs clustered by district
all_ndyn_party<-felm(Distance_CFnonDyn~CleanYear|UniqueDistrict_CensusGroup+year+party|0|UniqueDistrict_CensusGroup,
                     data=can_fe, subset=CensusLines==1 & HasDistanceCFnonDyn==1)
summary(all_ndyn_party)

## Pooled States: Non-Dynamic Distance Estimate District, Party Fixed Effects, District x Time Trend
## SEs clustered by district
all_ndyn_party_trend<-felm(Distance_CFnonDyn~CleanYear|UniqueDistrict_CensusGroup:RedistTime+party|0|UniqueDistrict_CensusGroup,
                           data=can_fe, subset=CensusLines==1 & HasDistanceCFnonDyn==1)
summary(all_ndyn_party_trend)


## Create Latex Table
stargazer(all_dyn,all_dyn_party,all_dyn_party_trend,all_ndyn,all_ndyn_party,all_ndyn_party_trend,
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


## Effect size interpretation
## Quantile of ideological distance
quantile(can_fe$Distance_CFDyn, na.rm = TRUE, probs=seq(0,1,.1))

## Proportion of SD
all_dyn$coefficients[1]/sd(can_fe$Distance_CFDyn, na.rm = TRUE)

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
ind_cf<-felm(Distance_CFDyn~CleanYear|bonica.rid+year+UniqueDistrict_CensusGroup|0|bonica.rid,
             data=can_fe, subset=CensusLines==1& PFStatusSwitcher==1 & HasDistanceCFDyn==1)
summary(ind_cf)

# can_fe<-can_fe%>%mutate(CYIncInteraction=paste0(CleanYear,Incumbent),
#                         CYIncInteraction=fct_relevel(CYIncInteraction,"10",
#                                                      "00",
#                                                      "11",
#                                                      "01"))
## Add candidate incumbency variable, SEs clustered by candidate
ind_cf_incumb<-felm(Distance_CFDyn~CleanYear+Incumbent|bonica.rid+year+UniqueDistrict_CensusGroup|0|bonica.rid,
                    data=can_fe, subset=CensusLines==1& PFStatusSwitcher==1 & HasDistanceCFDyn==1)
summary(ind_cf_incumb)

## Subset by party: Democrats, candidate fixed effects, incumbency indicator, SEs clustered by candidate
ind_cf_dem<-felm(recipient.cfscore.dyn ~CleanYear+Incumbent|bonica.rid+year+UniqueDistrict_CensusGroup|0|bonica.rid,
                 data=can_fe, subset=CensusLines==1& PFStatusSwitcher==1 &party==100 & HasDistanceCFDyn==1)
summary(ind_cf_dem)

## Subset by party: Republican, candidate fixed effects, incumbency indicator, SEs clustered by candidate
ind_cf_rep<-felm(recipient.cfscore.dyn ~CleanYear+Incumbent|bonica.rid+year+UniqueDistrict_CensusGroup|0|bonica.rid,
                 data=can_fe, subset=CensusLines==1& PFStatusSwitcher==1& party==200 & HasDistanceCFDyn==1)
summary(ind_cf_rep)

#AV: Do you think this is wroth showing by party? Substantive effect sizes are
#the same but the N is tiny on each, it's not really doing much. We had it in
#the last version I think, so we can write in the memo why we removed it.
#MK We subsetted to party here in Table 3
## because reviewer 3 asked us to in the last comment in
#the feedback.
## MK Replace table with code


## Format latex table
stargazer(ind_cf,ind_cf_incumb,ind_cf_dem,ind_cf_rep,
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
  select(year, sab, sen, dno, bonica.rid, name, party, seat, recipient.cfscore,
         recipient.cfscore.dyn,ran.general,CleanYear,CleanFirstRun,
         Distance_CFDyn,Distance_CFnonDyn, WonElection)%>%
  ## Convert year to numeric from factor to facilitate merging
  mutate(year=as.numeric(as.character(year)))

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

## Combined parties
Dyn<-t.test(pairedGC$CDistance_CFDyn, pairedGC$UCDistance_CFDyn, paired=TRUE)
nonDyn<-t.test(pairedGC$CDistance_CFnonDyn, pairedGC$UCDistance_CFnonDyn, paired=TRUE)

## Create lm models to incorporate T-Test results into stargazer
modNonDyn<-modDyn<-modNPDist<-lm(Crecipient.cfscore~Cyear, data=pairedDemsW)

## Store Estimate
modNonDyn$coefficients[1]<-nonDyn$estimate[[1]]
modDyn$coefficients[1]<-Dyn$estimate[[1]]
modNPDist$coefficients[1]<-NPResultsTable$Estimate[[3]]
## Create line for observations
## Have to add back one due to degree of freedom calculation,
## not sure how to pull out total
## observations from T-test object
stargazer(modDyn, modNonDyn,modNPDist,
          se=list(Dyn$stderr,nonDyn$stderr,NPResultsTable$SE[[3]]),
          p=list(Dyn$p.value, nonDyn$p.value, NPResultsTable$P[[3]]),
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
          add.lines = list(c("N",Dyn$parameter+1, nonDyn$parameter+1, NPResultsTable$N[[3]])))

## Binomial Test Results
## Dynamic CFScore Estimates: For how many cases does the
## publicly funded candidate have a larger CFScore distance
sum(pairedGC$CLMoreExtremeDyn, na.rm = TRUE)/sum(!is.na(pairedGC$CLMoreExtremeDyn))
binom.test(sum(pairedGC$CLMoreExtremeDyn, na.rm = TRUE),sum(!is.na(pairedGC$CLMoreExtremeDyn)), .5)

## Static CFScore Estimates: For how many cases does the
## publicly funded candidate have a larger CFScore distance
sum(pairedGC$CLMoreExtremeNonDyn, na.rm = TRUE)/sum(!is.na(pairedGC$CLMoreExtremeNonDyn))
binom.test(sum(pairedGC$CLMoreExtremeNonDyn, na.rm = TRUE), sum(!is.na(pairedGC$CLMoreExtremeNonDyn)), .5)


####Table 7 NP-Score Analysis####
## See NP_Score_Tab7.R script which
## merges NP scores to public financing candidate status list
## Can't just use the NP_Scores from can_fe because
## can_fe doesn't include np_scores for legislators in 2000,2002 in AZ
## and also has more missingness from the bonica data
## in 2012 and 2014 than if
## we combine the public financing list and the np_scores.
## Results are substantively identical however if we use can_fe
## np_scores


####SI-A2 Valence Analysis####

## Do public financing candidates receive higher voteshare?

## Need to use election_allres data because this is aggregated at the contest
## level and also contains counts for clean candidates at the district level.

valence<-election_allres%>%

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
incumb_cf<-felm(Distance_CFDyn~CleanYear*Incumbent|UniqueDistrict_CensusGroup+year|0|UniqueDistrict_CensusGroup,
                data=can_fe, subset=CensusLines==1 & HasDistanceCFDyn==1)
summary(incumb_cf)


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
major_party<-can_fe%>%filter(party %in%c(100,200))
party_cf<-felm(Distance_CFDyn~CleanYear*RepubIndicator|UniqueDistrict_CensusGroup+year|0|UniqueDistrict_CensusGroup,
               data=major_party, subset=CensusLines==1 & HasDistanceCFDyn==1)
summary(party_cf)
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
compet_cf<-felm(Distance_CFDyn~CompetitiveInteraction|UniqueDistrict_CensusGroup+year|0|UniqueDistrict_CensusGroup,
                data=can_fe, subset=CensusLines==1 & HasDistanceCFDyn==1)
summary(compet_cf)
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
                       select(UniqueDistrict_CensusGroup,CompetitiveElection)%>%
                       distinct()%>%
                       group_by(UniqueDistrict_CensusGroup,CompetitiveElection)%>%
                       count()%>%
                       group_by(UniqueDistrict_CensusGroup)%>%count()%>%filter(n>1)

totalDistricts<-can_fe%>%filter(CensusLines==1 & HasDistanceCFDyn==1)%>%
  select(UniqueDistrict_CensusGroup)%>%
  distinct()%>%group_by(UniqueDistrict_CensusGroup)%>%count()

paste(round(nrow(changeDistricts)/nrow(totalDistricts)*100),
"% of redistricting specific cycle districts see a change in competitiveness in our sample", sep="")

####SI-A5 State By State Analysis####

## Maine 2004-2016 Two-Way Fixed effects: CF Score
## SEs clustered by district
me_cf<-felm(Distance_CFDyn~CleanYear|UniqueDistrict_CensusGroup+year|0|UniqueDistrict_CensusGroup,
            data=can_fe, subset=CensusLines==1 & sab=="ME" & HasDistanceCFDyn==1)
summary(me_cf)


## Arizona 2004-2016 Two-Way Fixed effects: CF Score
## SEs clustered by district
az_cf<-felm(Distance_CFDyn~CleanYear|UniqueDistrict_CensusGroup+year|0|UniqueDistrict_CensusGroup,
            data=can_fe, subset=CensusLines==1 & sab=="AZ" & HasDistanceCFDyn==1)
summary(az_cf)

## CT 2008-2016 Two-Way Fixed effects: CF Score
## SEs clustered by district
ct_cf<-felm(Distance_CFDyn~CleanYear|UniqueDistrict_CensusGroup+year|0|UniqueDistrict_CensusGroup,
            data=can_fe, subset=CensusLines==1 & sab=="CT" & HasDistanceCFDyn==1)
summary(ct_cf)

## Pooled States (This is the same as Table 2 column 1)
## SEs clustered by district
all_cf<-felm(Distance_CFDyn~CleanYear|UniqueDistrict_CensusGroup+year|0|UniqueDistrict_CensusGroup,
             data=can_fe, subset=CensusLines==1 & HasDistanceCFDyn==1)
summary(all_cf)

## Effect size interpretation
## Quantile of ideological distance
quantile(can_fe$Distance_CFDyn, na.rm = TRUE, probs=seq(0,1,.1))
sd(can_fe$Distance_CFDyn, na.rm = TRUE)

## Calculate pooled variance
## Count total numbers observations for each state
nObs<-can_fe%>%filter(CensusLines==1 & HasDistanceCFDyn==1)%>%
               group_by(sab)%>%count()%>%pull()

## Average coefficients
AvgCoef<-(az_cf$coefficients[1]+ct_cf$coefficients[1]+me_cf$coefficients[1])/3

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
stargazer(all_cf,az_cf, ct_cf, me_cf,
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

## Calculate IPW for dynamic and stable CFScore estimate subsets
weights_CFDyn<-can_fe%>%filter(CensusLines==1 & HasDistanceCFDyn==1)%>%
  group_by(sab)%>%summarize(IPW=1/(n()/nrow(.)))

weights_CFnonDyn<-can_fe%>%filter(CensusLines==1& HasDistanceCFnonDyn==1)%>%
  group_by(sab)%>%summarize(IPW=1/(n()/nrow(.)))

## Join weights to can_fe and subset to relevant data
## Have to create dyn and ndyn subsets because lfe
## throws error when trying to use subset and weights in same felm call

can_fe_weights_dyn<-left_join(can_fe, weights_CFDyn, by=c("sab"))%>%
  filter(CensusLines==1 &HasDistanceCFDyn==1)
can_fe_weights_ndyn<-left_join(can_fe, weights_CFnonDyn, by=c("sab"))%>%
  filter(CensusLines==1 & HasDistanceCFnonDyn==1)

## Save weights
weightsPrint<-weights_CFDyn%>%pull()
weightsLabel<-paste("AZ:", round(weightsPrint[1],2), "CT:", round(weightsPrint[2],2), "ME:",
                    round(weightsPrint[3],2))

## Pooled States: Dynamic Distance Estimate
all_dyn<-felm(Distance_CFDyn~CleanYear|UniqueDistrict_CensusGroup+year|0|UniqueDistrict_CensusGroup,
              data=can_fe_weights_dyn, weights=can_fe_weights_dyn$IPW)
summary(all_dyn)

## Pooled States: Dynamic Distance Estimate Party Fixed Effects
all_dyn_party<-felm(Distance_CFDyn~CleanYear|UniqueDistrict_CensusGroup+year+party|0|UniqueDistrict_CensusGroup,
                    data=can_fe_weights_dyn, weights=can_fe_weights_dyn$IPW)
summary(all_dyn_party)

## Pooled States: Dynamic Distance Estimate Party Fixed Effects Time Trend
all_dyn_party_trend<-felm(Distance_CFDyn~CleanYear|UniqueDistrict_CensusGroup:RedistTime+party|0|UniqueDistrict_CensusGroup,
                          data=can_fe_weights_dyn, weights=can_fe_weights_dyn$IPW)
summary(all_dyn_party_trend)


## Pooled States: Non-Dynamic Distance Estimate
all_ndyn<-felm(Distance_CFnonDyn~CleanYear|UniqueDistrict_CensusGroup+year|0|UniqueDistrict_CensusGroup,
               data=can_fe_weights_ndyn, weights=can_fe_weights_ndyn$IPW)
summary(all_ndyn)

## Pooled States: Non-Dynamic Distance Estimate Party Fixed Effects
all_ndyn_party<-felm(Distance_CFnonDyn~CleanYear|UniqueDistrict_CensusGroup+year+party|0|UniqueDistrict_CensusGroup,
                     data=can_fe_weights_ndyn, weights=can_fe_weights_ndyn$IPW)
summary(all_ndyn_party)

## Pooled States: Non-Dynamic Distance Estimate Party Fixed Effects Time Trend
all_ndyn_party_trend<-felm(Distance_CFnonDyn~CleanYear|UniqueDistrict_CensusGroup:RedistTime+party|0|UniqueDistrict_CensusGroup,
                           data=can_fe_weights_ndyn, weights=can_fe_weights_ndyn$IPW)
summary(all_ndyn_party_trend)

## Create latex table
stargazer(all_dyn,all_dyn_party,all_dyn_party_trend,all_ndyn,all_ndyn_party,all_ndyn_party_trend,
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
all_dyn<-felm(Distance_CFDyn~CleanYear+tenure1|UniqueDistrict_CensusGroup+year|0|UniqueDistrict_CensusGroup,
              data=can_fe, subset=CensusLines==1 & HasDistanceCFDyn==1 )
summary(all_dyn)

## Pooled States: Add party fixed effects
all_dyn_party<-felm(Distance_CFDyn~CleanYear+tenure1|UniqueDistrict_CensusGroup+year+party|0|UniqueDistrict_CensusGroup,
                    data=can_fe, subset=CensusLines==1 & HasDistanceCFDyn==1)
summary(all_dyn_party)

## Pooled States: add Time Trend
all_dyn_party_trend<-felm(Distance_CFDyn~CleanYear+tenure1|UniqueDistrict_CensusGroup:RedistTime+party|0|UniqueDistrict_CensusGroup,
                          data=can_fe, subset=CensusLines==1 & HasDistanceCFDyn==1)
summary(all_dyn_party_trend)


## Pooled States: Non-dynamic distance estimate, district and year fixed effects,
## Standard errors clustered on redistricting cycle specific districts
## Subset data to 2000 and 2010 redistricting cycle data and observations
## which have a stable CF distance estimate
all_ndyn<-felm(Distance_CFnonDyn~CleanYear+tenure1|UniqueDistrict_CensusGroup+year|0|UniqueDistrict_CensusGroup,
               data=can_fe, subset=CensusLines==1 & HasDistanceCFnonDyn==1)
summary(all_ndyn)

##Pooled States: Add party fixed effects
all_ndyn_party<-felm(Distance_CFnonDyn~CleanYear+tenure1|UniqueDistrict_CensusGroup+year+party|0|UniqueDistrict_CensusGroup,
                     data=can_fe, subset=CensusLines==1 & HasDistanceCFnonDyn==1)
summary(all_ndyn_party)

##Pooled States: Add time trend
all_ndyn_party_trend<-felm(Distance_CFnonDyn~CleanYear+tenure1|UniqueDistrict_CensusGroup:RedistTime+party|0|UniqueDistrict_CensusGroup,
                           data=can_fe, subset=CensusLines==1 & HasDistanceCFnonDyn==1)
summary(all_ndyn_party_trend)

## Create latex table
stargazer(all_dyn,all_dyn_party,all_dyn_party_trend,all_ndyn,all_ndyn_party,all_ndyn_party_trend,
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
all_dwdime<-felm(Distance_DWDIME~CleanYear|UniqueDistrict_CensusGroup+year|0|UniqueDistrict_CensusGroup,
                 data=can_fe, subset=CensusLines==1 & HasDistanceDWDIME==1 & HasDistanceCFDyn==1)
summary(all_dwdime)


## Pooled States: Repeat Table 2 column 1 analysis, subsetting to data which is
## used in DW-DIME estimate for comparison.
all_dyn_subset<-felm(Distance_CFDyn~CleanYear|UniqueDistrict_CensusGroup+year|0|UniqueDistrict_CensusGroup,
                     data=can_fe, subset=CensusLines==1& HasDistanceDWDIME==1& HasDistanceCFDyn==1)
summary(all_dyn_subset)


## Create latex table
stargazer(all_dwdime,all_dyn_subset,
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
all_dyn<-felm(Distance_CFDyn~CleanFirstRun|UniqueDistrict_CensusGroup+year|0|UniqueDistrict_CensusGroup,
              data=can_fe, subset=CensusLines==1 & HasDistanceCFDyn==1)
summary(all_dyn)

## Pooled States: Dynamic Distance Estimate Party Fixed Effects
all_dyn_party<-felm(Distance_CFDyn~CleanFirstRun|UniqueDistrict_CensusGroup+year+party|0|UniqueDistrict_CensusGroup,
                    data=can_fe, subset=CensusLines==1 & HasDistanceCFDyn==1)
summary(all_dyn_party)

## Pooled States: Dynamic Distance Estimate Party Fixed Effects Time Trend
all_dyn_party_trend<-felm(Distance_CFDyn~CleanFirstRun|UniqueDistrict_CensusGroup:RedistTime+party|0|UniqueDistrict_CensusGroup,
                          data=can_fe, subset=CensusLines==1 & HasDistanceCFDyn==1)
summary(all_dyn_party_trend)

## Pooled States: Non-Dynamic Distance Estimate
all_ndyn<-felm(Distance_CFnonDyn~CleanFirstRun|UniqueDistrict_CensusGroup+year|0|UniqueDistrict_CensusGroup,
               data=can_fe, subset=CensusLines==1 & HasDistanceCFnonDyn==1)
summary(all_ndyn)

## Pooled States: Non-Dynamic Distance Estimate Party Fixed Effects
all_ndyn_party<-felm(Distance_CFnonDyn~CleanFirstRun|UniqueDistrict_CensusGroup+year+party|0|UniqueDistrict_CensusGroup,
                     data=can_fe, subset=CensusLines==1 & HasDistanceCFnonDyn==1)
summary(all_ndyn_party)

## Pooled States: Non-Dynamic Distance Estimate Party Fixed Effects Time Trend
all_ndyn_party_trend<-felm(Distance_CFnonDyn~CleanFirstRun|UniqueDistrict_CensusGroup:RedistTime+party|0|UniqueDistrict_CensusGroup,
                           data=can_fe, subset=CensusLines==1 & HasDistanceCFnonDyn==1)
summary(all_ndyn_party_trend)

## Create latex table
stargazer(all_dyn,all_dyn_party,all_dyn_party_trend,all_ndyn,all_ndyn_party,all_ndyn_party_trend,
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
