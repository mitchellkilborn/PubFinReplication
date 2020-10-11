#########################################################
##                 Mitchell Kilborn                    ##
##                 Arjun Vishwanath                    ##
##        Code to recreate Valence.RDS for             ##
##        "Public Money Talks Too: How Public          ##
##     Campaign Financing Degrades Representation"     ##
#########################################################



###################Load Required Libraries#############################

# install required packages if they are missing:
list.of.packages <- c("tidyverse", "stargazer","scales","sjPlot","lfe","stringdist","readstata13", "arm")
new.packages <- list.of.packages[!(list.of.packages %in% installed.packages()[,"Package"])];
if(length(new.packages)){install.packages(new.packages)}
lapply(list.of.packages, require, character.only = TRUE)


## Package version check
packinfo <- as.data.frame(installed.packages(fields = c("Package", "Version")))
packinfo[which(packinfo$Package %in% list.of.packages),c("Package", "Version")]

## Package                      Version
## arm                          1.11-2
## lfe                          2.8-5.1
## scales                       1.1.1
## sjPlot                       2.8.5
## stargazer                    5.2.2
## stringdist                   0.9.6
## tidyverse                    1.3.0
## readstata13                  0.9.2

#############################Functions#######################################
'%!in%' <- function(x,y)!('%in%'(x,y))# check if value not in list
substrRight <- function(x, n){##Reverse substring function
  substr(x, nchar(x)-n+1, nchar(x))
}
## Replicate rows of DF
row_rep <- function(df, n) {
  df[rep(1:nrow(df), times = n),]
}
## Load RData with name of Rdata as object name
loadRData <- function(fileName){
  #loads an RData file, and returns it
  load(fileName)
  get(ls()[ls() != "fileName"])
}


#############################Session Info#######################################

sessionInfo()
## R version 4.0.2 (2020-06-22)
## Platform: x86_64-apple-darwin17.0 (64-bit)
## Running under: macOS Catalina 10.15.6


####Operations####



#### Clean Klarner contest level election results####

## Data downloaded from:
## https://dataverse.harvard.edu/dataset.xhtml?persistentId=doi:10.7910/DVN/DRSACA
## Klarner, Carl, 2018, "State Legislative Election Returns, 1967-2016:
## Restructured For Use", https://doi.org/10.7910/DVN/DRSACA, 
## Harvard Dataverse, V1, UNF:6:hjXo+znmhZCoZ5P4cMo7Yw== [fileUNF]

## Need this data for valence analysis and
## competitiveness interaction analysis.

## Had to convert Klarner's original dta object to RDS
## format to be able to upload project to github
## while observing 100mb file limit.
# election<-read.dta13("102slersuoacontest20181024-1.dta")
# saveRDS(election, file="102slersuoacontest20181024-1.RDS")
## Load Klarner data
election<-readRDS("102slersuoacontest20181024-1.RDS")

## Filter relevant observations for study
election_all<-election %>%
  filter( (year>=2000 &  sab == "ME") | (year>=2008 & sab=="CT") |
            (year>=2000 &  sab == "AZ") ) %>%
  
  ## Select relevant variables
  
  dplyr::select(year, sab, sen, dno, dvote, rvote, ovote, dcand,
                rcand, ocand, dinc, rinc, oinc, dwin, rwin, owin, dinc2, 
                rinc2, oinc2, dinc3,rinc3,oinc3)%>%
  
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

####Create District Counts of Public Financing Candidates####

## This section creates state-year-district level counts of public
## financing candidates.

## Public financing  list gathered from https://www.followthemoney.org/
## File includes all recipients from PUBLIC FUND, which is what
## the National Institute on Money in Politics (NIMP) uses to
## categorize campaign funds received by candidates from
## public financing programs in applicable states.
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
            ElectionStatus %!in% c("LOST-PRIMARY RUNOFF",#gab public financing GE contestants only
                                   "LOST-PRIMARY", "DISQUALIFIED-GENERAL",
                                   "WITHDREW-GENERAL") & ElectionType %!in% c("SPECIAL"))%>%##Drop special elections
  mutate(District=substr(Office, 16,18), District=ifelse(Senate==1,substr(Office, 17,19),District),##Grab district id numbers
         District=as.numeric(as.character(District)),## Convert district to character
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

## Join the election contest data from Klarner (election_all)
## with the district counts of public financing candidates by
## year, state, chamber, and district. (count_district)
## Note, this generates NA counts
## of public financing candidates in contests
## which saw no clean candidates

election_allres<-left_join(election_all, count_district, by=c("year"="Year", "sab"="State",
                                                              "sen"="Senate",
                                                              "dno"="District"))


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
         year=as.factor(year), sab=as.factor(sab))%>%
  dplyr::select(sab, year, sen, dno, UniqueDistrict_CensusGroup,CleanFactor,
                dinc, rinc, oinc, 
                ocand,dcand,rcand, RedistTime, CensusLines, RepVS)

#saveRDS(valence, "Valence.RDS")

