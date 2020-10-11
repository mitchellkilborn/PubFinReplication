#########################################################
##                 Mitchell Kilborn                    ##
##                 Arjun Vishwanath                    ##
##              Creates shor_az.RDS                    ##
##        "Public Money Talks Too: How Public          ##
##     Campaign Financing Degrades Representation"     ##
#########################################################

###################Load Required Libraries#############################


# install required packages if they are missing:
list.of.packages <- c("tidyverse", "stargazer","scales","sjPlot", "lfe","readstata13","stringdist")
new.packages <- list.of.packages[!(list.of.packages %in% installed.packages()[,"Package"])];
if(length(new.packages)){install.packages(new.packages)}
lapply(list.of.packages, require, character.only = TRUE)


## Package version check
packinfo <- as.data.frame(installed.packages(fields = c("Package", "Version")))
packinfo[which(packinfo$Package %in% list.of.packages),c("Package", "Version")]

## Package                      Version
## lfe                           2.8-5
## tidyverse                     1.3.0
## scales                        1.1.0
## sjPlot                        2.8.2
## stargazer                     5.2.2



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


####Operations#####
## 1. Match Shor and McCarty (2011) state legislator ideology scores
## to public financing status for each legislator and save resultant
## dataframe as shor_az.RDS
## 2. Merge MRP estimates of state legislative district ideology 
## to shor_az to create NPResultsBaseFrame.RDS.



#### Load public financing candidate list####

## Public financing  list gathered from https://www.followthemoney.org/
## File includes all recipients from PUBLIC FUND, which is what
## the National Institute on Money in Politics (NIMP) uses to
## categorize campaign funds received by candidates from
## public financing programs in applicable states.


ftm<-read.csv("PublicFundingFTM.csv", stringsAsFactors = FALSE)##contains all public financing follow the money data,
pubfinlist<-ftm %>%
  mutate(House=ifelse(str_detect(Office, "HOUSE"), 1, 0),## Create House indicator 
         Senate=ifelse(str_detect(Office, "SENATE"), 1, 0))%>%## Create Senate indicator
  filter( (State=="AZ") & Year <=2016 & (Senate==1 | House==1) & ## Select AZ State Leg before 2016 obs
            ElectionStatus %!in% c("LOST-PRIMARY RUNOFF",#Grab general election competitors only
                                   "LOST-PRIMARY", "DISQUALIFIED-GENERAL",
                                   "WITHDREW-GENERAL"))%>%
  mutate(District=substr(Office, 17,18), ## Get District ID for House observations
         District=ifelse(Senate==1,substr(Office, 18,19),District),## Get District ID for Senate Observations
         District=as.numeric(as.character(District)),## Convert District to numeric
         CleanWinner=ifelse(ElectionStatus=="WON-GENERAL", 1,0),## Create PF Winner Indicator
         CleanDem=ifelse(GeneralParty=="DEMOCRATIC", 1,0),## Create PF Dem Indicator
         CleanRepub=ifelse(GeneralParty=="REPUBLICAN", 1,0),## Create PF Rep Indicator
         MatchParty=case_when(GeneralParty=="DEMOCRATIC" ~ "D", ## Create Party variable
                              GeneralParty=="REPUBLICAN" ~ "R",
                              TRUE ~ "X"),
         MatchName=tolower(str_remove_all(Name, ",.*")),## Select last name for matching
         MatchedChar=0)%>%## Create variable counting how many characters in pubfinlist match legislator name in shor_az
  arrange(Year,Senate, District)%>%
  ## Select victorious candidates who receive a roll-call
  ## voting score by entering state legislature
  filter(StatusOfCandidate=="WON")

#### Load MRP Data####

## MRP Data Source: http://www.americanideologyproject.com/
## from Tausanovitch, C., & Warshaw, C. (2013). Measuring Constituent Policy
## Preferences in Congress, State Legislatures, and Cities. #
## The Journal of Politics, 75(2), 330–342. https://doi.org/10.1017/S0022381613000042

## Operations:
## Stack MRP Estimates for each redistricting cycle for upper and lower chambers to merge with
## candidate ideology estimates below
## Create vector of years with same length as one year's worth of observations of
## the MRP dataset. (ie 2000, 2000...2000, 2002)
## Select relevant states
## Mark Sen=0 for lower chamber,  Sen=1 for upper chamber.
## Filter for each state and relevant years for each state given redistricting
## Select relevant variables and repeat for each chamber


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
MRP<-bind_rows(SH02, SH12, SS02, SS12)%>%
  mutate(State=abb,Election_Year=as.numeric(Election_Year),
         MRP_Mean=mrp_mean)%>%
  dplyr::select(-c(abb, mrp_mean))%>%filter(State=="AZ")





#### Read in Shor-McCarty Data####

## Citation: 
## Shor, B., & McCarty, N. M. (2011). The Ideological Mapping of American Legislatures.
## American Political Science Review, 105(3), 530–551. 
## https://dataverse.harvard.edu/dataset.xhtml?persistentId=doi:10.7910/DVN/BSLEFD
## Note that we use version 5 of this dataset, released in May 2018.
## Read in data

shor <- read.dta13("shor_update.dta")
##Get Shor data in legislator x year format
shor_az<- shor %>% filter(st =="AZ")%>%gather("Year", "YearServed", 6:53)%>%gather("YearDist", "District", 6:53)%>%
  mutate(District=ifelse(is.na(District)==TRUE, 0, District),District=as.numeric(District),##Fix NAs in district numbers
         DistYearNum=substrRight(YearDist,4), ##Create year value for valid observations to drop invalid observations
         YearServed=ifelse(is.na(YearServed)==TRUE, 0, YearServed), Senate=ifelse(str_detect(Year, "senate"),1,0),
         Year=as.numeric(substrRight(Year, 4)),##Create year number
         MatchName=tolower(str_remove_all(name, ",.*")),## Set MatchName to legislator last name
         RanClean=0, MatchedLegislatorName=NA)%>%##Create string for name matching
  ##Because the gather commands above create duplicate district numbers across years, 
  #drop any district observations that don't match the year, so (DistYearNumber has to equal Year, the indicator for whether a Senator served in a session)
  filter( District !=0 & YearServed==1 & DistYearNum==Year)%>%
  mutate(Year=ifelse(Year==2016, 2015, Year),##Impute 2015 data from 2016 data
         Election_Year=as.numeric(Year)-1)%>%##2001 session = legislators elected in 2000
  ## Select even numbered years between 2000 and 2014
  filter(Election_Year>=2000 & Election_Year%%2==0 & Election_Year<2016)%>%
  arrange(Election_Year, Senate, District)

#### Match Shor-McCarty data to public financing list####

## Create temporary version of public financing candidate list
pubfinlist_temp<-pubfinlist

## Match legislators to public financing list
for(i in 1:nrow(shor_az)){
  
  ## Select ith row
  subrow<-shor_az[i,]
  
  ## Get public financing list for year, chamber, district, party combination of
  ## shor data to see if there are any matches, which would suggest a public financing
  ## state legislator
  
  subPFL<-pubfinlist_temp%>%filter(Year==subrow$Election_Year & 
                                     District==subrow$District &
                                     MatchParty==subrow$party &
                                     Senate==subrow$Senate)
  
  ## If there aren't any matches for that year-chamber-district-party combination
  ## between the public financing list and the shor data, 
  ## mark legislator as not using public financing and skip to next row of shor data
  
  if(nrow(subPFL)==0){
    shor_az$RanClean[i]<-0
    next
  }
  ## If there are public financing candidates for the
  ## year-chamber-district-party combination iterate through the rows of the
  ## public financing list and calculate the string distance between each
  ## candidate's last name in the public financing data and the legislator's
  ## last name in the shor data.
  
  for(j in 1:nrow(subPFL)){
    subPFL$MatchedChar[j]<-stringdist(subrow$MatchName, subPFL$MatchName[j])
  }
  
  ## If the minimum string distance between a public financing list's last name
  ## and the legislator last name is less than 2, mark the legislator as a clean
  ## candidate because we have identified a match and store the name from the
  ## public financing list in the shor data for subsequent QA.
  
  if(subPFL$MatchedChar[which.min(subPFL$MatchedChar)]<2){
    shor_az$RanClean[i]<-1
    shor_az$MatchedLegislatorName[i]<-subPFL$Name[which.min(subPFL$MatchedChar)]
    
    ## Finally, drop the matched legislator from the temporary public financing list to guard 
    ## against false repeat matches in a given year district
    
    pubfinlist_temp<-pubfinlist_temp%>%filter(!(Name==subPFL$Name[which.min(subPFL$MatchedChar)] &
                                                  Year==subPFL$Year[which.min(subPFL$MatchedChar)] ))
  }
}



## Fix ~ 15 Misses from pubfinlist_temp, where the public financing list name
## wasn't matched to a legislator name
## Merge the Shor data to the fixMissing data by district, year, party, chamber,
## then compare names and select rows where the candidate name matches the legislator name
fixMissing<-pubfinlist_temp%>%
  left_join(shor_az,by=c("Year"="Election_Year", "District"="District",
                         "MatchParty"="party", "Senate"="Senate"))%>%
  dplyr::select(st_id,name, Name, Year,District, Senate)%>%
  slice(c(1,3,4,6,8,9,11,12,13,14,15,16,19,20,23))%>%
  mutate(RanCleanFix=1,FixName=Name)%>%
  dplyr::select(st_id, Year,RanCleanFix,FixName)

## Now merge the fixMissing data to the shor data, and impute
## clean status to legislators who have a legislator name-candidate name (coded by hand)
## match in the fixMissing data
shor_az<-shor_az%>%
  left_join(fixMissing, by=c("st_id"="st_id", "Election_Year"="Year") )%>%
  mutate(RanClean=ifelse(is.na(RanCleanFix)==FALSE, 1,RanClean),## If they have a fixed RanClean, substitute
         MatchedLegislatorName=ifelse(is.na(FixName)==FALSE, ## Input fix name as MatchedLegislatorName
                                      FixName,MatchedLegislatorName))%>%
  dplyr::select(name, party, st, st_id, np_score, Election_Year, District, RanClean,
                Senate)




#### Merge shor_az with MRP####
## To apply the error propagation function in 
## PrepEFFunctions.R to the paired T-test in 
## Table 6, we need to merge shor_az, containing
## legislator NPAT scores and their year specific public financing
## status with the ideology of their district as measured by
## Tausanovitch and Warshaw (2013)


## Join shor voting data to MRP estimates for chamber and year
## Drop state column from MRP
mrp_distcalc<-left_join(shor_az , MRP, by=c("Election_Year"="Election_Year",
                                            "District"="District",
                                            "Senate"="Sen"))%>%
  dplyr::select(-State)


## Save mrp_distcalc object for use in PrepEPFunctions.R
## and in RunAnalyses.R
#saveRDS(mrp_distcalc, file="shor_az.RDS")

