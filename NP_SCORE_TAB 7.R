## Load libraries (will paste in nice formatting for top part
## once we finalize the main code header. I need to
## figure out the packrat packge to for package version
## control)


#########################################################
##                 Mitchell Kilborn                    ##
##                 Arjun Vishwanath                    ##
##    Replication code for Table 7 included in:        ##
##    "Public Money Talks Too: Clean Elections         ##                  
##      and Representation in State Legislatures"      ##
#########################################################



###################Load Required Libraries#############################

rm(list=ls())
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
# R version 3.6.2 (2019-12-12)
# Platform: x86_64-apple-darwin15.6.0 (64-bit)
# Running under: macOS Catalina 10.15.4
# May 12, 2020


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

#############################Clean Data#######################################

#### Load public financing candidate list####
ftm<-read.csv("PublicFundingFTM.csv", stringsAsFactors = FALSE)##contains all public financing follow the money data,
pubfinlist<-ftm %>%
  mutate(House=ifelse(str_detect(Office, "HOUSE"), 1, 0), Senate=ifelse(str_detect(Office, "SENATE"), 1, 0))%>%
  filter( (State=="AZ") & Year <=2016 & (Senate==1 | House==1) & 
            ElectionStatus %!in% c("LOST-PRIMARY RUNOFF",#Grab general election competitors only
                                   "LOST-PRIMARY", "DISQUALIFIED-GENERAL",
                                   "WITHDREW-GENERAL"))%>%
  mutate(District=substr(Office, 17,18), District=ifelse(Senate==1,substr(Office, 18,19),District),
         District=as.numeric(as.character(District)),
         CleanWinner=ifelse(ElectionStatus=="WON-GENERAL", 1,0),
         CleanDem=ifelse(GeneralParty=="DEMOCRATIC", 1,0),
         CleanRepub=ifelse(GeneralParty=="REPUBLICAN", 1,0),
         MatchParty=case_when(GeneralParty=="DEMOCRATIC" ~ "D",
                              GeneralParty=="REPUBLICAN" ~ "R",
                              TRUE ~ "X"),
         MatchName=tolower(str_remove_all(Name, ",.*")),
         MatchedChar=0)%>%
  arrange(Year,Senate, District)%>%
  filter(StatusOfCandidate=="WON")


#### Load MRP Data####
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
  select(abb, Sen, District, Election_Year, mrp_mean)

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
  select(abb, Sen, District, Election_Year, mrp_mean)

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
  select(abb, Sen, District, Election_Year, mrp_mean)

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
  select(abb, Sen, District, Election_Year, mrp_mean)

## Bind together all observations, rename columns for clarity
MRP<-bind_rows(SH02, SH12, SS02, SS12)%>%
  mutate(State=abb,Election_Year=as.numeric(Election_Year),
         MRP_Mean=mrp_mean)%>%
  select(-c(abb, mrp_mean))%>%filter(State=="AZ")





####Read in Shor Data####
shor <- read.dta13("shor_update.dta")
##Get Shor data in legislator x year format
shor_az<- shor %>% filter(st =="AZ")%>%gather("Year", "YearServed", 6:53)%>%gather("YearDist", "District", 6:53)%>%
  mutate(District=ifelse(is.na(District)==TRUE, 0, District),District=as.numeric(District),##Fix NAs in district numbers
         DistYearNum=substrRight(YearDist,4), ##Create year value for valid observations to drop invalid observations
         YearServed=ifelse(is.na(YearServed)==TRUE, 0, YearServed), Senate=ifelse(str_detect(Year, "senate"),1,0),
         Year=as.numeric(substrRight(Year, 4)),##Create year number
         MatchName=tolower(str_remove_all(name, ",.*")),
         RanClean=0, MatchedLegislatorName=NA)%>%##Create string for name matching
  ##Because the gather commands above create duplicate district numbers across years, 
  #drop any district observations that don't match the year, so (DistYearNumber has to equal Year, the indicator for whether a Senator served in a session)
  filter( District !=0 & YearServed==1 & DistYearNum==Year)%>%
  mutate(Year=ifelse(Year==2016, 2015, Year),##Impute 2015 data from 2016 data
         Election_Year=as.numeric(Year)-1)%>%##2001 session = legislators elected in 2000
  ## Select even numbered years between 2000 and 2014
  filter(Election_Year>=2000 & Election_Year%%2==0 & Election_Year<2016)%>%
  arrange(Election_Year, Senate, District)

#### Match Shor data to public financing list####

## Create temporary version of public financing candidate list

pubfinlist_temp<-pubfinlist

## Match legislators to public financing list
for(i in 1:nrow(shor_az)){
  
  ## Select ith row
  
  subrow<-shor_az[i,]

  ## Get public financing list for year, chamber, district, party combination of
  ## shor data to see if there are any matches, which would suggest a public financing
  ## state legislator
  
  subPFL<-pubfinlist_temp%>%filter(Year==subrow$Election_Year & District==subrow$District & MatchParty==subrow$party
                              & Senate==subrow$Senate)

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
      select(st_id,name, Name, Year,District, Senate)%>%
      slice(c(1,3,4,6,8,9,11,12,13,14,15,16,19,20,23))%>%
      mutate(RanCleanFix=1,FixName=Name)%>%
      select(st_id, Year,RanCleanFix,FixName)

## Now merge the fixMissing data to the shor data, and impute
## clean status to legislators who have a legislator name-candidate name (coded by hand)
## match in the fixMissing data
shor_az<-shor_az%>%
          left_join(fixMissing, by=c("st_id"="st_id", "Election_Year"="Year") )%>%
           mutate(RanClean=ifelse(is.na(RanCleanFix)==FALSE, 1,RanClean),
                MatchedLegislatorName=ifelse(is.na(FixName)==FALSE, 
                                             FixName,MatchedLegislatorName))

####Within Party Ideology Paired T-Test####

## This section calculates paired t-test for Arizona state legislators
## by party for ideology


## Republicans
## Positive value of Clean/Unclean ideology T-Test indicates that Clean
## Republicans are more conservative than Unclean Republicans

## Select republicans, create clean and unclean 
## ideology variables, merge variables by district and election year
reps<-shor_az  %>%filter(party=="R")
cleanReps<-reps %>%filter( RanClean==1)%>%mutate(CleanIdeology=np_score)%>%select(-np_score)
uncleanReps<-reps %>%filter(RanClean==0)%>%mutate(UncleanIdeology=np_score)%>%select(-np_score)
rep_test<-left_join(cleanReps, uncleanReps, by=c("Election_Year"="Election_Year", "District"="District"))

## Paired T-Test
rtest<-t.test(rep_test$CleanIdeology, rep_test$UncleanIdeology, na.rm=TRUE, paired = TRUE)


## Democrats
## Negative value of Clean/Unclean ideology T-Test indicates that Clean Democrats are more conservative than 
## Unclean Democrats

## Select Democrats, create clean and unclean 
## ideology variables, merge variables by district and election year
dems<-shor_az  %>%filter(party=="D")
cleanDems<-dems %>%filter( RanClean==1)%>%mutate(CleanIdeology=np_score)%>%select(-np_score)
uncleanDems<-dems %>%filter(RanClean==0)%>%mutate(UncleanIdeology=np_score)%>%select(-np_score)
dem_test<-left_join(cleanDems, uncleanDems, by=c("Election_Year"="Election_Year", "District"="District"))

## Paired T-Test
dtest<-t.test(dem_test$CleanIdeology, dem_test$UncleanIdeology, na.rm=TRUE, paired = TRUE)

## Create lm models to incorporate T-Test results into stargazer
modRTest<-modDTest<-lm(np_score~RanClean, data=dems)

## Store Estimates for stargazer presentation below
modRTest$coefficients[1]<-rtest$estimate[[1]]
modDTest$coefficients[1]<-dtest$estimate[[1]]


####Within party NP-Score Distance Paired T-Test####

## Calculate NP-Score ideological distance using MRP
## district estimates within district, year, party.
## Given that Arizona started its 2000 cycle census lines in
## 2004, have to subset to >=2004

## Join shor voting data to MRP estimates for chamber and year
mrp_distcalc<-left_join(shor_az , MRP, by=c("Election_Year"="Election_Year",
                                            "District"="District","Senate"="Sen"))%>%
## Drop independents, MRP scores available starting 2004
              filter(Election_Year>=2004 & party !="X")

## Regress legislator ideology on district ideology
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
                              select(-np_score)
unclean_mrpdist<-mrp_distcalc%>%filter( RanClean==0)%>%
                              mutate(UncleanDistance=Distance_NP,UncleanIdeology=np_score)%>%
                              select(-np_score)

## Merge clean and unclean ideological distance scores by year, district, and party,

mrp_paired_test<-left_join(clean_mrpdist, unclean_mrpdist, by=c("Election_Year"="Election_Year",
                                                                "District"="District",
                                                                "party"="party"))%>%
  ## Also add indicator for whether clean elections legislator is more ideologically distant 
  ## for binomial test below
                 filter(is.na(CleanDistance)==FALSE & is.na(UncleanDistance)==FALSE)%>%
                 mutate(CLMoreExtreme=ifelse(CleanDistance>UncleanDistance, 1, 0))




## Within party, within district, within year paired t-test shows significant
## ideological distance to district ideal point between clean and unclean
## legislators
npDistTest<-t.test(mrp_paired_test$CleanDistance, mrp_paired_test$UncleanDistance, paired=TRUE)##Distance to district
modDistTest<-lm(RanClean~Senate, data=dems)
modDistTest$coefficients[1]<-npDistTest$estimate[[1]]


## Stargazer for NPAT Scores
## Also must move 1st row third column to second row third column,
## can't get stargazer to move standard error and coefficient, but 
## DV is different in third column from first two.
stargazer(modDTest, modRTest, modDistTest, 
          se=list(dtest$stderr, rtest$stderr, npDistTest$stderr),
          p=list(dtest$p.value, rtest$p.value, npDistTest$p.value),
          model.names = FALSE, model.numbers = TRUE,
          dep.var.labels.include = FALSE,
          column.labels   = c("Democratic Legislator Pairs", "Republican Legislator Pairs", "All Pairs"),
          column.separate = c(1,1,1),
          star.cutoffs = c(0.05, 0.01, 0.001),
          omit=c("RanClean"),
          style = 'apsr',
          order=c("Constant"),
          table.placement = "H",
          title=c("Paired T-Test Estimates: Arizona State Legislators' Polarization and Ideological Distance NPAT Score, 2000-2014"),
          label=c("NPDistance TTest"),
          covariate.labels =c("NPAT Score Diff.", "Ideological Distance"),
          multicolumn = FALSE,
          omit.stat = c("rsq","adj.rsq","f","ser", "n"),
          add.lines = list(c("N", dtest$parameter+1, rtest$parameter+1, npDistTest$parameter+1)))


NPResultsTable<-tibble(Name=c("DemNPScore","RepNPScore","AllIdeoDist"),
                       Estimate=c(dtest$estimate[[1]],
                                  rtest$estimate[[1]],
                                  npDistTest$estimate[[1]]),
                       SE=c(dtest$stderr, rtest$stderr, npDistTest$stderr),
                       P=c(dtest$p.value, rtest$p.value, npDistTest$p.value),
                       N=c(dtest$parameter+1, rtest$parameter+1, npDistTest$parameter+1))
#saveRDS(NPResultsTable, file="NPResultsTable.RDS")
## Count how many times the clean elections legislator is more ideologically distant
## than the non-clean legislator and perform binomial exact tests
binom.test(sum(mrp_paired_test$CLMoreExtreme, na.rm = TRUE),
           sum(!is.na(mrp_paired_test$CLMoreExtreme)),(1/2))


