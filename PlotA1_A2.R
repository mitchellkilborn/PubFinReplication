#########################################################
##                 Mitchell Kilborn                    ##
##                 Arjun Vishwanath                    ##
##        Code to plot Figures A.1 & A.2 for           ##
##    "Public Money Talks Too: Clean Elections         ##
##      and Representation in State Legislatures"      ##
#########################################################

# install required packages if they are missing:
list.of.packages <- c("tidyverse")
new.packages <- list.of.packages[!(list.of.packages %in% installed.packages()[,"Package"])];
if(length(new.packages)){install.packages(new.packages)}
lapply(list.of.packages, require, character.only = TRUE)


## Package version check
packinfo <- as.data.frame(installed.packages(fields = c("Package", "Version")))
packinfo[which(packinfo$Package %in% list.of.packages),c("Package", "Version")]

## Package                      Version
## scales                       1.1.1
## tidyverse                    1.3.0



#############################Session Info#######################################

sessionInfo()
## R version 4.0.2 (2020-06-22)
## Platform: x86_64-apple-darwin17.0 (64-bit)
## Running under: macOS Catalina 10.15.6



#############################Functions#######################################
'%!in%' <- function(x,y)!('%in%'(x,y))# check if value not in list



####Operations####

## 1. Clean state legislative election results for relevant time
##    period from Klarner (2018). 
## 2. Combine election results with counts of full public financing
##    candidates
## 3. Plot trends in full public campaign financing over time.




####Clean Klarner (2018) State Legislative Election Results####


## Data downloaded from:
## https://dataverse.harvard.edu/dataset.xhtml?persistentId=doi:10.7910/DVN/DRSACA
## Klarner, Carl, 2018, "State Legislative Election Returns, 1967-2016:
## Restructured For Use", https://doi.org/10.7910/DVN/DRSACA, 
## Harvard Dataverse, V1, UNF:6:hjXo+znmhZCoZ5P4cMo7Yw== [fileUNF]


## Had to convert Klarner's original dta object to RDS
## format to be able to upload project to github
## while observing 100mb file limit.
# election<-read.dta13("102slersuoacontest20181024-1.dta")
# saveRDS(election, file="102slersuoacontest20181024-1.RDS")

## Read in contest level election results.
election<-readRDS("102slersuoacontest20181024-1.RDS")

## Select Relevant years
election_all<-election %>%
  filter( (year>=2000 &  sab == "ME") | (year>=2008 & sab=="CT") |
            (year>=2000 &  sab=="AZ") )%>%##Select relevant observations
  dplyr::select(year, sab, sen, dno, dcand, eseats,
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
election_all<-election_all%>%
  group_by(sab, year, sen, dno)%>%## For each state-year-district combination
  summarize(TotalRepCan=sum(rcand, na.rm = TRUE),## Count total Republican candidates
            TotalDemCan=sum(dcand, na.rm = TRUE))%>%## Count total Democratic candidates
  gather("Party", "TotalCandidates", 5:6)%>%## Convert to wide format
  ## Rename factor levels for presentation purposes
  mutate(Party=recode(Party, "TotalRepCan"="REPUBLICAN",
                      "TotalDemCan"="DEMOCRATIC"))%>%
  ## Count total candidates by state, year, and party
  group_by(sab, year, Party)%>%
  summarize(TotalCandidates=sum(TotalCandidates))


####Create District Counts of Public Financing Candidates####

## This section creates state-year-district level counts of public
## financing candidates.

## Public financing  list gathered from https://www.followthemoney.org/
## File includes all recipients from PUBLIC FUND, which is what
## the National Institute on Money in Politics (NIMP) uses to
## categorize campaign funds received by candidates from
## public financing programs in applicable states.

ftm<-read.csv("PublicFundingFTM.csv", stringsAsFactors = FALSE)


count_district<-ftm %>%
  mutate(House=ifelse(str_detect(Office, "HOUSE"), 1, 0),## Create indicator for House candidates
         Senate=ifelse(str_detect(Office, "SENATE"),1,0))%>%## Create indicator for Senate candidates
  ## Select AZ, CT, and ME state legislative candidates from 2000-2016, excluding 2011, 2013, 2015
  filter( ( (State=="ME") | (State=="CT") | (State=="AZ") ) & ( House==1 | Senate==1)
          & Year <=2016 &  Year %!in% c(2011, 2013, 2015) &
            ElectionStatus %!in% c("LOST-PRIMARY RUNOFF",## Select General election candidates only
                                   "LOST-PRIMARY", "DISQUALIFIED-GENERAL",
                                   "WITHDREW-GENERAL"))%>%
  mutate(District=substr(Office, 17,18), ## Extract district ID for House candidates
         District=ifelse(Senate==1,substr(Office, 18,19),District),## Extract district ID for Senate candidates
         District=as.numeric(as.character(District)),## Convert district ID to numeric
         CleanWinner=ifelse(ElectionStatus=="WON-GENERAL", 1,0),## Create indicator for whether PF candidate wins general
         CleanDem=ifelse(GeneralParty=="DEMOCRATIC", 1,0),## Create indicator for PF Democratic candidate
         CleanChallenger=ifelse(IncumbencyStatus!="INCUMBENT",1,0),## Create indicator for PF Challenger candidate
         CleanIncumbent=ifelse(IncumbencyStatus=="INCUMBENT",1,0),## Create indicator for PF Incumbent candidate
         CleanRepub=ifelse(GeneralParty=="REPUBLICAN", 1,0))%>%## Create indicator for PF Republican candidate
  arrange(Senate, District, Year)%>%
  group_by(State, Year, GeneralParty)%>%
  ## For each state-year-party combination, count the total number of 
  ## PF candidates, total PF general election winners,
  ## total PF incumbents, total PF challengers
  summarize(TotalCleanCan=n(), 
            TotalCleanWinners=sum(CleanWinner), 
            TotalCleanIncumbents=sum(CleanIncumbent),
            TotalCleanChallengers=sum(CleanChallenger))%>%
  ## Filter to Democrats and Republicans
  filter(GeneralParty %in% c("DEMOCRATIC", "REPUBLICAN"))

## Create seats dataframe to calculate 
## percentage of legislators using public financing
## below
seats<-data_frame(State=c("AZ", "CT", "ME"), Seats=c(90, 187, 186 ))

## Join election total candidates to clean candidate data
partplot<-left_join(election_all, count_district, 
                    by=c("sab"="State", "year"="Year", "Party"="GeneralParty"))

## Calculate pct of candidates using public financing by party
party_pct<-partplot%>%mutate(PartyCanPart=100*TotalCleanCan/TotalCandidates)%>%
  spread(Party, PartyCanPart)## Convert to wide format
dem_year_pct<-party_pct%>%mutate(DemocratPct=DEMOCRATIC)%>%dplyr::select(sab, year, DemocratPct)%>%
  filter(is.na(DemocratPct)==FALSE)
rep_year_pct<-party_pct%>%mutate(RepublicanPct=REPUBLICAN)%>%dplyr::select(sab, year, RepublicanPct)%>%
  filter(is.na(RepublicanPct)==FALSE)

## Calculate pct of all legislators using public financing (combining parties)
leg_pct<-partplot%>%group_by(sab, year)%>%summarize(TotalCleanWinners=sum(TotalCleanWinners))%>%
  left_join(seats, by=c("sab"="State"))%>%mutate(LegislatorPct=100*TotalCleanWinners/Seats)%>%
  dplyr::select(sab, year, LegislatorPct)

## Combine party and legislator PF participation tables for plotting
plotframe<-dem_year_pct%>%left_join(rep_year_pct, by=c("sab"="sab", "year"="year"))%>%
  left_join(leg_pct, by=c("sab"="sab", "year"="year"))%>%
  gather("Category", "Pct", 3:5)%>%## Convert to long format
  ungroup()%>%
  ## Edit factor labels for presentation purposes
  mutate(Category=recode(Category,"DemocratPct"="Democratic Candidates",
                         "RepublicanPct"="Republican Candidates",
                         "LegislatorPct"="Legislators"),
         sab=trimws(sab),
         sab=recode(sab, "CT"="Connecticut", "AZ"="Arizona", "ME"="Maine"))



#### Plot Figure A.1####
####Plot Republican, Democrat, and Legislator participation

ggplot(plotframe, aes(x=year, y=Pct/100, group=Category)) +
  geom_line(aes(color=Category))+
  ylab("Pct. Participation in Public Campaign Financing")+
  geom_point(aes(color=Category))+
  xlab("Election Year")+
  scale_color_manual(values=c("DodgerBlue", "Black","IndianRed"))+
  xlim(2000,2016)+
  scale_x_continuous(breaks = seq(2000, 2016, by=2))+
  theme_classic()+
  ylim(0,100)+
  scale_y_continuous(labels=percent)+
  theme(axis.text=element_text(size=24),
        axis.title=element_text(size=20,face="bold"),
        axis.text.x = element_text(angle = 90, hjust = 1),
        legend.position = c(.75, .25),
        legend.title=element_blank(),
        text=element_text(size=24))+facet_wrap(~sab, nrow=2)


#### Plot Figure A.2####
## Calculate program participation among challengers and incumbents
## by each state-election-year.
IC_plot<-count_district%>%group_by(State, Year)%>%
  summarize(ChallengerShare=100*sum(TotalCleanChallengers)/sum(TotalCleanCan),
            IncumbentShare=100*sum(TotalCleanIncumbents)/sum(TotalCleanCan))%>%
  ungroup()%>%
  gather("Category", "Pct", 3:4)%>%## Convert to long format
  ## Relabel factors for presentation
  mutate(Category=recode(Category, "ChallengerShare"="Challenger Share",
                                    "IncumbentShare" ="Incumbent Share"),
                  State=recode(State, "ME"="Maine", "AZ"="Arizona", 
                               "CT"="Connecticut"))

## Plot program participation among challengers and incumbents
ggplot(IC_plot, aes(x=Year, y=Pct/100, group=Category)) +
  geom_line(aes(color=Category))+
  ylab("Pct. of All Public Financing Candidates")+
  geom_point(aes(color=Category))+
  xlab("Election Year")+
  scale_color_manual(values=c("DodgerBlue","IndianRed"))+
  xlim(2000,2016)+
  scale_x_continuous(breaks = seq(2000, 2016, by=2))+
  theme_classic()+ylim(0,100)+
  scale_y_continuous(labels=percent)+
  theme(axis.text=element_text(size=24),
        axis.title=element_text(size=20,face="bold"),
        axis.text.x = element_text(angle = 90, hjust = 1),
        legend.position = c(.75, .25),
        legend.title=element_blank(),
        text=element_text(size=24))+facet_wrap(~State, nrow=2)
