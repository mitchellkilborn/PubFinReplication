#########################################################
##                 Mitchell Kilborn                    ##
##                 Arjun Vishwanath                    ##
##     Code to merge NPAT scores to Bonica Data        ##
##    "Public Money Talks Too: Clean Elections         ##
##      and Representation in State Legislatures"      ##
#########################################################



###################Load Required Libraries#############################

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
## scales                       1.1.1
## sjPlot                       2.8.5
## stargazer                    5.2.2
## stringdist                   0.9.6
## tidyverse                    1.3.0
## readstata13                  0.9.2


#############################Session Info#######################################

sessionInfo()
## R version 4.0.2 (2020-06-22)
## Platform: x86_64-apple-darwin17.0 (64-bit)
## Running under: macOS Catalina 10.15.6



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
#############################OPERATIONS#######################################

## Read in Bonica DIME Data
## Read in Shor-McCarty data, format to legislator-year long format
## Match Bonica candidates to their Shor-McCarty entry

## This is complicated by Arizona's multi-member districts, so
## we can't just merge by district-year-state. Instead we have to
## match candidates in the Bonica DIME dataset to their Shor-McCarty
## counterpart by name.

####Read in Bonica DIME Data####



## Data only includes 2000-2016
## Read in Fixed 2014 bonica data which was missing ran.general variables
fixed2014<-read.csv("MissingGeneralElectionAZCTMEFixed.csv", 
                    stringsAsFactors = FALSE)%>%
  distinct(bonica.rid, .keep_all=TRUE)%>% ## Drop duplicates by bonica.rid 
  select(-c(name,district, seat,state, X, MissingCandidates_2014))%>%## Drop unneeded variables
  ## Insert 2014 ran.general fixes into ran.general column 
  mutate(ran.general_2014=ifelse(is.na(ran.general_2014),0, ran.general_2014),
         cycle=as.character(cycle))
## Load Bonica (2016) data
## Load DIME dataset from: 
## Bonica, A. (2016). Database on Ideology, Money in Politics, and Elections: 
## Public version 2.0 [Computer file]. Stanford, CA: Stanford University Libraries.
## Retrieved from https://data.stanford.edu/dime
dime<-loadRData("dime_recipients_all_1979_2018.rdata")

## Create object which will be matched to
dimeClean<-dime%>%
  ## Select relevant variables
  select(election,cycle, bonica.rid, name,lname, 
                         state, seat, district,ran.general,winner )%>%
  ## Filter to 2000-2016 and AZ, ME, and CT state legislative observations
  filter(election>=2000 & election<=2016 & state%in%c("AZ", "ME", "CT") &
           seat %in%c("state:upper", "state:lower"))%>%
  arrange(election, state, district)%>%
  left_join(fixed2014, by=c("cycle","bonica.rid"))%>%## Merge fixed 2014 ran.general variable
  mutate(ran.general=ifelse(cycle==2014, ran.general_2014, ran.general),
         Sen=ifelse(seat=="state:upper",1,0),## Create Senate indicator
         District=parse_number(district),## Get district number
         District=str_remove_all(District, "-"),## remove - from district number
         NPAT_Score=NA, MatchName=NA,
         ShorMcCartyID=NA)%>%## Create variables to receive NPAT scores and MatchedName
  
  ## Subset the data to general election winners who are the only 
  ## ones who will vote in the state legislature and therefore receive
  ## an NPAT score
  filter(ran.general==1 & winner=="W")


#### Read in Shor-McCarty Data####

## Citation: 
## Shor, B., & McCarty, N. M. (2011). The Ideological Mapping of American Legislatures.
## American Political Science Review, 105(3), 530–551. 
## https://dataverse.harvard.edu/dataset.xhtml?persistentId=doi:10.7910/DVN/BSLEFD
## Note that we use version 5 of this dataset, released in May 2018.
## Read in data
shor<-readstata13::read.dta13("shor_update.dta")

## Next split into house and senate sections.
## The shor data is formatted so that each unique legislator in the dataset
## has one row and a series of columns indicating which legislative sessions they
## served in and the district IDs for the sessions they served in.
## So, to match this data to the Bonica data, we have to 
## convert the Shor-McCarty data to the long format
shorHouse<-shor%>%
  filter(st %in% c("AZ","ME","CT"))%>% ## Select AZ, CT, and ME states
  dplyr::select(c(1:5, 30:53, 78:101))%>%## Select House Session/District Columns
  
  ## Gather wide data into long format to 
  ## create year variable for each legislator for each of their sessions
  gather("SessionYear", "YearServed",6:29)%>%
  mutate(SessionYear=parse_number(SessionYear))%>%## Extract Year for each session
  ## The gather command creates a row for each session year for each legislator
  ##  but legislators who don't actually serve in a given year won't have a
  ## value for YearServed, so drop by YearServed
  filter(is.na(YearServed)==FALSE)%>%
  ## Gather wide data into long format again, this time to create
  ## district variable for each legislator for each of their sessions
  gather("DistYear", "District", 6:29)%>%
  mutate(DistYear=parse_number(DistYear))%>%## Extract District-Year number
  ## Drop duplicates created by gather process,
  ## SessionYear must equal the value for DistYear for there to be an actual existing observation
  filter(is.na(YearServed)==FALSE & is.na(District)==FALSE &SessionYear==DistYear)%>% 
  ## Convert district to numeric, add Chamber variable identifying rows in this dataset
  ## as coming from the lower chamber for each state.
  mutate(District=as.numeric(as.character(District)), Chamber="House")%>%
  arrange(DistYear,District)



## Convert Upper chamber data from Shor-McCarty (2011) to wide format
## so that each row is a legislator-session roll-call voting observation
shorSenate<-shor%>%
  filter(st %in% c("AZ","ME","CT"))%>% ## Select legislator observations from AZ, CT, ME
  dplyr::select(c(1:29, 54:77))%>%## Select House Session/District Columns
  ## Gather wide data into long format to 
  ## create year variable for each legislator for each of their sessions
  gather("SessionYear", "YearServed",6:29)%>%
  mutate(SessionYear=parse_number(SessionYear))%>%
  ## The gather command creates a row for each session year for each legislator
  ##  but legislators who don't actually serve in a given year won't have a
  ## value for YearServed, so drop by YearServed
  filter(is.na(YearServed)==FALSE)%>%
  gather("DistYear", "District", 6:29)%>%
  mutate(DistYear=parse_number(DistYear))%>%## Get district ID
  ## Drop duplicates created by gather process,
  ## SessionYear must equal the value for DistYear for there to be an actual existing observation
  filter(is.na(YearServed)==FALSE & is.na(District)==FALSE &SessionYear==DistYear)%>%
  ## Convert district to numeric, add Chamber variable identifying rows in this dataset
  ## as coming from the lower chamber for each state.
  mutate(District=as.numeric(as.character(District)), Chamber="Senate")%>%arrange(DistYear,District)


## Bind together the shorHouse and shorSenate datasets
shor<-bind_rows(shorHouse, shorSenate)%>%
  ## Session Year is always 1 year after Election Year for odd number year
  ## sessions. Note that Shor-McCarty data has observation
  ## for even and odd session years
  mutate(ElectionYear=SessionYear-1,
         District=as.numeric(District),## Convert district to numeric
         Sen=ifelse(Chamber=="Senate",1,0), ## Create Senate indicator
         LastName=tolower(str_remove_all(name, ",.*")),## Get last name for matching
         MatchDist=100)%>%## Set default MatchDist to 100
  arrange(ElectionYear, st, District, Chamber)


## As the matching process is easier when subsetting to 
## candidates who are actually elected to the state legislature because
## there are fewer potential matches for each observation in the Bonica
## dataset, this matching procedure is less involved.

## Iterate through rows of Bonica DIME data containing state legislators
for(i in 1:nrow(dimeClean)){
  
  ## Get ith row
  subrow<-dimeClean[i,]
  ## Get the Shor-McCarty data
  ## for the same election year, state, 
  ## chamber, and district as the
  ## Bonica DIME data
  subShor<-shor%>%filter(ElectionYear==subrow$election & st==subrow$st &
                           Sen==subrow$Sen & District==subrow$District)
  ## If there aren't any matches in the shor data for the
  ## ith Bonica data candidate, don't match
  if(nrow(subShor)==0){next}
  
  ## Calculate string distance between Bonica candidate name
  ## and shor legislator name
  for(j in 1:nrow(subShor)){
    subShor$MatchDist[j]<-stringdist(subrow$lname, subShor$LastName[j])
  }
  
  ## Save matched NPAT score and the match name
  dimeClean$NPAT_Score[i]<-subShor$np_score[which.min(subShor$MatchDist)]
  dimeClean$MatchName[i]<-subShor$LastName[which.min(subShor$MatchDist)]
  dimeClean$ShorMcCartyID[i]<-subShor$st_id[which.min(subShor$MatchDist)]
}

## Select columns to save and merge with can_fe.RDS dataset
save<-dimeClean%>%select(NPAT_Score,cycle, bonica.rid, ShorMcCartyID)%>%
      filter(is.na(NPAT_Score)==FALSE)
#saveRDS(save, file="NP_ScoresMatchedToBonicaRIDS2000_2014_AZMECT.RDS")