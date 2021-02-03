#########################################################
##                 Mitchell Kilborn                    ##
##                 Arjun Vishwanath                    ##
##            Code to recreate can_fe.RDS for          ##
##        "Public Money Talks Too: How Public          ##
##     Campaign Financing Degrades Representation"     ##
#########################################################



###################Load Required Libraries#############################

rm(list=ls())
# install required packages if they are missing:
list.of.packages <- c("tidyverse", "stargazer","scales","sjPlot","lfe","stringdist","readstata13", "arm", "stringi")
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
## stringi                      1.4.6
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




#### Operations included in this file####
## 1. Import and clean Bonica (2016) DIME dataset.
## 2. Identify public financing candidates in Bonica (2016) dataset
## 3. Add Tausanovitch and Warshaw (2013) data to Bonica (2016) dataset.
## 4. Add Klarner (2018) data to Bonica (2016) dataset.
## 5. Create vectors of simulated coefficients for error propagation
##    analyses where applicable.
## 6. Save can_fe.RDS object, which is used as the basis for most
##    regression analyses in paper.
## 7. Save valence.RDS object, which is used for Table C.1





#### Clean Bonica (2016) data####



## Load DIME dataset from: 
## Bonica, A. (2016). Database on Ideology, Money in Politics, and Elections: 
## Public version 2.0 [Computer file]. Stanford, CA: Stanford University Libraries.
## Retrieved from https://data.stanford.edu/dime
dime<-loadRData("dime_recipients_all_1979_2018.rdata")


## Create ideo object. 
## Subset to 2000-2016 for AZ, ME, and CT state legislative races
ideo<-dime%>%filter(cycle>=2000 & cycle<=2016 & state%in%c("AZ", "ME", "CT") &
                      seat %in%c("state:upper", "state:lower" ))%>%
  mutate(name=gsub("[^[:alnum:] ]", "",name),## Drop numbers from name
         name=tolower(gsub(" ", "", name)),## format name for matching
         House=ifelse(str_detect(seat, "state:lower"), 1, 0), ## Create House Indicator
         Senate=ifelse(str_detect(seat, "state:upper"), 1, 0),## Create Senate Indicator
         District=as.numeric(parse_number(district)),## Extract numbers from District
         District=as.numeric(gsub("-","",District)),## Drop - from District
         ## Create CleanYear-Indicator for PF
         ## Create MatchName & MatchPct used to record results of matching
         CleanYear=NA, MatchName=NA, MatchPct=NA)%>%
  arrange(state,cycle,Senate, District)

## `ideo' contains candidate ideology scores, but external
## data on public financing status is needed to identify which candidates
## are public/privately financed. The next section loads the list of 
## public financing candidates for the relevant states.

####Read in NIMP data####

## Public financing  list gathered from https://www.followthemoney.org/
## File includes all recipients from PUBLIC FUND, which is what
## the National Institute on Money in Politics (NIMP) uses to
## categorize campaign funds received by candidates from
## public financing programs in applicable states.

ftm<-read.csv("PublicFundingFTM.csv", stringsAsFactors = FALSE)

## Create pubfinlist which will be matched to ideo
pubfinlist<-ftm %>%
  mutate(House=ifelse(str_detect(Office, "HOUSE"), 1, 0), ## House indicator
         Senate=ifelse(str_detect(Office, "SENATE"), 1, 0))%>%## Senate Indicator
  ## Select observations from AZ, CT, and ME from 2000 to 2016 in state legislature
    filter( State %in% c("AZ","CT","ME") & Year <=2016 & (Senate==1 | House==1) & 
  ## Grab general election competitors only
            ElectionStatus %!in% c("LOST-PRIMARY RUNOFF",
                                   "LOST-PRIMARY", "DISQUALIFIED-GENERAL",
                                   "WITHDREW-GENERAL"))%>%
  mutate(District=readr::parse_number(Office),## Get district ID
         CleanWinner=ifelse(ElectionStatus=="WON-GENERAL", 1,0),## Indicator if candidate won
         CleanDem=ifelse(GeneralParty=="DEMOCRATIC", 1,0),## PF Democrat indicator
         CleanRepub=ifelse(GeneralParty=="REPUBLICAN", 1,0), ## PF Republican indicator
         MatchName=gsub("[^[:alnum:] ]", "",Name),## Create name in matching format (no space, lowercase)
         MatchName=tolower(gsub(" ", "", MatchName)),##Remove whitespaces
         MatchedChar=0)%>%## Create variable which will store how many characters are matched
  arrange(State, Year, Senate, District)


#### Match Candidate Ideology Scores with Public Financing Status####

## This section matches candidate ideology scores from Bonica (2016)
## to the list of public financing candidates.

## Operations: 
## Loop through years
## Loop through chamber
## Loop through district
## Get list of candidates in district-chamber-year combination
## from Bonica (2016) and public financing list. Iterate
## through lists and calculate whether a candidate
## has a match in the public financing data or not.


## Create starting tibbles, to which the for loop 
## will add matches as they are found
start_dime<-ideo[1,]
start_dime[1,]<-NA
patch<-ideo[1,]
patch[1,]<-NA
pct<-.2##Set tolerance for matches
years<-c(seq(from=2000, to =2016, by=2))##Create vector of usable years
chamber<-c(0,1)##Create indicator for Senate/House
states<-c("AZ", "CT", "ME")## Create vector of states

## Start for loop matching public financing status to Bonica (2016) data
for(s in 1:3){##Loop through state
  sub_state<-states[s]
  for(c in 1:2){##Loop through chambers
    sub_chamber<-chamber[c]##Set sub chamber
    for(y in 1:length(years)){##Loop through years
      sub_year<-years[y]##Set sub year
      for(d in 1:151){##Loop through districts
        
        ##Select possible matches from shor data and public financing data for each district, chamber, year
        sub_dime<-filter(ideo, Senate==sub_chamber & cycle==sub_year & District==d & state==sub_state)
        sub_ftm<-filter(pubfinlist, Senate==sub_chamber & Year==sub_year & District==d & State==sub_state)
        
        ##If no one in that district, year chamber combination used public financing, skip district
        if(nrow(sub_ftm)==0){
          
          ##If there isn't even DIME data for that district year combo, add NAs
          if(nrow(sub_dime)==0){
            sub_patch<-patch%>%mutate(Senate=sub_chamber,cycle=sub_year,District=d,state=sub_state)
            start_dime<-rbind(start_dime, sub_patch) ## Bind NA row to start_dime
            next}
          start_dime<-rbind(start_dime, sub_dime)## Bind NA row to start_dime
          next
        }
        ## Case where public financing status is known,
        ## but Bonica data is missing for that district, chamber, state combo
        ## Add NA row to start dime, and mark that there was MissingBonicaData
        ## for that district, chamber, state combo
        if(nrow(sub_dime)==0 & sub_ftm>0){
          sub_patch<-patch%>%mutate(Senate=sub_chamber,cycle=sub_year,
                                    District=d,state=sub_state,
                                    MatchName="MissingBonicaData")
          start_dime<-rbind(start_dime, sub_patch)
          next}
        
        
        ## Go through each dime data row
        for(i in 1:nrow(sub_dime)){
          ## Get candidate name from Bonica data, split into 2 char strings
          dime_name<-unlist(stri_sub(sub_dime[i,11], seq(1, stri_length(sub_dime[i,11]),by=2), length=2))
          ## Go through ftm and calculate what percentage of Bonica data candidate's 
          ## name characters are in public financing list
          for(j in 1:nrow(sub_ftm)){
            ## Get candidate name from public financing list, split into 2 char strings
            ftm_name<-unlist(stri_sub(sub_ftm[j,19], seq(1, stri_length(sub_ftm[j,19]),by=2), length=2))
            ## Store pct of matched 2 char strings in sub_ftm
            sub_ftm[j,20]<-sum(!is.na(pmatch(dime_name, ftm_name)))/nchar(sub_ftm[j,19], allowNA = TRUE)
          }
          ## Identify best match in public financing list
          ## based on percentage of characters matched to
          ## Bonica candidate name
          bestmatch<-sub_ftm[which.max(sub_ftm[,20]),20]
          
          ## See if best match meets threshold of accuracy,
          ## if so, mark Bonica data row as clean candidate and record 
          ## matched name from pubfinlist for QA
          if(bestmatch>pct){
            sub_dime[i,78]<-1
            sub_dime[i,79]<-sub_ftm[which.max(sub_ftm[,20]),1]
            sub_ftm<-sub_ftm[-which.max(sub_ftm[,20]),]
            sub_dime[i,80]<-bestmatch
          }
          ##If Only 1 legislator used public financing in a given year, 
          ## district, chamber, break out of district loop
          if(nrow(sub_ftm)==0){break}
        }
        ## Add row to running tibble start_dime
        start_dime<-rbind(start_dime, sub_dime)
      }
    }
  }
}



####QA and Spot Repairs for Matching Procedure####

## The matching procedure is quite accurate, but was conservative
## in matching candidates from Bonica (2016) to the public financing
## list. The next section checks which candidates were listed on
## the public financing list, but do not have a match in the
## newly create start_dime dataset. This often occurs the 
## use of candidate nicknames or candidate name spelling varies
## between datasets, such that the match level between the two strings
## is below the tolerance threshold. Or as is often the case,
## one of the datasets will include a middle name while the other does not.


## Operations:
## Identify list of people who are in NIMP data but not in matched Bonica data
## Select only general election results (not off year special elections)
## for available data (<=2016) from this vintage of Bonica data



## Get list of candidates from pubfinlist whose names are not in the 
## start_dime dataset
a<-pubfinlist[pubfinlist$Name %!in% start_dime$MatchName,]
a<-a%>%arrange(State, Senate, District,Year)%>%
  filter(Year %!in% c(2011,2013, 2015))%>%
  dplyr::select(Name, Year,State,Senate,District)

## Bind the a dataset back to the start_dime dataset
## to easily check which candidates are in the pubfinlist dataset
## but were not matched to the Bonica/start_dime dataset
## because the tolerance threshold used in the matching procedure
## was too low. 
look<-start_dime%>%
  dplyr::select(bonica.rid,name,state, District,cycle,Senate)%>%
  mutate(cycle=as.numeric(cycle))
a<-a%>%left_join(look, by=c("State"="state","District",
                            "Year"="cycle","Senate"="Senate"))


## Begin manually marking candidates as
## public financing (start_dime[,78]=CleanYear)
## and storing the MatchName for further QA (start_dime[,79]=MatchName)
##CARSON, EVERETT BROWNIE
start_dime[which(start_dime$cycle=="2016" & start_dime$bonica.rid=="cand68967807"),78]<-1
start_dime[which(start_dime$cycle=="2016" & start_dime$bonica.rid=="cand68967807"),79]<-"CARSON, EVERETT BROWNIE"


##BELLOWS, SHENNA LEE
start_dime[which(start_dime$cycle=="2016" & start_dime$bonica.rid=="cand68967981"),78]<-1
start_dime[which(start_dime$cycle=="2016" & start_dime$bonica.rid=="cand68967981"),79]<-"BELLOWS, SHENNA LEE"


##FOGELMAN, LAURIE W 
start_dime[which(start_dime$cycle=="2016" & start_dime$bonica.rid=="cand69789"),78]<-1
start_dime[which(start_dime$cycle=="2016" & start_dime$bonica.rid=="cand69789"),79]<-"FOGELMAN, LAURIE W"


##DRINKWATER, GARY A
start_dime[which(start_dime$cycle=="2016" & start_dime$bonica.rid=="cand68967997"),78]<-1
start_dime[which(start_dime$cycle=="2016" & start_dime$bonica.rid=="cand68967997"),79]<-"DRINKWATER, GARY A"


##ESTABROOK, PHILIP ANTHONY
start_dime[which(start_dime$cycle=="2016" & start_dime$bonica.rid=="cand68967884"),78]<-1
start_dime[which(start_dime$cycle=="2016" & start_dime$bonica.rid=="cand68967884"),79]<-"ESTABROOK, PHILIP ANTHONY"


##ZEIGLER JR, STANLEY PAIGE
start_dime[which(start_dime$cycle=="2016" & start_dime$bonica.rid=="cand68967904"),78]<-1
start_dime[which(start_dime$cycle=="2016" & start_dime$bonica.rid=="cand68967904"),79]<-"ZEIGLER JR, STANLEY PAIGE"


##SPEAR, JOHN ALDEN
start_dime[which(start_dime$cycle=="2016" & start_dime$bonica.rid=="cand68967866"),78]<-1
start_dime[which(start_dime$cycle=="2016" & start_dime$bonica.rid=="cand68967866"),79]<-"SPEAR, JOHN ALDEN"


##WILLIAMS, SCOTT DOUGLAS
start_dime[which(start_dime$cycle=="2016" & start_dime$bonica.rid=="cand68967991"),78]<-1
start_dime[which(start_dime$cycle=="2016" & start_dime$bonica.rid=="cand68967991"),79]<-"WILLIAMS, SCOTT DOUGLAS"


##THERIAULT, TIMOTHY S
start_dime[which(start_dime$cycle=="2016" & start_dime$bonica.rid=="cand68967809"),78]<-1
start_dime[which(start_dime$cycle=="2016" & start_dime$bonica.rid=="cand68967809"),79]<-"THERIAULT, TIMOTHY S"


##TWITCHELL, MICHAEL W
start_dime[which(start_dime$cycle=="2016" & start_dime$bonica.rid=="cand68967918"),78]<-1
start_dime[which(start_dime$cycle=="2016" & start_dime$bonica.rid=="cand68967918"),79]<-"TWITCHELL, MICHAEL W"


##RUMSON, RACHEL LYN
start_dime[which(start_dime$cycle=="2016" & start_dime$bonica.rid=="cand68968007"),78]<-1
start_dime[which(start_dime$cycle=="2016" & start_dime$bonica.rid=="cand68968007"),79]<-"RUMSON, RACHEL LYN"

##FULLER, ROGER JASON
start_dime[which(start_dime$cycle=="2016" & start_dime$bonica.rid=="cand68967963"),78]<-1
start_dime[which(start_dime$cycle=="2016" & start_dime$bonica.rid=="cand68967963"),79]<-"FULLER, ROGER JASON"


##SYLVESTER, MICHAEL A
start_dime[which(start_dime$cycle=="2016" & start_dime$bonica.rid=="cand68967909"),78]<-1
start_dime[which(start_dime$cycle=="2016" & start_dime$bonica.rid=="cand68967909"),79]<-"SYLVESTER, MICHAEL A"


##PARKER, JENNIFER ELLEN
start_dime[which(start_dime$cycle=="2016" & start_dime$bonica.rid=="cand68967976"),78]<-1
start_dime[which(start_dime$cycle=="2016" & start_dime$bonica.rid=="cand68967976"),79]<-"PARKER, JENNIFER ELLEN"


##PULCHLOPEK, RONALD S
start_dime[which(start_dime$cycle=="2016" & start_dime$bonica.rid=="cand68968049"),78]<-1
start_dime[which(start_dime$cycle=="2016" & start_dime$bonica.rid=="cand68968049"),79]<-"PULCHLOPEK, RONALD S"


##BOYLE, JAMES
start_dime[which(start_dime$cycle=="2014" & start_dime$bonica.rid=="cand68967490"),78]<-1
start_dime[which(start_dime$cycle=="2014" & start_dime$bonica.rid=="cand68967490"),79]<-"BOYLE, JAMES"


##CORNELL DU HOUX, REBECCA A
start_dime[which(start_dime$cycle=="2014" & start_dime$bonica.rid=="cand68967662"),78]<-1
start_dime[which(start_dime$cycle=="2014" & start_dime$bonica.rid=="cand68967662"),79]<-"CORNELL DU HOUX, REBECCA A"

##BETZ, LEE ANN
start_dime[which(start_dime$cycle=="2016" & start_dime$bonica.rid=="cand68968033"),78]<-1
start_dime[which(start_dime$cycle=="2016" & start_dime$bonica.rid=="cand68968033"),79]<-"BETZ, LEE ANN"


##ALLEN, ELIZABETH A
start_dime[which(start_dime$cycle=="2014" & start_dime$bonica.rid=="cand68967505"),78]<-1
start_dime[which(start_dime$cycle=="2014" & start_dime$bonica.rid=="cand68967505"),79]<-"ALLEN, ELIZABETH A"


##KENNEDY JR, EDWARD M (TED)
start_dime[which(start_dime$cycle=="2014" & start_dime$bonica.rid=="cand69018566"),78]<-1
start_dime[which(start_dime$cycle=="2014" & start_dime$bonica.rid=="cand69018566"),79]<-"KENNEDY JR, EDWARD M (TED)"

##WILMS, FRIEDRICH N
start_dime[which(start_dime$cycle=="2016" & start_dime$bonica.rid=="cand30386"),78]<-1
start_dime[which(start_dime$cycle=="2016" & start_dime$bonica.rid=="cand30386"),79]<-"WILMS, FRIEDRICH N"

##WILMS, FRIEDRICH N
start_dime[which(start_dime$cycle=="2014" & start_dime$bonica.rid=="cand30386"),78]<-1
start_dime[which(start_dime$cycle=="2014" & start_dime$bonica.rid=="cand30386"),79]<-"WILMS, FRIEDRICH N"

##MASTRO, PETE DEL
start_dime[which(start_dime$cycle=="2016" & start_dime$bonica.rid=="cand30130"),78]<-1
start_dime[which(start_dime$cycle=="2016" & start_dime$bonica.rid=="cand30130"),79]<-"MASTRO, PETE DEL"


##ANEST-KLETT, CAROL ANNE
start_dime[which(start_dime$cycle=="2014" & start_dime$bonica.rid=="cand69019465"),78]<-1
start_dime[which(start_dime$cycle=="2014" & start_dime$bonica.rid=="cand69019465"),79]<-"ANEST-KLETT, CAROL ANNE"



##Beth Bye
start_dime[which(start_dime$cycle=="2008" & start_dime$bonica.rid=="cand31082"),78]<-1
start_dime[which(start_dime$cycle=="2008" & start_dime$bonica.rid=="cand31082"),79]<-"BYE, ELIZABETH ANN (BETH)"

##Melissa Olson
start_dime[which(start_dime$cycle=="2008" & start_dime$bonica.rid=="cand31012"),78]<-1
start_dime[which(start_dime$cycle=="2008" & start_dime$bonica.rid=="cand31012"),79]<-"OLSON RILEY, MELISSA M"

##Brendan Sharkey
start_dime[which(start_dime$cycle=="2008" & start_dime$bonica.rid=="cand31065"),78]<-1
start_dime[which(start_dime$cycle=="2008" & start_dime$bonica.rid=="cand31065"),79]<-"SHARKEY, J BRENDAN"

##Anthony Hwang
start_dime[which(start_dime$cycle=="2008" & start_dime$bonica.rid=="cand31107"),78]<-1
start_dime[which(start_dime$cycle=="2008" & start_dime$bonica.rid=="cand31107"),79]<-"HWANG, ANTHONY (TONY)"

##RUSSO III, ROBERT D
start_dime[which(start_dime$cycle=="2008" & start_dime$bonica.rid=="cand35760"),78]<-1
start_dime[which(start_dime$cycle=="2008" & start_dime$bonica.rid=="cand35760"),79]<-"RUSSO III, ROBERT D"

##DUFF, ROBERT B (BOB)
start_dime[which(start_dime$cycle=="2008" & start_dime$bonica.rid=="cand31028"),78]<-1
start_dime[which(start_dime$cycle=="2008" & start_dime$bonica.rid=="cand31028"),79]<-"DUFF, ROBERT B (BOB)"

##LOFTUS III, WILLIAM P (BILL)
start_dime[which(start_dime$cycle=="2010" & start_dime$bonica.rid=="cand30067"),78]<-1
start_dime[which(start_dime$cycle=="2010" & start_dime$bonica.rid=="cand30067"),79]<-"LOFTUS III, WILLIAM P (BILL)"

##BECKETT III, STEWART (CHIP)
start_dime[which(start_dime$cycle=="2010" & start_dime$bonica.rid=="cand30847"),78]<-1
start_dime[which(start_dime$cycle=="2010" & start_dime$bonica.rid=="cand30847"),79]<-"BECKETT III, STEWART (CHIP)"

##SUZIO JR, LEONARD FRANK
start_dime[which(start_dime$cycle=="2010" & start_dime$bonica.rid=="cand30323"),78]<-1
start_dime[which(start_dime$cycle=="2010" & start_dime$bonica.rid=="cand30323"),79]<-"SUZIO JR, LEONARD FRANK"

##LIBBY JONES, SHARON H
start_dime[which(start_dime$cycle=="2000" & start_dime$bonica.rid=="cand68381"),78]<-1
start_dime[which(start_dime$cycle=="2000" & start_dime$bonica.rid=="cand68381"),79]<-"LIBBY JONES, SHARON H"

##HARMON, ROBERT RYAN
start_dime[which(start_dime$cycle=="2006" & start_dime$bonica.rid=="cand68859"),78]<-1
start_dime[which(start_dime$cycle=="2006" & start_dime$bonica.rid=="cand68859"),79]<-"HARMON, ROBERT RYAN"

####Drop Duplicates from start_dime####

## After checking to ensure that all candidates listed in pubfinlist
## are accounted for in the start_dime dataset, we next proceed to
## drop some of the duplicated entries that exist in the Bonica dataset.
## These survived previous QA because they are instances where
## the same candidate has multiple bonica.rid labels. bonica.rid is a stable
## identifier that should be constant between years and unique to candidates,
## but we found a small number of instances where candidates had multiple entries.


## Drop duplicated entries.
# Drop duplicate John Allen in AZ 2014 House 15th
start_dime<-start_dime[-which(start_dime$bonica.rid=="cand68976913" 
                              & start_dime$cycle=="2014"),]

# Drop Craig A Miner entry in CT 2014 Senate 30th
start_dime<-start_dime[-which(start_dime$bonica.rid=="cand31061" 
                              & start_dime$cycle=="2014"),]

# Drop duplicate Stafstrom entry in CT 2016 House 129th
start_dime<-start_dime[-which(start_dime$bonica.rid=="cand69019186" 
                              & start_dime$cycle=="2016"),]

# Rename Gale Alexander wrong bonica.rid
# There was a duplicate of David Alexander in 2014 CT House
start_dime$bonica.rid[which(start_dime$bonica.rid=="cand123503"
                            & start_dime$fname=="gale")]<-"fixcand123503"

# Drop duplicate Stephen Harding Jr. entry in CT 2016 House 107th
start_dime<-start_dime[-which(start_dime$bonica.rid=="cand69019481" 
                              & start_dime$cycle=="2016"),]

# Maine House District 91 2014 has two incumbents, Evangelos and Winchenbach
# competing against each other

# Maine House District 98 2014 has two incumbents, Brooks and Gillway
# competing against each other

# Arizona House District 28 2012 has three incumbents competing
# against each other


## Another type of duplicate error occurs when two candidates are erroneously
## given the same bonica.rid, which causes problems when we use candidate
## fixed effects in some analyses. So we assign one of 
## the candidates a new bonica.rid.


## Give Rick Gray a new bonica.rid because Linda Gray has
## the same bonica.rid
start_dime$bonica.rid[which(start_dime$bonica.rid=="cand6021"
                            & start_dime$name=="grayrick")]<-"fixcand6021"

##Give Hilda Santiago a different bonica.rid from
##Ezequiel Santiago 
start_dime$name[which(start_dime$name=="santiagoforstaterepresentative")]<-"santiagohildae"
start_dime$bonica.rid[which(start_dime$bonica.rid=="cand31090"
                            & start_dime$name=="santiagohildae")]<-"fixcand31090"

##Drop Warren Petersen AZ 2014 Senate observation
start_dime<-start_dime[-which(start_dime$bonica.rid=="cand123013" 
                              & start_dime$cycle=="2014" & start_dime$Senate==1),]

##Give David Butler a different bonica.rid from 
##Kelli Butler to duplicate in 2014
start_dime$bonica.rid[which(start_dime$bonica.rid=="cand123041" &
                              start_dime$name=="butlerkelli")]<-"fixcand123041"

##Drop duplicate Cynthia Cartier observation in 
##2014 CT SEN 12.
start_dime<-start_dime[-which(start_dime$bonica.rid=="cand30139" 
                              & start_dime$cycle=="2014" & start_dime$District==12 & start_dime$Senate==1),]

##Drop duplicate stephen mullins in CT SEN 10
start_dime<-start_dime[-which(start_dime$bonica.rid=="cand30241" 
                              & start_dime$cycle=="2014" & start_dime$District==10 & 
                                start_dime$Senate==1),]

##Drop duplicate John Michael ME District 62 in 2014
start_dime<-start_dime[-which(start_dime$bonica.rid=="cand47173" 
                              & start_dime$cycle=="2014" & start_dime$District==62),]

##Drop duplicate Mark Eves 2014 District 146
start_dime<-start_dime[-which(start_dime$bonica.rid=="cand69888" 
                              & start_dime$cycle=="2014" & start_dime$District==146),]

##Give David Farnsworth a different bonica.rid from 
##Eddie Farnsworth in 2016
start_dime$bonica.rid[which(start_dime$bonica.rid=="cand5926" &
                              start_dime$name=="farnsworthdavidchristian")]<-"fixcandcand5926"

## Mark Linda Binder as a winner in 2000 (Henry Camarot is already marked)
## cand5883 and 2000. It seems that Binder is already marked, 

## Drop Russell Pearce misprints: 2008 District 25
start_dime<-start_dime[-which(start_dime$bonica.rid=="cand53400" 
                              & start_dime$cycle=="2008" & start_dime$District==25),]
## Drop Russell Pearce misprints: 2010 District 25,
start_dime<-start_dime[-which(start_dime$bonica.rid=="cand53400" 
                              & start_dime$cycle=="2010" & start_dime$District==25),]
##Drop Russell Pearce misprints: 2012 District 18
start_dime<-start_dime[-which(start_dime$bonica.rid=="cand53400" 
                              & start_dime$cycle=="2012" & start_dime$District==18),]


## Drop duplicate Steven B. Yarborough cand68976910
start_dime<-start_dime[-which(start_dime$bonica.rid=="cand68976910"),]

## Drop Andre Cushing district (10) for 2014, he is actually in 2014 33
start_dime<-start_dime[-which(start_dime$bonica.rid=="cand125454" 
                              & start_dime$cycle=="2014" & start_dime$District==33),]


## A few candidates also had observations for some years they ran for office,
## but not others. To supply data for candidate's missing years, 
## we duplicate their rows from the closest observation year
## Add extra Livvy Floren observation for 2010
extraFlorenRow2010<-start_dime[which(start_dime$bonica.rid=="cand31063" 
                                     & start_dime$cycle=="2008" & start_dime$Cand.ID=="CT7474"),]
extraFlorenRow2010$election<-"2010"
extraFlorenRow2010$cycle<-"2010"
extraFlorenRow2010$fecyear<-"2010"

## Add extra Livvy Floren observation for 2012
extraFlorenRow2012<-start_dime[which(start_dime$bonica.rid=="cand31063" 
                                     & start_dime$cycle=="2008" & start_dime$Cand.ID=="CT7474"),]
extraFlorenRow2012$election<-"2012"
extraFlorenRow2012$cycle<-"2012"
extraFlorenRow2012$fecyear<-"2012"

## Bind extra floren observations to start_dime
start_dime<-bind_rows(start_dime, extraFlorenRow2010,extraFlorenRow2012)

## Delete extra Jerry Lewis observations
start_dime<-start_dime[-which(start_dime$bonica.rid=="cand126209"& start_dime$District==18),]

## Mark Len Suzio as loser in 2012 CT Senate District 13
start_dime$winner[which(start_dime$bonica.rid=="cand30323" & start_dime$cycle=="2012"
                        & start_dime$winner=="W")]<-"L"

## Mark Brian Duprey as ran.general for 2012
start_dime$ran.general[which(start_dime$bonica.rid=="cand68727" & start_dime$cycle=="2012"
                             & start_dime$winner=="W")]<-1

## Create duplicate Brian Bolduc from 2010 to create missing 2012 observation ME2648201
extraBolducRow2012<-start_dime[which(start_dime$ICPSR=="ME26482010"),]
extraBolducRow2012$election<-"2012"
extraBolducRow2012$cycle<-"2012"
extraBolducRow2012$fecyear<-"2012"
start_dime<-bind_rows(start_dime, extraBolducRow2012)

## Drop Kaenrath 2012 Senate observations
start_dime<-start_dime[-which(start_dime$ICPSR=="ME78772012"),]
start_dime<-start_dime[-which(start_dime$ICPSR=="me_57402012"),]
## Fix missing ran.general status and winner variable
start_dime$ran.general[which(start_dime$ICPSR=="me_62952012")]<-1
start_dime$winner[which(start_dime$ICPSR=="me_62952012")]<-"W"

##Fix mismarked winners in ME 2012 Senate District 7 
## Brian Kaenrath lost in Senate Primary 7 and runs in house instead
start_dime$winner[which(start_dime$ICPSR=="ME626802012")]<-"L"
start_dime$winner[which(start_dime$ICPSR=="ME79212012")]<-"L"



## Add Sylvia Allen 2014 observation
## Fix missing ran.general, winner in Sylvia Allen
extraSylviaRow2014<-start_dime[which(start_dime$ICPSR=="az_2012000862012"),]
extraSylviaRow2014$election<-"2014"
extraSylviaRow2014$cycle<-"2014"
extraSylviaRow2014$fecyear<-"2014"
extraSylviaRow2014$winner<-"W"
extraSylviaRow2014$ran.general<-"1"
extraSylviaRow2014$District<-6
start_dime<-bind_rows(start_dime, extraSylviaRow2014)
## Fix AZ 2014 18
## Mark Jeff Dial as ran.general and winner
start_dime$winner[which(start_dime$ICPSR=="az_2014000172014")]<-"W"
start_dime$ran.general[which(start_dime$ICPSR=="az_2014000172014")]<-"1"

## Mark Jane Hydrick as ran.general and loser
start_dime$winner[which(start_dime$ICPSR=="az_2014003502014")]<-"L"
start_dime$ran.general[which(start_dime$ICPSR=="az_2014003502014")]<-"1"

## Fix AZ 2014 19
## Mark Joe Hobbs as loser and ran.general
start_dime$winner[which(start_dime$ICPSR=="az_2014008872014")]<-"L"
start_dime$ran.general[which(start_dime$ICPSR=="az_2014008872014")]<-"1"

## Mark Lupe Contreras as winner and ran.general
start_dime$winner[which(start_dime$ICPSR=="az_2014005402014")]<-"W"
start_dime$ran.general[which(start_dime$ICPSR=="az_2014005402014")]<-"1"


## Fix CT 2014 15 House
## Mark John McGovern as loser and ran.general
start_dime$winner[which(start_dime$ICPSR=="ct_f3e1bce8a92014")]<-"L"
start_dime$ran.general[which(start_dime$ICPSR=="ct_f3e1bce8a92014")]<-"1"

## Mark David Baram as winner and ran.general
start_dime$winner[which(start_dime$ICPSR=="ct_8354a2f64b2014")]<-"W"
start_dime$ran.general[which(start_dime$ICPSR=="ct_8354a2f64b2014")]<-"1"

## Fix CT 2014 92 House
## Mark Patricia Dillon as winner and ran.general
start_dime$winner[which(start_dime$ICPSR=="ct_cba65e553e2014")]<-"W"
start_dime$ran.general[which(start_dime$ICPSR=="ct_cba65e553e2014")]<-"1"

## Fix CT 2014 121 House
## Mark Richard Fredete as loser and ran.general
start_dime$winner[which(start_dime$ICPSR=="ct_581f5abc2b2014")]<-"L"
start_dime$ran.general[which(start_dime$ICPSR=="ct_581f5abc2b2014")]<-"1"

## Mark Terrance Backer as winner and ran.general
start_dime$winner[which(start_dime$ICPSR=="ct_87a3d2a66c2014")]<-"W"
start_dime$ran.general[which(start_dime$ICPSR=="ct_87a3d2a66c2014")]<-"1"

## Fix ME 2014 4 Senate
## Mark David Ziemer as loser and ran.general
start_dime$winner[which(start_dime$ICPSR=="me_78522014")]<-"L"
start_dime$ran.general[which(start_dime$ICPSR=="me_78522014")]<-"1"

## Mark Paul T. Davis as winner and ran.general
start_dime$winner[which(start_dime$ICPSR=="me_66012014")]<-"W"
start_dime$ran.general[which(start_dime$ICPSR=="me_66012014")]<-"1"

## Fix ME 2014 11 Senate
## Mark Jonathan Fulford as loser and ran.general
start_dime$winner[which(start_dime$ICPSR=="me_70292014")]<-"L"
start_dime$ran.general[which(start_dime$ICPSR=="me_70292014")]<-"1"

## Mark Paul T. Davis as winner, incubent, and ran.general
start_dime$winner[which(start_dime$ICPSR=="me_78222014"| start_dime$ICPSR=="me_94272014")]<-"W"
start_dime$ran.general[which(start_dime$ICPSR=="me_78222014"| start_dime$ICPSR=="me_94272014")]<-"1"
start_dime$Incum.Chall[which(start_dime$ICPSR=="me_78222014"| start_dime$ICPSR=="me_94272014")]<-"I"

## Fix Andy Biggs 2016 AZ Sen 12th Marked Incorrectly as Winner by dropping row
## He actually runs for House of Representatives in this cycles
start_dime<-start_dime[-which(start_dime$ICPSR=="az_2016001762016"),]

## Fix CT 2016 Senate 12th 
## Mark Ted Kennedy Jr as winner, incubent, and ran.general
start_dime$winner[which(start_dime$ICPSR=="ct_0aa8b042022016")]<-"W"
start_dime$ran.general[which(start_dime$ICPSR=="ct_0aa8b042022016")]<-"1"
start_dime$Incum.Chall[which(start_dime$ICPSR=="ct_0aa8b042022016")]<-"I"

## Fix ME 2016 House 6th 
## Mark Jennifer Parker as winner and ran.general
start_dime$winner[which(start_dime$ICPSR=="me_92992016")]<-"W"
start_dime$ran.general[which(start_dime$ICPSR=="me_92992016")]<-"1"

## Fix ME 2016 House 7th  
## Mark Robert Foley as winner, incubent, and ran.general
start_dime$winner[which(start_dime$ICPSR=="me_86942016")]<-"W"
start_dime$ran.general[which(start_dime$ICPSR=="me_86942016")]<-"1"
start_dime$Incum.Chall[which(start_dime$ICPSR=="me_86942016")]<-"I"

## Fix ME 2016 House 11th  
## Mark Ryan Fecteau as winner, incubent, and ran.general
start_dime$winner[which(start_dime$ICPSR=="me_90802016")]<-"W"
start_dime$ran.general[which(start_dime$ICPSR=="me_90802016")]<-"1"
start_dime$Incum.Chall[which(start_dime$ICPSR=="me_90802016")]<-"I"

## Fix ME 2016 House 17th  
## Mark Dwayne Prescott as winner, incubent, and ran.general
start_dime$winner[which(start_dime$ICPSR=="me_86132016")]<-"W"
start_dime$ran.general[which(start_dime$ICPSR=="me_86132016")]<-"1"
start_dime$Incum.Chall[which(start_dime$ICPSR=="me_86132016")]<-"I"

## Fix ME 2016 House 26th  
## Mark Maureen Terry as winner, incubent, and ran.general
start_dime$winner[which(start_dime$ICPSR=="me_92592016")]<-"W"
start_dime$ran.general[which(start_dime$ICPSR=="me_92592016")]<-"1"

## Fix ME 2016 House 30th  
## Mark Kimberly Monaghan as winner, incubent, and ran.general
start_dime$winner[which(start_dime$ICPSR=="me_92732016")]<-"W"
start_dime$ran.general[which(start_dime$ICPSR=="me_92732016")]<-1
start_dime$Incum.Chall[which(start_dime$ICPSR=="me_92732016")]<-"I"

## Fix ME 2016 House 41th  
## Mark Erik Jorgenson as winner, incubent, and ran.general
start_dime$winner[which(start_dime$ICPSR=="me_89272016")]<-"W"
start_dime$ran.general[which(start_dime$ICPSR=="me_89272016")]<-"1"
start_dime$Incum.Chall[which(start_dime$ICPSR=="me_89272016")]<-"I"

## Fix ME 2016 House 49th  
## Mark Matthea Daughtry as winner, incubent, and ran.general
start_dime$winner[which(start_dime$ICPSR=="me_91922016")]<-"W"
start_dime$ran.general[which(start_dime$ICPSR=="me_91922016")]<-"1"
start_dime$Incum.Chall[which(start_dime$ICPSR=="me_91922016")]<-"I"


## Fix ME 2016 House 61th  
## Mark Heidi Brooks as winner, incubent, and ran.general
start_dime$winner[which(start_dime$ICPSR=="me_89192016")]<-"W"
start_dime$ran.general[which(start_dime$ICPSR=="me_89192016")]<-"1"
start_dime$Incum.Chall[which(start_dime$ICPSR=="me_89192016")]<-"I"

## Fix ME 2016 House 64th  
## Mark Bettyann Sheets as winner, incubent, and ran.general
start_dime$winner[which(start_dime$ICPSR=="me_92332016")]<-"W"
start_dime$ran.general[which(start_dime$ICPSR=="me_92332016")]<-"1"


## Fix ME 2016 House 75th  
## Mark Jeffrey Timberlake as winner, incubent, and ran.general
start_dime$winner[which(start_dime$ICPSR=="me_87392016")]<-"W"
start_dime$ran.general[which(start_dime$ICPSR=="me_87392016")]<-"1"
start_dime$Incum.Chall[which(start_dime$ICPSR=="me_87392016")]<-"I"

## Fix ME 2016 House 77th  
## Mark Michael Perkins as winner and ran.general
start_dime$winner[which(start_dime$ICPSR=="me_88102016")]<-"W"
start_dime$ran.general[which(start_dime$ICPSR=="me_88102016")]<-"1"

## Fix ME 2016 House 79th  
## Mark Timothy Theriault as winner, incubent, and ran.general
start_dime$winner[which(start_dime$ICPSR=="me_87272016")]<-"W"
start_dime$ran.general[which(start_dime$ICPSR=="me_87272016")]<-"1"
start_dime$Incum.Chall[which(start_dime$ICPSR=="me_87272016")]<-"I"

## Fix ME 2016 House 92th  
## Mark John Spear as winner, incubent, and ran.general
start_dime$winner[which(start_dime$ICPSR=="me_89252016")]<-"W"
start_dime$ran.general[which(start_dime$ICPSR=="me_89252016")]<-"1"

## Fix ME 2016 House 96th  
## Mark Stanley Ziegler as winner and ran.general
start_dime$winner[which(start_dime$ICPSR=="me_91512016")]<-"W"
start_dime$ran.general[which(start_dime$ICPSR=="me_91512016")]<-"1"

## Fix ME 2016 House 98th  
## Mark James Gillway as winner and ran.general
start_dime$winner[which(start_dime$ICPSR=="me_90612016")]<-"W"
start_dime$ran.general[which(start_dime$ICPSR=="me_90612016")]<-"1"


## Fix ME 2016 House 104th  
## Mark Jeffrey Timberlake as winner, incubent, and ran.general
start_dime$winner[which(start_dime$ICPSR=="me_89852016")]<-"W"
start_dime$ran.general[which(start_dime$ICPSR=="me_89852016")]<-"1"
start_dime$Incum.Chall[which(start_dime$ICPSR=="me_89852016")]<-"I"

## Fix ME 2016 House 111th  
## Mark Bradley Farrin as winner, incubent, and ran.general
start_dime$winner[which(start_dime$ICPSR=="me_86992016")]<-"W"
start_dime$ran.general[which(start_dime$ICPSR=="me_86992016")]<-"1"
start_dime$Incum.Chall[which(start_dime$ICPSR=="me_86992016")]<-"I"

## Fix ME 2016 House 131th  
## Mark Karleton Ward as winner, incubent, and ran.general
start_dime$winner[which(start_dime$ICPSR=="me_90822016")]<-"W"
start_dime$ran.general[which(start_dime$ICPSR=="me_90822016")]<-"1"
start_dime$Incum.Chall[which(start_dime$ICPSR=="me_90822016")]<-"I"

## Fix ME 2016 House 132th  
## Mark Louis Luchini as winner, incubent, and ran.general
start_dime$winner[which(start_dime$ICPSR=="me_93012016")]<-"W"
start_dime$ran.general[which(start_dime$ICPSR=="me_93012016")]<-"1"
start_dime$Incum.Chall[which(start_dime$ICPSR=="me_93012016")]<-"I"

## Fix ME 2016 House 142th  
## Mark Sheldon Hanington as winner, incubent, and ran.general
start_dime$winner[which(start_dime$ICPSR=="me_86662016")]<-"W"
start_dime$ran.general[which(start_dime$ICPSR=="me_86662016")]<-"1"
start_dime$Incum.Chall[which(start_dime$ICPSR=="me_86662016")]<-"I"

## Fix ME 2016 House 146th  
## Mark Dustin White as winner, incubent, and ran.general
start_dime$winner[which(start_dime$ICPSR=="me_86612016")]<-"W"
start_dime$ran.general[which(start_dime$ICPSR=="me_86612016")]<-"1"
start_dime$Incum.Chall[which(start_dime$ICPSR=="me_86612016")]<-"I"

## Fix ME 2016 House 148th  
## Mark David McCrea as ran.general
start_dime$ran.general[which(start_dime$ICPSR=="me_90422016")]<-"1"

## Fix ME 2016 Senate 14th  
## Mark Shenna Bellows as winner and ran.general
start_dime$winner[which(start_dime$ICPSR=="me_93072016")]<-"W"
start_dime$ran.general[which(start_dime$ICPSR=="me_93072016")]<-"1"

## Fix ME 2016 Senate 24th  
## Mark Everett Carson as winner and ran.general
start_dime$winner[which(start_dime$ICPSR=="me_87162016")]<-"W"
start_dime$ran.general[which(start_dime$ICPSR=="me_87162016")]<-"1"

## Fix 2016 AZ House 11th
## Mark Vince Finchem as winner, incumbent, and ran.general 
start_dime$winner[which(start_dime$ICPSR=="az_2016001232016")]<-"W"
start_dime$ran.general[which(start_dime$ICPSR=="az_2016001232016")]<-"1"
start_dime$Incum.Chall[which(start_dime$ICPSR=="az_2016001232016")]<-"I"

## Mark Cardenas AZ House 19 2016 missing from start_dime dataset

## Fix 2016 AZ House 21st
## Mark Jose Rivero as winner, incumbent, and ran.general 
start_dime$winner[which(start_dime$ICPSR=="az_2016002362016")]<-"W"
start_dime$ran.general[which(start_dime$ICPSR=="az_2016002362016")]<-"1"
start_dime$Incum.Chall[which(start_dime$ICPSR=="az_2016002362016")]<-"I"

## Fix 2016 AZ House 30th
## Mark Tony Navarette as winner, incumbent, and ran.general 
start_dime$winner[which(start_dime$ICPSR=="az_2016004442016")]<-"W"
start_dime$ran.general[which(start_dime$ICPSR=="az_2016004442016")]<-"1"


## After all that data wrangling, we finally
## have accounted for all names in the public financing list
## in the Bonica dataset, identified missing data in the Bonica dataset,
## or deleted duplicates where candidates have multiple entries
## with different bonica.rids
## Save this object as RDS for future use.
save_dime<-start_dime%>%
  distinct(cycle,state, Senate, District,## Drop duplicates identifiable from duplicate bonica.rids
           bonica.rid,.keep_all = TRUE)%>%
  ## Drop rows in bonica dataset which were NA, but added due to behavior of matching loop
  filter(is.na(election)==FALSE)%>%
  ## Select needed variables for analysis below
  dplyr::select(name,cycle, state,Senate,District,bonica.rid,
         winner,gen.elec.stat,ran.general)

#saveRDS(save_dime, "FTM_BONICA_MERGE_AddedThru2016_V5.RDS")


## Next, we need to add several other contextual variables to the dataset,
## including district ideology and election results.

####Clean MRP Data####

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

## Join election results+clean candidate 
## count to MRP scores for each district-chamber-state-year

election_allres<-left_join(election_allres, MRP, by=c("year"="Election_Year",
                                                      "sab"="State",
                                                      "sen"="Sen",
                                                      "dno"="District"))


## Because we extended the Bonica ideology data to include 2014 and 2016, had to
## fill in missing ran.general field for 2014, missing party for 2014 and 2016,
## and missing incumbency for 2016 by hand
## Note that there are quite a few missing observations for 2014 and 2016
## because we use Bonica's 2018 update which hadn't gone through
## full cleaning yet based on correspondence with Adam.

## Load data with missing ran.general column for 2014
fixed2014<-read.csv("MissingGeneralElectionAZCTMEFixed.csv", stringsAsFactors = FALSE)%>%
  distinct(bonica.rid, .keep_all=TRUE)%>%## drop duplicate observations by bonica.rid
  dplyr::select(-c(name,district, seat,state, MissingCandidates_2014))%>%
  mutate(ran.general_2014=ifelse(is.na(ran.general_2014),0, ran.general_2014))## Convert empty cells to 0
## Load data with missing party for 2014-2016
fixedMissingParty<-read.csv("Bonica_2014_2016MissingParty_Fixed.csv", stringsAsFactors = FALSE)%>%
  dplyr::select(bonica.rid, cycle, party_fixed)
## Load data with missing incumbency variable 2016
missingIncumbents2016<-read.csv("MissingIncumbency2016_fixed.csv", stringsAsFactors = FALSE)%>%
  dplyr::select(bonica.rid, cycle, Incum.Chall_2016)%>%distinct(bonica.rid, cycle, .keep_all = TRUE)


#### Read in Bonica Data####
## Read in Bonica candidate data matched to public 
## financing data above
## and merge with fixed datasets

bonica_ftm<-readRDS("FTM_BONICA_MERGE_AddedThru2016_V5.RDS")%>%
  mutate(cycle=as.numeric(cycle))%>%## Convert cycle to numeric for join
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

## Data source: https://dataverse.harvard.edu/dataset.xhtml?persistentId=doi:10.7910/DVN/3WZFK9
## Citation: Klarner, Carl, 2018, "State Legislative Election Returns, 1967-2016",
## https://doi.org/10.7910/DVN/3WZFK9, Harvard Dataverse, V3, 
## UNF:6:pV4h1CP/B8pHthjjQThTTw== [fileUNF]
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
  
  dplyr::select(state, Senate, District,election,
                tenure1, tenure2,combName, Partyk,outcome,NameMatchDistance)%>%
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
  
  ## Subset Klarner data to district, chamber, state, and year of candidate
  ## from bonica data.
  
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
## in MatchBonicaNPATScores.R script. Once again,
## this had to be done using string distance
## method in Arizona multi-members districts.
## 2014 is the last election cycle for which Shor-McCarty scores are available
## for legislators.
## These NP-Scores need to be merged to can_fe in order to establish
## that NP-scores are correlated with bonica scores in the 
## data that enters the analysis.

npscores<-readRDS("NP_ScoresMatchedToBonicaRIDS2000_2014_AZMECT.RDS")%>%
  mutate(cycle=as.numeric(cycle))%>%distinct(bonica.rid,cycle,.keep_all = TRUE)
can_fe<-left_join(can_fe, npscores, by=c("bonica.rid"="bonica.rid","year"="cycle"))

#### Calculate (simulated) coefficients for Rogers (2017) district ideal point imputation####

## Regress dynamic CF Score on district ideology
ideal_points_CF<-lm(recipient.cfscore.dyn~MRP_Mean, data=can_fe)
summary(ideal_points_CF)

## Regress DWDIME on district ideology

ideal_points_DWDIME<-lm(dwdime~MRP_Mean, data=can_fe)
summary(ideal_points_DWDIME)


## Create vectors of N simulated coefficients
## for error propagation analysis.
## Set set.seed for reproducibility
set.seed(02138)
ideal_points_CF_sims1000<-sim(ideal_points_CF, n.sims=1000)
ideal_points_DWDIME_sims1000<-sim(ideal_points_DWDIME, n.sims=1000)
#saveRDS(ideal_points_CF_sims1000, file="ideal_points_CF_sims1000.RDS")
#saveRDS(ideal_points_DWDIME_sims1000, file="ideal_points_DWDIME_sims1000.RDS")


## Construct needed variables in combined dataset can_fe, 
## which at this point consists of
## candidate-year observations of Bonica ideology data,
## Klarner contest level stats,
## Klarner candidate level stats, Shor McCarty NP_Scores, 
## Tausanovitch-Warshaw MRP estimates,
## and all candidates are coded by year whether 
## they used public financing in the general election

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
  ## Create Redistricting Time variable that is specific
  ## to state
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

## In Appendix G.5, we need an indicator for whether a candidate used public
## financing in their first campaign. We also need an indicator for whether a
## candidate switched public financing status between campaign cycles
## The following code loops through the can_fe dataset
## and marks each candidate-year observation as 
## having switched pf status in the past or having used
## pf in their first campaign, successful or not.

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
## We checked visually the first 200 matches and the last 100 matches
## to ensure they are correct and the match
## rate is 100%.
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
                recipient.cfscore,NPAT_Score,dwdime,
                recipient.cfscore.dyn,ran.general,
                HasDynamicCF,RepubIndicator,CompetitiveInteraction,CompetitiveElection,
                tenure1, Distance_DWDIME, HasDistanceDWDIME)

#### Load can_fe from here####
#saveRDS(can_fe, file="can_fe.RDS")