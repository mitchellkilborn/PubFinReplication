can_fe = readRDS("can_fe.RDS")

#duplicate candidates in a year? i assume they can't run more than 1x/yr. 
duplicate_candidates = left_join(can_fe %>% 
                                group_by(year,bonica.rid) %>% 
                                summarize(count = n()) %>%
                                filter(count > 1),
                              can_fe)

#districts in AZ house without two winners
# Mark Cardenas AZ House 19 Winner 2016 is missing from dime dataset
missing_winners_AZH = left_join(can_fe %>% group_by(year,sab,sen,dno) %>% summarize(wins = sum(WonElection)) %>%
                                   filter(sab == "AZ" & sen == 0 & wins != 2),
                                 can_fe)

#districts outside AZ house without one winner
# Missing Zachary E. Matthews ME 102 Winner from dime
# Missing Peter D. Chase  ME 2000 122 winner from dime
# Missing Maitland Richardson ME 2006 Senate 26 winner from dime 
missing_winners_other = left_join(can_fe %>% group_by(year,sab,sen,dno) %>% 
                                    summarize(wins = sum(WonElection)) %>%
                                    filter((sab != "AZ" | sen != 0) & wins != 1),
                                  can_fe)

#incumbents by year. when we do the incumbent anaylsis, i might exclude redistricting yrs bc they aren't true incumbents
table(can_fe$year,can_fe$Incumbent)

#districts with >1 incumbent. are these all bc of redistricting? 
# Arizona House District 28 2012 has three incumbents competing
# against each other, good stuff
missing_incs = can_fe %>% group_by(year,sab,sen,dno) %>% summarize(Inc = sum(Incumbent)) %>%
  filter(((sab != "AZ" | sen != 0) & Inc > 1) | (sab == "AZ" & sen == 0 & Inc > 2))


#this observation seems weird, look at the cf score. i doubt it changes anything. 
can_fe %>% filter(bonica.rid == "cand6661" & year == 2010)

#cases where columns seem redundant?
table(can_fe$sen,can_fe$seat)
table(can_fe$party,can_fe$RepubIndicator)

#calc of switchers: what's the reason for the discrepancy? 
# not saying you're wrong, just curious for your operationalization
# Discrepancy is caused by CensusLines not including 2000,2002 for ME and AZ
# So my method counted some people who switched within 2004-2016, while yours 
# gets 2000-2016. Both numbers are relevant, one describes the number of switchers
# in the data used in the analysis, one describes the total number throughout
## all our data.
look<-can_fe %>% filter(CleanYear != CleanFirstRun & CensusLines==1) %>% 
      select(bonica.rid, CleanYear,CleanFirstRun)%>%
       distinct(bonica.rid, .keep_all = TRUE)%>%select(bonica.rid, CleanYear,CleanFirstRun)%>%
       arrange(bonica.rid)




#just comparing this to A4... interesting that AZ & ME are pretty close to the
#simple avg, while CT changes by ~10 pts
can_fe %>% group_by(sab,CleanYear) %>% summarize(dist = mean(Distance_CFDyn,na.rm=T))

##plots:

ggplot(data=can_fe)+
  geom_histogram(aes(x=Distance_CFDyn),color="red",fill="white", alpha=0.5, position="identity")+
  geom_histogram(aes(x=Distance_CFnonDyn),color="green",fill="white", alpha=0.5, position="identity")

ggplot(data=can_fe %>% filter(party %in% c(100,200)),aes(x=Distance_NP,color=party))+
  geom_histogram(fill="white", alpha=0.5, position="identity")


###### random stuff

hist(can_fe$Distance_CFDyn)






table(is.na(can_fe$Distance_CFDyn),can_fe$year,can_fe$sab)

a = can_fe %>% group_by(UniqueDistrict_CensusGroup) %>% summarize(count = n())

table(can_fe$CleanYear,can_fe$CleanFirstRun)

