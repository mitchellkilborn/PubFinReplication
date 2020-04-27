can_fe = readRDS("can_fe.RDS")

#duplicate candidates in a year? i assume they can't run more than 1x/yr. 
duplicate_candidates = left_join(can_fe %>% 
                                group_by(year,bonica.rid) %>% 
                                summarize(count = n()) %>%
                                filter(count > 1),
                              can_fe)

#districts in AZ house without two winners
missing_winners_AZH = left_join(can_fe %>% group_by(year,sab,sen,dno) %>% summarize(wins = sum(WonElection)) %>%
                                   filter(sab == "AZ" & sen == 0 & wins != 2),
                                 can_fe)
#districts outside AZ house without one winner
missing_winners_other = left_join(can_fe %>% group_by(year,sab,sen,dno) %>% summarize(wins = sum(WonElection)) %>%
                                    filter((sab != "AZ" | sen != 0) & wins != 1),
                                  can_fe)

#incumbents by year. when we do the incumbent anaylsis, i might exclude redistricting yrs bc they aren't true incumbents
table(can_fe$year,can_fe$Incumbent)

#districts with >1 incumbent. are these all bc of redistricting? the 2014 AZ sen ones shouldn't be
missing_incs = can_fe %>% group_by(year,sab,sen,dno) %>% summarize(Inc = sum(Incumbent)) %>%
  filter(((sab != "AZ" | sen != 0) & Inc > 1) | (sab == "AZ" & sen == 0 & Inc > 2))


#this observation seems weird, look at the cf score. i doubt it changes anything. 
can_fe %>% filter(bonica.rid == "cand6661" & year == 2010)

#cases where columns seem redundant?
table(can_fe$sen,can_fe$seat)
table(can_fe$party,can_fe$RepubIndicator)

#calc of switchers: what's the reason for the discrepancy? 
# not saying you're wrong, just curious for your operationalization
#MK figure this out 
table(can_fe$PFStatusSwitcher) #1320 by your def
table(can_fe$CleanYear==can_fe$CleanFirstRun) #816 by this def: anyone whose CleanFirstRun!= CleanYear
nrow(can_fe %>% filter(CleanYear != CleanFirstRun & CensusLines==1) %>% distinct(bonica.rid)) #1597 by this one: anyone who ever switched

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

