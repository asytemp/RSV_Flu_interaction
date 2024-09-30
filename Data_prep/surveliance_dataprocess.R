library(dplyr)
library(plyr)
library(tidyr)
library(lubridate)
#install.packages("ggridges")
library(ggridges)
library(ggplot2)
## vislization only for RSV data

RSV_NREVSS<-read.csv("Surveliance_1028/NREVSS_RSV.csv")

RSV<-RSV_NREVSS %>%
  drop_na()%>%
  mutate(date=decimal_date(as.Date(RepWeekDate,"%m/%d/%y")))%>%
  mutate(TestType=as.factor(TestType))%>%
  dplyr::group_by(RepWeekDate,date,HHS_REGION,TestType)%>%
  dplyr::summarise(RSVtest=sum(RSVtest),
                   RSVpos=sum(RSVpos))%>%
  mutate(percent_positive=RSVpos/RSVtest)%>%
  mutate(percent_positive = replace(percent_positive,is.na(percent_positive),0))
  


RSV_national<-RSV %>% 
  dplyr::group_by(RepWeekDate,date,TestType) %>%
  dplyr::summarise(RSVtest_national = sum(RSVtest),
                   RSVpos_national=sum(RSVpos))%>%
  mutate(percent_positive=RSVpos_national/RSVtest_national)

## plot antigen
RSV%>%
  ggplot(aes(x=date,y=RSVpos,linetype=TestType))+
  geom_line()+
  facet_wrap(~HHS_REGION,ncol=1,strip.position="right")+
  theme_bw()+
  scale_color_discrete(name = "Test type", labels = c("antigen_dectection", "PCR"))

## plot together with flu
## after 2015, data reported from public health lab were used as they reportd detail subtype
flu1<-read.csv("Surveliance_1028/FluViewPhase2Data/WHO_NREVSS_Combined_prior_to_2015_16.csv",skip=1,header=TRUE)
flu1%>%
  mutate(flupos=as.integer(`TOTAL.SPECIMENS`*`PERCENT.POSITIVE`/100))%>%
  mutate(fluApos=`A..2009.H1N1.`+`A..H1.`+`A..H3.`+`A..Subtyping.not.Performed.`+`A..Unable.to.Subtype.`+`H3N2v`)%>%
  mutate(H1pos=`A..2009.H1N1.`+`A..H1.`)%>%
  mutate(H3pos=`A..H3.`+`H3N2v`)%>%
  dplyr::rename(HHSREGION=REGION,flutest=`TOTAL.SPECIMENS`,flupos_percent=`PERCENT.POSITIVE`,fluBpos=B)%>%
  mutate(flupos_percent=flupos_percent/100,
         fluApos_percent=fluApos/flutest,
         fluBpos_percent=fluBpos/flutest)%>%
  dplyr::select(HHSREGION,YEAR,WEEK,flutest,flupos,fluApos,fluBpos,H1pos,H3pos,flupos_percent,fluApos_percent,fluBpos_percent)->flu1_clean


flu2<-read.csv("Surveliance_1028/FluViewPhase2Data/WHO_NREVSS_Public_Health_Labs.csv",skip=1,header=TRUE)
flu2%>%
  mutate(flupos=`A..2009.H1N1.`+`A..H3.`+`A..Subtyping.not.Performed.`+B + BVic + BYam + H3N2v)%>%
  mutate(fluApos=`A..2009.H1N1.`+`A..H3.`+`A..Subtyping.not.Performed.`+`H3N2v`)%>%
  mutate(fluBpos=B+BVic+BYam)%>%
  mutate(H1pos=`A..2009.H1N1.`)%>%
  mutate(H3pos=`A..H3.`+`H3N2v`)%>%
  mutate(flupos_percent=flupos/TOTAL.SPECIMENS,
         fluApos_percent=fluApos/TOTAL.SPECIMENS,
         fluBpos_percent=fluBpos/TOTAL.SPECIMENS)%>%
  dplyr::rename(HHSREGION=REGION,flutest=`TOTAL.SPECIMENS`)%>%
  dplyr::select(HHSREGION,YEAR,WEEK,flutest,flupos,fluApos,fluBpos,H1pos,H3pos,flupos_percent,fluApos_percent,fluBpos_percent)->flu2_clean


## combine flu1 and flu2
rbind(flu1_clean,flu2_clean)%>%
  mutate(RepWeekDate=as.Date(paste(YEAR,WEEK, "6", sep="-"), "%Y-%U-%u"))%>%
  mutate(date=decimal_date(RepWeekDate),
         fluApos_percent=fluApos/flutest,
         fluBpos_percent=fluBpos/flutest)%>%
  mutate(HHSREGION=gsub("Region *","\\1",HHSREGION))->flu_combine


## sum up HHS regins to get flu national

flu_national<-flu_combine %>% 
  dplyr::group_by(RepWeekDate,date) %>%
  dplyr::summarise(flutest_national = sum(flutest),
                   flupos_national=sum(flupos),
                   fluApos_national=sum(fluApos),
                   fluBpos_national=sum(fluBpos))%>%
  mutate(percent_positive=flupos_national/flutest_national)




## plot RSV PCR data and flu together
RSV%>%
  mutate(HHS_REGION=as.character(HHS_REGION))%>%
  filter(TestType=="1")%>%
  full_join(flu_combine,by=c("HHS_REGION"="HHSREGION","date"="date"))%>%
  drop_na()->RSV_flu_combine

RSV_flu_combine%>%
  ggplot(aes(x=date,y=RSVpos,color=TestType))+
  geom_line()+
  geom_line(aes(x=date,y=flupos,color="blue"))+
  facet_wrap(~HHS_REGION,ncol=1,strip.position="right")+
  theme_bw()+
  scale_color_discrete(name = "Pathogen", labels = c("RSV", "flu"))



RSV_flu_combine%>%
  ggplot(aes(x=date,y=RSVpos,color=TestType))+
  geom_line()+
  geom_line(aes(x=date,y=fluApos,color="darkblue"))+
  geom_line(aes(x=date,y=fluBpos,color="green"))+
  facet_wrap(~HHS_REGION,ncol=1,strip.position="right")+
  theme_bw()+
  scale_color_discrete(name = "Pathogen", labels = c("RSV", "fluA", "fluB"))


## generage data for POMP model

RSV_flu_combine%>%
  filter(HHS_REGION=="3")%>%
  filter(date > 2015.5 & date < 2018.5 )%>%
  ggplot(aes(x=date,y=RSVpos,color=TestType))+
  geom_line()+
  geom_line(aes(x=date,y=fluApos,color="darkblue"))+
  geom_line(aes(x=date,y=fluBpos,color="green"))+
  theme_bw()+
  scale_color_discrete(name = "Pathogen", labels = c("RSV", "fluA", "fluB"))

RSV_flu_combine%>%
  filter(HHS_REGION=="3")%>%
  filter(date > 2015.5 & date < 2018.5 )%>%
  mutate(time=date-2013)%>%
  ungroup() %>%
  select(time,RSVpos,fluApos,fluBpos)%>%
  arrange(time)%>%
  add_row(time = 2.5, RSVpos = NA, fluApos = NA, fluBpos=NA, .before = 1)-> RSV_flu_combine_16_18_region3

## prepare csv file to run pomp

RSV_flu_combine_16_18_region3%>%
  select(time,RSVpos,fluApos)%>%
  dplyr::rename(total_a=RSVpos,total_b=fluApos)->region3_rsv_fluA_16_18
  
RSV_flu_combine_16_18_region3%>%
  select(time,RSVpos,fluBpos)%>%
  dplyr::rename(total_a=RSVpos,total_b=fluBpos)->region3_rsv_fluB_16_18

write.csv(region3_rsv_fluA_16_18,"surveliance_1028/Pomp/region3_rsv_fluA_16_18.csv",row.names = FALSE)
write.csv(region3_rsv_fluB_16_18,"surveliance_1028/Pomp/region3_rsv_fluA_16_18.csv",row.names = FALSE)


## get the peak time for each HHS region in each season
RSV %>% 
  mutate(season = case_when(
    between(date, 2010.5, 2011.5) ~ "10-11",
    between(date, 2011.5, 2012.5) ~ "11-12",
    between(date, 2012.5, 2013.5) ~ "12-13",
    between(date, 2013.5, 2014.5) ~ "13-14",
    between(date, 2014.5, 2015.5) ~ "14-15",
    between(date, 2015.5, 2016.5) ~ "15-16",
    between(date, 2016.5, 2017.5) ~ "16-17",
    between(date, 2017.5, 2018.5) ~ "17-18",
    between(date, 2018.5, 2019.5) ~ "18-19",
    between(date, 2019.5, 2020.5) ~ "19-20",
    TRUE ~ NA_character_
  ))%>%
  dplyr::group_by(season,HHS_REGION,TestType)%>%
  dplyr::summarise(RSVpos_max=max(RSVpos),RSVpos_maxdate=date[which.max(RSVpos)],
                   RSVper_max=max(percent_positive),RSVper_maxdate=date[which.max(percent_positive)])->RSV_peaktime
  
## percent vs observed cases
## test typ3 1 vs 4


  
flu_combine %>%
  mutate(HHS_REGION=as.integer(HHSREGION))%>%
  mutate(season = case_when(
    between(date, 2010.5, 2011.5) ~ "10-11",
    between(date, 2011.5, 2012.5) ~ "11-12",
    between(date, 2012.5, 2013.5) ~ "12-13",
    between(date, 2013.5, 2014.5) ~ "13-14",
    between(date, 2014.5, 2015.5) ~ "14-15",
    between(date, 2015.5, 2016.5) ~ "15-16",
    between(date, 2016.5, 2017.5) ~ "16-17",
    between(date, 2017.5, 2018.5) ~ "17-18",
    between(date, 2018.5, 2019.5) ~ "18-19",
    between(date, 2019.5, 2020.5) ~ "19-20",
    TRUE ~ NA_character_
  ))%>%
  dplyr::group_by(season,HHS_REGION)%>%
  dplyr::summarise(fluApos_max=max(fluApos),fluApos_maxdate=date[which.max(fluApos)],
                   fluBpos_max=max(fluBpos),fluBpos_maxdate=date[which.max(fluBpos)],
                   flupos_max=max(flupos),flupos_maxdate=date[which.max(flupos)],
                   fluAper_max=max(fluApos_percent),fluAper_maxdate=date[which.max(fluApos_percent)],
                   fluBper_max=max(fluBpos_percent),fluBper_maxdate=date[which.max(fluBpos_percent)],
                   fluper_max=max(flupos_percent),fluper_maxdate=date[which.max(flupos_percent)],
                   H3pos_max=max(H3pos),H3pos_maxdate=date[which.max(H3pos)],
                   H1pos_max=max(H1pos),H1pos_maxdate=date[which.max(H1pos)])->flu_peaktime


flu_peaktime%>%
  mutate(per_pos=fluper_maxdate-flupos_maxdate)->test

RSV_peaktime%>%
  filter(TestType==1)%>%
  full_join(flu_peaktime,by=c("HHS_REGION"="HHS_REGION","season"="season"))%>%
  mutate(`RSV-flu`=RSVper_maxdate-fluper_maxdate,
         `RSV-fluA`=RSVper_maxdate-fluAper_maxdate,
         `RSV-fluB`=RSVper_maxdate-fluBper_maxdate,
         `fluA-fluB`=fluAper_maxdate-fluBper_maxdate)->peaktime


RSV_peaktime%>%
  filter(TestType==4)%>%
  full_join(flu_peaktime,by=c("HHS_REGION"="HHS_REGION","season"="season"))%>%
  mutate(`RSV-flu`=RSVpos_maxdate-flupos_maxdate,
         `RSV-fluA`=RSVpos_maxdate-fluApos_maxdate,
         `RSV-H3`=RSVpos_maxdate-H3pos_maxdate,
         `RSV-H1`=RSVpos_maxdate-H1pos_maxdate,
         `RSV-fluB`=RSVpos_maxdate-fluBpos_maxdate,
         `fluA-fluB`=fluApos_maxdate-fluBpos_maxdate)->peaktime

peaktime %>% 
  select(season, HHS_REGION,`RSV-H3`,`RSV-H1`,`RSV-fluA`,`RSV-fluB`)%>%
  pivot_longer(-c(season, HHS_REGION),values_to = "peak_diff",names_to = "pathogens")%>%
  ggplot(aes(x=peak_diff,y=pathogens))+
  geom_density_ridges(aes(fill = pathogens))+
  scale_fill_brewer(palette = "Dark2")+
  theme_bw()+
  geom_vline(xintercept = 0, color="red",linetype="dashed")+
  ylab("Pathogens")+
  xlab("Peak time differences (year)")+
  theme(legend.position='none')

 
peaktime %>% 
  select(season, HHS_REGION,`RSV-fluA`,`RSV-fluB`,`fluA-fluB`)%>%
  pivot_longer(-c(season, HHS_REGION),values_to = "peak_diff",names_to = "pathogens")%>%
  mutate(HHS_REGION=as.factor(HHS_REGION))%>%
  ggplot(aes(x=pathogens,y=peak_diff,fill = HHS_REGION))+
  geom_dotplot(binaxis='y', stackdir='center')+
  theme_bw()
  
  
#################################################################################################
## prepare case count data for pomp model

## generage data for POMP model
## plot RSV PCR data and flu together
RSV%>%
  mutate(HHS_REGION=as.character(HHS_REGION))%>%
  filter(TestType=="4")%>%
  full_join(flu_combine,by=c("HHS_REGION"="HHSREGION","date"="date"))%>%
  drop_na()->RSV_flu_combine

RSV_flu_combine%>%
  filter(date > 2013.5 & date < 2018.5 )%>%
  ggplot(aes(x=date,y=RSVpos,color=TestType))+
  geom_line()+
  geom_line(aes(x=date,y=fluApos,color="darkblue"))+
  geom_line(aes(x=date,y=fluBpos,color="green"))+
  facet_wrap(~HHS_REGION,ncol=1,strip.position="right")+
  theme_bw()+
  scale_color_discrete(name = "Pathogen", labels = c("RSV", "fluA", "fluB"))

RSV_flu_combine%>%
  filter(date > 2013.5 & date < 2018.5 )%>%
  mutate(time=date-2011)%>%
  ungroup() %>%
  select(date,time,RSVpos,fluApos,fluBpos,HHS_REGION)%>%
  arrange(time)->pomp_HHSregion_case

write.csv(pomp_HHSregion_case,"surveliance_1028/Pomp/pomp_HHSregion_case_2018.csv",row.names = FALSE)


## popultaion data process
pop<-read.csv("RSV_flu_git/demographic-data/nst-est2019-alldata.csv")
region_state<-read.csv("RSV_flu_git/demographic-data/state_region.csv")

pop%>%
  left_join(region_state,by=c("US.STATE"="State_name"))%>%
  select(US.STATE,POPESTIMATE2014,POPESTIMATE2015,POPESTIMATE2016,POPESTIMATE2017,state,HHS_region)%>%
  mutate(pop_ave=(POPESTIMATE2014+POPESTIMATE2015+POPESTIMATE2016 +POPESTIMATE2017)/4)%>%
  drop_na()%>%
  mutate(HHS_region=as.factor(HHS_region))%>%
  group_by(HHS_region)%>%
  dplyr::summarise(pop_ave_sum=as.integer(sum(pop_ave)))->pop_HHSRegion


write.csv(pop_HHSRegion,"RSV_flu_git/demographic-data/pop_HHSRegion.csv")
  












%>%
  arrange(time)%>%
  add_row(time = 2.5, RSVpos = NA, fluApos = NA, fluBpos=NA, .before = 1)-> RSV_flu_combine_16_18_region3
