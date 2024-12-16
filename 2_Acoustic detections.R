# Pumpstation 
# Reading and filtering acoustic receiver detections
# S lingard
# created August 28, 2024

# Steps to complete: 
# Read in data and compile into one database
# Filter out erroneous tag IDs (not in tagging dat)
# Filter detections prior to release date/time

# come up with an order for the receivers. SO group upstream hammersley into one location.
# right now there are no data for upstream of Hatzic because i haven't been able to get the receivers back yet.
# then look at first receivers fish were detected on downstream of each pump

library(tidyverse)
library(purrr)
library(here)
library(ggforce)


# read in raw detections ####
df <- list.files(path=here("raw data","acoustic receivers"), full.names=TRUE ) %>% 
  map_dfr(read.csv)

# format the dates and times
df$datetime.utc= force_tz(as.POSIXct(df$Date.and.Time..UTC., format="%Y-%m-%d %H:%M:%S"), "UTC")
df$datetime.local=with_tz(df$datetime.utc, "America/Vancouver")

# break out receiver and tag serials so this will match with the information in the deployment files
df$receiver.serial <- gsub(".*-","",df$Receiver)
df$tagID <- gsub(".*-*-","",df$Transmitter)

#remove suspected false detections
# read in tagging and release data ####

tag.deployment <- read.csv(here("raw data", "acoustic tagging.csv"))%>%
  rename(trough.section=trough)
release.data <- read.csv(here("raw data", "acoustic fish release data.csv"))%>%
  rename(release.date=date)

tag.meta <- tag.deployment%>%
  full_join(., release.data, by=c("release.date", "trough.section"))%>%
  mutate(release.datetime=ymd_hm(paste(release.date, release.time, by=" ")), 
         release.date=ymd(release.date))

tag.meta %>%
  group_by(release.location)%>%
  summarise(length(unique(tagid)))


fish.dat <- tag.meta%>%
  select(tagid, release.datetime,release.location)%>%
  rename(tagID=tagid)

write.csv(fish.dat, file=here("cleaned data"," fish release date time location.csv"))

fish.dat %>%
  group_by(release.location)%>%
  summarise(length(unique(tagID))) # 298 tags missing here?

tag.deployment.receiver <- df %>%
  filter(receiver.serial == 108656)

# filter out tag detections prior to release ####

df2 <- df%>%
  filter(receiver.serial != 108656 & tagID %in% tag.meta$tagid & datetime.local > tag.meta$release.datetime)

# now filter detections less than 45 seconds apart
# first have to create a time.diff column

df3 <- df2 %>%
  select(-c(Sensor.Precision, Sensor.Unit, Sensor.Value, Transmitter.Name, Transmitter.Serial, Transmitter.Type))%>%
  group_by(tagID)%>%
  arrange(., tagID, datetime.local)%>%
  mutate(diff.time = difftime(datetime.local, lag(datetime.local), units="secs"))%>%
  filter(diff.time >=45)

# now recalculate time diffs after filtering out erroneous detections

df4 <- df3 %>% 
  group_by(tagID)%>%
  arrange(., tagID, datetime.local)%>%
  mutate(diff.time = difftime(datetime.local, lag(datetime.local), units="secs"))

# check the filter worked
min(df4$diff.time, na.rm=T)

# Read in receiver meta ####

r.meta <- read.csv(here("raw data", "receiver metadata_2024.csv"))%>%
  select(-Way.Point)

# remove "VR2W-" and "VR2Tx" from Receiver in df4

df4$Receiver <- gsub("VR2W-","", df4$Receiver)
df4$Receiver <- gsub("VR2Tx-","", df4$Receiver)

# now add in location and up or downstream by merging dfs
# also add in release.datetime and location column

df5 <- df4 %>% 
  select(-c(Station.Name, Latitude, Longitude))%>%
  merge(., r.meta, by="Receiver")

df6 <- df5%>%
  mutate(location_site=paste(Location, Site, sep="-"))%>%
  merge(., fish.dat, by="tagID")

# Remove tags that aren't detected at least twice in the same region with in 1 hour ####

df7 <- df6 %>%
  group_by(tagID, location_site)%>%
  mutate(cond1 = ifelse(lead(diff.time) > 1800, FALSE, TRUE), 
         cond2= ifelse(diff.time > 1800, FALSE, TRUE))%>%
  filter(cond1 | cond2 == TRUE)

# Data exploration and QA ####

length(unique(df7$tagID)) #293 IDs detected out of 300

# Fish detected

df7 %>%
  group_by(release.location)%>%
  summarise(length(unique(tagID)))

# Hammersley: 103 released upstream and 103 detected, # 105 released downstream but 100 detected
# Hatzic: 51 released ds with 50 detected, 41 released up with 39 detected

# were any fish released downstream detected upstream?

df7 %>%
  group_by(release.location, location_site)%>%
  summarise(length(unique(tagID))) # a few. Could be noise

# what about fish moving upstream? only interested in Hammersley because there is no upstream path (apparently)

df7 %>%
  mutate(location_site=paste(Location, Site, sep="-"))%>%
  group_by(release.location, location_site)%>%
  summarise(length(unique(tagID)))

up.movement <- df7%>%
  filter(release.location %in% "ham.ds" & location_site %in% "upstream-mountain slough")

up.movement%>%
  ggplot(., aes(x=datetime.local))+
  geom_histogram()+
  facet_wrap(~tagID)

df7%>%
  filter(tagID %in% up.movement$tagID)%>%
  ggplot(., aes(x=datetime.local, y=Receiver))+
  geom_point()+
  facet_wrap(~tagID)

# there are a lot of detections of some of these fish and the filters are very conservative
upIDs <- up.movement%>%
  group_by(tagID)%>%
  filter(row_number()==1)
# checked the map and it looks like dets could transmit through the flood gates as fish were detected on receiver ds of flood gate and the pump
# at the same time.

## Data QA and Clean Up ####
dat_qa <- df7 %>%
  filter(release.location %in% c("hat.ds","ham.ds") & location_site %in% c("upstream-mountain slough", "upstream-hatzic"))

dat_qa %>%
  group_by(release.location, release.datetime, location_site)%>%
  summarise(length(unique(tagID)))

df7 %>%
  filter(release.location %in% "hat.ds" & release.datetime %in% "2024-05-15 11:46:00" & location_site %in% "upstream-hatzic") %>%
  group_by(tagID)%>%
  summarise(n())%>%
  rename(tagid=tagID)%>%
  merge(., tag.meta, by="tagid") # all these fish could have gone up stream with the tides as the tide gates were still open at hatzic.

## Plot Detection Histories for each fish ####
# start with numbering receivers at each location. Ham.us=1, ham.sd=2, hat.us=3, hat.ds=4. 
# add row for release at start.
# plot the release group that was detected upstream of hatzic first (17 fish). 

### Adding in release data as a row in the data frame for plotting and analysis ####

fish.dat$release.location <- as.factor(fish.dat$release.location)
fish.dat$location_site <- fish.dat$release.location

levels(fish.dat$location_site) <- forcats::fct_recode(fish.dat$location_site, 
                                                         'upstream-mountain slough'="ham.us",
                                                         'downstream-mountain slough'="ham.ds",
                                                         'upstream-hatzic'="hat.us",
                                                         'downstream-hatzic'="hat.ds")
release.df <- fish.dat%>%
  mutate(Receiver="release",
         datetime.local=release.datetime, 
         location=release.location,
         tagID=as.character(tagID))

plot.df <- df7%>%
  mutate(location_site=paste(Location, Site, sep="-"))%>%
  select(tagID, location_site, datetime.local, release.location, 
         release.datetime, Receiver)%>%
  bind_rows(., release.df)%>%
  group_by(tagID)%>%
  arrange(datetime.local)%>%
  mutate(location_site=as.factor(location_site), 
         location=fct_recode(location_site, "1"="upstream-mountain slough",
                                            "2"="downstream-mountain slough",
                                            "3"="upstream-hatzic",
                                            "4"="downstream-hatzic"))

plot.df$Receiver <- factor(plot.df$Receiver, levels=c("release","108647","101503","10866","108661","108658","108654"))

# Exploring through a plot why fish were detected moving up stream past pump ####
up.fish <- plot.df %>%
  filter(tagID %in% up.movement$tagID)%>%
  ggplot(., aes(x=datetime.local, y=Receiver))+
  geom_point()+
  geom_line()+
  facet_wrap(~tagID, scales="free_x")

ggsave(up.fish, file=here("figures","Hammersley Upstream Movement.png"),
       width=9, height=8)

# all the fish detected at the same time up and down were detected on the receiver ds of flood gates.
# so transmission could travel through flood gates. Will have to edit this out later when interpreting things

# Read in Fraser Data ####

fish.rel.dat <- fish.dat %>% 
  select(tagID, release.location)

fraser.dets <- read.csv(here("raw data","Detections_DI_JS_Fraser_Lingard_2024_30Nov2024.csv"))%>%
  select(tag_id, receiver_sn, detection_datetime, subarray)%>%
  rename(tagID=tag_id,
         Receiver=receiver_sn, 
         datetime.local=detection_datetime,
         location_site=subarray)%>%
  mutate(datetime.local=ymd_hm(datetime.local), 
         Site="fraser", 
         tagID=as.integer(tagID))%>%
  merge(., fish.rel.dat, by="tagID")

# merge all detections and arrange up to downstream  ####

all.dets <- df7%>%
  mutate(tagID=as.integer(tagID), Receiver=as.integer(Receiver))%>%
  select(tagID, datetime.local,Receiver, location_site, Site, release.location)%>%
  bind_rows(., fraser.dets)

ham.dets <- all.dets%>%
  filter(release.location %in% c("ham.ds", "ham.us"))%>%
  group_by(tagID)%>%
  arrange(datetime.local)%>%
  mutate(type="detection", 
         tagID=as.integer(tagID),
         Receiver=as.character(Receiver))

### data check ####
ham.dets %>%
  ungroup()%>%
  filter(release.location %in% c("ham.ds", "ham.us"))%>%
  summarise(length(unique(tagID)))# 201 detected

## create row with relase information ###
ham.releases <- release.df%>%
  filter(release.location %in% c("ham.us","ham.ds"))%>%
  select(tagID,datetime.local,release.location,location_site,Receiver)%>%
  mutate(Site="mountain slough",
         type="release", 
         tagID=as.integer(tagID))

ham.dets2 <- ham.dets %>%
  bind_rows(., ham.releases)%>%
  group_by(tagID)%>%
  arrange(tagID, datetime.local)

#locations <- hammersley>mission>derby>barnston>portman>new west


ham.dets2$location_site <- factor(ham.dets2$location_site, 
                                      levels=c(
                                      "NEW_WEST",
                                      "PORT_MANN",
                                      "BARNSTON",
                                      "DERBY",
                                      "MISSION",
                                      "downstream-hatzic",
                                      "downstream-mountain slough",
                                      "upstream-mountain slough",
                                      "release"))

ham.plots1 <- ham.dets2%>%
  ggplot(., aes(x=datetime.local, y=location_site))+
  geom_point()+
  geom_line()+
  theme(axis.text.x=element_text(angle=(-45)))+
  labs(x="", y="Site Name")+
  facet_wrap_paginate(~tagID, scales="free_x",
             nrow=4, ncol=4, page=1)

ham.plots2 <- ham.dets2%>%
  ggplot(., aes(x=datetime.local, y=location_site))+
  geom_point()+
  geom_line()+
  theme(axis.text.x=element_text(angle=(-45)))+
  labs(x="", y="Site Name")+
  facet_wrap_paginate(~tagID, scales="free_x",
                      nrow=4, ncol=4, page=2)

ham.plots3 <- ham.dets2%>%
  ggplot(., aes(x=datetime.local, y=location_site))+
  geom_point()+
  geom_line()+
  theme(axis.text.x=element_text(angle=(-45)))+
  labs(x="", y="Site Name")+
  facet_wrap_paginate(~tagID, scales="free_x",
                      nrow=4, ncol=4, page=3)


ham.plots4 <- ham.dets2%>%
  ggplot(., aes(x=datetime.local, y=location_site))+
  geom_point()+
  geom_line()+
  theme(axis.text.x=element_text(angle=(-45)))+
  labs(x="", y="Site Name")+
  facet_wrap_paginate(~tagID, scales="free_x",
                      nrow=4, ncol=4, page=4)
  
ham.plots5 <- ham.dets2%>%
  ggplot(., aes(x=datetime.local, y=location_site))+
  geom_point()+
  geom_line()+
  theme(axis.text.x=element_text(angle=(-45)))+
  labs(x="", y="Site Name")+
  facet_wrap_paginate(~tagID, scales="free_x",
                      nrow=4, ncol=4, page=5)   

ggsave(ham.plots1, file=here("figures", "hammersley fish page 1.png"), 
       width=10, height=8)
ggsave(ham.plots2, file=here("figures", "hammersley fish page 2.png"), 
       width=10, height=8)
ggsave(ham.plots3, file=here("figures", "hammersley fish page 3.png"), 
       width=10, height=8)
ggsave(ham.plots4, file=here("figures", "hammersley fish page 4.png"), 
       width=10, height=8)
ggsave(ham.plots5, file=here("figures", "hammersley fish page 5.png"), 
       width=10, height=8)

## How many fish passed hammersley? #####
dets_location <- ham.dets2 %>%
  group_by(release.location, tagID, Site)%>%
  summarise(n=n())

detected.in.fraser <- ham.dets2 %>%
  group_by(tagID, location_site)%>%
  summarise(n= n())%>%
  ungroup()%>%
  filter(!location_site %in% c("upstream-mountain slough", "downstream-mountain slough"))%>%
  merge(., fish.rel.dat, by="tagID")%>%
  group_by(release.location)%>%
  summarise(length(unique(tagID)))

# how many sites in the fraser for each fish - will influence CJS models

number.sites.detected.fraser <- 
  ham.dets2 %>%
  group_by(tagID)%>%
  filter(Site %in% "fraser")%>%
  summarise(n=length(unique(location_site)))
  

summary <- ham.dets2 %>%
  ungroup()%>%
  group_by(location_site,release.location)%>%
  summarise(n=length(unique(tagID)))%>%
  spread(release.location,value = n)

write.csv(summary, file=here("data summaries", "very rough detection summary.csv"))

write.csv(ham.dets2, file=here("cleaned data","cleaned detections from hammersley.csv"))

# # filter out non-study tags ####
non.study.tags <- df%>%
  filter(receiver.serial != 108656 & !tagID %in% tag.meta$tagid )

sensors <- non.study.tags %>%
  mutate(sensor=ifelse(Sensor.Value > 0, "Yes","No"))%>%
  filter(sensor=="Yes")

summary <- non.study.tags %>%
  group_by(tagID)%>%
  summarise(n=n())%>%
  filter(n>1)%>%
  mutate(sensor= ifelse(tagID %in% sensors$tagID, "Yes", "No"))

#write.csv(summary, file=here("data summaries","non study tags.csv"))
