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

unique(tag.meta$release.datetime)

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

# Fish were detected up and downstream simultaneously because the floodgate transmitted detections.  
# look at which fish were detected on the mid-channel vs floodgate receiver to determine if we can drop
# a receiver from this dataframe and clean up dets

ID_per_receiver <- df6%>%
  group_by(location_site, Receiver, release.location)%>%
  summarise(n=length(unique(tagID)))

# mid-channel detected 183 IDs, floodgate detected 113 IDs.
mid_chan <- df6 %>%
  filter(Receiver == c(108647,101503))%>%
  select(tagID)%>%
  reframe(ID=unique(tagID))

det_check <- df6%>%
  filter(Receiver==108660)%>%
  mutate(uniqueID= ifelse(tagID %in% mid_chan$ID, F, T))%>%
  filter(uniqueID==T)%>%
  select(tagID)%>%
  group_by(tagID)%>%
  summarise(n()) 

# 5 tags only heard on floodgate and not on the 

# how many of these unique 5 were from upstream releases?
qa1 <- det_check %>% merge(., fish.dat, by="tagID") # all five are upstream released fish. 
#  what do dets look like for these fish?
floodgate_unique <- df6%>%
  filter(tagID %in% qa1$tagID)%>%
  group_by(tagID)%>%
  arrange(datetime.local)%>%
  select(tagID, datetime.local,Receiver, location_site)

# quick plot of dets for these fish
#ggplot(floodgate_unique, aes(x=datetime.local, y=location_site, color=Receiver))+
  #geom_point()+
  #facet_wrap(~tagID)

# none of these fish were detected on the Fraser arrays. Thus, I can delete the floodgate receiver

# Remove tags that aren't detected at least twice in the same region with in 1 hour ####

df7 <- df6 %>%
  filter(Receiver != 108660)

# Data exploration and QA ####

length(unique(df7$tagID)) #295 IDs detected out of 300

## detected by release location####

df7 %>%
  group_by(release.location)%>%
  summarise(length(unique(tagID)))

# Hammersley: 103 released upstream and 103 detected, # 105 released downstream but 102 detected
# Hatzic: 51 released ds with 50 detected, 41 released up with 39 detected
# were any fish released downstream detected upstream?

df7 %>%
  group_by(release.location, location_site)%>%
  summarise(length(unique(tagID))) # some odd patterns here i.e., upstream movement

# Hammersley apparent upstream movement ####
df7 %>%
  mutate(location_site=paste(Location, Site, sep="-"))%>%
  group_by(release.location, location_site)%>%
  summarise(length(unique(tagID)))

up_ham <- df7%>%
  filter(release.location %in% "ham.ds" & location_site %in% "upstream-mountain slough")

# quick plot of det histories
df7%>%
  filter(tagID %in% up_ham$tagID)%>%
  ggplot(., aes(x=datetime.local, y=Receiver))+
  geom_point()+
  facet_wrap(~tagID) # lots of dets of these fish.
# It looks like dets could transmit through the floodgates as fish were detected on receiver ds of floodgate and the pump
# at the same time.

# Hatzic Lake upstream movement (plausible due to floodgates being open) ####
df7 %>%
  filter(release.location %in% "hat.ds" & release.datetime %in% "2024-05-15 11:46:00" & location_site %in% "upstream-hatzic") %>%
  group_by(tagID)%>%
  summarise(n())%>%
  rename(tagid=tagID)%>%
  merge(., tag.meta, by="tagid") # all these fish could have gone up stream with the tides as the tide gates were still open at hatzic.

## Data Clean Up ####
## will need to remove detections on 108658 after first detection downstream-mountain slough to 
# account for transmission of dets through floodgate once downstream

df8 <- df7 %>%
  mutate(last_loc = lag(location_site),
    move_dir = ifelse(location_site %in% c("upstream-mountain slough","upstream-hatzic") &
  last_loc %in% c("downstream-mountain slough", "downstream-hatzic"), "UP","DOWN"))%>%
  filter(!(Site %in% "mountain slough" & move_dir %in% "UP"))


test <- df8%>%filter(move_dir %in% "UP")%>% select(tagID, location_site, Site, last_loc)

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

plot.df <- df8%>%
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

plot.df$Receiver <- factor(plot.df$Receiver, levels=c("release","108647","101503","108661","108658","108654"))

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

all.dets <- df8%>%
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
  summarise(length(unique(tagID)))# 205 detected

## create row with relase information ###
ham.releases <- release.df%>%
  filter(release.location %in% c("ham.us","ham.ds"))%>%
  select(tagID,datetime.local,release.location,location_site,Receiver)%>%
  mutate(Site="mountain slough",
         type="release", 
         tagID=as.integer(tagID), 
         datetime.local=force_tz(datetime.local, tz="America/Vancouver"))

ham.dets2 <- ham.dets %>%
  rbind(., ham.releases)%>%
  group_by(tagID)%>%
  arrange(tagID, datetime.local)

#locations <- hammersley (mountain slought) >mission>derby>barnston>portman>new west

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

i <- ceiling(
  length(unique(ham.dets2$tagID)) / 16) # set the number of pages

pdf(file=here("figures", "hammersley detection plots.pdf"), 
    width=10, height=8)

lapply(seq(i), function(page) {
  ham.plots1 <- ham.dets2%>%
  ggplot(., aes(x=datetime.local, y=location_site, shape=type))+
  geom_point()+
  geom_line()+
  theme(axis.text.x=element_text(angle=(-45)))+
  labs(x="", y="Site Name")+
  facet_wrap_paginate(~tagID, scales="free_x",
             nrow=4, ncol=4, page=page)
})

dev.off()

#ggsave(ham.plots1, file=here("figures", "Hammersley fish page 5.png"), 
       #width=10, height=8)

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
