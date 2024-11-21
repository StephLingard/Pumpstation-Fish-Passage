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


# read in raw detections ####
df <- list.files(path=here("raw data","acoustic receivers"), full.names=TRUE ) %>% 
  map_dfr(read.csv)

# format the dates and times
df$datetime.utc= force_tz(as.POSIXct(df$Date.and.Time..UTC., format="%Y-%m-%d %H:%M:%S"), "UTC")
df$datetime.local=with_tz(df$datetime.utc, "America/Vancouver")

# break out receiver and tag serials so this will match with the information in the deployment files
df$receiver.serial <- gsub(".*-","",df$Receiver)
df$tagID <- gsub(".*-*-","",df$Transmitter)

#remove false detections
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
  group_by(release.date, release.location)%>%
  summarise(length(unique(tagid)))

length(unique(tag.meta$tagid)) # one ID is missing????

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

# One minute data filter #### 
# Explanation: there must be a second detection with in 1 minute of the current one.
# diff_time is the time difference between the current time and the previous record. lead diff.time is the time between obs and next obs

df5 <- df4 %>%
  group_by(tagID)%>%
  mutate(Cond1=ifelse(diff.time <=60, TRUE, FALSE), 
         Cond2=ifelse(lead(diff.time) <=60, TRUE, FALSE))%>%
  filter(Cond2 == TRUE | Cond1==TRUE)


# filter out non-study tags ####
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

# Read in receiver meta ####

r.meta <- read.csv(here("raw data", "receiver metadata_2024.csv"))%>%
  select(-Way.Point)

# remove "VR2W-" and "VR2Tx" from Receiver in df4

df5$Receiver <- gsub("VR2W-","", df5$Receiver)
df5$Receiver <- gsub("VR2Tx-","", df5$Receiver)

# now add in location and up or downstream by merging dfs
# also add in release.datetime and location column

df6 <- df5 %>% 
  select(-c(Station.Name, Latitude, Longitude))%>%
  merge(., r.meta, by="Receiver")

fish.dat <- tag.meta%>%
  select(tagid, release.datetime,release.location)%>%
  rename(tagID=tagid)

fish.dat %>%
  group_by(release.location)%>%
  summarise(length(unique(tagID))) # 300 total

df6 <- df6%>%
  mutate(location_site=paste(Location, Site, sep="-"))%>%
  merge(., fish.dat, by="tagID")

# Remove tags that aren't detected at least twice in the same "location-site" with in 1 hour ####

df7 <- df6 %>%
  group_by(tagID)%>%
  mutate(cond1 = ifelse(lead(diff.time) > 3600, FALSE, TRUE), 
         cond2= ifelse(diff.time > 3600, FALSE, TRUE))%>%
  filter(cond1 | cond2 == TRUE)

# Data exploration and QA ####

length(unique(df7$tagID)) #287 IDs detected out of 300

# Fish detected

df7 %>%
  group_by(release.location)%>%
  summarise(length(unique(tagID)))

# Hammersley: 103 released upstream but 102 detected, # 105 released downstream but 98 detected
# Hatzic: 51 released ds with 50 detected, 41 released up with 39 detected

# were any fish released downstream detected upstream?

df7 %>%
  group_by(release.location,Location)%>%
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

# there are a lot of detections of some of these fish and the filters are very conservative

upIDs <- up.movement%>%
  group_by(tagID)%>%
  filter(row_number()==1)

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
  mutate(Receiver=NA,
         datetime.local=release.datetime, 
         location=release.location)

plot.df <- df7%>%
  mutate(location_site=paste(Location, Site, sep="-"))%>%
  select(tagID, location_site, datetime.local, release.location, 
         release.datetime, Receiver)%>%
  bind_rows(., release.df)%>%
  group_by(tagID)%>%
  arrange(datetime.local)%>%
  mutate(location_site=as.factor(location_site), 
         location=location_site)


levels(plot.df$location) <- fct_recode(plot.df$location, 
                                            '1'="upstream-mountain slough",
                                            '2'="downstream-mountain slough",
                                            '3'="upstream-hatzic",
                                            '4'="downstream-hatzic")


up.fish <- plot.df %>%
  filter(tagID %in% up.movement$tagID)%>%
  mutate(location=as.numeric(location))%>%
  ggplot(., aes(x=datetime.local, y=location_site))+
  geom_point()+
  geom_line()+
  facet_wrap(~tagID)


ggsave(up.fish, file=here("figures","Hammersley Upstream Movement.png"),
                          width=9, height=8)
  
plot.df %>%
  filter(tagID %in% up.movement$tagID)%>%
  group_by(Receiver)%>%
  summarise(n())
