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

df <- list.files(path=here("raw data","acoustic receivers"), full.names=TRUE ) %>% 
  map_dfr(read.csv)

# format the dates and times
df$datetime.utc= force_tz(as.POSIXct(df$Date.and.Time..UTC., format="%Y-%m-%d %H:%M:%S"), "UTC")
df$datetime.local=with_tz(df$datetime.utc, "America/Vancouver")

# break out receiver and tag serials so this will match with the information in the deployment files
df$receiver.serial <- gsub(".*-","",df$Receiver)
df$tagID <- gsub(".*-*-","",df$Transmitter)

#remove false detections
# read in tagging and release data

tag.deployment <- read.csv(here("raw data", "acoustic tagging.csv"))%>%
  rename(trough.section=trough)
release.data <- read.csv(here("raw data", "acoustic fish release data.csv"))%>%
  rename(release.date=date)

tag.meta <- tag.deployment%>%
  full_join(., release.data, by=c("release.date", "trough.section"))%>%
  mutate(release.datetime=ymd_hm(paste(release.date, release.time, by=" ")))

tag.meta %>%
  group_by(release.location)%>%
  summarise(length(unique(tagid)))

length(unique(tag.meta$tagid)) # one ID is missing????

tag.deployment.receiver <- df %>%
  filter(receiver.serial == 108656)

# now filter out tag detections prior to release

df2 <- df%>%
  filter(receiver.serial != 108656 & tagID %in% tag.meta$tagid & datetime.local > tag.meta$release.datetime)

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


write.csv(summary, file=here("data summaries","non study tags.csv"))

# now filter detections less than 45 seconds apart

df3 <- df2 %>%
  select(-c(Sensor.Precision, Sensor.Unit, Sensor.Value, Transmitter.Name, Transmitter.Serial, Transmitter.Type))%>%
  group_by(tagID)%>%
  arrange(., tagID, datetime.local)%>%
  mutate(diff.time = difftime(datetime.local, lag(datetime.local), units="secs"))

# now recalculate time diffs
df4 <- df4 %>% 
  group_by(tagID)%>%
  arrange(., tagID, datetime.local)%>%
  mutate(diff.time = difftime(datetime.local, lag(datetime.local), units="secs"))

# check the filter worked
min(df4$diff.time, na.rm=T)


## Read in receiver meta ####

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

fish.dat <- tag.meta%>%
  select(tagid, release.datetime,release.location)%>%
  rename(tagID=tagid)

df5 <- df5%>%
  merge(., fish.dat, by="tagID")%>%
  select(-c('Date.and.Time..UTC.',Transmitter))

# remove un-needed vectors to free up memory
rm(fish.dat)

#how many IDs detected?

length(unique(df5$tagID)) #292 IDs detected.

## Lets first focus on Hammersley

df5 %>%
  group_by(release.location)%>%
  summarise(length(unique(tagID)))

# 102 released upstream, # 68 downstream. Need to confirm in notes this is correct

ham.fish <-df5 %>%
  filter(release.location %in% c("ham.ds", "ham.us"))

ham.fish %>% 
  group_by(release.location)%>%
  summarise(length(unique(tagID)))

# were any fish released downstream detected upstream?

ham.fish %>%
  group_by(release.location,Location)%>%
  summarise(length(unique(tagID)))

# so about 80% of fish released downstream may have made it downstream
# but 11 fish appearantly moved upstream from downstream?

up.movement <- ham.fish%>%
  filter(release.location %in% "ham.ds" & Location %in% "upstream")

up.movement%>%
  group_by(tagID)%>%
  summarise(n()) # there are a lot of detections of these fish

min(up.movement$datetime.local)
max(up.movement$datetime.local)

#hmmm these dets occured while gates were shut. So either there is a hole in the flood gates, a lot of noise, 
# or there are mistakes in the release records. Will have to go and check the raw data

df5 %>%
  mutate(location_site=paste(Location, Site, sep="-"))%>%
  group_by(release.location, location_site)%>%
  summarise(length(unique(tagID)))

# there are a lot of fish detected in places theye weren't relased.

