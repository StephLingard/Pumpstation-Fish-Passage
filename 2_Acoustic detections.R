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
# read in tagging and relase data

tag.deployment <- read.csv(here("raw data", "acoustic tagging.csv"))%>%
  rename(trough.section=trough)
release.data <- read.csv(here("raw data", "acoustic fish release data.csv"))%>%
  rename(release.date=date)

tag.meta <- tag.deployment%>%
  full_join(., release.data, by=c("release.date", "trough.section"))%>%
  mutate(release.datetime=ymd_hm(paste(release.date, release.time, by=" ")))

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


df4 <- df3 %>% 
  filter(diff.time >= 45)


