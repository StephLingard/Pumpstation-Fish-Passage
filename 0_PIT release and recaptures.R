# Pump station year 1 PIT releases
# reading in release tag IDs from each time stamp and matching with recaptures

library(tidyverse)
library(lubridate)
library(here)

here()

# read in release data 

rel.dir <- list.files(here("raw data","pit releases"), pattern='.csv', full.names = TRUE)

rel.dat <- rel.dir %>% map_dfr(read_csv)%>%
  mutate(release.date=ymd(release.date),
         date.time=ymd_hms(paste(release.date, release.time, sep=" ")))%>%
  rename(tag.id='last.four')%>%
  select(date.time, tag.id,treatment)

# read in recap data

recap.dat <- read_csv(here("raw data", "injury assessment and blood sampling_final.csv"))%>%
  mutate(release.date.time=ymd_hms(paste(date, release.time, sep=" ")),
         sample.date.time=ymd_hms(paste(date, sample.time, sep=" ")))

# drop the hatchery data as not of interest- for training only
recap.dat2 <- recap.dat %>%
  filter(site != "hatchery" )%>%
  select(release.date.time, sample.date.time, tag.id, treatment)%>%
  rename(date.time=release.date.time)

# Do the release times/groups match in both DFs? 

recaptured.df <- recap.dat2 %>% # Are all the IDs in the recaptured data matched to fish release?
  filter(tag.id %in% rel.dat$tag.id) # all accounted for 

release.recap.groups <- recap.dat2 %>%
  rename(recap.group=treatment)%>%
  merge(., rel.dat, by="tag.id")%>%
  mutate(time_diff=difftime(sample.date.time, date.time.y, units="mins"))%>%
  select(tag.id, time_diff, treatment)%>%
  rename(release_treatment=treatment)

# merge diff_time with the injury and physiology data

recap.final <- recap.dat %>% 
  filter(tag.id %in% release.recap.groups$tag.id)%>%
  merge(., release.recap.groups, by="tag.id")%>%
  mutate(hemoglobin=as.numeric(hemoglobin),
         lactate=as.numeric(lactate))%>%
  filter(time_diff < 60)

names(recap.final)

lactate <- ggplot(recap.final, aes(x=release_treatment, y=lactate))+
  geom_boxplot()+
  geom_point()

ggsave(lactate, file=here("figures", "lactate by treatment.png"),
       width=7, height=6)

hemo <- ggplot(recap.final, aes(x=release_treatment, y=hemoglobin))+
  geom_boxplot()+
  geom_point()

ggsave(hemo, file=here("figures", "hemoglobin by treatment.png"),width=7, height=6)

hemo_time <- ggplot(recap.final, aes(x=time_diff, y=hemoglobin, color=release_treatment))+
  geom_point()+
  labs(x="Time between release and sampling (minutes)")

ggsave(hemo_time, file=here("figures", "hemoglobin vs time.png"),width=7, height=6)

recap.final%>%
  filter(!is.na(hemoglobin))%>%
  

# Injuries

injuries <- recap.final %>%
  select(-c(contusion.location, hem.location, lactate, hemoglobin, hematocrit, sample.no, comment, time_diff, do, temp,sample.time,release.time, sample.date.time,comment,release.date.time))%>%
  gather(., key="injury", value="value", -c(tag.id,date,site,treatment,length))

unique(injuries$injury)

injury.table <- injuries %>% 
  filter(! injury %in% "scale.loss")%>%
  mutate(pump=ifelse(treatment %in% "control", "control", "pump"))%>%
  group_by(injury, pump)%>%
  summarise(total=sum(value))%>%
  spread(., pump, total)

write.csv(injury.table, here("data summaries","summaries of injuries 2024.csv"))

scale.loss.table <- injuries %>% 
  filter(injury %in% "scale.loss")%>%
  mutate(pump=ifelse(treatment %in% "control", "control", "pump"), 
         value=as.factor(value))%>%
  group_by(pump, value)%>%
  summarise(n())
