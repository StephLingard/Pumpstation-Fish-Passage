# working with detections cleaned in 2_acoustic detections
# only fish from hammersley are considered here as that was the only pump run
# S Lingard
# Created: December 12, 2024

library(tidyverse)
library(lubridate)
library(here)
library(marked)

# Distance between sites for travel times: 
# mountain slough = rkm 110, hatzic=72, Mission=67, Derby = 45, Barnston = 38, PORTMANN=30
# NEWWEST= 23
# have to remove detections of fish that have traveled downstream or were released downstream of the mountain slough upstream 
#  pump as the flood gates transmitted the signals and it creates false fast travel times.

# make location-rkm table ####

rkm <- c(110.2,110.1,72,67,45,38,30,23)
location_site <-c("upstream-mountain slough","downstream-mountain slough","downstream-hatzic","MISSION",
                  "DERBY","BARNSTON","PORT_MANN","NEW_WEST")

site_rkm <- tibble(rkm, location_site)%>%
  mutate(rkm=as.numeric(rkm))

# read in dets and add rkm column ####
dat <- read.csv(here("cleaned data","cleaned detections from hammersley.csv"))

# manipulate data to have previous and next detections for filtering ####
dets <- dat%>%
  filter(Receiver!=	108660)%>%
  merge(., site_rkm, by="location_site")%>%
  group_by(tagID)%>%
  arrange(tagID, datetime.local)%>%
  mutate(time_prev=difftime(datetime.local, lag(datetime.local), units="secs"),
         transition=ifelse(location_site == lag(location_site),F,T),
         prev_rkm=lag(rkm),
         next_rkm=lead(rkm),
         time_next=difftime(lead(datetime.local), datetime.local, units="secs"),
         dist_traveled=lag(rkm)-rkm)
  
length(unique(dets$tagID))# all 208 are here

test.dir <- dets %>%
  group_by(tagID)%>%
  arrange(datetime.local)%>%
  filter(rkm > prev_rkm)# need to remove these

# need to remove dets of fish upstream of pump once they have gone through or been released ds
# then need to recalculate everything. But don't want to drop releases.

### create release df to merge back in after filtering ####
releases <- dets%>%
  filter(Receiver %in% "release")

# create filtered data frame to calculate travel times ####
dets2 <- dets %>%
  group_by(tagID)%>%
  arrange(datetime.local)%>%
  filter(rkm < prev_rkm & transition==TRUE)%>%
  bind_rows(., releases)%>%
  arrange(datetime.local)%>%
  mutate(time_prev=difftime(datetime.local, lag(datetime.local), units="secs"),
         transition=ifelse(location_site == lag(location_site),F,T),
         prev_rkm=lag(rkm),
         next_rkm=lead(rkm),
         time_next=difftime(lead(datetime.local), datetime.local, units="secs"),
         dist_traveled=lag(rkm)-rkm,
         speed_bodyL_secs=(dist_traveled/0.0012)/as.numeric(time_prev),
         time_diff_days=as.numeric(time_prev)/(60*60*24),
         speed_km_day=dist_traveled/time_diff_days)

length(unique(dets2$tagID))
unique(dets2$Receiver)

# speed plots ####
speed_distribution <- dets2%>%
  filter(speed_bodyL_secs > 0)%>%
  ggplot(., aes(x=speed_bodyL_secs))+
  geom_histogram()+
  labs(x="Speed (body lengths per second)")+theme_classic()+
  theme(axis.text = element_text(size=12),
        axis.title=element_text(size=14))

speed_km_per_day <- dets2%>%
  filter(speed_km_day > 0)%>%
  ggplot(., aes(x=speed_km_day))+
  geom_histogram(binwidth=5)+
  labs(x="Speed (km per day)")+
  scale_fill_grey()+
  theme_classic()+
  theme(axis.text = element_text(size=12),
        axis.title=element_text(size=14))

ggsave(speed_distribution, file=here("figures","travel speeds.png"),
       width=6, height=8)

ggsave(speed_km_per_day, file=here("figures","travel speeds km per day.png"),
       width=8, height=7)
# travel times look okay!

# Time in each location ####
first_last <- dets %>%
  group_by(tagID, rkm)%>%
  arrange(datetime.local)%>%
  summarise(first_det=first(datetime.local),
            last_det=last(datetime.local),
            duration=difftime(last_det,first_det,  units="hours"))%>%
  arrange(.by_group = TRUE, desc(rkm))%>%
  merge(., site_rkm, by="rkm")%>%
  ungroup()

dur_plot <- first_last %>%
  mutate(rkm=as.factor(rkm))%>%
  ggplot(., aes(x=rkm,y=duration))+
  geom_boxplot()+
  theme_classic()+
  labs(x="River Kilometer", y="Duration of Detections (hours)")+
  theme(axis.text = element_text(size=12),
        axis.title=element_text(size=14))

ggsave(dur_plot, file=here("figures", "time detected at each array.png"),
       width=6, height=7)

dur_summary <- first_last %>%
  mutate(rkm=as.factor(rkm))%>%
  group_by(rkm)%>%
  summarise(median=median(duration, na.rm=TRUE),
            min=min(duration, na.rm=TRUE),
            max=max(duration, na.rm=TRUE))%>%
  merge(., site_rkm, by="rkm")%>%
  relocate(location_site, .before = rkm)%>%
  rename(site=location_site)%>%
  arrange(rkm)

# Fish that died after going through pump? ####
pump_morts <- first_last %>%
  filter(rkm == 110.1 & duration > 72)%>%
  select(tagID)

fraser_dets <- dets %>%
  filter(rkm < 110)

morts_det_ds <- fraser_dets %>%
  merge(., pump_morts, by="tagID")%>%
  group_by(tagID, location_site)%>%
  summarise(n())

# what about fish that stayed upstream for 72 hours?
up_morts <- first_last %>%
  filter(rkm == 110.2 & duration > 240)%>%
  select(tagID)

ds_dets <- dets %>%
  filter(rkm==110.1)%>%
  filter(tagID %in% up_morts$tagID)%>%
  group_by(tagID)%>%
  summarise(n()) # only 10 of 15 fish were detected downstream after this long upstream

up_morts_det_ds <- fraser_dets %>%
  merge(., up_morts, by="tagID")%>%
  group_by(tagID, location_site)%>%
  summarise(n())

up_morts_det_ds %>%
  ggplot(., aes(x=location_site))+
  geom_histogram(stat="count")

# Make CJS model table ####
# I only want the first time a fish was detected at each location

dets3 <- dets %>%
  group_by(tagID)%>%
  arrange(datetime.local)%>%
  group_by(tagID, location_site)%>%
  filter(row_number()==1)%>%
  select(location_site, tagID, release.location, rkm)%>%
  mutate(dummy_var = 1,
         rkm=as.factor(rkm))%>%
  ungroup()

dets_wide <- dets3 %>%
  select(-location_site)%>%
  group_by(tagID,)%>%
  pivot_wider(names_from = rkm, values_from = dummy_var, 
              values_fill = 0, id_cols=c("tagID", "release.location"),
              names_prefix="rkm_")

# CJS Table ####
cjs_table_ham <- dets_wide%>%
  unite(., "ch", rkm_110.2:rkm_23, sep="")%>%
  ungroup()%>%
  select(-tagID)%>%
  relocate(ch, .before="release.location")%>%
  mutate(release.location=as.factor(release.location))%>%
  ungroup()%>%
  rename(release=release.location)%>%
  as.data.frame(.)

write.csv(cjs_table_ham, file=here("cleaned data", "cjs table all dets.csv"))

# CJS table no single dets ####

dets4 <- dets %>%
  group_by(tagID)%>%
  arrange(datetime.local)%>%
  group_by(tagID, location_site)%>%
  filter(row_number()==2)%>% # selecting 2nd row of each tag-locaiton combo single dets removed
  select(location_site, tagID, release.location, rkm)%>%
  mutate(dummy_var = 1,
         rkm=as.factor(rkm))%>%
  ungroup()

dets_wide_singles_rm <- dets4 %>%
  select(-location_site)%>%
  group_by(tagID,)%>%
  pivot_wider(names_from = rkm, values_from = dummy_var, 
              values_fill = 0, id_cols=c("tagID", "release.location"),
              names_prefix="rkm_")

cjs_table_ham2 <- dets_wide_singles_rm%>%
  unite(., "ch", rkm_110.2:rkm_23, sep="")%>%
  ungroup()%>%
  select(-tagID)%>%
  relocate(ch, .before="release.location")%>%
  mutate(release.location=as.factor(release.location))%>%
  ungroup()%>%
  rename(release=release.location)%>%
  as.data.frame(.)

write.csv(cjs_table_ham2, file=here("cleaned data", "cjs table no single dets.csv"))

# time from release to pump passage ###
release_dt <- releases%>%
  select(datetime.local, Receiver, release.location, tagID)%>%
  rename(release_dt=datetime.local)

time_to_pass <- dets %>%
  filter(location_site %in% "downstream-mountain slough")%>%
  group_by(tagID)%>%
  arrange(datetime.local)%>%
  filter(row_number()==1)%>%
  select(datetime.local, location_site, tagID)%>%
  rename(first_ds_det=datetime.local, site=location_site)%>%
  merge(., release_dt, by="tagID")%>%
  mutate(time_to_pass= difftime(first_ds_det, release_dt, units="hours"))%>%
  filter(release.location %in% "ham.us")

passage_plot <- time_to_pass %>%
  mutate(release_dt=as.factor(release_dt))%>%
  ggplot(., aes(x=release_dt, y=time_to_pass))+
  geom_boxplot()+
  theme_classic()+
  theme(axis.text.x=element_text(angle=90))+
  labs(x="Release Date", y="Time to passage (hours)")+
  geom_text(data=(time_to_pass %>% group_by(release_dt) %>% filter(!is.na(time_to_pass))%>% 
                    summarise(n=n())), 
            aes(x=seq(1,7), y=630, label= paste0("n = ", n)))+
  expand_limits(y = c(0, 650))

ggsave(passage_plot, file=here("figures", "time to passage.png"),
       width=5, height=4)

time_to_pass %>%
  summarise(n=n(),
            min=min(time_to_pass, na.rm=TRUE),
            max=max(time_to_pass, na.rm=TRUE),
            mean=mean(time_to_pass, na.rm=TRUE), 
            sd=sd(time_to_pass, na.rm=TRUE), 
            median=median(time_to_pass, na.rm=TRUE))

# Time to reach New West

new_west_dets <- dets2 %>%
  filter(location_site %in% "NEW_WEST")%>%
  group_by(tagID)%>%
  filter(row_number()==1)%>%
  select(tagID, location_site, datetime.local)%>%
  merge(., release_dt, by="tagID")%>%
  mutate(time_to_nw=as.numeric(difftime(datetime.local, release_dt, units="days")))


new_west_time <- new_west_dets %>%
    group_by(release.location)%>%
    summarise(n=n(),
            min=min(time_to_nw, na.rm=TRUE),
            max=max(time_to_nw, na.rm=TRUE),
            mean=mean(time_to_nw, na.rm=TRUE), 
            sd=sd(time_to_nw, na.rm=TRUE), 
            median=median(time_to_nw, na.rm=TRUE))

# Time in each segment

# How do I calculate time in each segment. Its the time from their first detection at one site to the first detection at the next site
# I think i filter out all the "transitions" and then calculate the time diff between each

seg_durations <- dets %>%
  select(tagID, location_site, Site, datetime.local, transition, release.location)%>%
  filter(transition==TRUE)%>%
  group_by(tagID)%>%
  arrange(datetime.local)%>%
  mutate(time_next=difftime(datetime.local, lag(datetime.local)),
         prev_location=lag(location_site))%>%
  select(-time_next)%>%
  arrange(tagID, datetime.local)%>%
  group_by(tagID)%>%
  mutate(time_to=difftime(datetime.local, lag(datetime.local),units="hours"))%>%
  ungroup()
  
summary_durations <- seg_durations%>%
  group_by(location_site, prev_location)%>%
  summarise(n=n(),
            min=min(time_to, na.rm=TRUE),
            max=max(time_to, na.rm=TRUE),
            mean=mean(time_to, na.rm=TRUE), 
            sd=sd(time_to, na.rm=TRUE), 
            median=median(time_to, na.rm=TRUE))%>%
  filter(!location_site %in% "upstream-mountain slough" & !prev_location %in% c(NA, "upstream-mountain slough"))%>%
  mutate(prev_current=paste(location_site, prev_location, sep="_"))

summary_durations%>%
  ggplot(., aes(x=prev_current, y=mean))+
  geom_point()+
  theme(axis.text = element_text(angle=90))

# need to clean up this plot and to condense the segments down. 
# I would also like to add the 95% CI bars
# I first plotted the means by release location separately but there was now difference between groups
# So i can calcluate these all together in one plot

# Need a fish size plot


