# working with detections cleaned in 2_acoustic detections
# only fish from hammersley are considered here as that was the only pump run
# S Lingard
# Created: December 12, 2024

library(tidyverse)
library(lubridate)
library(here)

# Distance between sites for travel times: 
# mountain slough = rkm 110, hatzic=72, Mission=67, Derby = 45, Barnston = 38, PORTMANN=30
# NEWWEST= 23

rkm <- c(110,110,72,67,45,38,30,23)
location_site <-c("upstream-mountain slough","downstream-mountain slough","downsrtream-hatzic","MISSION",
                  "DERBY","BARNSTON","PORT_MANN","NEW_WEST")

site_rkm <- tibble(rkm, location_site)

# read in dets and add rkm column
dets <- read_csv(here("cleaned data","cleaned detections from hammersley.csv"),show_col_types = FALSE)%>%
  select(-'...1')%>%
  merge(., site_rkm, by="location_site")%>%
  group_by(tagID)%>%
  arrange(tagID, datetime.local)%>%
  mutate(time_prev=difftime(datetime.local, lag(datetime.local), units="secs"),
         transition=ifelse(location_site == lag(location_site),F,T),
         prev_rkm=lag(rkm),
         dist_traveled=abs(rkm-lag(rkm)),
         speed_bodyL_secs=(dist_traveled/0.0012)/as.numeric(time_prev))

test.speed <- dets %>%
  filter(speed_bodyL_secs>1)

test.dir <- dets %>%
  mutate(next_rkm=lead(rkm),
    upstream_movement=ifelse(next_rkm > rkm, TRUE, FALSE))%>%
  filter(upstream_movement==TRUE)

# need to remove these two detections


speed_distribution <- dets%>%
  filter(speed_bodyL_secs > 0.0000)%>%
  ggplot(., aes(x=speed_bodyL_secs))+
  geom_histogram()+
  labs(x="Speed (body lengths per second)")

ggsave(speed_distribution, file=here("figures","travel speeds.png"),
       width=6, height=8)
  
# travel times look okay! moving on to CJS models

# Make CJS model table ####





