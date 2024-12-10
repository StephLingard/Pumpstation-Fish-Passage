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
  mutate(time_next=difftime(lead(datetime.local), datetime.local, units="mins"),
         transition=ifelse(location_site == lead(location_site),F,T))

# need to add a row for release location and datetime. so want to add a column for "type" release/det
# then release location = location_site and release datetime=datetime.local

release.dat <- read.csv(here("cleaned data","fish release date time location.csv"), show_col_types = FALSE)


# we need to know how many were missed by each of the receivers in the Fraser to decide
# how to group the data or clean it.
# ideally i would remove any fish only detected once on one receiver





