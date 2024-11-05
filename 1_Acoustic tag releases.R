# reading in tagging data and release data
library(tidyverse)
library(here)

surg.dat <- read_csv(here("raw data","acoustic tagging.csv"))%>%
  mutate(release.date_trough = paste(release.date, trough, sep="_"))

a.rel.dat <- read_csv(here("raw data", "acoustic fish release data.csv"))%>%
  rename(release.date=date)%>%
  mutate(release.date_trough = paste(release.date, trough.section, sep="_"))


complete_dat <- surg.dat %>%
  merge(a.rel.dat, by="release.date_trough")

complete_dat %>%
  group_by(release.location)%>%
  summarise(n())
                          