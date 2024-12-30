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

head(complete_dat)

size.plot <- complete_dat%>%
  filter(release.location %in% c("ham.us","ham.ds"))%>%
  mutate(release.location = fct_recode(release.location,
             Control = "ham.ds",
             Archimedes = "ham.us"))%>%
  ggplot(., aes(x=release.location, y=length))+
  geom_point()+
  geom_boxplot()+
  theme_classic()+
  labs(x="Release Group",y="Fork Length (mm)")+
  theme(axis.text=element_text(size=12),
        axis.title=element_text(size=14))

ggsave(size.plot, file=here("figures", "fish length by release group.png"),
                            width=6, height=6)
                          