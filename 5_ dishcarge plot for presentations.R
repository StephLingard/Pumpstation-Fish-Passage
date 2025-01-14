# plotting Fraser River discharge at hope 2000-2024

library(tidyverse)
library(lubridate)
library(here)
here()
dat <- read.csv(here("raw data","daily discharge_08MF005.csv"))

dat_long <- dat %>%
  gather(., value="discharge", key="month", -c(X.ID, YEAR, DD))%>%
  mutate(month2 = fct_recode(month, '01'="Jan", '02'="Feb",'03'="Mar", 
                             '04'="Apr",'05'="May",'06'="Jun",
                             '07'="Jul",'08'="Aug",
                             '09'="Sep",'10'="Oct",'11'="Nov",'12'="Dec"),
         year_month=paste(YEAR, month2, sep="-"),
         date=paste(year_month, DD, sep="-"), 
         date2=as.Date(date, format="%Y-%m-%d"),
         doy=yday(date2))%>%
  filter(!is.na(discharge))# only NA for dates and discharge are February 29-31

QA <- dat_long%>%
  filter(is.na(date2))


doy_dates <- dat_long %>%
  filter(doy %in% c(1, 50, 100, 150, 200,250,300,350))%>%
  group_by(doy)%>%
  filter(row_number()==1)

date_labs <- c("Jan 1", "Feb 15", "Apr 10", "May 30", "Jul 15", "Sep 10", "Oct 25","Dec 15")

plot <- dat_long%>%
  filter(YEAR>=2010 & discharge > 50)%>%
  mutate(YEAR=as.factor(YEAR))%>%
  ggplot(., aes(x=doy, y=discharge, color=YEAR))+
  geom_point(show.legend = FALSE, size=0.5)+
  geom_hline(yintercept=2800, colour="red", linetype="dashed")+
  theme_classic()+
  scale_colour_grey()+
  labs(x="Day of Year", y=expression(paste("Discharge ", m^{3}/s,)))+
  scale_x_continuous(breaks=seq(0,350,50), labels=date_labs)+
  theme(axis.text=element_text(size=12))

ggsave(plot, file=here("figures","discharge plot fraser at hope 2010-2024.png"),
       width=6, height=6)


