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

rkm <- c(110.2,110.1,72,67,45,38,30,23)
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
         speed_bodyL_secs=(dist_traveled/0.0012)/as.numeric(time_prev),
         speed_km_day=dist_traveled/(as.numeric(time_prev)/86400))

test.speed <- dets %>%
  filter(speed_bodyL_secs>1)

test.dir <- dets %>%
  mutate(next_rkm=lead(rkm),
    upstream_movement=ifelse(next_rkm > rkm, TRUE, FALSE))%>%
  filter(upstream_movement==TRUE)

# need to remove these two detections

dets2 <- dets %>%
  mutate(next_rkm=lead(rkm),
         upstream_movement=ifelse(next_rkm > rkm, TRUE, FALSE))%>%
  filter(!upstream_movement %in% TRUE)
  
speed_distribution <- dets2%>%
  filter(speed_bodyL_secs > 0.0000)%>%
  ggplot(., aes(x=speed_bodyL_secs))+
  geom_histogram()+
  labs(x="Speed (body lengths per second)")

ggsave(speed_distribution, file=here("figures","travel speeds.png"),
       width=6, height=8)

speed_km_per_day <- dets2%>%
  filter(speed_km_day > 0.0000)%>%
  ggplot(., aes(x=speed_km_day))+
  geom_histogram()+
  labs(x="Speed (km per day)")

ggsave(speed_distribution, file=here("figures","travel speeds.png"),
       width=6, height=8)

ggsave(speed_km_per_day, file=here("figures","travel speeds km per day.png"),
       width=6, height=8)
  
# travel times look okay! moving on to CJS models

# Make CJS model table ####
# I only want the first time a fish was detected at each location

dets3 <- dets2 %>%
  mutate(up.move = ifelse(release.location %in% "ham.ds" & Receiver == 108658, TRUE, FALSE))%>%
  filter(!up.move %in% TRUE)%>%
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

# I released 103+105 fish, so everyone is here!

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

cjs_table_ham$release <-  fct_recode(cjs_table_ham$release, 
                                     '1'="ham.us",'0'="ham.ds")

cjs.m1 <- crm(cjs_table_ham, groups="release")
cjs.m1 <- cjs.hessian(cjs.m1)

plogis(cjs.m1$results$beta$Phi)

# getting more complicated
ham.proc <- process.data(cjs_table_ham, group="release") # group variable has to be in the data

# Make design data (from processed data)
ham.ddl <- make.design.data(ham.proc)

# formulas for params
Phi.dot <- list(formula=~1)  # ~1 is always a constant (or single estimate)
Phi.rl<- list(formula=~release) # This formula will have an intercept (for females) and an estimate for the difference between females and males
p.rl <- list(formula=~release) # Be careful of case-sensitive names. Use the exact group column that was in data

test.m <- crm(ham.proc, ham.ddl,
    model.parameters = list(Phi = Phi.rl, 
                            p = p.rl),
    accumulate = FALSE)# works!

fit.models=function()
   {
       Phi.rl=list(formula=~release)
       Phi.time=list(formula=~time)
       p.time=list(formula=~time)
       p.rl=list(formula=~release)
       p.dot=list(formula=~1)
       cml=create.model.list(c("Phi","p"))
       results=crm.wrapper(cml,data=ham.proc, ddl=ham.ddl,
                                                   external=FALSE,accumulate=FALSE)
       return(results)
}
ham.models=fit.models()
ham.models

ham.models[[3]]$results
plogis(ham.models[[3]]$results$beta$Phi)
plogis(ham.models[[3]]$results$beta$p)
# Try with RMark ####
library(RMark)


test <- process.data(data = cjs_table_ham,
             model = "CJS", groups="release")

ddl <- make.design.data(test)

ham.mark1 <- mark(test, ddl)

# more complicated model ####
phi.release <- list(formula=~release)
p.release <- list(formula=~release)

phi.time <- list(formula=~time)
p.time <- list(formula =~time)
p<- list(formual=~1)


release.model <- mark(test, ddl,
                      model.parameters=list(Phi=phi.release))

estimates <- plogis(release.model$results$beta$estimate)   
lcl <- plogis(release.model$results$beta$lcl)
ucl <- plogis(release.model$results$beta$ucl)
params <- c("Phi:(Intercept)","Phi:release1 ","p:(Intercept)")

results.df <- data.frame(estimates, lcl, ucl, params)

results.df %>% 
  filter(!params %in% p:(Intercept))%>%
  ggplot(., aes(x=params, y=estimates))+
  geom_point()+
  geom_errorbar(aes(x=params, ymin=lcl, ymax=ucl))
