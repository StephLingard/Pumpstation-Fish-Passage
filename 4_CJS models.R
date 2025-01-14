# fitting CJS survival models
# S Lingard
# Created: December 18, 2024
library(marked)
library(RMark)
library(tidyverse)
library(here)

cjs_table_ham <- read.csv(here("cleaned data","cjs table all dets.csv"))%>%select(ch,release)
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
all_dat <- process.data(data = cjs_table_ham,
                     model = "CJS", groups="release")
ddl_all <- make.design.data(all_dat)
ham.mark1 <- mark(all_dat, ddl_all)

# more complicated model ####
phi.release <- list(formula=~release)
p.release <- list(formula=~release)

phi.time <- list(formula=~time)
p.time <- list(formula =~time)
p<- list(formual=~1)

release.model <- mark(all_dat, ddl_all,
                      model.parameters=list(Phi=phi.release))

estimates <- plogis(release.model$results$beta$estimate)   
lcl <- plogis(release.model$results$beta$lcl)
ucl <- plogis(release.model$results$beta$ucl)
params <- c("Phi:(Intercept)","Phi:release1 ","p:(Intercept)")
params_plot <- c("Control","Archimedes","p")

results.df <- data.frame(estimates, lcl, ucl, params, params_plot)

prelim <- results.df %>% 
  filter(!params %in% "p:(Intercept)")%>%
  ggplot(., aes(x=params_plot, y=estimates))+
  geom_point()+
  geom_errorbar(aes(x=params_plot, ymin=lcl, ymax=ucl))+
  theme_classic()+
  labs(x="Treatment", y="Survival (Phi)")+
  theme(axis.title = element_text(size=16),
        axis.text= element_text(size=14),
        panel.grid = element_blank())

ggsave(prelim, file=here("figures","survival estimates all.png"),
       width=6, height=6)

# next thing to do is remove all the single fraser detections and run the model again. That's for another day :)
cjs_table_ham2 <- read_csv(here("cleaned data","cjs table no single dets.csv"))
cjs_table_ham2$release <-  fct_recode(cjs_table_ham2$release, 
                                     '1'="ham.us",'0'="ham.ds")

ns_dat <- process.data(data = cjs_table_ham2,
                        model = "CJS", groups="release")
ddl_ns <- make.design.data(ns_dat)
ham.mark1 <- mark(ns_dat, ddl_ns)

# more complicated model ####
phi.release <- list(formula=~release)
p.release <- list(formula=~release)

phi.time <- list(formula=~time)
p.time <- list(formula =~time)
p<- list(formual=~1)

release.model.ns <- mark(ns_dat, ddl_ns,
                      model.parameters=list(Phi=phi.release))

estimates_ns <- plogis(release.model.ns$results$beta$estimate)   
lcl_ns<- plogis(release.model.ns$results$beta$lcl)
ucl_ns <- plogis(release.model.ns$results$beta$ucl)
params_ns <- c("Phi:(Intercept)","Phi:release1 ","p:(Intercept)")
params_plot_ns <- c("Control","Archimedes","p")

results_df_ns <- data.frame(estimates_ns, lcl_ns, ucl_ns, params_ns, params_plot_ns)

prelim_ns <- results_df_ns %>% 
  filter(!params_ns %in% "p:(Intercept)")%>%
  ggplot(., aes(x=params_plot_ns, y=estimates_ns))+
  geom_point()+
  geom_errorbar(aes(x=params_plot_ns, ymin=lcl_ns, ymax=ucl_ns))+
  theme_classic()+
  labs(x="Treatment", y="Survival (Phi)")+
  theme(axis.title = element_text(size=14),
        axis.text= element_text(size=12),
        panel.grid = element_blank())

ggsave(prelim_ns, file=here("figures","survival estimates no single dets.png"),
       width=8, height=7)