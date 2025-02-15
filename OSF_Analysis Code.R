# ----------------------------------------------------------------------- #
#
# STUDY: Parental Education & Accelerated Aging: A Lifescourse Perspective
# MANUSCRIPT AUTHORS: Korous, KM, Surachman, A, Rogers, CR, & Cuevas, AG
# SYNTAX AUTHOR: Kevin M. Korous, PhD
# DATE: 11/30/2022
# 
# ----------------------------------------------------------------------- #
#
# REVIEWER RECOMMENDATIONS: 
#
## Minimally adjusted model - sex and race/ethnicity only
### Is including income, BMI, & smoking overadjusting?
#
## Consider childhood health indicators as confounders
#
# ----------------------------------------------------------------------- #

### Packages Needed ----
library(tidyverse)
library(survey)
library(srvyr)
library(broom)
library(emmeans)
library(jtools)
library(interactions)

### Combine Data ----
# Data (Request via Health and Retirement Study)
hrs <- haven::read_sas("randhrs1992_2018v2.sas7bdat")
epidat <- haven::read_sas("epiclocka_r.sas7bdat")
wghts <- haven::read_sas("trk2018tr_r.sas7bdat", 
                         col_select = c("HHID", "PN", "PLBWGTR", "VBS16VALID", "VBSI16WGTRA"))
hrs16 <- asciiSetupReader::read_ascii_setup(data="H16LB_R.da",
                                            setup_file = "H16LB_R.sas",
                                            use_value_labels = F,
                                            use_clean_names = F,
                                            coerce_numeric = F)

## RandHRS Data
# Select study variables from Wave 13 (2016) 
edaa <- hrs %>% select(HHIDPN, RAHHIDPN, R13WTRESP, R13WTHH, RAESTRAT, RAEHSAMP, # IDs & Sampling Weights
                       RACOHBYR, RAGENDER, RARACEM, RAHISPAN, R13AGEY_E, # Demographic information
                       RAEDYRS, RAMEDUC, RAFEDUC, R13MSTAT, H13ITOT, H13POVHHI, R13LBRF, # SES Indicators
                       R13SMOKEV, R13SMOKEN, R13DRINK, R13DRINKN, R13BMI) # Lifstyle factors

## Add tracker data to epigenetic clock data
epidat <- epidat %>% mutate(RAHHIDPN = paste(HHID,PN,sep = "")) %>%
                             left_join(.,wghts, by=c('HHID', 'PN')) %>%  
                             select(RAHHIDPN,everything(),-c(HHID,PN))

## Merge data files 
edaa <- edaa %>% right_join(epidat, by = "RAHHIDPN") 

### Export RDS ----
saveRDS(edaa,"pared_epi_081722.RDS")

rm(epidat,hrs,hrs16,wghts)

### Load Data ----
edaa <- readRDS("pared_epi_081722.RDS")

### Mutate Variables ----
## Education
# Recode education to be consistent across parental and personal education
# Keep a continuous and categorical version
edaa <- edaa %>% mutate_at(vars(matches(c("RAEDYRS","RAMEDUC","RAFEDUC"))),
                           list(~as.numeric(.),
                                ~factor(case_when(.<=11~"No Degree",
                                                  .==12|.==13~"High School or GED",
                                                  .==14|.==15~"Associates",
                                                  .==16|.==17~"Bachelors or Higher"),
                                                  levels=c("No Degree","High School or GED","Associates","Bachelors or Higher")))) %>% 
                             select(everything(),-RAEDYRS_as.numeric,-RAMEDUC_as.numeric,-RAFEDUC_as.numeric)

# Create parental education (highest, or whichever is reported)
edaa <- edaa %>% mutate(par_edu_yr=pmax(RAMEDUC,RAFEDUC,na.rm=T))
# Make categorical version
edaa <- edaa %>% mutate(par_edu_cat=
                          factor(
                            case_when(par_edu_yr<=11~"No Degree",
                                      par_edu_yr==12|par_edu_yr==13~"High School or GED",
                                      par_edu_yr==14|par_edu_yr==15~"Associates",
                                      par_edu_yr==16|par_edu_yr==17~"Bachelors or Higher"),
                            levels=c("No Degree","High School or GED","Associates","Bachelors or Higher")))

# Summary for years of education
edu_cont_summ <- data.frame(rbind(personal=c(print(psych::describe(edaa$RAEDYRS),digits=2)),
                            maternal=c(print(psych::describe(edaa$RAMEDUC),digits=2)),
                            paternal=c(print(psych::describe(edaa$RAFEDUC),digits=2)),
                            parental=c(print(psych::describe(edaa$par_edu_yr),digits=2))))
edu_cont_summ

# Table of educational attainment
table(edaa$RAEDYRS_factor)
table(edaa$RAMEDUC_factor)
table(edaa$RAFEDUC_factor)
table(edaa$par_edu_cat)
table(edaa$RAEDYRS_factor,edaa$RAMEDUC_factor,dnn=c('personal','maternal'))
table(edaa$RAEDYRS_factor,edaa$RAFEDUC_factor,dnn=c('personal','paternal'))
table(edaa$RAEDYRS_factor,edaa$par_edu_cat,dnn=c('personal','parental'))

# Recategorize parental education into two bins (doi:10.1016/j.socscimed.2010.11.028)
edaa <- edaa %>% mutate(RAMEDUC_bin=
                          case_when(RAMEDUC_factor=='No Degree'~'No Degree',
                                    is.na(RAMEDUC_factor)~NA_character_,
                                    T~'High School or More'),
                        RAFEDUC_bin=
                          case_when(RAFEDUC_factor=='No Degree'~'No Degree',
                                    is.na(RAMEDUC_factor)~NA_character_,
                                    T~'High School or More'),
                        par_edu_bin=
                          case_when(par_edu_cat=='No Degree'~'No Degree',
                                    is.na(RAMEDUC_factor)~NA_character_,
                                    T~'High School or More'))
                          
table(edaa$RAEDYRS_factor,edaa$RAMEDUC_bin,dnn=c('personal','maternal'))
table(edaa$RAEDYRS_factor,edaa$RAFEDUC_bin,dnn=c('personal','paternal'))
table(edaa$RAEDYRS_factor,edaa$par_edu_bin,dnn=c('personal','parental'))

# Recode other participant characteristics
edaa <- edaa %>% mutate(raceth=factor(case_when(
                        RARACEM==1&RAHISPAN==0~'NHW',
                        RARACEM==2&RAHISPAN==0~'NHB',
                        RARACEM==3&RAHISPAN==0~ 'NHOTH',
                        RAHISPAN==1~'Hispanic'),levels=c('NHW','NHB','Hispanic','NHOTH')),
                        sex=factor(case_when(
                        RAGENDER==1~'Male',
                        RAGENDER==2~'Female'),levels=c('Male','Female')),
                        smokestatus=factor(case_when(
                        R13SMOKEV==0&R13SMOKEN==0~'Never',
                        R13SMOKEV==1&R13SMOKEN==0~'Past',
                        R13SMOKEV==1&R13SMOKEN==1~'Current'),levels=c('Never','Past','Current')))

### Filter Data ----
# No missing data for key variables
edaa <- edaa %>% filter(!is.na(VBSI16WGTRA))

### Create Survey Object ----
svy.edaa <- edaa %>% as_survey_design(ids=RAEHSAMP,
                                      strata=RAESTRAT,
                                      weights=VBSI16WGTRA,nest=T)

### Calculate Age Accelerating Variables ----
svy.edaa$variables$acchorv<-resid(svyglm(HORVATH_DNAMAGE~R13AGEY_E,design=svy.edaa))
svy.edaa$variables$acchannum<-resid(svyglm(HANNUM_DNAMAGE~R13AGEY_E,design=svy.edaa))
svy.edaa$variables$acclevine<-resid(svyglm(LEVINE_DNAMAGE~R13AGEY_E,design=svy.edaa))
svy.edaa$variables$acchorvsk<-resid(svyglm(HORVATHSKIN_DNAMAGE~R13AGEY_E,design=svy.edaa))
svy.edaa$variables$acclin<-resid(svyglm(LIN_DNAMAGE~R13AGEY_E,design=svy.edaa))
svy.edaa$variables$accweid<-resid(svyglm(WEIDNER_DNAMAGE~R13AGEY_E,design=svy.edaa))
svy.edaa$variables$accvidalb<-resid(svyglm(VIDALBRALO_DNAMAGE~R13AGEY_E,design=svy.edaa))
svy.edaa$variables$accyang<-resid(svyglm(YANG_DNAMAGE~R13AGEY_E,design=svy.edaa))
svy.edaa$variables$acczhang<-resid(svyglm(ZHANG_DNAMAGE~R13AGEY_E,design=svy.edaa))
svy.edaa$variables$accbock<-resid(svyglm(BOCKLANDT_DNAMAGE~R13AGEY_E,design=svy.edaa))
svy.edaa$variables$accgarag<-resid(svyglm(GARAGNANI_DNAMAGE~R13AGEY_E,design=svy.edaa))
svy.edaa$variables$accdnam<-resid(svyglm(DNAMGRIMAGE~R13AGEY_E,design=svy.edaa))
svy.edaa$variables$accmpoa<-resid(svyglm(MPOA~R13AGEY_E,design=svy.edaa))

### Regression Models (Parental Years of Education) ----
## Center years of education ----
svy.edaa <- gscale(data=svy.edaa,vars=c('RAEDYRS','RAMEDUC','RAFEDUC','par_edu_yr'),center.only=T,binary.inputs='center')
svy.edaa$variables <- svy.edaa$variables %>% rename(edu.c=RAEDYRS,
                                                    pedu.c=par_edu_yr,
                                                    fedu.c=RAFEDUC,
                                                    medu.c=RAMEDUC)

## HORVATH_DNAMAGE ----
# Unadjusted
edu.yr.par <- svyglm(acchorv~edu.c*pedu.c,design=svy.edaa)
summary(edu.yr.par)
edu.yr.par.tab <- tidy(edu.yr.par) %>% mutate(LL=sprintf(confint(edu.yr.par)[,1],fmt='%#.3f'),
                                              UL=sprintf(confint(edu.yr.par)[,2],fmt='%#.3f')) %>%
                                              unite(col=CI,LL:UL, sep = ", ", remove=F)

# Adjusted
adj.edu.yr.par <- svyglm(acchorv~edu.c*pedu.c +
                                 raceth + 
                                 sex +
                                 scale(H13ITOT) + 
                                 smokestatus +
                                 R13BMI,design=svy.edaa)
summary(adj.edu.yr.par)
adj.edu.yr.par.tab <- tidy(adj.edu.yr.par) %>% mutate(LL=sprintf(confint(adj.edu.yr.par)[,1],fmt='%#.3f'),
                                                      UL=sprintf(confint(adj.edu.yr.par)[,2],fmt='%#.3f')) %>%
                                                      unite(col=CI,LL:UL, sep = ", ", remove=F)

names(adj.edu.yr.par.tab) <- gsub("^(.*?)","adj_\\2", names(adj.edu.yr.par.tab))

# Combine results into one table
edu.yr.par.HORVATH_DNAMAGE <- merge(edu.yr.par.tab,adj.edu.yr.par.tab,by.x='term',by.y='adj_term',all.y=T,sort=F) 
write.csv(edu.yr.par.HORVATH_DNAMAGE,'edu.yr.par.HORVATH_DNAMAGE.csv')

# Plot coefficients
horvath1.coef <- bind_rows(undj=edu.yr.par.tab,adj=rename_with(adj.edu.yr.par.tab,cols=everything(),~str_remove(names(adj.edu.yr.par.tab),'adj_')),.id='model') %>% 
  filter(grepl('edu.c',term)) %>% 
  ggplot(aes(x=estimate, y=factor(term,levels=c('edu.c:pedu.c','edu.c','pedu.c'))))+ 
  geom_point(aes(shape=model),size=3,position = position_dodge(.75)) +
  scale_shape_manual(values=c(15, 16),labels=c('Adjusted Model','Unadjusted Model'))+
  geom_vline(xintercept=0,color='black',lty=2)+
  geom_errorbar(aes(xmin=as.numeric(LL),xmax=as.numeric(UL),group=model),width=.1,position = position_dodge(.75)) +
  scale_y_discrete(labels=c('Interactive Effect','Personal','Parental')) +
  scale_x_continuous(labels = function(x) format(x, nsmall = 3),limits = c(-0.60,0.60))+
  labs(x="Slope",y='Years of Education',title='Horvath 1') +
  theme_apa(legend.pos = 'top') + theme(plot.title = element_text(hjust = 0.5))
horvath1.coef

# Plot significant interaction
HORVATH1.simslopes <- tidy(interactions::sim_slopes(adj.edu.yr.par,pred=pedu.c,
                                                    modx=edu.c,confint=T,cond.int=T,
                                                    modx.values='terciles'))

HORVATH1.plot <- HORVATH1.simslopes %>% 
  ggplot(aes(x=estimate,y=term)) +
  geom_vline(xintercept=0,color='black',lty=2)+
  geom_point(aes(shape=term),size=3) +
  geom_errorbar(aes(xmin=conf.low,xmax=conf.high),width=.1)+
  scale_shape_manual(values=c(17, 16, 15))+
  scale_y_discrete(labels=c('Lower','Medium','Higher'))+
  scale_x_continuous(labels = function(x) format(x, nsmall = 3))+
  labs(x="Slope for Parental Years of Education",y='Terciles for\nPersonal Years of Education',title='Horvath 1')+
  theme_apa(legend.pos = 'none') + theme(plot.title = element_text(hjust = 0.5))

HORVATH1.plot

## Stratify by race/ethnicity
# NH Black
nhb.edu.yr.par <- svyglm(acchorv~edu.c*pedu.c +
                                 sex +
                                 scale(H13ITOT) + 
                                 smokestatus +
                                 R13BMI,design=subset(svy.edaa,raceth=="NHB"))
summary(nhb.edu.yr.par)
nhb.edu.yr.par.tab <- tidy(nhb.edu.yr.par) %>% mutate(LL=sprintf(confint(nhb.edu.yr.par)[,1],fmt='%#.3f'),
                                                      UL=sprintf(confint(nhb.edu.yr.par)[,2],fmt='%#.3f')) %>%
                                                      unite(col=CI,LL:UL, sep = ", ", remove=F)
                                                      
# Hispanic
h.edu.yr.par <- svyglm(acchorv~edu.c*pedu.c +
                               sex +
                               scale(H13ITOT) + 
                               smokestatus +
                               R13BMI,design=subset(svy.edaa,raceth=="Hispanic"))
summary(h.edu.yr.par)
h.edu.yr.par.tab <- tidy(h.edu.yr.par) %>% mutate(LL=sprintf(confint(h.edu.yr.par)[,1],fmt='%#.3f'),
                                                  UL=sprintf(confint(h.edu.yr.par)[,2],fmt='%#.3f')) %>%
                                                  unite(col=CI,LL:UL, sep = ", ", remove=F)
                                                  
#NH White
nhw.edu.yr.par <- svyglm(acchorv~edu.c*pedu.c +
                                 sex +
                                 scale(H13ITOT) + 
                                 smokestatus +
                                 R13BMI,design=subset(svy.edaa,raceth=="NHW"))
summary(nhw.edu.yr.par)
nhw.edu.yr.par.tab <- tidy(nhw.edu.yr.par) %>% mutate(LL=sprintf(confint(nhw.edu.yr.par)[,1],fmt='%#.3f'),
                                                      UL=sprintf(confint(nhw.edu.yr.par)[,2],fmt='%#.3f')) %>%
                                                      unite(col=CI,LL:UL, sep = ", ", remove=F)
                                                      
# Stack Columns 
re.edu.yr.par.HORVATH_DNAMAGE <- bind_rows("NH Black"=nhb.edu.yr.par.tab,"NH Hispanic"=h.edu.yr.par.tab,"NH White"=nhw.edu.yr.par.tab,.id='Subgroup')
write.csv(re.edu.yr.par.HORVATH_DNAMAGE,file='re.edu.yr.par.HORVATH_DNAMAGE.csv')

## HANNUM_DNAMAGE ----
# Unadjusted
edu.yr.par <- svyglm(acchannum~edu.c*pedu.c,design=svy.edaa)
summary(edu.yr.par)
edu.yr.par.tab <- tidy(edu.yr.par) %>% mutate(LL=sprintf(confint(edu.yr.par)[,1],fmt='%#.3f'),
                                              UL=sprintf(confint(edu.yr.par)[,2],fmt='%#.3f')) %>%
                                              unite(col=CI,LL:UL, sep = ", ", remove=F)
                                       

# Adjusted
adj.edu.yr.par <- svyglm(acchannum~edu.c*pedu.c +
                                 raceth + 
                                 sex +
                                 scale(H13ITOT) + 
                                 smokestatus +
                                 R13BMI,design=svy.edaa)
summary(adj.edu.yr.par)
adj.edu.yr.par.tab <- tidy(adj.edu.yr.par) %>% mutate(LL=sprintf(confint(adj.edu.yr.par)[,1],fmt='%#.3f'),
                                               UL=sprintf(confint(adj.edu.yr.par)[,2],fmt='%#.3f')) %>%
                                               unite(col=CI,LL:UL, sep = ", ", remove=F)
                                               
names(adj.edu.yr.par.tab) <- gsub("^(.*?)","adj_\\2", names(adj.edu.yr.par.tab))

# Combine results into one table
edu.yr.par.HANNUM_DNAMAGE <- merge(edu.yr.par.tab,adj.edu.yr.par.tab,by.x='term',by.y='adj_term',all.y=T,sort=F) 
write.csv(edu.yr.par.HANNUM_DNAMAGE,'edu.yr.par.HANNUM_DNAMAGE.csv')

# Plot coefficients
hannum.coef <- bind_rows(undj=edu.yr.par.tab,adj=rename_with(adj.edu.yr.par.tab,cols=everything(),~str_remove(names(adj.edu.yr.par.tab),'adj_')),.id='model') %>% 
  filter(grepl('edu.c',term)) %>% 
  ggplot(aes(x=estimate, y=factor(term,levels=c('edu.c:pedu.c','edu.c','pedu.c'))))+ 
  geom_point(aes(shape=model),size=3,position = position_dodge(.75)) +
  scale_shape_manual(values=c(15, 16),labels=c('Adjusted Model','Unadjusted Model'))+
  geom_vline(xintercept=0,color='black',lty=2)+
  geom_errorbar(aes(xmin=as.numeric(LL),xmax=as.numeric(UL),group=model),width=.1,position = position_dodge(.75)) +
  scale_y_discrete(labels=c('Interactive Effect','Personal','Parental')) +
  scale_x_continuous(labels = function(x) format(x, nsmall = 3),limits = c(-0.60,0.60))+
  labs(x="Slope",y='Years of Education',title='Hannum') +
  theme_apa(legend.pos = 'top') + theme(plot.title = element_text(hjust = 0.5))
hannum.coef

## Stratify by race/ethnicity
# NH Black
nhb.edu.yr.par <- svyglm(acchannum~edu.c*pedu.c +
                                 sex +
                                 scale(H13ITOT) + 
                                 smokestatus +
                                 R13BMI,design=subset(svy.edaa,raceth=="NHB"))
summary(nhb.edu.yr.par)
nhb.edu.yr.par.tab <- tidy(nhb.edu.yr.par) %>% mutate(LL=sprintf(confint(nhb.edu.yr.par)[,1],fmt='%#.3f'),
                                                      UL=sprintf(confint(nhb.edu.yr.par)[,2],fmt='%#.3f')) %>%
                                                      unite(col=CI,LL:UL, sep = ", ", remove=F)
                                                      
# Hispanic
h.edu.yr.par <- svyglm(acchannum~edu.c*pedu.c +
                               sex +
                               scale(H13ITOT) + 
                               smokestatus +
                               R13BMI,design=subset(svy.edaa,raceth=="Hispanic"))
summary(h.edu.yr.par)
h.edu.yr.par.tab <- tidy(h.edu.yr.par) %>% mutate(LL=sprintf(confint(h.edu.yr.par)[,1],fmt='%#.3f'),
                                                  UL=sprintf(confint(h.edu.yr.par)[,2],fmt='%#.3f')) %>%
                                                  unite(col=CI,LL:UL, sep = ", ", remove=F)
                                                  
#NH White
nhw.edu.yr.par <- svyglm(acchannum~edu.c*pedu.c +
                                 sex +
                                 scale(H13ITOT) + 
                                 smokestatus +
                                 R13BMI,design=subset(svy.edaa,raceth=="NHW"))
summary(nhw.edu.yr.par)
nhw.edu.yr.par.tab <- tidy(nhw.edu.yr.par) %>% mutate(LL=sprintf(confint(nhw.edu.yr.par)[,1],fmt='%#.3f'),
                                                      UL=sprintf(confint(nhw.edu.yr.par)[,2],fmt='%#.3f')) %>%
                                                      unite(col=CI,LL:UL, sep = ", ", remove=F)
                                                      

# Stack Columns 
re.edu.yr.par.HANNUM_DNAMAGE<- bind_rows("NH Black"=nhb.edu.yr.par.tab,"NH Hispanic"=h.edu.yr.par.tab,"NH White"=nhw.edu.yr.par.tab,.id='Subgroup')
write.csv(re.edu.yr.par.HANNUM_DNAMAGE,file='re.edu.yr.par.HANNUM_DNAMAGE.csv')

## LEVINE_DNAMAGE ----
# Unadjusted
edu.yr.par <- svyglm(acclevine~edu.c*pedu.c,design=svy.edaa)
summary(edu.yr.par)
edu.yr.par.tab <- tidy(edu.yr.par) %>% mutate(LL=sprintf(confint(edu.yr.par)[,1],fmt='%#.3f'),
                                              UL=sprintf(confint(edu.yr.par)[,2],fmt='%#.3f')) %>%
                                       unite(col=CI,LL:UL, sep = ", ", remove=F)
                                       

# Adjusted
adj.edu.yr.par <- svyglm(acclevine~edu.c*pedu.c +
                                 raceth + 
                                 sex +
                                 scale(H13ITOT) + 
                                 smokestatus +
                                 R13BMI,design=svy.edaa)
summary(adj.edu.yr.par)
adj.edu.yr.par.tab <- tidy(adj.edu.yr.par) %>% mutate(LL=sprintf(confint(adj.edu.yr.par)[,1],fmt='%#.3f'),
                                               UL=sprintf(confint(adj.edu.yr.par)[,2],fmt='%#.3f')) %>%
                                               unite(col=CI,LL:UL, sep = ", ", remove=F)
                                               
names(adj.edu.yr.par.tab) <- gsub("^(.*?)","adj_\\2", names(adj.edu.yr.par.tab))

# Combine results into one table
edu.yr.par.LEVINE_DNAMAGE <- merge(edu.yr.par.tab,adj.edu.yr.par.tab,by.x='term',by.y='adj_term',all.y=T,sort=F) 
write.csv(edu.yr.par.LEVINE_DNAMAGE,'edu.yr.par.LEVINE_DNAMAGE.csv')

# Plot coefficients
levine.coef <- bind_rows(undj=edu.yr.par.tab,adj=rename_with(adj.edu.yr.par.tab,cols=everything(),~str_remove(names(adj.edu.yr.par.tab),'adj_')),.id='model') %>% 
  filter(grepl('edu.c',term)) %>% 
  ggplot(aes(x=estimate, y=factor(term,levels=c('edu.c:pedu.c','edu.c','pedu.c'))))+ 
  geom_point(aes(shape=model),size=3,position = position_dodge(.75)) +
  scale_shape_manual(values=c(15, 16),labels=c('Adjusted Model','Unadjusted Model'))+
  geom_vline(xintercept=0,color='black',lty=2)+
  geom_errorbar(aes(xmin=as.numeric(LL),xmax=as.numeric(UL),group=model),width=.1,position = position_dodge(.75)) +
  scale_y_discrete(labels=c('Interactive Effect','Personal','Parental')) +
  scale_x_continuous(labels = function(x) format(x, nsmall = 3),limits = c(-0.60,0.60))+
  labs(x="Slope",y='Years of Education',title='Levine-PhenoAge') +
  theme_apa(legend.pos = 'top') + theme(plot.title = element_text(hjust = 0.5))
levine.coef

## Stratify by race/ethnicity
# NH Black
nhb.edu.yr.par <- svyglm(acclevine~edu.c*pedu.c +
                                 sex +
                                 scale(H13ITOT) + 
                                 smokestatus +
                                 R13BMI,design=subset(svy.edaa,raceth=="NHB"))
summary(nhb.edu.yr.par)
nhb.edu.yr.par.tab <- tidy(nhb.edu.yr.par) %>% mutate(LL=sprintf(confint(nhb.edu.yr.par)[,1],fmt='%#.3f'),
                                                      UL=sprintf(confint(nhb.edu.yr.par)[,2],fmt='%#.3f')) %>%
                                                      unite(col=CI,LL:UL, sep = ", ", remove=F)
                                                      
# Hispanic
h.edu.yr.par <- svyglm(acclevine~edu.c*pedu.c +
                               sex +
                               scale(H13ITOT) + 
                               smokestatus +
                               R13BMI,design=subset(svy.edaa,raceth=="Hispanic"))
summary(h.edu.yr.par)
h.edu.yr.par.tab <- tidy(h.edu.yr.par) %>% mutate(LL=sprintf(confint(h.edu.yr.par)[,1],fmt='%#.3f'),
                                                  UL=sprintf(confint(h.edu.yr.par)[,2],fmt='%#.3f')) %>%
                                                  unite(col=CI,LL:UL, sep = ", ", remove=F)
                                                  
#NH White
nhw.edu.yr.par <- svyglm(acclevine~edu.c*pedu.c +
                                 sex +
                                 scale(H13ITOT) + 
                                 smokestatus +
                                 R13BMI,design=subset(svy.edaa,raceth=="NHW"))
summary(nhw.edu.yr.par)
nhw.edu.yr.par.tab <- tidy(nhw.edu.yr.par) %>% mutate(LL=sprintf(confint(nhw.edu.yr.par)[,1],fmt='%#.3f'),
                                                      UL=sprintf(confint(nhw.edu.yr.par)[,2],fmt='%#.3f')) %>%
                                                      unite(col=CI,LL:UL, sep = ", ", remove=F)
                                                      

# Stack Columns 
re.edu.yr.par.LEVINE_DNAMAGE<- bind_rows("NH Black"=nhb.edu.yr.par.tab,"NH Hispanic"=h.edu.yr.par.tab,"NH White"=nhw.edu.yr.par.tab,.id='Subgroup')
write.csv(re.edu.yr.par.LEVINE_DNAMAGE,file='re.edu.yr.par.LEVINE_DNAMAGE.csv')

## HORVATHSKIN_DNAMAGE ----
# Unadjusted
edu.yr.par <- svyglm(acchorvsk~edu.c*pedu.c,design=svy.edaa)
summary(edu.yr.par)
edu.yr.par.tab <- tidy(edu.yr.par) %>% mutate(LL=sprintf(confint(edu.yr.par)[,1],fmt='%#.3f'),
                                              UL=sprintf(confint(edu.yr.par)[,2],fmt='%#.3f')) %>%
                                       unite(col=CI,LL:UL, sep = ", ", remove=F)
                                       

# Adjusted
adj.edu.yr.par <- svyglm(acchorvsk~edu.c*pedu.c +
                                 raceth + 
                                 sex +
                                 scale(H13ITOT) + 
                                 smokestatus +
                                 R13BMI,design=svy.edaa)
summary(adj.edu.yr.par)
adj.edu.yr.par.tab <- tidy(adj.edu.yr.par) %>% mutate(LL=sprintf(confint(adj.edu.yr.par)[,1],fmt='%#.3f'),
                                               UL=sprintf(confint(adj.edu.yr.par)[,2],fmt='%#.3f')) %>%
                                               unite(col=CI,LL:UL, sep = ", ", remove=F)
                                               
names(adj.edu.yr.par.tab) <- gsub("^(.*?)","adj_\\2", names(adj.edu.yr.par.tab))

# Combine results into one table
edu.yr.par.HORVATHSKIN_DNAMAGE<- merge(edu.yr.par.tab,adj.edu.yr.par.tab,by.x='term',by.y='adj_term',all.y=T,sort=F) 
write.csv(edu.yr.par.HORVATHSKIN_DNAMAGE,'edu.yr.par.HORVATHSKIN_DNAMAGE.csv')

# Plot coefficients
horvath2.coef <- bind_rows(undj=edu.yr.par.tab,adj=rename_with(adj.edu.yr.par.tab,cols=everything(),~str_remove(names(adj.edu.yr.par.tab),'adj_')),.id='model') %>% 
  filter(grepl('edu.c',term)) %>% 
  ggplot(aes(x=estimate, y=factor(term,levels=c('edu.c:pedu.c','edu.c','pedu.c'))))+ 
  geom_point(aes(shape=model),size=3,position = position_dodge(.75)) +
  scale_shape_manual(values=c(15, 16),labels=c('Adjusted Model','Unadjusted Model'))+
  geom_vline(xintercept=0,color='black',lty=2)+
  geom_errorbar(aes(xmin=as.numeric(LL),xmax=as.numeric(UL),group=model),width=.1,position = position_dodge(.75)) +
  scale_y_discrete(labels=c('Interactive Effect','Personal','Parental')) +
  scale_x_continuous(labels = function(x) format(x, nsmall = 3),limits = c(-0.60,0.60))+
  labs(x="Slope",y='Years of Education',title='Horvath 2') +
  theme_apa(legend.pos = 'top') + theme(plot.title = element_text(hjust = 0.5))
horvath2.coef

## Stratify by race/ethnicity
# NH Black
nhb.edu.yr.par <- svyglm(acchorvsk~edu.c*pedu.c +
                                 sex +
                                 scale(H13ITOT) + 
                                 smokestatus +
                                 R13BMI,design=subset(svy.edaa,raceth=="NHB"))
summary(nhb.edu.yr.par)
nhb.edu.yr.par.tab <- tidy(nhb.edu.yr.par) %>% mutate(LL=sprintf(confint(nhb.edu.yr.par)[,1],fmt='%#.3f'),
                                                      UL=sprintf(confint(nhb.edu.yr.par)[,2],fmt='%#.3f')) %>%
                                                      unite(col=CI,LL:UL, sep = ", ", remove=F)
                                                      
# Hispanic
h.edu.yr.par <- svyglm(acchorvsk~edu.c*pedu.c +
                               sex +
                               scale(H13ITOT) + 
                               smokestatus +
                               R13BMI,design=subset(svy.edaa,raceth=="Hispanic"))
summary(h.edu.yr.par)
h.edu.yr.par.tab <- tidy(h.edu.yr.par) %>% mutate(LL=sprintf(confint(h.edu.yr.par)[,1],fmt='%#.3f'),
                                                  UL=sprintf(confint(h.edu.yr.par)[,2],fmt='%#.3f')) %>%
                                                  unite(col=CI,LL:UL, sep = ", ", remove=F)
                                                  
#NH White
nhw.edu.yr.par <- svyglm(acchorvsk~edu.c*pedu.c +
                                 sex +
                                 scale(H13ITOT) + 
                                 smokestatus +
                                 R13BMI,design=subset(svy.edaa,raceth=="NHW"))
summary(nhw.edu.yr.par)
nhw.edu.yr.par.tab <- tidy(nhw.edu.yr.par) %>% mutate(LL=sprintf(confint(nhw.edu.yr.par)[,1],fmt='%#.3f'),
                                                      UL=sprintf(confint(nhw.edu.yr.par)[,2],fmt='%#.3f')) %>%
                                                      unite(col=CI,LL:UL, sep = ", ", remove=F)
                                                      

# Stack Columns 
re.edu.yr.par.HORVATHSKIN_DNAMAGE<- bind_rows("NH Black"=nhb.edu.yr.par.tab,"NH Hispanic"=h.edu.yr.par.tab,"NH White"=nhw.edu.yr.par.tab,.id='Subgroup')
write.csv(re.edu.yr.par.HORVATHSKIN_DNAMAGE,file='re.edu.yr.par.HORVATHSKIN_DNAMAGE.csv')

## LIN_DNAMAGE ----
# Unadjusted
edu.yr.par <- svyglm(acclin~edu.c*pedu.c,design=svy.edaa)
summary(edu.yr.par)
edu.yr.par.tab <- tidy(edu.yr.par) %>% mutate(LL=sprintf(confint(edu.yr.par)[,1],fmt='%#.3f'),
                                              UL=sprintf(confint(edu.yr.par)[,2],fmt='%#.3f')) %>%
                                       unite(col=CI,LL:UL, sep = ", ", remove=F)
                                       

# Adjusted
adj.edu.yr.par <- svyglm(acclin~edu.c*pedu.c +
                                 raceth + 
                                 sex +
                                 scale(H13ITOT) + 
                                 smokestatus +
                                 R13BMI,design=svy.edaa)
summary(adj.edu.yr.par)
adj.edu.yr.par.tab <- tidy(adj.edu.yr.par) %>% mutate(LL=sprintf(confint(adj.edu.yr.par)[,1],fmt='%#.3f'),
                                               UL=sprintf(confint(adj.edu.yr.par)[,2],fmt='%#.3f')) %>%
                                               unite(col=CI,LL:UL, sep = ", ", remove=F)
                                               
names(adj.edu.yr.par.tab) <- gsub("^(.*?)","adj_\\2", names(adj.edu.yr.par.tab))

# Combine results into one table
edu.yr.par.LIN_DNAMAGE <- merge(edu.yr.par.tab,adj.edu.yr.par.tab,by.x='term',by.y='adj_term',all.y=T,sort=F) 
write.csv(edu.yr.par.LIN_DNAMAGE,'edu.yr.par.LIN_DNAMAGE.csv')

# Plot coefficients
lin.coef <- bind_rows(undj=edu.yr.par.tab,adj=rename_with(adj.edu.yr.par.tab,cols=everything(),~str_remove(names(adj.edu.yr.par.tab),'adj_')),.id='model') %>% 
  filter(grepl('edu.c',term)) %>% 
  ggplot(aes(x=estimate, y=factor(term,levels=c('edu.c:pedu.c','edu.c','pedu.c'))))+ 
  geom_point(aes(shape=model),size=3,position = position_dodge(.75)) +
  scale_shape_manual(values=c(15, 16),labels=c('Adjusted Model','Unadjusted Model'))+
  geom_vline(xintercept=0,color='black',lty=2)+
  geom_errorbar(aes(xmin=as.numeric(LL),xmax=as.numeric(UL),group=model),width=.1,position = position_dodge(.75)) +
  scale_y_discrete(labels=c('Interactive Effect','Personal','Parental')) +
  scale_x_continuous(labels = function(x) format(x, nsmall = 3),limits = c(-0.60,0.60))+
  labs(x="Slope",y='Years of Education',title='Lin') +
  theme_apa(legend.pos = 'top') + theme(plot.title = element_text(hjust = 0.5))
lin.coef

## Stratify by race/ethnicity
# NH Black
nhb.edu.yr.par <- svyglm(acclin~edu.c*pedu.c +
                                 sex +
                                 scale(H13ITOT) + 
                                 smokestatus +
                                 R13BMI,design=subset(svy.edaa,raceth=="NHB"))
summary(nhb.edu.yr.par)
nhb.edu.yr.par.tab <- tidy(nhb.edu.yr.par) %>% mutate(LL=sprintf(confint(nhb.edu.yr.par)[,1],fmt='%#.3f'),
                                                      UL=sprintf(confint(nhb.edu.yr.par)[,2],fmt='%#.3f')) %>%
                                                      unite(col=CI,LL:UL, sep = ", ", remove=F)
                                                      
# Hispanic
h.edu.yr.par <- svyglm(acclin~edu.c*pedu.c +
                               sex +
                               scale(H13ITOT) + 
                               smokestatus +
                               R13BMI,design=subset(svy.edaa,raceth=="Hispanic"))
summary(h.edu.yr.par)
h.edu.yr.par.tab <- tidy(h.edu.yr.par) %>% mutate(LL=sprintf(confint(h.edu.yr.par)[,1],fmt='%#.3f'),
                                                  UL=sprintf(confint(h.edu.yr.par)[,2],fmt='%#.3f')) %>%
                                                  unite(col=CI,LL:UL, sep = ", ", remove=F)
                                                  
#NH White
nhw.edu.yr.par <- svyglm(acclin~edu.c*pedu.c +
                                 sex +
                                 scale(H13ITOT) + 
                                 smokestatus +
                                 R13BMI,design=subset(svy.edaa,raceth=="NHW"))
summary(nhw.edu.yr.par)
nhw.edu.yr.par.tab <- tidy(nhw.edu.yr.par) %>% mutate(LL=sprintf(confint(nhw.edu.yr.par)[,1],fmt='%#.3f'),
                                                      UL=sprintf(confint(nhw.edu.yr.par)[,2],fmt='%#.3f')) %>%
                                                      unite(col=CI,LL:UL, sep = ", ", remove=F)
                                                      

# Stack Columns 
re.edu.yr.par.LIN_DNAMAGE<- bind_rows("NH Black"=nhb.edu.yr.par.tab,"NH Hispanic"=h.edu.yr.par.tab,"NH White"=nhw.edu.yr.par.tab,.id='Subgroup')
write.csv(re.edu.yr.par.LIN_DNAMAGE,file='re.edu.yr.par.LIN_DNAMAGE.csv')

## WEIDNER_DNAMAGE ----
# Unadjusted
edu.yr.par <- svyglm(accweid~edu.c*pedu.c,design=svy.edaa)
summary(edu.yr.par)
edu.yr.par.tab <- tidy(edu.yr.par) %>% mutate(LL=sprintf(confint(edu.yr.par)[,1],fmt='%#.3f'),
                                              UL=sprintf(confint(edu.yr.par)[,2],fmt='%#.3f')) %>%
                                       unite(col=CI,LL:UL, sep = ", ", remove=F)
                                       

# Adjusted
adj.edu.yr.par <- svyglm(accweid~edu.c*pedu.c +
                                 raceth + 
                                 sex +
                                 scale(H13ITOT) + 
                                 smokestatus +
                                 R13BMI,design=svy.edaa)
summary(adj.edu.yr.par)
adj.edu.yr.par.tab <- tidy(adj.edu.yr.par) %>% mutate(LL=sprintf(confint(adj.edu.yr.par)[,1],fmt='%#.3f'),
                                               UL=sprintf(confint(adj.edu.yr.par)[,2],fmt='%#.3f')) %>%
                                               unite(col=CI,LL:UL, sep = ", ", remove=F)
                                               
names(adj.edu.yr.par.tab) <- gsub("^(.*?)","adj_\\2", names(adj.edu.yr.par.tab))

# Combine results into one table
edu.yr.par.WEIDNER_DNAMAGE <- merge(edu.yr.par.tab,adj.edu.yr.par.tab,by.x='term',by.y='adj_term',all.y=T,sort=F) 
write.csv(edu.yr.par.WEIDNER_DNAMAGE,'edu.yr.par.WEIDNER_DNAMAGE.csv')

# Plot coefficients
weidner.coef <- bind_rows(undj=edu.yr.par.tab,adj=rename_with(adj.edu.yr.par.tab,cols=everything(),~str_remove(names(adj.edu.yr.par.tab),'adj_')),.id='model') %>% 
  filter(grepl('edu.c',term)) %>% 
  ggplot(aes(x=estimate, y=factor(term,levels=c('edu.c:pedu.c','edu.c','pedu.c'))))+ 
  geom_point(aes(shape=model),size=3,position = position_dodge(.75)) +
  scale_shape_manual(values=c(15, 16),labels=c('Adjusted Model','Unadjusted Model'))+
  geom_vline(xintercept=0,color='black',lty=2)+
  geom_errorbar(aes(xmin=as.numeric(LL),xmax=as.numeric(UL),group=model),width=.1,position = position_dodge(.75)) +
  scale_y_discrete(labels=c('Interactive Effect','Personal','Parental')) +
  scale_x_continuous(labels = function(x) format(x, nsmall = 3),limits = c(-0.60,0.60))+
  labs(x="Slope",y='Years of Education',title='Weidner') +
  theme_apa(legend.pos = 'top') + theme(plot.title = element_text(hjust = 0.5))
weidner.coef

## Stratify by race/ethnicity
# NH Black
nhb.edu.yr.par <- svyglm(accweid~edu.c*pedu.c +
                                 sex +
                                 scale(H13ITOT) + 
                                 smokestatus +
                                 R13BMI,design=subset(svy.edaa,raceth=="NHB"))
summary(nhb.edu.yr.par)
nhb.edu.yr.par.tab <- tidy(nhb.edu.yr.par) %>% mutate(LL=sprintf(confint(nhb.edu.yr.par)[,1],fmt='%#.3f'),
                                                      UL=sprintf(confint(nhb.edu.yr.par)[,2],fmt='%#.3f')) %>%
                                                      unite(col=CI,LL:UL, sep = ", ", remove=F)
                                                      
# Hispanic
h.edu.yr.par <- svyglm(accweid~edu.c*pedu.c +
                               sex +
                               scale(H13ITOT) + 
                               smokestatus +
                               R13BMI,design=subset(svy.edaa,raceth=="Hispanic"))
summary(h.edu.yr.par)
h.edu.yr.par.tab <- tidy(h.edu.yr.par) %>% mutate(LL=sprintf(confint(h.edu.yr.par)[,1],fmt='%#.3f'),
                                                  UL=sprintf(confint(h.edu.yr.par)[,2],fmt='%#.3f')) %>%
                                                  unite(col=CI,LL:UL, sep = ", ", remove=F)
                                                  
#NH White
nhw.edu.yr.par <- svyglm(accweid~edu.c*pedu.c +
                                 sex +
                                 scale(H13ITOT) + 
                                 smokestatus +
                                 R13BMI,design=subset(svy.edaa,raceth=="NHW"))
summary(nhw.edu.yr.par)
nhw.edu.yr.par.tab <- tidy(nhw.edu.yr.par) %>% mutate(LL=sprintf(confint(nhw.edu.yr.par)[,1],fmt='%#.3f'),
                                                      UL=sprintf(confint(nhw.edu.yr.par)[,2],fmt='%#.3f')) %>%
                                                      unite(col=CI,LL:UL, sep = ", ", remove=F)
                                                      

# Stack Columns 
re.edu.yr.par.WEIDNER_DNAMAGE<- bind_rows("NH Black"=nhb.edu.yr.par.tab,"NH Hispanic"=h.edu.yr.par.tab,"NH White"=nhw.edu.yr.par.tab,.id='Subgroup')
write.csv(re.edu.yr.par.WEIDNER_DNAMAGE,file='re.edu.yr.par.WEIDNER_DNAMAGE.csv')

## VIDALBRALO_DNAMAGE ----
# Unadjusted
edu.yr.par <- svyglm(accvidalb~edu.c*pedu.c,design=svy.edaa)
summary(edu.yr.par)
edu.yr.par.tab <- tidy(edu.yr.par) %>% mutate(LL=sprintf(confint(edu.yr.par)[,1],fmt='%#.3f'),
                                              UL=sprintf(confint(edu.yr.par)[,2],fmt='%#.3f')) %>%
                                       unite(col=CI,LL:UL, sep = ", ", remove=F)
                                       

# Adjusted
adj.edu.yr.par <- svyglm(accvidalb~edu.c*pedu.c +
                                 raceth + 
                                 sex +
                                 scale(H13ITOT) + 
                                 smokestatus +
                                 R13BMI,design=svy.edaa)
summary(adj.edu.yr.par)
adj.edu.yr.par.tab <- tidy(adj.edu.yr.par) %>% mutate(LL=sprintf(confint(adj.edu.yr.par)[,1],fmt='%#.3f'),
                                               UL=sprintf(confint(adj.edu.yr.par)[,2],fmt='%#.3f')) %>%
                                               unite(col=CI,LL:UL, sep = ", ", remove=F)
                                               
names(adj.edu.yr.par.tab) <- gsub("^(.*?)","adj_\\2", names(adj.edu.yr.par.tab))

# Combine results into one table
edu.yr.par.VIDALBRALO_DNAMAGE <- merge(edu.yr.par.tab,adj.edu.yr.par.tab,by.x='term',by.y='adj_term',all.y=T,sort=F) 
write.csv(edu.yr.par.VIDALBRALO_DNAMAGE,'edu.yr.par.VIDALBRALO_DNAMAGE.csv')

# Plot coefficients
vidalbralo.coef <- bind_rows(undj=edu.yr.par.tab,adj=rename_with(adj.edu.yr.par.tab,cols=everything(),~str_remove(names(adj.edu.yr.par.tab),'adj_')),.id='model') %>% 
  filter(grepl('edu.c',term)) %>% 
  ggplot(aes(x=estimate, y=factor(term,levels=c('edu.c:pedu.c','edu.c','pedu.c'))))+ 
  geom_point(aes(shape=model),size=3,position = position_dodge(.75)) +
  scale_shape_manual(values=c(15, 16),labels=c('Adjusted Model','Unadjusted Model'))+
  geom_vline(xintercept=0,color='black',lty=2)+
  geom_errorbar(aes(xmin=as.numeric(LL),xmax=as.numeric(UL),group=model),width=.1,position = position_dodge(.75)) +
  scale_y_discrete(labels=c('Interactive Effect','Personal','Parental')) +
  scale_x_continuous(labels = function(x) format(x, nsmall = 3),limits = c(-0.60,0.60))+
  labs(x="Slope",y='Years of Education',title='VidalBralo') +
  theme_apa(legend.pos = 'top') + theme(plot.title = element_text(hjust = 0.5))
vidalbralo.coef

## Stratify by race/ethnicity
# NH Black
nhb.edu.yr.par <- svyglm(accvidalb~edu.c*pedu.c +
                                 sex +
                                 scale(H13ITOT) + 
                                 smokestatus +
                                 R13BMI,design=subset(svy.edaa,raceth=="NHB"))
summary(nhb.edu.yr.par)
nhb.edu.yr.par.tab <- tidy(nhb.edu.yr.par) %>% mutate(LL=sprintf(confint(nhb.edu.yr.par)[,1],fmt='%#.3f'),
                                                      UL=sprintf(confint(nhb.edu.yr.par)[,2],fmt='%#.3f')) %>%
                                                      unite(col=CI,LL:UL, sep = ", ", remove=F)
                                                      
# Hispanic
h.edu.yr.par <- svyglm(accvidalb~edu.c*pedu.c +
                               sex +
                               scale(H13ITOT) + 
                               smokestatus +
                               R13BMI,design=subset(svy.edaa,raceth=="Hispanic"))
summary(h.edu.yr.par)
h.edu.yr.par.tab <- tidy(h.edu.yr.par) %>% mutate(LL=sprintf(confint(h.edu.yr.par)[,1],fmt='%#.3f'),
                                                  UL=sprintf(confint(h.edu.yr.par)[,2],fmt='%#.3f')) %>%
                                                  unite(col=CI,LL:UL, sep = ", ", remove=F)
                                                  
#NH White
nhw.edu.yr.par <- svyglm(accvidalb~edu.c*pedu.c +
                                 sex +
                                 scale(H13ITOT) + 
                                 smokestatus +
                                 R13BMI,design=subset(svy.edaa,raceth=="NHW"))
summary(nhw.edu.yr.par)
nhw.edu.yr.par.tab <- tidy(nhw.edu.yr.par) %>% mutate(LL=sprintf(confint(nhw.edu.yr.par)[,1],fmt='%#.3f'),
                                                      UL=sprintf(confint(nhw.edu.yr.par)[,2],fmt='%#.3f')) %>%
                                                      unite(col=CI,LL:UL, sep = ", ", remove=F)
                                                      

# Stack Columns 
re.edu.yr.par.VIDALBRALO_DNAMAGE<- bind_rows("NH Black"=nhb.edu.yr.par.tab,"NH Hispanic"=h.edu.yr.par.tab,"NH White"=nhw.edu.yr.par.tab,.id='Subgroup')
write.csv(re.edu.yr.par.VIDALBRALO_DNAMAGE,file='re.edu.yr.par.VIDALBRALO_DNAMAGE.csv')

## YANG_DNAMAGE ----
# Unadjusted
edu.yr.par <- svyglm(accyang~edu.c*pedu.c,design=svy.edaa)
summary(edu.yr.par)
edu.yr.par.tab <- tidy(edu.yr.par) %>% mutate(LL=sprintf(confint(edu.yr.par)[,1],fmt='%#.3f'),
                                              UL=sprintf(confint(edu.yr.par)[,2],fmt='%#.3f')) %>%
                                       unite(col=CI,LL:UL, sep = ", ", remove=F)
                                       

# Adjusted
adj.edu.yr.par <- svyglm(accyang~edu.c*pedu.c +
                                 raceth + 
                                 sex +
                                 scale(H13ITOT) + 
                                 smokestatus +
                                 R13BMI,design=svy.edaa)
summary(adj.edu.yr.par)
adj.edu.yr.par.tab <- tidy(adj.edu.yr.par) %>% mutate(LL=sprintf(confint(adj.edu.yr.par)[,1],fmt='%#.3f'),
                                               UL=sprintf(confint(adj.edu.yr.par)[,2],fmt='%#.3f')) %>%
                                               unite(col=CI,LL:UL, sep = ", ", remove=F)
                                               
names(adj.edu.yr.par.tab) <- gsub("^(.*?)","adj_\\2", names(adj.edu.yr.par.tab))

# Combine results into one table
edu.yr.par.YANG_DNAMAGE <- merge(edu.yr.par.tab,adj.edu.yr.par.tab,by.x='term',by.y='adj_term',all.y=T,sort=F) 
write.csv(edu.yr.par.YANG_DNAMAGE,'edu.yr.par.YANG_DNAMAGE.csv')

# Plot coefficients
yang.coef <- bind_rows(undj=edu.yr.par.tab,adj=rename_with(adj.edu.yr.par.tab,cols=everything(),~str_remove(names(adj.edu.yr.par.tab),'adj_')),.id='model') %>% 
  filter(grepl('edu.c',term)) %>% 
  ggplot(aes(x=estimate, y=factor(term,levels=c('edu.c:pedu.c','edu.c','pedu.c'))))+ 
  geom_point(aes(shape=model),size=3,position = position_dodge(.75)) +
  scale_shape_manual(values=c(15, 16),labels=c('Adjusted Model','Unadjusted Model'))+
  geom_vline(xintercept=0,color='black',lty=2)+
  geom_errorbar(aes(xmin=as.numeric(LL),xmax=as.numeric(UL),group=model),width=.1,position = position_dodge(.75)) +
  scale_y_discrete(labels=c('Interactive Effect','Personal','Parental')) +
  scale_x_continuous(labels = function(x) format(x, nsmall = 3),limits = c(-0.60,0.60))+
  labs(x="Slope",y='Years of Education',title='Yang') +
  theme_apa(legend.pos = 'top') + theme(plot.title = element_text(hjust = 0.5))
yang.coef

## Stratify by race/ethnicity
# NH Black
nhb.edu.yr.par <- svyglm(accyang~edu.c*pedu.c +
                                 sex +
                                 scale(H13ITOT) + 
                                 smokestatus +
                                 R13BMI,design=subset(svy.edaa,raceth=="NHB"))
summary(nhb.edu.yr.par)
nhb.edu.yr.par.tab <- tidy(nhb.edu.yr.par) %>% mutate(LL=sprintf(confint(nhb.edu.yr.par)[,1],fmt='%#.3f'),
                                                      UL=sprintf(confint(nhb.edu.yr.par)[,2],fmt='%#.3f')) %>%
                                                      unite(col=CI,LL:UL, sep = ", ", remove=F)
                                                      
# Hispanic
h.edu.yr.par <- svyglm(accyang~edu.c*pedu.c +
                               sex +
                               scale(H13ITOT) + 
                               smokestatus +
                               R13BMI,design=subset(svy.edaa,raceth=="Hispanic"))
summary(h.edu.yr.par)
h.edu.yr.par.tab <- tidy(h.edu.yr.par) %>% mutate(LL=sprintf(confint(h.edu.yr.par)[,1],fmt='%#.3f'),
                                                  UL=sprintf(confint(h.edu.yr.par)[,2],fmt='%#.3f')) %>%
                                                  unite(col=CI,LL:UL, sep = ", ", remove=F)
                                                  
#NH White
nhw.edu.yr.par <- svyglm(accyang~edu.c*pedu.c +
                                 sex +
                                 scale(H13ITOT) + 
                                 smokestatus +
                                 R13BMI,design=subset(svy.edaa,raceth=="NHW"))
summary(nhw.edu.yr.par)
nhw.edu.yr.par.tab <- tidy(nhw.edu.yr.par) %>% mutate(LL=sprintf(confint(nhw.edu.yr.par)[,1],fmt='%#.3f'),
                                                      UL=sprintf(confint(nhw.edu.yr.par)[,2],fmt='%#.3f')) %>%
                                                      unite(col=CI,LL:UL, sep = ", ", remove=F)
                                                      

# Stack Columns 
re.edu.yr.par.YANG_DNAMAGE<- bind_rows("NH Black"=nhb.edu.yr.par.tab,"NH Hispanic"=h.edu.yr.par.tab,"NH White"=nhw.edu.yr.par.tab,.id='Subgroup')
write.csv(re.edu.yr.par.YANG_DNAMAGE,file='re.edu.yr.par.YANG_DNAMAGE.csv')

## ZHANG_DNAMAGE ----
# Unadjusted
edu.yr.par <- svyglm(acczhang~edu.c*pedu.c,design=svy.edaa)
summary(edu.yr.par)
edu.yr.par.tab <- tidy(edu.yr.par) %>% mutate(LL=sprintf(confint(edu.yr.par)[,1],fmt='%#.3f'),
                                              UL=sprintf(confint(edu.yr.par)[,2],fmt='%#.3f')) %>%
                                       unite(col=CI,LL:UL, sep = ", ", remove=F)
                                       

# Adjusted
adj.edu.yr.par <- svyglm(acczhang~edu.c*pedu.c +
                                 raceth + 
                                 sex +
                                 scale(H13ITOT) + 
                                 smokestatus +
                                 R13BMI,design=svy.edaa)
summary(adj.edu.yr.par)
adj.edu.yr.par.tab <- tidy(adj.edu.yr.par) %>% mutate(LL=sprintf(confint(adj.edu.yr.par)[,1],fmt='%#.3f'),
                                               UL=sprintf(confint(adj.edu.yr.par)[,2],fmt='%#.3f')) %>%
                                               unite(col=CI,LL:UL, sep = ", ", remove=F)
                                               
names(adj.edu.yr.par.tab) <- gsub("^(.*?)","adj_\\2", names(adj.edu.yr.par.tab))

# Combine results into one table
edu.yr.par.ZHANG_DNAMAGE <- merge(edu.yr.par.tab,adj.edu.yr.par.tab,by.x='term',by.y='adj_term',all.y=T,sort=F) 
write.csv(edu.yr.par.ZHANG_DNAMAGE,'edu.yr.par.ZHANG_DNAMAGE.csv')

# Plot coefficients
zhang.coef <- bind_rows(undj=edu.yr.par.tab,adj=rename_with(adj.edu.yr.par.tab,cols=everything(),~str_remove(names(adj.edu.yr.par.tab),'adj_')),.id='model') %>% 
  filter(grepl('edu.c',term)) %>% 
  ggplot(aes(x=estimate, y=factor(term,levels=c('edu.c:pedu.c','edu.c','pedu.c'))))+ 
  geom_point(aes(shape=model),size=3,position = position_dodge(.75)) +
  scale_shape_manual(values=c(15, 16),labels=c('Adjusted Model','Unadjusted Model'))+
  geom_vline(xintercept=0,color='black',lty=2)+
  geom_errorbar(aes(xmin=as.numeric(LL),xmax=as.numeric(UL),group=model),width=.1,position = position_dodge(.75)) +
  scale_y_discrete(labels=c('Interactive Effect','Personal','Parental')) +
  scale_x_continuous(labels = function(x) format(x, nsmall = 3),limits = c(-0.60,0.60))+
  labs(x="Slope",y='Years of Education',title='Zhang') +
  theme_apa(legend.pos = 'top') + theme(plot.title = element_text(hjust = 0.5))
zhang.coef

## Stratify by race/ethnicity
# NH Black
nhb.edu.yr.par <- svyglm(acczhang~edu.c*pedu.c +
                                 sex +
                                 scale(H13ITOT) + 
                                 smokestatus +
                                 R13BMI,design=subset(svy.edaa,raceth=="NHB"))
summary(nhb.edu.yr.par)
nhb.edu.yr.par.tab <- tidy(nhb.edu.yr.par) %>% mutate(LL=sprintf(confint(nhb.edu.yr.par)[,1],fmt='%#.3f'),
                                                      UL=sprintf(confint(nhb.edu.yr.par)[,2],fmt='%#.3f')) %>%
                                                      unite(col=CI,LL:UL, sep = ", ", remove=F)
                                                      
# Hispanic
h.edu.yr.par <- svyglm(acczhang~edu.c*pedu.c +
                               sex +
                               scale(H13ITOT) + 
                               smokestatus +
                               R13BMI,design=subset(svy.edaa,raceth=="Hispanic"))
summary(h.edu.yr.par)
h.edu.yr.par.tab <- tidy(h.edu.yr.par) %>% mutate(LL=sprintf(confint(h.edu.yr.par)[,1],fmt='%#.3f'),
                                                  UL=sprintf(confint(h.edu.yr.par)[,2],fmt='%#.3f')) %>%
                                                  unite(col=CI,LL:UL, sep = ", ", remove=F)
                                                  
#NH White
nhw.edu.yr.par <- svyglm(acczhang~edu.c*pedu.c +
                                 sex +
                                 scale(H13ITOT) + 
                                 smokestatus +
                                 R13BMI,design=subset(svy.edaa,raceth=="NHW"))
summary(nhw.edu.yr.par)
nhw.edu.yr.par.tab <- tidy(nhw.edu.yr.par) %>% mutate(LL=sprintf(confint(nhw.edu.yr.par)[,1],fmt='%#.3f'),
                                                      UL=sprintf(confint(nhw.edu.yr.par)[,2],fmt='%#.3f')) %>%
                                                      unite(col=CI,LL:UL, sep = ", ", remove=F)
                                                      

# Stack Columns 
re.edu.yr.par.ZHANG_DNAMAGE<- bind_rows("NH Black"=nhb.edu.yr.par.tab,"NH Hispanic"=h.edu.yr.par.tab,"NH White"=nhw.edu.yr.par.tab,.id='Subgroup')
write.csv(re.edu.yr.par.ZHANG_DNAMAGE,file='re.edu.yr.par.ZHANG_DNAMAGE.csv')

## BOCKLANDT_DNAMAGE ----
# Unadjusted
edu.yr.par <- svyglm(accbock~edu.c*pedu.c,design=svy.edaa)
summary(edu.yr.par)
edu.yr.par.tab <- tidy(edu.yr.par) %>% mutate(LL=sprintf(confint(edu.yr.par)[,1],fmt='%#.3f'),
                                              UL=sprintf(confint(edu.yr.par)[,2],fmt='%#.3f')) %>%
                                       unite(col=CI,LL:UL, sep = ", ", remove=F)
                                       

# Adjusted
adj.edu.yr.par <- svyglm(accbock~edu.c*pedu.c +
                                 raceth + 
                                 sex +
                                 scale(H13ITOT) + 
                                 smokestatus +
                                 R13BMI,design=svy.edaa)
summary(adj.edu.yr.par)
adj.edu.yr.par.tab <- tidy(adj.edu.yr.par) %>% mutate(LL=sprintf(confint(adj.edu.yr.par)[,1],fmt='%#.3f'),
                                               UL=sprintf(confint(adj.edu.yr.par)[,2],fmt='%#.3f')) %>%
                                               unite(col=CI,LL:UL, sep = ", ", remove=F)
                                               
names(adj.edu.yr.par.tab) <- gsub("^(.*?)","adj_\\2", names(adj.edu.yr.par.tab))

# Combine results into one table
edu.yr.par.BOCKLANDT_DNAMAGE <- merge(edu.yr.par.tab,adj.edu.yr.par.tab,by.x='term',by.y='adj_term',all.y=T,sort=F) 
write.csv(edu.yr.par.BOCKLANDT_DNAMAGE,'edu.yr.par.BOCKLANDT_DNAMAGE.csv')

# Plot coefficients
bocklandt.coef <- bind_rows(undj=edu.yr.par.tab,adj=rename_with(adj.edu.yr.par.tab,cols=everything(),~str_remove(names(adj.edu.yr.par.tab),'adj_')),.id='model') %>% 
  filter(grepl('edu.c',term)) %>% 
  ggplot(aes(x=estimate, y=factor(term,levels=c('edu.c:pedu.c','edu.c','pedu.c'))))+ 
  geom_point(aes(shape=model),size=3,position = position_dodge(.75)) +
  scale_shape_manual(values=c(15, 16),labels=c('Adjusted Model','Unadjusted Model'))+
  geom_vline(xintercept=0,color='black',lty=2)+
  geom_errorbar(aes(xmin=as.numeric(LL),xmax=as.numeric(UL),group=model),width=.1,position = position_dodge(.75)) +
  scale_y_discrete(labels=c('Interactive Effect','Personal','Parental')) +
  scale_x_continuous(labels = function(x) format(x, nsmall = 3),limits = c(-0.60,0.60))+
  labs(x="Slope",y='Years of Education',title='Bocklandt') +
  theme_apa(legend.pos = 'top') + theme(plot.title = element_text(hjust = 0.5))
bocklandt.coef

## Stratify by race/ethnicity
# NH Black
nhb.edu.yr.par <- svyglm(accbock~edu.c*pedu.c +
                                 sex +
                                 scale(H13ITOT) + 
                                 smokestatus +
                                 R13BMI,design=subset(svy.edaa,raceth=="NHB"))
summary(nhb.edu.yr.par)
nhb.edu.yr.par.tab <- tidy(nhb.edu.yr.par) %>% mutate(LL=sprintf(confint(nhb.edu.yr.par)[,1],fmt='%#.3f'),
                                                      UL=sprintf(confint(nhb.edu.yr.par)[,2],fmt='%#.3f')) %>%
                                                      unite(col=CI,LL:UL, sep = ", ", remove=F)
                                                      
# Hispanic
h.edu.yr.par <- svyglm(accbock~edu.c*pedu.c +
                               sex +
                               scale(H13ITOT) + 
                               smokestatus +
                               R13BMI,design=subset(svy.edaa,raceth=="Hispanic"))
summary(h.edu.yr.par)
h.edu.yr.par.tab <- tidy(h.edu.yr.par) %>% mutate(LL=sprintf(confint(h.edu.yr.par)[,1],fmt='%#.3f'),
                                                  UL=sprintf(confint(h.edu.yr.par)[,2],fmt='%#.3f')) %>%
                                                  unite(col=CI,LL:UL, sep = ", ", remove=F)
                                                  
#NH White
nhw.edu.yr.par <- svyglm(accbock~edu.c*pedu.c +
                                 sex +
                                 scale(H13ITOT) + 
                                 smokestatus +
                                 R13BMI,design=subset(svy.edaa,raceth=="NHW"))
summary(nhw.edu.yr.par)
nhw.edu.yr.par.tab <- tidy(nhw.edu.yr.par) %>% mutate(LL=sprintf(confint(nhw.edu.yr.par)[,1],fmt='%#.3f'),
                                                      UL=sprintf(confint(nhw.edu.yr.par)[,2],fmt='%#.3f')) %>%
                                                      unite(col=CI,LL:UL, sep = ", ", remove=F)
                                                      

# Stack Columns 
re.edu.yr.par.BOCKLANDT_DNAMAGE<- bind_rows("NH Black"=nhb.edu.yr.par.tab,"NH Hispanic"=h.edu.yr.par.tab,"NH White"=nhw.edu.yr.par.tab,.id='Subgroup')
write.csv(re.edu.yr.par.BOCKLANDT_DNAMAGE,file='re.edu.yr.par.BOCKLANDT_DNAMAGE.csv')

## GARAGNANI_DNAMAGE ----
# Unadjusted
edu.yr.par <- svyglm(accgarag~edu.c*pedu.c,design=svy.edaa)
summary(edu.yr.par)
edu.yr.par.tab <- tidy(edu.yr.par) %>% mutate(LL=sprintf(confint(edu.yr.par)[,1],fmt='%#.3f'),
                                              UL=sprintf(confint(edu.yr.par)[,2],fmt='%#.3f')) %>%
                                       unite(col=CI,LL:UL, sep = ", ", remove=F)
                                       

# Adjusted
adj.edu.yr.par <- svyglm(accgarag~edu.c*pedu.c +
                                 raceth + 
                                 sex +
                                 scale(H13ITOT) + 
                                 smokestatus +
                                 R13BMI,design=svy.edaa)
summary(adj.edu.yr.par)
adj.edu.yr.par.tab <- tidy(adj.edu.yr.par) %>% mutate(LL=sprintf(confint(adj.edu.yr.par)[,1],fmt='%#.3f'),
                                               UL=sprintf(confint(adj.edu.yr.par)[,2],fmt='%#.3f')) %>%
                                               unite(col=CI,LL:UL, sep = ", ", remove=F)
                                               
names(adj.edu.yr.par.tab) <- gsub("^(.*?)","adj_\\2", names(adj.edu.yr.par.tab))

# Combine results into one table
edu.yr.par.GARAGNANI_DNAMAGE <- merge(edu.yr.par.tab,adj.edu.yr.par.tab,by.x='term',by.y='adj_term',all.y=T,sort=F) 
write.csv(edu.yr.par.GARAGNANI_DNAMAGE,'edu.yr.par.GARAGNANI_DNAMAGE.csv')

# Plot coefficients
garagnani.coef <- bind_rows(undj=edu.yr.par.tab,adj=rename_with(adj.edu.yr.par.tab,cols=everything(),~str_remove(names(adj.edu.yr.par.tab),'adj_')),.id='model') %>% 
  filter(grepl('edu.c',term)) %>% 
  ggplot(aes(x=estimate, y=factor(term,levels=c('edu.c:pedu.c','edu.c','pedu.c'))))+ 
  geom_point(aes(shape=model),size=3,position = position_dodge(.75)) +
  scale_shape_manual(values=c(15, 16),labels=c('Adjusted Model','Unadjusted Model'))+
  geom_vline(xintercept=0,color='black',lty=2)+
  geom_errorbar(aes(xmin=as.numeric(LL),xmax=as.numeric(UL),group=model),width=.1,position = position_dodge(.75)) +
  scale_y_discrete(labels=c('Interactive Effect','Personal','Parental')) +
  scale_x_continuous(labels = function(x) format(x, nsmall = 3),limits = c(-0.60,0.60))+
  labs(x="Slope",y='Years of Education',title='Garagnani') +
  theme_apa(legend.pos = 'top') + theme(plot.title = element_text(hjust = 0.5))
garagnani.coef

# Plot significant interaction
GARAGNANI.simslopes <- tidy(interactions::sim_slopes(adj.edu.yr.par,pred=pedu.c,
                                                     modx=edu.c,confint=T,cond.int=T,
                                                     modx.values='terciles'))

GARAGNANI.plot <- GARAGNANI.simslopes %>% 
  ggplot(aes(x=estimate,y=term)) +
  geom_vline(xintercept=0,color='black',lty=2)+
  geom_point(aes(shape=term),size=3) +
  geom_errorbar(aes(xmin=conf.low,xmax=conf.high),width=.1)+
  scale_shape_manual(values=c(17, 16, 15))+
  scale_y_discrete(labels=c('Lower','Medium','Higher'))+
  scale_x_continuous(labels = function(x) format(x, nsmall = 3))+
  labs(x="Slope for Parental Years of Education",y='Terciles for\nPersonal Years of Education',title='Garagnani')+
  theme_apa(legend.pos = 'none') + theme(plot.title = element_text(hjust = 0.5))

GARAGNANI.plot


## Stratify by race/ethnicity
# NH Black
nhb.edu.yr.par <- svyglm(accgarag~edu.c*pedu.c +
                                 sex +
                                 scale(H13ITOT) + 
                                 smokestatus +
                                 R13BMI,design=subset(svy.edaa,raceth=="NHB"))
summary(nhb.edu.yr.par)
nhb.edu.yr.par.tab <- tidy(nhb.edu.yr.par) %>% mutate(LL=sprintf(confint(nhb.edu.yr.par)[,1],fmt='%#.3f'),
                                                      UL=sprintf(confint(nhb.edu.yr.par)[,2],fmt='%#.3f')) %>%
                                                      unite(col=CI,LL:UL, sep = ", ", remove=F)
                                                      
# Hispanic
h.edu.yr.par <- svyglm(accgarag~edu.c*pedu.c +
                               sex +
                               scale(H13ITOT) + 
                               smokestatus +
                               R13BMI,design=subset(svy.edaa,raceth=="Hispanic"))
summary(h.edu.yr.par)
h.edu.yr.par.tab <- tidy(h.edu.yr.par) %>% mutate(LL=sprintf(confint(h.edu.yr.par)[,1],fmt='%#.3f'),
                                                  UL=sprintf(confint(h.edu.yr.par)[,2],fmt='%#.3f')) %>%
                                                  unite(col=CI,LL:UL, sep = ", ", remove=F)
                                                  
#NH White
nhw.edu.yr.par <- svyglm(accgarag~edu.c*pedu.c +
                                 sex +
                                 scale(H13ITOT) + 
                                 smokestatus +
                                 R13BMI,design=subset(svy.edaa,raceth=="NHW"))
summary(nhw.edu.yr.par)
nhw.edu.yr.par.tab <- tidy(nhw.edu.yr.par) %>% mutate(LL=sprintf(confint(nhw.edu.yr.par)[,1],fmt='%#.3f'),
                                                      UL=sprintf(confint(nhw.edu.yr.par)[,2],fmt='%#.3f')) %>%
                                                      unite(col=CI,LL:UL, sep = ", ", remove=F)
                                                      

# Stack Columns 
re.edu.yr.par.GARAGNANI_DNAMAGE<- bind_rows("NH Black"=nhb.edu.yr.par.tab,"NH Hispanic"=h.edu.yr.par.tab,"NH White"=nhw.edu.yr.par.tab,.id='Subgroup')
write.csv(re.edu.yr.par.GARAGNANI_DNAMAGE,file='re.edu.yr.par.GARAGNANI_DNAMAGE.csv')

## DNAMGRIMAGE ----
# Unadjusted
edu.yr.par <- svyglm(accdnam~edu.c*pedu.c,design=svy.edaa)
summary(edu.yr.par)
edu.yr.par.tab <- tidy(edu.yr.par) %>% mutate(LL=sprintf(confint(edu.yr.par)[,1],fmt='%#.3f'),
                                              UL=sprintf(confint(edu.yr.par)[,2],fmt='%#.3f')) %>%
                                       unite(col=CI,LL:UL, sep = ", ", remove=F)
                                       

# Adjusted
adj.edu.yr.par <- svyglm(accdnam~edu.c*pedu.c +
                                 raceth + 
                                 sex +
                                 scale(H13ITOT) + 
                                 smokestatus +
                                 R13BMI,design=svy.edaa)
summary(adj.edu.yr.par)
adj.edu.yr.par.tab <- tidy(adj.edu.yr.par) %>% mutate(LL=sprintf(confint(adj.edu.yr.par)[,1],fmt='%#.3f'),
                                               UL=sprintf(confint(adj.edu.yr.par)[,2],fmt='%#.3f')) %>%
                                               unite(col=CI,LL:UL, sep = ", ", remove=F)
                                               
names(adj.edu.yr.par.tab) <- gsub("^(.*?)","adj_\\2", names(adj.edu.yr.par.tab))

# Combine results into one table
edu.yr.par.DNAMGRIMAGE <- merge(edu.yr.par.tab,adj.edu.yr.par.tab,by.x='term',by.y='adj_term',all.y=T,sort=F) 
write.csv(edu.yr.par.DNAMGRIMAGE,'edu.yr.par.DNAMGRIMAGE.csv')

# Plot coefficients
grimage.coef <- bind_rows(undj=edu.yr.par.tab,adj=rename_with(adj.edu.yr.par.tab,cols=everything(),~str_remove(names(adj.edu.yr.par.tab),'adj_')),.id='model') %>% 
  filter(grepl('edu.c',term)) %>% 
  ggplot(aes(x=estimate, y=factor(term,levels=c('edu.c:pedu.c','edu.c','pedu.c'))))+ 
  geom_point(aes(shape=model),size=3,position = position_dodge(.75)) +
  scale_shape_manual(values=c(15, 16),labels=c('Adjusted Model','Unadjusted Model'))+
  geom_vline(xintercept=0,color='black',lty=2)+
  geom_errorbar(aes(xmin=as.numeric(LL),xmax=as.numeric(UL),group=model),width=.1,position = position_dodge(.75)) +
  scale_y_discrete(labels=c('Interactive Effect','Personal','Parental')) +
  scale_x_continuous(labels = function(x) format(x, nsmall = 3),limits = c(-0.60,0.60))+
  labs(x="Slope",y='Years of Education',title='Lu-GrimAge') +
  theme_apa(legend.pos = 'top') + theme(plot.title = element_text(hjust = 0.5))
grimage.coef

# Plot significant interaction
DNAMGRIMAGE.simslopes <- tidy(interactions::sim_slopes(adj.edu.yr.par,pred=pedu.c,
                                                     modx=edu.c,confint=T,cond.int=T,
                                                     modx.values='terciles'))

DNAMGRIMAGE.plot <- DNAMGRIMAGE.simslopes %>% 
  ggplot(aes(x=estimate,y=term)) +
  geom_vline(xintercept=0,color='black',lty=2)+
  geom_point(aes(shape=term),size=3) +
  geom_errorbar(aes(xmin=conf.low,xmax=conf.high),width=.1)+
  scale_shape_manual(values=c(17, 16, 15))+
  scale_y_discrete(labels=c('Lower','Medium','Higher'))+
  scale_x_continuous(labels = function(x) format(x, nsmall = 3))+
  labs(x="Slope for Parental Years of Education",y='Terciles for\nPersonal Years of Education',title='Lu-GrimAge')+
  theme_apa(legend.pos = 'none') + theme(plot.title = element_text(hjust = 0.5))

DNAMGRIMAGE.plot

## Stratify by race/ethnicity
# NH Black
nhb.edu.yr.par <- svyglm(accdnam~edu.c*pedu.c +
                                 sex +
                                 scale(H13ITOT) + 
                                 smokestatus +
                                 R13BMI,design=subset(svy.edaa,raceth=="NHB"))
summary(nhb.edu.yr.par)
nhb.edu.yr.par.tab <- tidy(nhb.edu.yr.par) %>% mutate(LL=sprintf(confint(nhb.edu.yr.par)[,1],fmt='%#.3f'),
                                                      UL=sprintf(confint(nhb.edu.yr.par)[,2],fmt='%#.3f')) %>%
                                                      unite(col=CI,LL:UL, sep = ", ", remove=F)
                                                      
# Hispanic
h.edu.yr.par <- svyglm(accdnam~edu.c*pedu.c +
                               sex +
                               scale(H13ITOT) + 
                               smokestatus +
                               R13BMI,design=subset(svy.edaa,raceth=="Hispanic"))
summary(h.edu.yr.par)
h.edu.yr.par.tab <- tidy(h.edu.yr.par) %>% mutate(LL=sprintf(confint(h.edu.yr.par)[,1],fmt='%#.3f'),
                                                  UL=sprintf(confint(h.edu.yr.par)[,2],fmt='%#.3f')) %>%
                                                  unite(col=CI,LL:UL, sep = ", ", remove=F)
                                                  
#NH White
nhw.edu.yr.par <- svyglm(accdnam~edu.c*pedu.c +
                                 sex +
                                 scale(H13ITOT) + 
                                 smokestatus +
                                 R13BMI,design=subset(svy.edaa,raceth=="NHW"))
summary(nhw.edu.yr.par)
nhw.edu.yr.par.tab <- tidy(nhw.edu.yr.par) %>% mutate(LL=sprintf(confint(nhw.edu.yr.par)[,1],fmt='%#.3f'),
                                                      UL=sprintf(confint(nhw.edu.yr.par)[,2],fmt='%#.3f')) %>%
                                                      unite(col=CI,LL:UL, sep = ", ", remove=F)
                                                      

# Stack Columns 
re.edu.yr.par.DNAMGRIMAGE<- bind_rows("NH Black"=nhb.edu.yr.par.tab,"NH Hispanic"=h.edu.yr.par.tab,"NH White"=nhw.edu.yr.par.tab,.id='Subgroup')
write.csv(re.edu.yr.par.DNAMGRIMAGE,file='re.edu.yr.par.DNAMGRIMAGE.csv')

## MPOA ----
# Unadjusted
edu.yr.par <- svyglm(accmpoa~edu.c*pedu.c,design=svy.edaa)
summary(edu.yr.par)
edu.yr.par.tab <- tidy(edu.yr.par) %>% mutate(LL=sprintf(confint(edu.yr.par)[,1],fmt='%#.3f'),
                                              UL=sprintf(confint(edu.yr.par)[,2],fmt='%#.3f')) %>%
                                       unite(col=CI,LL:UL, sep = ", ", remove=F)
                                       

# Adjusted
adj.edu.yr.par <- svyglm(accmpoa~edu.c*pedu.c +
                                 raceth + 
                                 sex +
                                 scale(H13ITOT) + 
                                 smokestatus +
                                 R13BMI,design=svy.edaa)
summary(adj.edu.yr.par)
adj.edu.yr.par.tab <- tidy(adj.edu.yr.par) %>% mutate(LL=sprintf(confint(adj.edu.yr.par)[,1],fmt='%#.3f'),
                                               UL=sprintf(confint(adj.edu.yr.par)[,2],fmt='%#.3f')) %>%
                                               unite(col=CI,LL:UL, sep = ", ", remove=F)
                                               
names(adj.edu.yr.par.tab) <- gsub("^(.*?)","adj_\\2", names(adj.edu.yr.par.tab))

# Combine results into one table
edu.yr.par.MPOA<- merge(edu.yr.par.tab,adj.edu.yr.par.tab,by.x='term',by.y='adj_term',all.y=T,sort=F) 
write.csv(edu.yr.par.MPOA,'edu.yr.par.MPOA.csv')

# Plot coefficients
mpoa.coef <- bind_rows(undj=edu.yr.par.tab,adj=rename_with(adj.edu.yr.par.tab,cols=everything(),~str_remove(names(adj.edu.yr.par.tab),'adj_')),.id='model') %>% 
  filter(grepl('edu.c',term)) %>% 
  ggplot(aes(x=estimate, y=factor(term,levels=c('edu.c:pedu.c','edu.c','pedu.c'))))+ 
  geom_point(aes(shape=model),size=3,position = position_dodge(.75)) +
  scale_shape_manual(values=c(15, 16),labels=c('Adjusted Model','Unadjusted Model'))+
  geom_vline(xintercept=0,color='black',lty=2)+
  geom_errorbar(aes(xmin=as.numeric(LL),xmax=as.numeric(UL),group=model),width=.1,position = position_dodge(.75)) +
  scale_y_discrete(labels=c('Interactive Effect','Personal','Parental')) +
  scale_x_continuous(labels = function(x) format(x, nsmall = 3),limits = c(-0.60,0.60))+
  labs(x="Slope",y='Years of Education',title='DunedinPoAm38') +
  theme_apa(legend.pos = 'top') + theme(plot.title = element_text(hjust = 0.5))
mpoa.coef

# Plot significant interaction
MPOA.simslopes <- tidy(interactions::sim_slopes(adj.edu.yr.par,pred=pedu.c,
                                                     modx=edu.c,confint=T,cond.int=T,
                                                     modx.values='terciles'))

MPOA.plot <- MPOA.simslopes %>% 
  ggplot(aes(x=estimate,y=term)) +
  geom_vline(xintercept=0,color='black',lty=2)+
  geom_point(aes(shape=term),size=3) +
  geom_errorbar(aes(xmin=conf.low,xmax=conf.high),width=.1)+
  scale_shape_manual(values=c(17, 16, 15))+
  scale_y_discrete(labels=c('Lower','Medium','Higher'))+
  scale_x_continuous(labels = function(x) format(x, nsmall = 3))+
  labs(x="Slope for Parental Years of Education",y='Terciles for\nPersonal Years of Education',title='DundedinPoAm38')+
  theme_apa(legend.pos = 'none') + theme(plot.title = element_text(hjust = 0.5))

MPOA.plot

## Stratify by race/ethnicity
# NH Black
nhb.edu.yr.par <- svyglm(accmpoa~edu.c*pedu.c +
                                 sex +
                                 scale(H13ITOT) + 
                                 smokestatus +
                                 R13BMI,design=subset(svy.edaa,raceth=="NHB"))
summary(nhb.edu.yr.par)
nhb.edu.yr.par.tab <- tidy(nhb.edu.yr.par) %>% mutate(LL=sprintf(confint(nhb.edu.yr.par)[,1],fmt='%#.3f'),
                                                      UL=sprintf(confint(nhb.edu.yr.par)[,2],fmt='%#.3f')) %>%
                                                      unite(col=CI,LL:UL, sep = ", ", remove=F)
                                                      
# Hispanic
h.edu.yr.par <- svyglm(accmpoa~edu.c*pedu.c +
                               sex +
                               scale(H13ITOT) + 
                               smokestatus +
                               R13BMI,design=subset(svy.edaa,raceth=="Hispanic"))
summary(h.edu.yr.par)
h.edu.yr.par.tab <- tidy(h.edu.yr.par) %>% mutate(LL=sprintf(confint(h.edu.yr.par)[,1],fmt='%#.3f'),
                                                  UL=sprintf(confint(h.edu.yr.par)[,2],fmt='%#.3f')) %>%
                                                  unite(col=CI,LL:UL, sep = ", ", remove=F)
                                                  
#NH White
nhw.edu.yr.par <- svyglm(accmpoa~edu.c*pedu.c +
                                 sex +
                                 scale(H13ITOT) + 
                                 smokestatus +
                                 R13BMI,design=subset(svy.edaa,raceth=="NHW"))
summary(nhw.edu.yr.par)
nhw.edu.yr.par.tab <- tidy(nhw.edu.yr.par) %>% mutate(LL=sprintf(confint(nhw.edu.yr.par)[,1],fmt='%#.3f'),
                                                      UL=sprintf(confint(nhw.edu.yr.par)[,2],fmt='%#.3f')) %>%
                                                      unite(col=CI,LL:UL, sep = ", ", remove=F)
                                                      

# Stack Columns 
re.edu.yr.par.MPOA<- bind_rows("NH Black"=nhb.edu.yr.par.tab,"NH Hispanic"=h.edu.yr.par.tab,"NH White"=nhw.edu.yr.par.tab,.id='Subgroup')
write.csv(re.edu.yr.par.MPOA,file='re.edu.yr.par.MPOA.csv')

## Combine Plots and Simple Slopes ----
edu.y.par.simpleslopes <- bind_rows(Horvath1=HORVATH1.simslopes,
                                    Garagnani=GARAGNANI.simslopes,
                                    LuGrimAge=DNAMGRIMAGE.simslopes,
                                    MPOA=MPOA.simslopes,.id='Clock') %>% select(-mod2,-mod2.value)
write.csv(edu.y.par.simpleslopes,file='edu.y.par.simpleslopes.csv')

save(horvath1.coef,hannum.coef,lin.coef,weidner.coef,vidalbralo.coef,horvath2.coef,
     yang.coef,bocklandt.coef,garagnani.coef,zhang.coef,levine.coef,grimage.coef,
     mpoa.coef,file='coefficients_plots.RData')
ggpubr::ggarrange(horvath1.coef,hannum.coef,lin.coef,weidner.coef,vidalbralo.coef,horvath2.coef,
     yang.coef,bocklandt.coef,garagnani.coef,zhang.coef,levine.coef,grimage.coef,
     mpoa.coef,ncol=3,nrow=5,common.legend=T,legend="top")

save(HORVATH1.plot,GARAGNANI.plot,DNAMGRIMAGE.plot,MPOA.plot,file='interaction_plots.RData')
gridExtra::grid.arrange(HORVATH1.plot,GARAGNANI.plot,DNAMGRIMAGE.plot,MPOA.plot,nrow=2)

### Regression Models (Parental Educational Attainment-Categorical) ----
## Re-level personal education ----
svy.edaa$variables$RAEDYRS_factor <- relevel(svy.edaa$variables$RAEDYRS_factor,ref='Bachelors or Higher')
## HORVATH_DNAMAGE ----
# Unadjusted
edu.c.par <- svyglm(acchorv~RAEDYRS_factor*par_edu_bin,design=svy.edaa)
summary(edu.c.par)
edu.c.par.aov <- anova(edu.c.par,test="Chisq",method='LRT')
edu.c.par.tab <- tidy(edu.c.par) %>% mutate(LL=sprintf(confint(edu.c.par)[,1],fmt='%#.3f'),
                                              UL=sprintf(confint(edu.c.par)[,2],fmt='%#.3f')) %>%
                                       unite(col=CI,LL:UL, sep = ", ", remove=F)
                                       

# Adjusted
adj.edu.c.par <- svyglm(acchorv~RAEDYRS_factor*par_edu_bin +
                                 raceth + 
                                 sex +
                                 scale(H13ITOT) + 
                                 smokestatus +
                                 R13BMI,design=svy.edaa)
summary(adj.edu.c.par)

adj.edu.c.par.tab <- tidy(adj.edu.c.par) %>% mutate(LL=sprintf(confint(adj.edu.c.par)[,1],fmt='%#.3f'),
                                               UL=sprintf(confint(adj.edu.c.par)[,2],fmt='%#.3f')) %>%
                                               unite(col=CI,LL:UL, sep = ", ", remove=F)
                                               
names(adj.edu.c.par.tab) <- gsub("^(.*?)","adj_\\2", names(adj.edu.c.par.tab))

# Combine results into one table
edu.c.par.HORVATH_DNAMAGE <- merge(edu.c.par.tab,adj.edu.c.par.tab,by.x='term',by.y='adj_term',all.y=T,sort=F) 
write.csv(edu.c.par.HORVATH_DNAMAGE,'edu.c.par.HORVATH_DNAMAGE.csv')

# Save ANOVA results
sink(file="aov.edu.c.HORVATH_DNAMAGE.txt")
print(aov<-list(anova(edu.c.par),anova(adj.edu.c.par)))
sink()

## Marginal Means
# Unadjusted
HORVATH_DNAMAGE.MM <- emmeans(edu.c.par,pairwise~RAEDYRS_factor*par_edu_bin,data=svy.edaa,adjust='mvt')

# Adjusted
adj.HORVATH_DNAMAGE.MM <- emmeans(adj.edu.c.par,pairwise~RAEDYRS_factor*par_edu_bin,data=svy.edaa,adjust='mvt')

## Stratify by race/ethnicity
# NH Black
nhb.edu.c.par <- svyglm(acchorv~RAEDYRS_factor*par_edu_bin +
                                 sex +
                                 scale(H13ITOT) + 
                                 smokestatus +
                                 R13BMI,design=subset(svy.edaa,raceth=="NHB"))
summary(nhb.edu.c.par)
nhb.edu.c.par.tab <- tidy(nhb.edu.c.par) %>% mutate(LL=sprintf(confint(nhb.edu.c.par)[,1],fmt='%#.3f'),
                                                      UL=sprintf(confint(nhb.edu.c.par)[,2],fmt='%#.3f')) %>%
                                                      unite(col=CI,LL:UL, sep = ", ", remove=F)
                                                      

# Hispanic
h.edu.c.par <- svyglm(acchorv~RAEDYRS_factor*par_edu_bin +
                               sex +
                               scale(H13ITOT) + 
                               smokestatus +
                               R13BMI,design=subset(svy.edaa,raceth=="Hispanic"))
summary(h.edu.c.par)
h.edu.c.par.tab <- tidy(h.edu.c.par) %>% mutate(LL=sprintf(confint(h.edu.c.par)[,1],fmt='%#.3f'),
                                                  UL=sprintf(confint(h.edu.c.par)[,2],fmt='%#.3f')) %>%
                                                  unite(col=CI,LL:UL, sep = ", ", remove=F)
                                                  
#NH White
nhw.edu.c.par <- svyglm(acchorv~RAEDYRS_factor*par_edu_bin +
                                 sex +
                                 scale(H13ITOT) + 
                                 smokestatus +
                                 R13BMI,design=subset(svy.edaa,raceth=="NHW"))
summary(nhw.edu.c.par)
nhw.edu.c.par.tab <- tidy(nhw.edu.c.par) %>% mutate(LL=sprintf(confint(nhw.edu.c.par)[,1],fmt='%#.3f'),
                                                      UL=sprintf(confint(nhw.edu.c.par)[,2],fmt='%#.3f')) %>%
                                                      unite(col=CI,LL:UL, sep = ", ", remove=F)
                                                      

# Stack Columns 
re.edu.c.par.HORVATH_DNAMAGE <- bind_rows("NH Black"=nhb.edu.c.par.tab,"NH Hispanic"=h.edu.c.par.tab,"NH White"=nhw.edu.c.par.tab,.id='Subgroup')
write.csv(re.edu.c.par.HORVATH_DNAMAGE,file='re.edu.c.par.HORVATH_DNAMAGE.csv')

# Save ANOVA results
sink(file="re.aov.edu.c.HORVATH_DNAMAGE.txt")
print(aov<-list(anova(nhb.edu.c.par),anova(h.edu.c.par),anova(nhw.edu.c.par)))
sink()

## Marginal Means
nhb.HORVATH_DNAMAGE.MM <- emmeans(nhb.edu.c.par,pairwise~RAEDYRS_factor*par_edu_bin,data=subset(svy.edaa,raceth=="NHB"),adjust='mvt')
h.HORVATH_DNAMAGE.MM <- emmeans(h.edu.c.par,pairwise~RAEDYRS_factor*par_edu_bin,data=subset(svy.edaa,raceth=="Hispanic"),adjust='mvt')
nhw.HORVATH_DNAMAGE.MM <- emmeans(nhw.edu.c.par,pairwise~RAEDYRS_factor*par_edu_bin,data=subset(svy.edaa,raceth=="NHW"),adjust='mvt')

## HANNUM_DNAMAGE ----
# Unadjusted
edu.c.par <- svyglm(acchannum~RAEDYRS_factor*par_edu_bin,design=svy.edaa)
summary(edu.c.par)
edu.c.par.tab <- tidy(edu.c.par) %>% mutate(LL=sprintf(confint(edu.c.par)[,1],fmt='%#.3f'),
                                              UL=sprintf(confint(edu.c.par)[,2],fmt='%#.3f')) %>%
                                       unite(col=CI,LL:UL, sep = ", ", remove=F)
                                       

# Adjusted
adj.edu.c.par <- svyglm(acchannum~RAEDYRS_factor*par_edu_bin +
                                 raceth + 
                                 sex +
                                 scale(H13ITOT) + 
                                 smokestatus +
                                 R13BMI,design=svy.edaa)
summary(adj.edu.c.par)
adj.edu.c.par.tab <- tidy(adj.edu.c.par) %>% mutate(LL=sprintf(confint(adj.edu.c.par)[,1],fmt='%#.3f'),
                                               UL=sprintf(confint(adj.edu.c.par)[,2],fmt='%#.3f')) %>%
                                               unite(col=CI,LL:UL, sep = ", ", remove=F)
                                               
names(adj.edu.c.par.tab) <- gsub("^(.*?)","adj_\\2", names(adj.edu.c.par.tab))

# Combine results into one table
edu.c.par.HANNUM_DNAMAGE <- merge(edu.c.par.tab,adj.edu.c.par.tab,by.x='term',by.y='adj_term',all.y=T,sort=F) 
write.csv(edu.c.par.HANNUM_DNAMAGE,'edu.c.par.HANNUM_DNAMAGE.csv')

# Save ANOVA results
sink(file="aov.edu.c.HANNUM_DNAMAGE.txt")
print(aov<-list(anova(edu.c.par),anova(adj.edu.c.par)))
sink()

## Marginal Means
# Unadjusted
HANNUM_DNAMAGE.MM <- emmeans(edu.c.par,pairwise~RAEDYRS_factor*par_edu_bin,data=svy.edaa,adjust='mvt')

# Adjusted
adj.HANNUM_DNAMAGE.MM <- emmeans(adj.edu.c.par,pairwise~RAEDYRS_factor*par_edu_bin,data=svy.edaa,adjust='mvt')

## Stratify by race/ethnicity
# NH Black
nhb.edu.c.par <- svyglm(acchannum~RAEDYRS_factor*par_edu_bin +
                                 sex +
                                 scale(H13ITOT) + 
                                 smokestatus +
                                 R13BMI,design=subset(svy.edaa,raceth=="NHB"))
summary(nhb.edu.c.par)
nhb.edu.c.par.tab <- tidy(nhb.edu.c.par) %>% mutate(LL=sprintf(confint(nhb.edu.c.par)[,1],fmt='%#.3f'),
                                                      UL=sprintf(confint(nhb.edu.c.par)[,2],fmt='%#.3f')) %>%
                                                      unite(col=CI,LL:UL, sep = ", ", remove=F)
                                                      
# Hispanic
h.edu.c.par <- svyglm(acchannum~RAEDYRS_factor*par_edu_bin +
                               sex +
                               scale(H13ITOT) + 
                               smokestatus +
                               R13BMI,design=subset(svy.edaa,raceth=="Hispanic"))
summary(h.edu.c.par)
h.edu.c.par.tab <- tidy(h.edu.c.par) %>% mutate(LL=sprintf(confint(h.edu.c.par)[,1],fmt='%#.3f'),
                                                  UL=sprintf(confint(h.edu.c.par)[,2],fmt='%#.3f')) %>%
                                                  unite(col=CI,LL:UL, sep = ", ", remove=F)
                                                  
#NH White
nhw.edu.c.par <- svyglm(acchannum~RAEDYRS_factor*par_edu_bin +
                                 sex +
                                 scale(H13ITOT) + 
                                 smokestatus +
                                 R13BMI,design=subset(svy.edaa,raceth=="NHW"))
summary(nhw.edu.c.par)
nhw.edu.c.par.tab <- tidy(nhw.edu.c.par) %>% mutate(LL=sprintf(confint(nhw.edu.c.par)[,1],fmt='%#.3f'),
                                                      UL=sprintf(confint(nhw.edu.c.par)[,2],fmt='%#.3f')) %>%
                                                      unite(col=CI,LL:UL, sep = ", ", remove=F)
                                                      

# Stack Columns 
re.edu.c.par.HANNUM_DNAMAGE<- bind_rows("NH Black"=nhb.edu.c.par.tab,"NH Hispanic"=h.edu.c.par.tab,"NH White"=nhw.edu.c.par.tab,.id='Subgroup')
write.csv(re.edu.c.par.HANNUM_DNAMAGE,file='re.edu.c.par.HANNUM_DNAMAGE.csv')

# Save ANOVA results
sink(file="re.aov.edu.c.HANNUM_DNAMAGE.txt")
print(aov<-list(anova(nhb.edu.c.par),anova(h.edu.c.par),anova(nhw.edu.c.par)))
sink()

## Marginal Means
nhb.HANNUM_DNAMAGE.MM <- emmeans(nhb.edu.c.par,pairwise~RAEDYRS_factor*par_edu_bin,data=subset(svy.edaa,raceth=="NHB"),adjust='mvt')
h.HANNUM_DNAMAGE.MM <- emmeans(h.edu.c.par,pairwise~RAEDYRS_factor*par_edu_bin,data=subset(svy.edaa,raceth=="Hispanic"),adjust='mvt')
nhw.HANNUM_DNAMAGE.MM <- emmeans(nhw.edu.c.par,pairwise~RAEDYRS_factor*par_edu_bin,data=subset(svy.edaa,raceth=="NHW"),adjust='mvt')

## LEVINE_DNAMAGE ----
# Unadjusted
edu.c.par <- svyglm(acclevine~RAEDYRS_factor*par_edu_bin,design=svy.edaa)
summary(edu.c.par)
edu.c.par.tab <- tidy(edu.c.par) %>% mutate(LL=sprintf(confint(edu.c.par)[,1],fmt='%#.3f'),
                                              UL=sprintf(confint(edu.c.par)[,2],fmt='%#.3f')) %>%
                                       unite(col=CI,LL:UL, sep = ", ", remove=F)
                                       

# Adjusted
adj.edu.c.par <- svyglm(acclevine~RAEDYRS_factor*par_edu_bin +
                                 raceth + 
                                 sex +
                                 scale(H13ITOT) + 
                                 smokestatus +
                                 R13BMI,design=svy.edaa)
summary(adj.edu.c.par)
adj.edu.c.par.tab <- tidy(adj.edu.c.par) %>% mutate(LL=sprintf(confint(adj.edu.c.par)[,1],fmt='%#.3f'),
                                               UL=sprintf(confint(adj.edu.c.par)[,2],fmt='%#.3f')) %>%
                                               unite(col=CI,LL:UL, sep = ", ", remove=F)
                                               
names(adj.edu.c.par.tab) <- gsub("^(.*?)","adj_\\2", names(adj.edu.c.par.tab))

# Combine results into one table
edu.c.par.LEVINE_DNAMAGE <- merge(edu.c.par.tab,adj.edu.c.par.tab,by.x='term',by.y='adj_term',all.y=T,sort=F) 
write.csv(edu.c.par.LEVINE_DNAMAGE,'edu.c.par.LEVINE_DNAMAGE.csv')

# Save ANOVA results
sink(file="aov.edu.c.LEVINE_DNAMAGE.txt")
print(aov<-list(anova(edu.c.par),anova(adj.edu.c.par)))
sink()

## Marginal Means
# Unadjusted
LEVINE_DNAMAGE.MM <- emmeans(edu.c.par,pairwise~RAEDYRS_factor*par_edu_bin,data=svy.edaa,adjust='mvt')

# Adjusted
adj.LEVINE_DNAMAGE.MM <- emmeans(adj.edu.c.par,pairwise~RAEDYRS_factor*par_edu_bin,data=svy.edaa,adjust='mvt')

## Stratify by race/ethnicity
# NH Black
nhb.edu.c.par <- svyglm(acclevine~RAEDYRS_factor*par_edu_bin +
                                 sex +
                                 scale(H13ITOT) + 
                                 smokestatus +
                                 R13BMI,design=subset(svy.edaa,raceth=="NHB"))
summary(nhb.edu.c.par)
nhb.edu.c.par.tab <- tidy(nhb.edu.c.par) %>% mutate(LL=sprintf(confint(nhb.edu.c.par)[,1],fmt='%#.3f'),
                                                      UL=sprintf(confint(nhb.edu.c.par)[,2],fmt='%#.3f')) %>%
                                                      unite(col=CI,LL:UL, sep = ", ", remove=F)
                                                      
# Hispanic
h.edu.c.par <- svyglm(acclevine~RAEDYRS_factor*par_edu_bin +
                               sex +
                               scale(H13ITOT) + 
                               smokestatus +
                               R13BMI,design=subset(svy.edaa,raceth=="Hispanic"))
summary(h.edu.c.par)
h.edu.c.par.tab <- tidy(h.edu.c.par) %>% mutate(LL=sprintf(confint(h.edu.c.par)[,1],fmt='%#.3f'),
                                                  UL=sprintf(confint(h.edu.c.par)[,2],fmt='%#.3f')) %>%
                                                  unite(col=CI,LL:UL, sep = ", ", remove=F)
                                                  
#NH White
nhw.edu.c.par <- svyglm(acclevine~RAEDYRS_factor*par_edu_bin +
                                 sex +
                                 scale(H13ITOT) + 
                                 smokestatus +
                                 R13BMI,design=subset(svy.edaa,raceth=="NHW"))
summary(nhw.edu.c.par)
nhw.edu.c.par.tab <- tidy(nhw.edu.c.par) %>% mutate(LL=sprintf(confint(nhw.edu.c.par)[,1],fmt='%#.3f'),
                                                      UL=sprintf(confint(nhw.edu.c.par)[,2],fmt='%#.3f')) %>%
                                                      unite(col=CI,LL:UL, sep = ", ", remove=F)
                                                      

# Stack Columns 
re.edu.c.par.LEVINE_DNAMAGE<- bind_rows("NH Black"=nhb.edu.c.par.tab,"NH Hispanic"=h.edu.c.par.tab,"NH White"=nhw.edu.c.par.tab,.id='Subgroup')
write.csv(re.edu.c.par.LEVINE_DNAMAGE,file='re.edu.c.par.LEVINE_DNAMAGE.csv')

# Save ANOVA results
sink(file="re.aov.edu.c.LEVINE_DNAMAGE.txt")
print(aov<-list(anova(nhb.edu.c.par),anova(h.edu.c.par),anova(nhw.edu.c.par)))
sink()

## Marginal Means
nhb.LEVINE_DNAMAGE.MM <- emmeans(nhb.edu.c.par,pairwise~RAEDYRS_factor*par_edu_bin,data=subset(svy.edaa,raceth=="NHB"),adjust='mvt')
h.LEVINE_DNAMAGE.MM <- emmeans(h.edu.c.par,pairwise~RAEDYRS_factor*par_edu_bin,data=subset(svy.edaa,raceth=="Hispanic"),adjust='mvt')
nhw.LEVINE_DNAMAGE.MM <- emmeans(nhw.edu.c.par,pairwise~RAEDYRS_factor*par_edu_bin,data=subset(svy.edaa,raceth=="NHW"),adjust='mvt')

## HORVATHSKIN_DNAMAGE ----
# Unadjusted
edu.c.par <- svyglm(acchorvsk~RAEDYRS_factor*par_edu_bin,design=svy.edaa)
summary(edu.c.par)
edu.c.par.tab <- tidy(edu.c.par) %>% mutate(LL=sprintf(confint(edu.c.par)[,1],fmt='%#.3f'),
                                              UL=sprintf(confint(edu.c.par)[,2],fmt='%#.3f')) %>%
                                       unite(col=CI,LL:UL, sep = ", ", remove=F)
                                       

# Adjusted
adj.edu.c.par <- svyglm(acchorvsk~RAEDYRS_factor*par_edu_bin +
                                 raceth + 
                                 sex +
                                 scale(H13ITOT) + 
                                 smokestatus +
                                 R13BMI,design=svy.edaa)
summary(adj.edu.c.par)
adj.edu.c.par.tab <- tidy(adj.edu.c.par) %>% mutate(LL=sprintf(confint(adj.edu.c.par)[,1],fmt='%#.3f'),
                                               UL=sprintf(confint(adj.edu.c.par)[,2],fmt='%#.3f')) %>%
                                               unite(col=CI,LL:UL, sep = ", ", remove=F)
                                               
names(adj.edu.c.par.tab) <- gsub("^(.*?)","adj_\\2", names(adj.edu.c.par.tab))

# Combine results into one table
edu.c.par.HORVATHSKIN_DNAMAGE<- merge(edu.c.par.tab,adj.edu.c.par.tab,by.x='term',by.y='adj_term',all.y=T,sort=F) 
write.csv(edu.c.par.HORVATHSKIN_DNAMAGE,'edu.c.par.HORVATHSKIN_DNAMAGE.csv')

# Save ANOVA results
sink(file="aov.edu.c.HORVATHSKIN_DNAMAGE.txt")
print(aov<-list(anova(edu.c.par),anova(adj.edu.c.par)))
sink()

## Marginal Means
# Unadjusted
HORVATHSKIN_DNAMAGE.MM <- emmeans(edu.c.par,pairwise~RAEDYRS_factor*par_edu_bin,data=svy.edaa,adjust='mvt')

# Adjusted
adj.HORVATHSKIN_DNAMAGE.MM <- emmeans(adj.edu.c.par,pairwise~RAEDYRS_factor*par_edu_bin,data=svy.edaa,adjust='mvt')

## Stratify by race/ethnicity
# NH Black
nhb.edu.c.par <- svyglm(acchorvsk~RAEDYRS_factor*par_edu_bin +
                                 sex +
                                 scale(H13ITOT) + 
                                 smokestatus +
                                 R13BMI,design=subset(svy.edaa,raceth=="NHB"))
summary(nhb.edu.c.par)
nhb.edu.c.par.tab <- tidy(nhb.edu.c.par) %>% mutate(LL=sprintf(confint(nhb.edu.c.par)[,1],fmt='%#.3f'),
                                                      UL=sprintf(confint(nhb.edu.c.par)[,2],fmt='%#.3f')) %>%
                                                      unite(col=CI,LL:UL, sep = ", ", remove=F)
                                                      
# Hispanic
h.edu.c.par <- svyglm(acchorvsk~RAEDYRS_factor*par_edu_bin +
                               sex +
                               scale(H13ITOT) + 
                               smokestatus +
                               R13BMI,design=subset(svy.edaa,raceth=="Hispanic"))
summary(h.edu.c.par)
h.edu.c.par.tab <- tidy(h.edu.c.par) %>% mutate(LL=sprintf(confint(h.edu.c.par)[,1],fmt='%#.3f'),
                                                  UL=sprintf(confint(h.edu.c.par)[,2],fmt='%#.3f')) %>%
                                                  unite(col=CI,LL:UL, sep = ", ", remove=F)
                                                  
#NH White
nhw.edu.c.par <- svyglm(acchorvsk~RAEDYRS_factor*par_edu_bin +
                                 sex +
                                 scale(H13ITOT) + 
                                 smokestatus +
                                 R13BMI,design=subset(svy.edaa,raceth=="NHW"))
summary(nhw.edu.c.par)
nhw.edu.c.par.tab <- tidy(nhw.edu.c.par) %>% mutate(LL=sprintf(confint(nhw.edu.c.par)[,1],fmt='%#.3f'),
                                                      UL=sprintf(confint(nhw.edu.c.par)[,2],fmt='%#.3f')) %>%
                                                      unite(col=CI,LL:UL, sep = ", ", remove=F)
                                                      

# Stack Columns 
re.edu.c.par.HORVATHSKIN_DNAMAGE<- bind_rows("NH Black"=nhb.edu.c.par.tab,"NH Hispanic"=h.edu.c.par.tab,"NH White"=nhw.edu.c.par.tab,.id='Subgroup')
write.csv(re.edu.c.par.HORVATHSKIN_DNAMAGE,file='re.edu.c.par.HORVATHSKIN_DNAMAGE.csv')

# Save ANOVA results
sink(file="re.aov.edu.c.HORVATHSKIN_DNAMAGE.txt")
print(aov<-list(anova(nhb.edu.c.par),anova(h.edu.c.par),anova(nhw.edu.c.par)))
sink()

## Marginal Means
nhb.HORVATHSKIN_DNAMAGE.MM <- emmeans(nhb.edu.c.par,pairwise~RAEDYRS_factor*par_edu_bin,data=subset(svy.edaa,raceth=="NHB"),adjust='mvt')
h.HORVATHSKIN_DNAMAGE.MM <- emmeans(h.edu.c.par,pairwise~RAEDYRS_factor*par_edu_bin,data=subset(svy.edaa,raceth=="Hispanic"),adjust='mvt')
nhw.HORVATHSKIN_DNAMAGE.MM <- emmeans(nhw.edu.c.par,pairwise~RAEDYRS_factor*par_edu_bin,data=subset(svy.edaa,raceth=="NHW"),adjust='mvt')

## LIN_DNAMAGE ----
# Unadjusted
edu.c.par <- svyglm(acclin~RAEDYRS_factor*par_edu_bin,design=svy.edaa)
summary(edu.c.par)
edu.c.par.tab <- tidy(edu.c.par) %>% mutate(LL=sprintf(confint(edu.c.par)[,1],fmt='%#.3f'),
                                              UL=sprintf(confint(edu.c.par)[,2],fmt='%#.3f')) %>%
                                       unite(col=CI,LL:UL, sep = ", ", remove=F)
                                       

# Adjusted
adj.edu.c.par <- svyglm(acclin~RAEDYRS_factor*par_edu_bin +
                                 raceth + 
                                 sex +
                                 scale(H13ITOT) + 
                                 smokestatus +
                                 R13BMI,design=svy.edaa)
summary(adj.edu.c.par)
adj.edu.c.par.tab <- tidy(adj.edu.c.par) %>% mutate(LL=sprintf(confint(adj.edu.c.par)[,1],fmt='%#.3f'),
                                               UL=sprintf(confint(adj.edu.c.par)[,2],fmt='%#.3f')) %>%
                                               unite(col=CI,LL:UL, sep = ", ", remove=F)
                                               
names(adj.edu.c.par.tab) <- gsub("^(.*?)","adj_\\2", names(adj.edu.c.par.tab))

# Combine results into one table
edu.c.par.LIN_DNAMAGE <- merge(edu.c.par.tab,adj.edu.c.par.tab,by.x='term',by.y='adj_term',all.y=T,sort=F) 
write.csv(edu.c.par.LIN_DNAMAGE,'edu.c.par.LIN_DNAMAGE.csv')

# Save ANOVA results
sink(file="aov.edu.c.LIN_DNAMAGE.txt")
print(aov<-list(anova(edu.c.par),anova(adj.edu.c.par)))
sink()

## Marginal Means
# Unadjusted
LIN_DNAMAGE.MM <- emmeans(edu.c.par,pairwise~RAEDYRS_factor*par_edu_bin,data=svy.edaa,adjust='mvt')

# Adjusted
adj.LIN_DNAMAGE.MM <- emmeans(adj.edu.c.par,pairwise~RAEDYRS_factor*par_edu_bin,data=svy.edaa,adjust='mvt')

## Stratify by race/ethnicity
# NH Black
nhb.edu.c.par <- svyglm(acclin~RAEDYRS_factor*par_edu_bin +
                                 sex +
                                 scale(H13ITOT) + 
                                 smokestatus +
                                 R13BMI,design=subset(svy.edaa,raceth=="NHB"))
summary(nhb.edu.c.par)
nhb.edu.c.par.tab <- tidy(nhb.edu.c.par) %>% mutate(LL=sprintf(confint(nhb.edu.c.par)[,1],fmt='%#.3f'),
                                                      UL=sprintf(confint(nhb.edu.c.par)[,2],fmt='%#.3f')) %>%
                                                      unite(col=CI,LL:UL, sep = ", ", remove=F)
                                                      
# Hispanic
h.edu.c.par <- svyglm(acclin~RAEDYRS_factor*par_edu_bin +
                               sex +
                               scale(H13ITOT) + 
                               smokestatus +
                               R13BMI,design=subset(svy.edaa,raceth=="Hispanic"))
summary(h.edu.c.par)
h.edu.c.par.tab <- tidy(h.edu.c.par) %>% mutate(LL=sprintf(confint(h.edu.c.par)[,1],fmt='%#.3f'),
                                                  UL=sprintf(confint(h.edu.c.par)[,2],fmt='%#.3f')) %>%
                                                  unite(col=CI,LL:UL, sep = ", ", remove=F)
                                                  
#NH White
nhw.edu.c.par <- svyglm(acclin~RAEDYRS_factor*par_edu_bin +
                                 sex +
                                 scale(H13ITOT) + 
                                 smokestatus +
                                 R13BMI,design=subset(svy.edaa,raceth=="NHW"))
summary(nhw.edu.c.par)
nhw.edu.c.par.tab <- tidy(nhw.edu.c.par) %>% mutate(LL=sprintf(confint(nhw.edu.c.par)[,1],fmt='%#.3f'),
                                                      UL=sprintf(confint(nhw.edu.c.par)[,2],fmt='%#.3f')) %>%
                                                      unite(col=CI,LL:UL, sep = ", ", remove=F)
                                                      

# Stack Columns 
re.edu.c.par.LIN_DNAMAGE<- bind_rows("NH Black"=nhb.edu.c.par.tab,"NH Hispanic"=h.edu.c.par.tab,"NH White"=nhw.edu.c.par.tab,.id='Subgroup')
write.csv(re.edu.c.par.LIN_DNAMAGE,file='re.edu.c.par.LIN_DNAMAGE.csv')

# Save ANOVA results
sink(file="re.aov.edu.c.LIN_DNAMAGEE.txt")
print(aov<-list(anova(nhb.edu.c.par),anova(h.edu.c.par),anova(nhw.edu.c.par)))
sink()

## Marginal Means
nhb.LIN_DNAMAGE.MM <- emmeans(nhb.edu.c.par,pairwise~RAEDYRS_factor*par_edu_bin,data=subset(svy.edaa,raceth=="NHB"),adjust='mvt')
h.LIN_DNAMAGE.MM <- emmeans(h.edu.c.par,pairwise~RAEDYRS_factor*par_edu_bin,data=subset(svy.edaa,raceth=="Hispanic"),adjust='mvt')
nhw.LIN_DNAMAGE.MM <- emmeans(nhw.edu.c.par,pairwise~RAEDYRS_factor*par_edu_bin,data=subset(svy.edaa,raceth=="NHW"),adjust='mvt')

## WEIDNER_DNAMAGE ----
# Unadjusted
edu.c.par <- svyglm(accweid~RAEDYRS_factor*par_edu_bin,design=svy.edaa)
summary(edu.c.par)
edu.c.par.tab <- tidy(edu.c.par) %>% mutate(LL=sprintf(confint(edu.c.par)[,1],fmt='%#.3f'),
                                              UL=sprintf(confint(edu.c.par)[,2],fmt='%#.3f')) %>%
                                       unite(col=CI,LL:UL, sep = ", ", remove=F)
                                       

# Adjusted
adj.edu.c.par <- svyglm(accweid~RAEDYRS_factor*par_edu_bin +
                                 raceth + 
                                 sex +
                                 scale(H13ITOT) + 
                                 smokestatus +
                                 R13BMI,design=svy.edaa)
summary(adj.edu.c.par)
adj.edu.c.par.tab <- tidy(adj.edu.c.par) %>% mutate(LL=sprintf(confint(adj.edu.c.par)[,1],fmt='%#.3f'),
                                               UL=sprintf(confint(adj.edu.c.par)[,2],fmt='%#.3f')) %>%
                                               unite(col=CI,LL:UL, sep = ", ", remove=F)
                                               
names(adj.edu.c.par.tab) <- gsub("^(.*?)","adj_\\2", names(adj.edu.c.par.tab))

# Combine results into one table
edu.c.par.WEIDNER_DNAMAGE <- merge(edu.c.par.tab,adj.edu.c.par.tab,by.x='term',by.y='adj_term',all.y=T,sort=F) 
write.csv(edu.c.par.WEIDNER_DNAMAGE,'edu.c.par.WEIDNER_DNAMAGE.csv')

# Save ANOVA results
sink(file="aov.edu.c.WEIDNER_DNAMAGE.txt")
print(aov<-list(anova(edu.c.par),anova(adj.edu.c.par)))
sink()

## Marginal Means
# Unadjusted
WEIDNER_DNAMAGE.MM <- emmeans(edu.c.par,pairwise~RAEDYRS_factor*par_edu_bin,data=svy.edaa,adjust='mvt')

# Adjusted
adj.WEIDNER_DNAMAGE.MM <- emmeans(adj.edu.c.par,pairwise~RAEDYRS_factor*par_edu_bin,data=svy.edaa,adjust='mvt')

## Stratify by race/ethnicity
# NH Black
nhb.edu.c.par <- svyglm(accweid~RAEDYRS_factor*par_edu_bin +
                                 sex +
                                 scale(H13ITOT) + 
                                 smokestatus +
                                 R13BMI,design=subset(svy.edaa,raceth=="NHB"))
summary(nhb.edu.c.par)
nhb.edu.c.par.tab <- tidy(nhb.edu.c.par) %>% mutate(LL=sprintf(confint(nhb.edu.c.par)[,1],fmt='%#.3f'),
                                                      UL=sprintf(confint(nhb.edu.c.par)[,2],fmt='%#.3f')) %>%
                                                      unite(col=CI,LL:UL, sep = ", ", remove=F)
                                                      
# Hispanic
h.edu.c.par <- svyglm(accweid~RAEDYRS_factor*par_edu_bin +
                               sex +
                               scale(H13ITOT) + 
                               smokestatus +
                               R13BMI,design=subset(svy.edaa,raceth=="Hispanic"))
summary(h.edu.c.par)
h.edu.c.par.tab <- tidy(h.edu.c.par) %>% mutate(LL=sprintf(confint(h.edu.c.par)[,1],fmt='%#.3f'),
                                                  UL=sprintf(confint(h.edu.c.par)[,2],fmt='%#.3f')) %>%
                                                  unite(col=CI,LL:UL, sep = ", ", remove=F)
                                                  
#NH White
nhw.edu.c.par <- svyglm(accweid~RAEDYRS_factor*par_edu_bin +
                                 sex +
                                 scale(H13ITOT) + 
                                 smokestatus +
                                 R13BMI,design=subset(svy.edaa,raceth=="NHW"))
summary(nhw.edu.c.par)
nhw.edu.c.par.tab <- tidy(nhw.edu.c.par) %>% mutate(LL=sprintf(confint(nhw.edu.c.par)[,1],fmt='%#.3f'),
                                                      UL=sprintf(confint(nhw.edu.c.par)[,2],fmt='%#.3f')) %>%
                                                      unite(col=CI,LL:UL, sep = ", ", remove=F)
                                                      

# Stack Columns 
re.edu.c.par.WEIDNER_DNAMAGE<- bind_rows("NH Black"=nhb.edu.c.par.tab,"NH Hispanic"=h.edu.c.par.tab,"NH White"=nhw.edu.c.par.tab,.id='Subgroup')
write.csv(re.edu.c.par.WEIDNER_DNAMAGE,file='re.edu.c.par.WEIDNER_DNAMAGE.csv')

# Save ANOVA results
sink(file="re.aov.edu.c.WEIDNER_DNAMAGE.txt")
print(aov<-list(anova(nhb.edu.c.par),anova(h.edu.c.par),anova(nhw.edu.c.par)))
sink()

## Marginal Means
nhb.WEIDNER_DNAMAGE.MM <- emmeans(nhb.edu.c.par,pairwise~RAEDYRS_factor*par_edu_bin,data=subset(svy.edaa,raceth=="NHB"),adjust='mvt')
h.WEIDNER_DNAMAGE.MM <- emmeans(h.edu.c.par,pairwise~RAEDYRS_factor*par_edu_bin,data=subset(svy.edaa,raceth=="Hispanic"),adjust='mvt')
nhw.WEIDNER_DNAMAGE.MM <- emmeans(nhw.edu.c.par,pairwise~RAEDYRS_factor*par_edu_bin,data=subset(svy.edaa,raceth=="NHW"),adjust='mvt')

## VIDALBRALO_DNAMAGE ----
# Unadjusted
edu.c.par <- svyglm(accvidalb~RAEDYRS_factor*par_edu_bin,design=svy.edaa)
summary(edu.c.par)
edu.c.par.tab <- tidy(edu.c.par) %>% mutate(LL=sprintf(confint(edu.c.par)[,1],fmt='%#.3f'),
                                              UL=sprintf(confint(edu.c.par)[,2],fmt='%#.3f')) %>%
                                       unite(col=CI,LL:UL, sep = ", ", remove=F)
                                       

# Adjusted
adj.edu.c.par <- svyglm(accvidalb~RAEDYRS_factor*par_edu_bin +
                                 raceth + 
                                 sex +
                                 scale(H13ITOT) + 
                                 smokestatus +
                                 R13BMI,design=svy.edaa)
summary(adj.edu.c.par)
adj.edu.c.par.tab <- tidy(adj.edu.c.par) %>% mutate(LL=sprintf(confint(adj.edu.c.par)[,1],fmt='%#.3f'),
                                               UL=sprintf(confint(adj.edu.c.par)[,2],fmt='%#.3f')) %>%
                                               unite(col=CI,LL:UL, sep = ", ", remove=F)
                                               
names(adj.edu.c.par.tab) <- gsub("^(.*?)","adj_\\2", names(adj.edu.c.par.tab))

# Combine results into one table
edu.c.par.VIDALBRALO_DNAMAGE <- merge(edu.c.par.tab,adj.edu.c.par.tab,by.x='term',by.y='adj_term',all.y=T,sort=F) 
write.csv(edu.c.par.VIDALBRALO_DNAMAGE,'edu.c.par.VIDALBRALO_DNAMAGE.csv')

# Save ANOVA results
sink(file="aov.edu.c.VIDALBRALO_DNAMAGE.txt")
print(aov<-list(anova(edu.c.par),anova(adj.edu.c.par)))
sink()

## Marginal Means
# Unadjusted
VIDALBRALO_DNAMAGE.MM <- emmeans(edu.c.par,pairwise~RAEDYRS_factor*par_edu_bin,data=svy.edaa,adjust='mvt')

# Adjusted
adj.VIDALBRALO_DNAMAGE.MM <- emmeans(adj.edu.c.par,pairwise~RAEDYRS_factor*par_edu_bin,data=svy.edaa,adjust='mvt')

## Stratify by race/ethnicity
# NH Black
nhb.edu.c.par <- svyglm(accvidalb~RAEDYRS_factor*par_edu_bin +
                                 sex +
                                 scale(H13ITOT) + 
                                 smokestatus +
                                 R13BMI,design=subset(svy.edaa,raceth=="NHB"))
summary(nhb.edu.c.par)
nhb.edu.c.par.tab <- tidy(nhb.edu.c.par) %>% mutate(LL=sprintf(confint(nhb.edu.c.par)[,1],fmt='%#.3f'),
                                                      UL=sprintf(confint(nhb.edu.c.par)[,2],fmt='%#.3f')) %>%
                                                      unite(col=CI,LL:UL, sep = ", ", remove=F)
                                                      
# Hispanic
h.edu.c.par <- svyglm(accvidalb~RAEDYRS_factor*par_edu_bin +
                               sex +
                               scale(H13ITOT) + 
                               smokestatus +
                               R13BMI,design=subset(svy.edaa,raceth=="Hispanic"))
summary(h.edu.c.par)
h.edu.c.par.tab <- tidy(h.edu.c.par) %>% mutate(LL=sprintf(confint(h.edu.c.par)[,1],fmt='%#.3f'),
                                                  UL=sprintf(confint(h.edu.c.par)[,2],fmt='%#.3f')) %>%
                                                  unite(col=CI,LL:UL, sep = ", ", remove=F)
                                                  
#NH White
nhw.edu.c.par <- svyglm(accvidalb~RAEDYRS_factor*par_edu_bin +
                                 sex +
                                 scale(H13ITOT) + 
                                 smokestatus +
                                 R13BMI,design=subset(svy.edaa,raceth=="NHW"))
summary(nhw.edu.c.par)
nhw.edu.c.par.tab <- tidy(nhw.edu.c.par) %>% mutate(LL=sprintf(confint(nhw.edu.c.par)[,1],fmt='%#.3f'),
                                                      UL=sprintf(confint(nhw.edu.c.par)[,2],fmt='%#.3f')) %>%
                                                      unite(col=CI,LL:UL, sep = ", ", remove=F)
                                                      

# Stack Columns 
re.edu.c.par.VIDALBRALO_DNAMAGE<- bind_rows("NH Black"=nhb.edu.c.par.tab,"NH Hispanic"=h.edu.c.par.tab,"NH White"=nhw.edu.c.par.tab,.id='Subgroup')
write.csv(re.edu.c.par.VIDALBRALO_DNAMAGE,file='re.edu.c.par.VIDALBRALO_DNAMAGE.csv')

# Save ANOVA results
sink(file="re.aov.edu.c.VIDALBRALO_DNAMAGE.txt")
print(aov<-list(anova(nhb.edu.c.par),anova(h.edu.c.par),anova(nhw.edu.c.par)))
sink()

## Marginal Means
nhb.VIDALBRALO_DNAMAGE.MM <- emmeans(nhb.edu.c.par,pairwise~RAEDYRS_factor*par_edu_bin,data=subset(svy.edaa,raceth=="NHB"),adjust='mvt')
h.VIDALBRALO_DNAMAGE.MM <- emmeans(h.edu.c.par,pairwise~RAEDYRS_factor*par_edu_bin,data=subset(svy.edaa,raceth=="Hispanic"),adjust='mvt')
nhw.VIDALBRALO_DNAMAGE.MM <- emmeans(nhw.edu.c.par,pairwise~RAEDYRS_factor*par_edu_bin,data=subset(svy.edaa,raceth=="NHW"),adjust='mvt')

## YANG_DNAMAGE ----
# Unadjusted
edu.c.par <- svyglm(accyang~RAEDYRS_factor*par_edu_bin,design=svy.edaa)
summary(edu.c.par)
edu.c.par.tab <- tidy(edu.c.par) %>% mutate(LL=sprintf(confint(edu.c.par)[,1],fmt='%#.3f'),
                                              UL=sprintf(confint(edu.c.par)[,2],fmt='%#.3f')) %>%
                                       unite(col=CI,LL:UL, sep = ", ", remove=F)
                                       

# Adjusted
adj.edu.c.par <- svyglm(accyang~RAEDYRS_factor*par_edu_bin +
                                 raceth + 
                                 sex +
                                 scale(H13ITOT) + 
                                 smokestatus +
                                 R13BMI,design=svy.edaa)
summary(adj.edu.c.par)
adj.edu.c.par.tab <- tidy(adj.edu.c.par) %>% mutate(LL=sprintf(confint(adj.edu.c.par)[,1],fmt='%#.3f'),
                                               UL=sprintf(confint(adj.edu.c.par)[,2],fmt='%#.3f')) %>%
                                               unite(col=CI,LL:UL, sep = ", ", remove=F)
                                               
names(adj.edu.c.par.tab) <- gsub("^(.*?)","adj_\\2", names(adj.edu.c.par.tab))

# Combine results into one table
edu.c.par.YANG_DNAMAGE <- merge(edu.c.par.tab,adj.edu.c.par.tab,by.x='term',by.y='adj_term',all.y=T,sort=F) 
write.csv(edu.c.par.YANG_DNAMAGE,'edu.c.par.YANG_DNAMAGE.csv')

# Save ANOVA results
sink(file="aov.edu.c.YANG_DNAMAGE.txt")
print(aov<-list(anova(edu.c.par),anova(adj.edu.c.par)))
sink()

## Marginal Means
# Unadjusted
YANG_DNAMAGE.MM <- emmeans(edu.c.par,pairwise~RAEDYRS_factor*par_edu_bin,data=svy.edaa,adjust='mvt')

# Adjusted
adj.YANG_DNAMAGE.MM <- emmeans(adj.edu.c.par,pairwise~RAEDYRS_factor*par_edu_bin,data=svy.edaa,adjust='mvt')

## Stratify by race/ethnicity
# NH Black
nhb.edu.c.par <- svyglm(accyang~RAEDYRS_factor*par_edu_bin +
                                 sex +
                                 scale(H13ITOT) + 
                                 smokestatus +
                                 R13BMI,design=subset(svy.edaa,raceth=="NHB"))
summary(nhb.edu.c.par)
nhb.edu.c.par.tab <- tidy(nhb.edu.c.par) %>% mutate(LL=sprintf(confint(nhb.edu.c.par)[,1],fmt='%#.3f'),
                                                      UL=sprintf(confint(nhb.edu.c.par)[,2],fmt='%#.3f')) %>%
                                                      unite(col=CI,LL:UL, sep = ", ", remove=F)
                                                      
# Hispanic
h.edu.c.par <- svyglm(accyang~RAEDYRS_factor*par_edu_bin +
                               sex +
                               scale(H13ITOT) + 
                               smokestatus +
                               R13BMI,design=subset(svy.edaa,raceth=="Hispanic"))
summary(h.edu.c.par)
h.edu.c.par.tab <- tidy(h.edu.c.par) %>% mutate(LL=sprintf(confint(h.edu.c.par)[,1],fmt='%#.3f'),
                                                  UL=sprintf(confint(h.edu.c.par)[,2],fmt='%#.3f')) %>%
                                                  unite(col=CI,LL:UL, sep = ", ", remove=F)
                                                  
#NH White
nhw.edu.c.par <- svyglm(accyang~RAEDYRS_factor*par_edu_bin +
                                 sex +
                                 scale(H13ITOT) + 
                                 smokestatus +
                                 R13BMI,design=subset(svy.edaa,raceth=="NHW"))
summary(nhw.edu.c.par)
nhw.edu.c.par.tab <- tidy(nhw.edu.c.par) %>% mutate(LL=sprintf(confint(nhw.edu.c.par)[,1],fmt='%#.3f'),
                                                      UL=sprintf(confint(nhw.edu.c.par)[,2],fmt='%#.3f')) %>%
                                                      unite(col=CI,LL:UL, sep = ", ", remove=F)
                                                      

# Stack Columns 
re.edu.c.par.YANG_DNAMAGE<- bind_rows("NH Black"=nhb.edu.c.par.tab,"NH Hispanic"=h.edu.c.par.tab,"NH White"=nhw.edu.c.par.tab,.id='Subgroup')
write.csv(re.edu.c.par.YANG_DNAMAGE,file='re.edu.c.par.YANG_DNAMAGE.csv')

# Save ANOVA results
sink(file="re.aov.edu.c.YANG_DNAMAGE.txt")
print(aov<-list(anova(nhb.edu.c.par),anova(h.edu.c.par),anova(nhw.edu.c.par)))
sink()

## Marginal Means
nhb.YANG_DNAMAGE.MM <- emmeans(nhb.edu.c.par,pairwise~RAEDYRS_factor*par_edu_bin,data=subset(svy.edaa,raceth=="NHB"),adjust='mvt')
h.YANG_DNAMAGE.MM <- emmeans(h.edu.c.par,pairwise~RAEDYRS_factor*par_edu_bin,data=subset(svy.edaa,raceth=="Hispanic"),adjust='mvt')
nhw.YANG_DNAMAGE.MM <- emmeans(nhw.edu.c.par,pairwise~RAEDYRS_factor*par_edu_bin,data=subset(svy.edaa,raceth=="NHW"),adjust='mvt')

## ZHANG_DNAMAGE ----
# Unadjusted
edu.c.par <- svyglm(acczhang~RAEDYRS_factor*par_edu_bin,design=svy.edaa)
summary(edu.c.par)
edu.c.par.tab <- tidy(edu.c.par) %>% mutate(LL=sprintf(confint(edu.c.par)[,1],fmt='%#.3f'),
                                              UL=sprintf(confint(edu.c.par)[,2],fmt='%#.3f')) %>%
                                       unite(col=CI,LL:UL, sep = ", ", remove=F)
                                       

# Adjusted
adj.edu.c.par <- svyglm(acczhang~RAEDYRS_factor*par_edu_bin +
                                 raceth + 
                                 sex +
                                 scale(H13ITOT) + 
                                 smokestatus +
                                 R13BMI,design=svy.edaa)
summary(adj.edu.c.par)
adj.edu.c.par.tab <- tidy(adj.edu.c.par) %>% mutate(LL=sprintf(confint(adj.edu.c.par)[,1],fmt='%#.3f'),
                                               UL=sprintf(confint(adj.edu.c.par)[,2],fmt='%#.3f')) %>%
                                               unite(col=CI,LL:UL, sep = ", ", remove=F)
                                               
names(adj.edu.c.par.tab) <- gsub("^(.*?)","adj_\\2", names(adj.edu.c.par.tab))

# Combine results into one table
edu.c.par.ZHANG_DNAMAGE <- merge(edu.c.par.tab,adj.edu.c.par.tab,by.x='term',by.y='adj_term',all.y=T,sort=F) 
write.csv(edu.c.par.ZHANG_DNAMAGE,'edu.c.par.ZHANG_DNAMAGE.csv')

# Save ANOVA results
sink(file="aov.edu.c.ZHANG_DNAMAGE.txt")
print(aov<-list(anova(edu.c.par),anova(adj.edu.c.par)))
sink()

## Marginal Means
# Unadjusted
ZHANG_DNAMAGE.MM <- emmeans(edu.c.par,pairwise~RAEDYRS_factor*par_edu_bin,data=svy.edaa,adjust='mvt')

# Adjusted
adj.ZHANG_DNAMAGE.MM <- emmeans(adj.edu.c.par,pairwise~RAEDYRS_factor*par_edu_bin,data=svy.edaa,adjust='mvt')

## Stratify by race/ethnicity
# NH Black
nhb.edu.c.par <- svyglm(acczhang~RAEDYRS_factor*par_edu_bin +
                                 sex +
                                 scale(H13ITOT) + 
                                 smokestatus +
                                 R13BMI,design=subset(svy.edaa,raceth=="NHB"))
summary(nhb.edu.c.par)
nhb.edu.c.par.tab <- tidy(nhb.edu.c.par) %>% mutate(LL=sprintf(confint(nhb.edu.c.par)[,1],fmt='%#.3f'),
                                                      UL=sprintf(confint(nhb.edu.c.par)[,2],fmt='%#.3f')) %>%
                                                      unite(col=CI,LL:UL, sep = ", ", remove=F)
                                                      
# Hispanic
h.edu.c.par <- svyglm(acczhang~RAEDYRS_factor*par_edu_bin +
                               sex +
                               scale(H13ITOT) + 
                               smokestatus +
                               R13BMI,design=subset(svy.edaa,raceth=="Hispanic"))
summary(h.edu.c.par)
h.edu.c.par.tab <- tidy(h.edu.c.par) %>% mutate(LL=sprintf(confint(h.edu.c.par)[,1],fmt='%#.3f'),
                                                  UL=sprintf(confint(h.edu.c.par)[,2],fmt='%#.3f')) %>%
                                                  unite(col=CI,LL:UL, sep = ", ", remove=F)
                                                  
#NH White
nhw.edu.c.par <- svyglm(acczhang~RAEDYRS_factor*par_edu_bin +
                                 sex +
                                 scale(H13ITOT) + 
                                 smokestatus +
                                 R13BMI,design=subset(svy.edaa,raceth=="NHW"))
summary(nhw.edu.c.par)
nhw.edu.c.par.tab <- tidy(nhw.edu.c.par) %>% mutate(LL=sprintf(confint(nhw.edu.c.par)[,1],fmt='%#.3f'),
                                                      UL=sprintf(confint(nhw.edu.c.par)[,2],fmt='%#.3f')) %>%
                                                      unite(col=CI,LL:UL, sep = ", ", remove=F)
                                                      

# Stack Columns 
re.edu.c.par.ZHANG_DNAMAGE<- bind_rows("NH Black"=nhb.edu.c.par.tab,"NH Hispanic"=h.edu.c.par.tab,"NH White"=nhw.edu.c.par.tab,.id='Subgroup')
write.csv(re.edu.c.par.ZHANG_DNAMAGE,file='re.edu.c.par.ZHANG_DNAMAGE.csv')

# Save ANOVA results
sink(file="re.aov.edu.c.ZHANG_DNAMAGE.txt")
print(aov<-list(anova(nhb.edu.c.par),anova(h.edu.c.par),anova(nhw.edu.c.par)))
sink()

## Marginal Means
nhb.ZHANG_DNAMAGE.MM <- emmeans(nhb.edu.c.par,pairwise~RAEDYRS_factor*par_edu_bin,data=subset(svy.edaa,raceth=="NHB"),adjust='mvt')
h.ZHANG_DNAMAGE.MM <- emmeans(h.edu.c.par,pairwise~RAEDYRS_factor*par_edu_bin,data=subset(svy.edaa,raceth=="Hispanic"),adjust='mvt')
nhw.ZHANG_DNAMAGE.MM <- emmeans(nhw.edu.c.par,pairwise~RAEDYRS_factor*par_edu_bin,data=subset(svy.edaa,raceth=="NHW"),adjust='mvt')

## BOCKLANDT_DNAMAGE ----
# Unadjusted
edu.c.par <- svyglm(accbock~RAEDYRS_factor*par_edu_bin,design=svy.edaa)
summary(edu.c.par)
edu.c.par.tab <- tidy(edu.c.par) %>% mutate(LL=sprintf(confint(edu.c.par)[,1],fmt='%#.3f'),
                                              UL=sprintf(confint(edu.c.par)[,2],fmt='%#.3f')) %>%
                                       unite(col=CI,LL:UL, sep = ", ", remove=F)
                                       

# Adjusted
adj.edu.c.par <- svyglm(accbock~RAEDYRS_factor*par_edu_bin +
                                 raceth + 
                                 sex +
                                 scale(H13ITOT) + 
                                 smokestatus +
                                 R13BMI,design=svy.edaa)
summary(adj.edu.c.par)
adj.edu.c.par.tab <- tidy(adj.edu.c.par) %>% mutate(LL=sprintf(confint(adj.edu.c.par)[,1],fmt='%#.3f'),
                                               UL=sprintf(confint(adj.edu.c.par)[,2],fmt='%#.3f')) %>%
                                               unite(col=CI,LL:UL, sep = ", ", remove=F)
                                               
names(adj.edu.c.par.tab) <- gsub("^(.*?)","adj_\\2", names(adj.edu.c.par.tab))

# Combine results into one table
edu.c.par.BOCKLANDT_DNAMAGE <- merge(edu.c.par.tab,adj.edu.c.par.tab,by.x='term',by.y='adj_term',all.y=T,sort=F) 
write.csv(edu.c.par.BOCKLANDT_DNAMAGE,'edu.c.par.BOCKLANDT_DNAMAGE.csv')

# Save ANOVA results
sink(file="aov.edu.c.BOCKLANDT_DNAMAGE.txt")
print(aov<-list(anova(edu.c.par),anova(adj.edu.c.par)))
sink()

## Marginal Means
# Unadjusted
BOCKLANDT_DNAMAGE.MM <- emmeans(edu.c.par,pairwise~RAEDYRS_factor*par_edu_bin,data=svy.edaa,adjust='mvt')

# Adjusted
adj.BOCKLANDT_DNAMAGE.MM <- emmeans(adj.edu.c.par,pairwise~RAEDYRS_factor*par_edu_bin,data=svy.edaa,adjust='mvt')

## Stratify by race/ethnicity
# NH Black
nhb.edu.c.par <- svyglm(accbock~RAEDYRS_factor*par_edu_bin +
                                 sex +
                                 scale(H13ITOT) + 
                                 smokestatus +
                                 R13BMI,design=subset(svy.edaa,raceth=="NHB"))
summary(nhb.edu.c.par)
nhb.edu.c.par.tab <- tidy(nhb.edu.c.par) %>% mutate(LL=sprintf(confint(nhb.edu.c.par)[,1],fmt='%#.3f'),
                                                      UL=sprintf(confint(nhb.edu.c.par)[,2],fmt='%#.3f')) %>%
                                                      unite(col=CI,LL:UL, sep = ", ", remove=F)
                                                      
# Hispanic
h.edu.c.par <- svyglm(accbock~RAEDYRS_factor*par_edu_bin +
                               sex +
                               scale(H13ITOT) + 
                               smokestatus +
                               R13BMI,design=subset(svy.edaa,raceth=="Hispanic"))
summary(h.edu.c.par)
h.edu.c.par.tab <- tidy(h.edu.c.par) %>% mutate(LL=sprintf(confint(h.edu.c.par)[,1],fmt='%#.3f'),
                                                  UL=sprintf(confint(h.edu.c.par)[,2],fmt='%#.3f')) %>%
                                                  unite(col=CI,LL:UL, sep = ", ", remove=F)
                                                  
#NH White
nhw.edu.c.par <- svyglm(accbock~RAEDYRS_factor*par_edu_bin +
                                 sex +
                                 scale(H13ITOT) + 
                                 smokestatus +
                                 R13BMI,design=subset(svy.edaa,raceth=="NHW"))
summary(nhw.edu.c.par)
nhw.edu.c.par.tab <- tidy(nhw.edu.c.par) %>% mutate(LL=sprintf(confint(nhw.edu.c.par)[,1],fmt='%#.3f'),
                                                      UL=sprintf(confint(nhw.edu.c.par)[,2],fmt='%#.3f')) %>%
                                                      unite(col=CI,LL:UL, sep = ", ", remove=F)
                                                      

# Stack Columns 
re.edu.c.par.BOCKLANDT_DNAMAGE<- bind_rows("NH Black"=nhb.edu.c.par.tab,"NH Hispanic"=h.edu.c.par.tab,"NH White"=nhw.edu.c.par.tab,.id='Subgroup')
write.csv(re.edu.c.par.BOCKLANDT_DNAMAGE,file='re.edu.c.par.BOCKLANDT_DNAMAGE.csv')

# Save ANOVA results
sink(file="re.aov.edu.c.BOCKLANDT_DNAMAGE.txt")
print(aov<-list(anova(nhb.edu.c.par),anova(h.edu.c.par),anova(nhw.edu.c.par)))
sink()

## Marginal Means
nhb.BOCKLANDT_DNAMAGE.MM <- emmeans(nhb.edu.c.par,pairwise~RAEDYRS_factor*par_edu_bin,data=subset(svy.edaa,raceth=="NHB"),adjust='mvt')
h.BOCKLANDT_DNAMAGE.MM <- emmeans(h.edu.c.par,pairwise~RAEDYRS_factor*par_edu_bin,data=subset(svy.edaa,raceth=="Hispanic"),adjust='mvt')
nhw.BOCKLANDT_DNAMAGE.MM <- emmeans(nhw.edu.c.par,pairwise~RAEDYRS_factor*par_edu_bin,data=subset(svy.edaa,raceth=="NHW"),adjust='mvt')

## GARAGNANI_DNAMAGE ----
# Unadjusted
edu.c.par <- svyglm(accgarag~RAEDYRS_factor*par_edu_bin,design=svy.edaa)
summary(edu.c.par)
edu.c.par.tab <- tidy(edu.c.par) %>% mutate(LL=sprintf(confint(edu.c.par)[,1],fmt='%#.3f'),
                                              UL=sprintf(confint(edu.c.par)[,2],fmt='%#.3f')) %>%
                                       unite(col=CI,LL:UL, sep = ", ", remove=F)
                                       

# Adjusted
adj.edu.c.par <- svyglm(accgarag~RAEDYRS_factor*par_edu_bin +
                                 raceth + 
                                 sex +
                                 scale(H13ITOT) + 
                                 smokestatus +
                                 R13BMI,design=svy.edaa)
summary(adj.edu.c.par)
adj.edu.c.par.tab <- tidy(adj.edu.c.par) %>% mutate(LL=sprintf(confint(adj.edu.c.par)[,1],fmt='%#.3f'),
                                               UL=sprintf(confint(adj.edu.c.par)[,2],fmt='%#.3f')) %>%
                                               unite(col=CI,LL:UL, sep = ", ", remove=F)
                                               
names(adj.edu.c.par.tab) <- gsub("^(.*?)","adj_\\2", names(adj.edu.c.par.tab))

# Combine results into one table
edu.c.par.GARAGNANI_DNAMAGE <- merge(edu.c.par.tab,adj.edu.c.par.tab,by.x='term',by.y='adj_term',all.y=T,sort=F) 
write.csv(edu.c.par.GARAGNANI_DNAMAGE,'edu.c.par.GARAGNANI_DNAMAGE.csv')

# Save ANOVA results
sink(file="aov.edu.c.GARAGNANI_DNAMAGE.txt")
print(aov<-list(anova(edu.c.par),anova(adj.edu.c.par)))
sink()

## Marginal Means
# Unadjusted
GARAGNANI_DNAMAGE.MM <- emmeans(edu.c.par,pairwise~RAEDYRS_factor*par_edu_bin,data=svy.edaa,adjust='mvt')

# Adjusted
adj.GARAGNANI_DNAMAGE.MM <- emmeans(adj.edu.c.par,pairwise~RAEDYRS_factor*par_edu_bin,data=svy.edaa,adjust='mvt')

## Stratify by race/ethnicity
# NH Black
nhb.edu.c.par <- svyglm(accgarag~RAEDYRS_factor*par_edu_bin +
                                 sex +
                                 scale(H13ITOT) + 
                                 smokestatus +
                                 R13BMI,design=subset(svy.edaa,raceth=="NHB"))
summary(nhb.edu.c.par)
nhb.edu.c.par.tab <- tidy(nhb.edu.c.par) %>% mutate(LL=sprintf(confint(nhb.edu.c.par)[,1],fmt='%#.3f'),
                                                      UL=sprintf(confint(nhb.edu.c.par)[,2],fmt='%#.3f')) %>%
                                                      unite(col=CI,LL:UL, sep = ", ", remove=F)
                                                      
# Hispanic
h.edu.c.par <- svyglm(accgarag~RAEDYRS_factor*par_edu_bin +
                               sex +
                               scale(H13ITOT) + 
                               smokestatus +
                               R13BMI,design=subset(svy.edaa,raceth=="Hispanic"))
summary(h.edu.c.par)
h.edu.c.par.tab <- tidy(h.edu.c.par) %>% mutate(LL=sprintf(confint(h.edu.c.par)[,1],fmt='%#.3f'),
                                                  UL=sprintf(confint(h.edu.c.par)[,2],fmt='%#.3f')) %>%
                                                  unite(col=CI,LL:UL, sep = ", ", remove=F)
                                                  
#NH White
nhw.edu.c.par <- svyglm(accgarag~RAEDYRS_factor*par_edu_bin +
                                 sex +
                                 scale(H13ITOT) + 
                                 smokestatus +
                                 R13BMI,design=subset(svy.edaa,raceth=="NHW"))
summary(nhw.edu.c.par)
nhw.edu.c.par.tab <- tidy(nhw.edu.c.par) %>% mutate(LL=sprintf(confint(nhw.edu.c.par)[,1],fmt='%#.3f'),
                                                      UL=sprintf(confint(nhw.edu.c.par)[,2],fmt='%#.3f')) %>%
                                                      unite(col=CI,LL:UL, sep = ", ", remove=F)
                                                      

# Stack Columns 
re.edu.c.par.GARAGNANI_DNAMAGE<- bind_rows("NH Black"=nhb.edu.c.par.tab,"NH Hispanic"=h.edu.c.par.tab,"NH White"=nhw.edu.c.par.tab,.id='Subgroup')
write.csv(re.edu.c.par.GARAGNANI_DNAMAGE,file='re.edu.c.par.GARAGNANI_DNAMAGE.csv')

# Save ANOVA results
sink(file="re.aov.edu.c.GARAGNANI_DNAMAGE.txt")
print(aov<-list(anova(nhb.edu.c.par),anova(h.edu.c.par),anova(nhw.edu.c.par)))
sink()

## Marginal Means
nhb.GARAGNANI_DNAMAGE.MM <- emmeans(nhb.edu.c.par,pairwise~RAEDYRS_factor*par_edu_bin,data=subset(svy.edaa,raceth=="NHB"),adjust='mvt')
h.GARAGNANI_DNAMAGE.MM <- emmeans(h.edu.c.par,pairwise~RAEDYRS_factor*par_edu_bin,data=subset(svy.edaa,raceth=="Hispanic"),adjust='mvt')
nhw.GARAGNANI_DNAMAGE.MM <- emmeans(nhw.edu.c.par,pairwise~RAEDYRS_factor*par_edu_bin,data=subset(svy.edaa,raceth=="NHW"),adjust='mvt')

## DNAMGRIMAGE ----
# Unadjusted
edu.c.par <- svyglm(accdnam~RAEDYRS_factor*par_edu_bin,design=svy.edaa)
summary(edu.c.par)
edu.c.par.tab <- tidy(edu.c.par) %>% mutate(LL=sprintf(confint(edu.c.par)[,1],fmt='%#.3f'),
                                              UL=sprintf(confint(edu.c.par)[,2],fmt='%#.3f')) %>%
                                       unite(col=CI,LL:UL, sep = ", ", remove=F)
                                       

# Adjusted
adj.edu.c.par <- svyglm(accdnam~RAEDYRS_factor*par_edu_bin +
                                 raceth + 
                                 sex +
                                 scale(H13ITOT) + 
                                 smokestatus +
                                 R13BMI,design=svy.edaa)
summary(adj.edu.c.par)
adj.edu.c.par.tab <- tidy(adj.edu.c.par) %>% mutate(LL=sprintf(confint(adj.edu.c.par)[,1],fmt='%#.3f'),
                                               UL=sprintf(confint(adj.edu.c.par)[,2],fmt='%#.3f')) %>%
                                               unite(col=CI,LL:UL, sep = ", ", remove=F)
                                               
names(adj.edu.c.par.tab) <- gsub("^(.*?)","adj_\\2", names(adj.edu.c.par.tab))

# Combine results into one table
edu.c.par.DNAMGRIMAGE <- merge(edu.c.par.tab,adj.edu.c.par.tab,by.x='term',by.y='adj_term',all.y=T,sort=F) 
write.csv(edu.c.par.DNAMGRIMAGE,'edu.c.par.DNAMGRIMAGE.csv')

# Save ANOVA results
sink(file="aov.edu.c.DNAMGRIMAGE.txt")
print(aov<-list(anova(edu.c.par),anova(adj.edu.c.par)))
sink()

## Marginal Means
# Unadjusted
DNAMGRIMAGE.MM <- emmeans(edu.c.par,pairwise~RAEDYRS_factor*par_edu_bin,data=svy.edaa,adjust='mvt')

# Adjusted
adj.DNAMGRIMAGE.MM <- emmeans(adj.edu.c.par,pairwise~RAEDYRS_factor*par_edu_bin,data=svy.edaa,adjust='mvt')

## Stratify by race/ethnicity
# NH Black
nhb.edu.c.par <- svyglm(accdnam~RAEDYRS_factor*par_edu_bin +
                                 sex +
                                 scale(H13ITOT) + 
                                 smokestatus +
                                 R13BMI,design=subset(svy.edaa,raceth=="NHB"))
summary(nhb.edu.c.par)
nhb.edu.c.par.tab <- tidy(nhb.edu.c.par) %>% mutate(LL=sprintf(confint(nhb.edu.c.par)[,1],fmt='%#.3f'),
                                                      UL=sprintf(confint(nhb.edu.c.par)[,2],fmt='%#.3f')) %>%
                                                      unite(col=CI,LL:UL, sep = ", ", remove=F)
                                                      
# Hispanic
h.edu.c.par <- svyglm(accdnam~RAEDYRS_factor*par_edu_bin +
                               sex +
                               scale(H13ITOT) + 
                               smokestatus +
                               R13BMI,design=subset(svy.edaa,raceth=="Hispanic"))
summary(h.edu.c.par)
h.edu.c.par.tab <- tidy(h.edu.c.par) %>% mutate(LL=sprintf(confint(h.edu.c.par)[,1],fmt='%#.3f'),
                                                  UL=sprintf(confint(h.edu.c.par)[,2],fmt='%#.3f')) %>%
                                                  unite(col=CI,LL:UL, sep = ", ", remove=F)
                                                  
#NH White
nhw.edu.c.par <- svyglm(accdnam~RAEDYRS_factor*par_edu_bin +
                                 sex +
                                 scale(H13ITOT) + 
                                 smokestatus +
                                 R13BMI,design=subset(svy.edaa,raceth=="NHW"))
summary(nhw.edu.c.par)
nhw.edu.c.par.tab <- tidy(nhw.edu.c.par) %>% mutate(LL=sprintf(confint(nhw.edu.c.par)[,1],fmt='%#.3f'),
                                                      UL=sprintf(confint(nhw.edu.c.par)[,2],fmt='%#.3f')) %>%
                                                      unite(col=CI,LL:UL, sep = ", ", remove=F)
                                                      

# Stack Columns 
re.edu.c.par.DNAMGRIMAGE<- bind_rows("NH Black"=nhb.edu.c.par.tab,"NH Hispanic"=h.edu.c.par.tab,"NH White"=nhw.edu.c.par.tab,.id='Subgroup')
write.csv(re.edu.c.par.DNAMGRIMAGE,file='re.edu.c.par.DNAMGRIMAGE.csv')

# Save ANOVA results
sink(file="re.aov.edu.c.DNAMGRIMAGE.txt")
print(aov<-list(anova(nhb.edu.c.par),anova(h.edu.c.par),anova(nhw.edu.c.par)))
sink()

## Marginal Means
nhb.DNAMGRIMAGE_DNAMAGE.MM <- emmeans(nhb.edu.c.par,pairwise~RAEDYRS_factor*par_edu_bin,data=subset(svy.edaa,raceth=="NHB"),adjust='mvt')
h.DNAMGRIMAGE_DNAMAGE.MM <- emmeans(h.edu.c.par,pairwise~RAEDYRS_factor*par_edu_bin,data=subset(svy.edaa,raceth=="Hispanic"),adjust='mvt')
nhw.DNAMGRIMAGE_DNAMAGE.MM <- emmeans(nhw.edu.c.par,pairwise~RAEDYRS_factor*par_edu_bin,data=subset(svy.edaa,raceth=="NHW"),adjust='mvt')

## MPOA ----
# Unadjusted
edu.c.par <- svyglm(accmpoa~RAEDYRS_factor*par_edu_bin,design=svy.edaa)
summary(edu.c.par)
edu.c.par.tab <- tidy(edu.c.par) %>% mutate(LL=sprintf(confint(edu.c.par)[,1],fmt='%#.3f'),
                                              UL=sprintf(confint(edu.c.par)[,2],fmt='%#.3f')) %>%
                                       unite(col=CI,LL:UL, sep = ", ", remove=F)
                                       

# Adjusted
adj.edu.c.par <- svyglm(accmpoa~RAEDYRS_factor*par_edu_bin +
                                 raceth + 
                                 sex +
                                 scale(H13ITOT) + 
                                 smokestatus +
                                 R13BMI,design=svy.edaa)
summary(adj.edu.c.par)
adj.edu.c.par.tab <- tidy(adj.edu.c.par) %>% mutate(LL=sprintf(confint(adj.edu.c.par)[,1],fmt='%#.3f'),
                                               UL=sprintf(confint(adj.edu.c.par)[,2],fmt='%#.3f')) %>%
                                               unite(col=CI,LL:UL, sep = ", ", remove=F)
                                               
names(adj.edu.c.par.tab) <- gsub("^(.*?)","adj_\\2", names(adj.edu.c.par.tab))

# Combine results into one table
edu.c.par.MPOA<- merge(edu.c.par.tab,adj.edu.c.par.tab,by.x='term',by.y='adj_term',all.y=T,sort=F) 
write.csv(edu.c.par.MPOA,'edu.c.par.MPOA.csv')

# Save ANOVA results
sink(file="aov.edu.c.MPOA.txt")
print(aov<-list(anova(edu.c.par),anova(adj.edu.c.par)))
sink()

## Marginal Means
# Unadjusted
MPOA.MM <- emmeans(edu.c.par,pairwise~RAEDYRS_factor*par_edu_bin,data=svy.edaa,adjust='mvt')

# Adjusted
adj.MPOA.MM <- emmeans(adj.edu.c.par,pairwise~RAEDYRS_factor*par_edu_bin,data=svy.edaa,adjust='mvt')

## Stratify by race/ethnicity
# NH Black
nhb.edu.c.par <- svyglm(accmpoa~RAEDYRS_factor*par_edu_bin +
                                 sex +
                                 scale(H13ITOT) + 
                                 smokestatus +
                                 R13BMI,design=subset(svy.edaa,raceth=="NHB"))
summary(nhb.edu.c.par)
nhb.edu.c.par.tab <- tidy(nhb.edu.c.par) %>% mutate(LL=sprintf(confint(nhb.edu.c.par)[,1],fmt='%#.3f'),
                                                      UL=sprintf(confint(nhb.edu.c.par)[,2],fmt='%#.3f')) %>%
                                                      unite(col=CI,LL:UL, sep = ", ", remove=F)
                                                      
# Hispanic
h.edu.c.par <- svyglm(accmpoa~RAEDYRS_factor*par_edu_bin +
                               sex +
                               scale(H13ITOT) + 
                               smokestatus +
                               R13BMI,design=subset(svy.edaa,raceth=="Hispanic"))
summary(h.edu.c.par)
h.edu.c.par.tab <- tidy(h.edu.c.par) %>% mutate(LL=sprintf(confint(h.edu.c.par)[,1],fmt='%#.3f'),
                                                  UL=sprintf(confint(h.edu.c.par)[,2],fmt='%#.3f')) %>%
                                                  unite(col=CI,LL:UL, sep = ", ", remove=F)
                                                  
#NH White
nhw.edu.c.par <- svyglm(accmpoa~RAEDYRS_factor*par_edu_bin +
                                 sex +
                                 scale(H13ITOT) + 
                                 smokestatus +
                                 R13BMI,design=subset(svy.edaa,raceth=="NHW"))
summary(nhw.edu.c.par)
nhw.edu.c.par.tab <- tidy(nhw.edu.c.par) %>% mutate(LL=sprintf(confint(nhw.edu.c.par)[,1],fmt='%#.3f'),
                                                      UL=sprintf(confint(nhw.edu.c.par)[,2],fmt='%#.3f')) %>%
                                                      unite(col=CI,LL:UL, sep = ", ", remove=F)
                                                      

# Stack Columns 
re.edu.c.par.MPOA<- bind_rows("NH Black"=nhb.edu.c.par.tab,"NH Hispanic"=h.edu.c.par.tab,"NH White"=nhw.edu.c.par.tab,.id='Subgroup')
write.csv(re.edu.c.par.MPOA,file='re.edu.c.par.MPOA.csv')

# Save ANOVA results
sink(file="re.aov.edu.c.MPOA.txt")
print(aov<-list(anova(nhb.edu.c.par),anova(h.edu.c.par),anova(nhw.edu.c.par)))
sink()

## Marginal Means
nhb.MPOA_DNAMAGE.MM <- emmeans(nhb.edu.c.par,pairwise~RAEDYRS_factor*par_edu_bin,data=subset(svy.edaa,raceth=="NHB"),adjust='mvt')
h.MPOA_DNAMAGE.MM <- emmeans(h.edu.c.par,pairwise~RAEDYRS_factor*par_edu_bin,data=subset(svy.edaa,raceth=="Hispanic"),adjust='mvt')
nhw.MPOA_NAMAGE.MM <- emmeans(nhw.edu.c.par,pairwise~RAEDYRS_factor*par_edu_bin,data=subset(svy.edaa,raceth=="NHW"),adjust='mvt')

## Save marginal means ----
save(HORVATH_DNAMAGE.MM,adj.HORVATH_DNAMAGE.MM,HANNUM_DNAMAGE.MM,adj.HANNUM_DNAMAGE.MM,
     LEVINE_DNAMAGE.MM,adj.LEVINE_DNAMAGE.MM,HORVATHSKIN_DNAMAGE.MM,adj.HORVATHSKIN_DNAMAGE.MM,
     LIN_DNAMAGE.MM,adj.LIN_DNAMAGE.MM,WEIDNER_DNAMAGE.MM,adj.WEIDNER_DNAMAGE.MM,
     VIDALBRALO_DNAMAGE.MM,adj.VIDALBRALO_DNAMAGE.MM,YANG_DNAMAGE.MM,adj.YANG_DNAMAGE.MM,
     ZHANG_DNAMAGE.MM,adj.ZHANG_DNAMAGE.MM,BOCKLANDT_DNAMAGE.MM,adj.BOCKLANDT_DNAMAGE.MM,
     GARAGNANI_DNAMAGE.MM,adj.GARAGNANI_DNAMAGE.MM,DNAMGRIMAGE.MM,adj.DNAMGRIMAGE.MM,
     MPOA.MM,adj.MPOA.MM,file='Parental_MMs.RData')

save(nhb.HORVATH_DNAMAGE.MM,h.HORVATH_DNAMAGE.MM,nhw.HORVATH_DNAMAGE.MM,
     nhb.HANNUM_DNAMAGE.MM,h.HANNUM_DNAMAGE.MM,nhw.HANNUM_DNAMAGE.MM,
     nhb.LEVINE_DNAMAGE.MM,h.LEVINE_DNAMAGE.MM,nhw.LEVINE_DNAMAGE.MM,
     nhb.HORVATHSKIN_DNAMAGE.MM,h.HORVATHSKIN_DNAMAGE.MM,nhw.HORVATHSKIN_DNAMAGE.MM,
     nhb.LIN_DNAMAGE.MM,h.LIN_DNAMAGE.MM,nhw.LIN_DNAMAGE.MM,
     nhb.WEIDNER_DNAMAGE.MM,h.WEIDNER_DNAMAGE.MM,nhw.WEIDNER_DNAMAGE.MM,
     nhb.VIDALBRALO_DNAMAGE.MM,h.VIDALBRALO_DNAMAGE.MM,nhw.VIDALBRALO_DNAMAGE.MM,
     nhb.YANG_DNAMAGE.MM,h.YANG_DNAMAGE.MM,nhw.YANG_DNAMAGE.MM,
     nhb.ZHANG_DNAMAGE.MM,h.ZHANG_DNAMAGE.MM,nhw.ZHANG_DNAMAGE.MM,
     nhb.BOCKLANDT_DNAMAGE.MM,h.BOCKLANDT_DNAMAGE.MM,nhw.BOCKLANDT_DNAMAGE.MM,
     nhb.GARAGNANI_DNAMAGE.MM,h.GARAGNANI_DNAMAGE.MM,nhw.GARAGNANI_DNAMAGE.MM,
     nhb.DNAMGRIMAGE_DNAMAGE.MM,h.DNAMGRIMAGE_DNAMAGE.MM,nhw.DNAMGRIMAGE_DNAMAGE.MM,
     nhb.MPOA_DNAMAGE.MM,h.MPOA_DNAMAGE.MM,nhw.MPOA_NAMAGE.MM,file='RE_Parental_MMs.RData')

### Regression Models (Father Years of Education) ----
## HORVATH_DNAMAGE ----
# Unadjusted
p.edu.yr.par <- svyglm(acchorv~edu.c*fedu.c,design=svy.edaa)
summary(p.edu.yr.par)
p.edu.yr.par.tab <- tidy(p.edu.yr.par) %>% mutate(LL=sprintf(confint(p.edu.yr.par)[,1],fmt='%#.3f'),
                                              UL=sprintf(confint(p.edu.yr.par)[,2],fmt='%#.3f')) %>%
  unite(col=CI,LL:UL, sep = ", ", remove=F)
  

# Adjusted
adj.p.edu.yr.par <- svyglm(acchorv~edu.c*fedu.c +
                           raceth + 
                           sex +
                           scale(H13ITOT) + 
                           smokestatus +
                           R13BMI,design=svy.edaa)
summary(adj.p.edu.yr.par)
adj.p.edu.yr.par.tab <- tidy(adj.p.edu.yr.par) %>% mutate(LL=sprintf(confint(adj.p.edu.yr.par)[,1],fmt='%#.3f'),
                                                      UL=sprintf(confint(adj.p.edu.yr.par)[,2],fmt='%#.3f')) %>%
  unite(col=CI,LL:UL, sep = ", ", remove=F)
  
names(adj.p.edu.yr.par.tab) <- gsub("^(.*?)","adj_\\2", names(adj.p.edu.yr.par.tab))

# Combine results into one table
p.edu.yr.par.HORVATH_DNAMAGE <- merge(p.edu.yr.par.tab,adj.p.edu.yr.par.tab,by.x='term',by.y='adj_term',all.y=T,sort=F) 
write.csv(p.edu.yr.par.HORVATH_DNAMAGE,'p.edu.yr.par.HORVATH_DNAMAGE.csv')

## Stratify by race/ethnicity
# NH Black
nhb.p.edu.yr.par <- svyglm(acchorv~edu.c*fedu.c +
                           sex +
                           scale(H13ITOT) + 
                           smokestatus +
                           R13BMI,design=subset(svy.edaa,raceth=="NHB"))
summary(nhb.p.edu.yr.par)
nhb.p.edu.yr.par.tab <- tidy(nhb.p.edu.yr.par) %>% mutate(LL=sprintf(confint(nhb.p.edu.yr.par)[,1],fmt='%#.3f'),
                                                      UL=sprintf(confint(nhb.p.edu.yr.par)[,2],fmt='%#.3f')) %>%
  unite(col=CI,LL:UL, sep = ", ", remove=F)
  
# Hispanic
h.p.edu.yr.par <- svyglm(acchorv~edu.c*fedu.c +
                         sex +
                         scale(H13ITOT) + 
                         smokestatus +
                         R13BMI,design=subset(svy.edaa,raceth=="Hispanic"))
summary(h.p.edu.yr.par)
h.p.edu.yr.par.tab <- tidy(h.p.edu.yr.par) %>% mutate(LL=sprintf(confint(h.p.edu.yr.par)[,1],fmt='%#.3f'),
                                                  UL=sprintf(confint(h.p.edu.yr.par)[,2],fmt='%#.3f')) %>%
  unite(col=CI,LL:UL, sep = ", ", remove=F)
  
#NH White
nhw.p.edu.yr.par <- svyglm(acchorv~edu.c*fedu.c +
                           sex +
                           scale(H13ITOT) + 
                           smokestatus +
                           R13BMI,design=subset(svy.edaa,raceth=="NHW"))
summary(nhw.p.edu.yr.par)
nhw.p.edu.yr.par.tab <- tidy(nhw.p.edu.yr.par) %>% mutate(LL=sprintf(confint(nhw.p.edu.yr.par)[,1],fmt='%#.3f'),
                                                      UL=sprintf(confint(nhw.p.edu.yr.par)[,2],fmt='%#.3f')) %>%
  unite(col=CI,LL:UL, sep = ", ", remove=F)
  

# Stack Columns 
re.p.edu.yr.par.HORVATH_DNAMAGE <- bind_rows("NH Black"=nhb.p.edu.yr.par.tab,"NH Hispanic"=h.p.edu.yr.par.tab,"NH White"=nhw.p.edu.yr.par.tab,.id='Subgroup')
write.csv(re.p.edu.yr.par.HORVATH_DNAMAGE,file='re.p.edu.yr.par.HORVATH_DNAMAGE.csv')

## HANNUM_DNAMAGE ----
# Unadjusted
p.edu.yr.par <- svyglm(acchannum~edu.c*fedu.c,design=svy.edaa)
summary(p.edu.yr.par)
p.edu.yr.par.tab <- tidy(p.edu.yr.par) %>% mutate(LL=sprintf(confint(p.edu.yr.par)[,1],fmt='%#.3f'),
                                              UL=sprintf(confint(p.edu.yr.par)[,2],fmt='%#.3f')) %>%
  unite(col=CI,LL:UL, sep = ", ", remove=F)
  

# Adjusted
adj.p.edu.yr.par <- svyglm(acchannum~edu.c*fedu.c +
                           raceth + 
                           sex +
                           scale(H13ITOT) + 
                           smokestatus +
                           R13BMI,design=svy.edaa)
summary(adj.p.edu.yr.par)
adj.p.edu.yr.par.tab <- tidy(adj.p.edu.yr.par) %>% mutate(LL=sprintf(confint(adj.p.edu.yr.par)[,1],fmt='%#.3f'),
                                                      UL=sprintf(confint(adj.p.edu.yr.par)[,2],fmt='%#.3f')) %>%
  unite(col=CI,LL:UL, sep = ", ", remove=F)
  
names(adj.p.edu.yr.par.tab) <- gsub("^(.*?)","adj_\\2", names(adj.p.edu.yr.par.tab))

# Combine results into one table
p.edu.yr.par.HANNUM_DNAMAGE <- merge(p.edu.yr.par.tab,adj.p.edu.yr.par.tab,by.x='term',by.y='adj_term',all.y=T,sort=F) 
write.csv(p.edu.yr.par.HANNUM_DNAMAGE,'p.edu.yr.par.HANNUM_DNAMAGE.csv')

## Stratify by race/ethnicity
# NH Black
nhb.p.edu.yr.par <- svyglm(acchannum~edu.c*fedu.c +
                           sex +
                           scale(H13ITOT) + 
                           smokestatus +
                           R13BMI,design=subset(svy.edaa,raceth=="NHB"))
summary(nhb.p.edu.yr.par)
nhb.p.edu.yr.par.tab <- tidy(nhb.p.edu.yr.par) %>% mutate(LL=sprintf(confint(nhb.p.edu.yr.par)[,1],fmt='%#.3f'),
                                                      UL=sprintf(confint(nhb.p.edu.yr.par)[,2],fmt='%#.3f')) %>%
  unite(col=CI,LL:UL, sep = ", ", remove=F)
  
# Hispanic
h.p.edu.yr.par <- svyglm(acchannum~edu.c*fedu.c +
                         sex +
                         scale(H13ITOT) + 
                         smokestatus +
                         R13BMI,design=subset(svy.edaa,raceth=="Hispanic"))
summary(h.p.edu.yr.par)
h.p.edu.yr.par.tab <- tidy(h.p.edu.yr.par) %>% mutate(LL=sprintf(confint(h.p.edu.yr.par)[,1],fmt='%#.3f'),
                                                  UL=sprintf(confint(h.p.edu.yr.par)[,2],fmt='%#.3f')) %>%
  unite(col=CI,LL:UL, sep = ", ", remove=F)
  
#NH White
nhw.p.edu.yr.par <- svyglm(acchannum~edu.c*fedu.c +
                           sex +
                           scale(H13ITOT) + 
                           smokestatus +
                           R13BMI,design=subset(svy.edaa,raceth=="NHW"))
summary(nhw.p.edu.yr.par)
nhw.p.edu.yr.par.tab <- tidy(nhw.p.edu.yr.par) %>% mutate(LL=sprintf(confint(nhw.p.edu.yr.par)[,1],fmt='%#.3f'),
                                                      UL=sprintf(confint(nhw.p.edu.yr.par)[,2],fmt='%#.3f')) %>%
  unite(col=CI,LL:UL, sep = ", ", remove=F)
  

# Stack Columns 
re.p.edu.yr.par.HANNUM_DNAMAGE<- bind_rows("NH Black"=nhb.p.edu.yr.par.tab,"NH Hispanic"=h.p.edu.yr.par.tab,"NH White"=nhw.p.edu.yr.par.tab,.id='Subgroup')
write.csv(re.p.edu.yr.par.HANNUM_DNAMAGE,file='re.p.edu.yr.par.HANNUM_DNAMAGE.csv')

## LEVINE_DNAMAGE ----
# Unadjusted
p.edu.yr.par <- svyglm(acclevine~edu.c*fedu.c,design=svy.edaa)
summary(p.edu.yr.par)
p.edu.yr.par.tab <- tidy(p.edu.yr.par) %>% mutate(LL=sprintf(confint(p.edu.yr.par)[,1],fmt='%#.3f'),
                                              UL=sprintf(confint(p.edu.yr.par)[,2],fmt='%#.3f')) %>%
  unite(col=CI,LL:UL, sep = ", ", remove=F)
  

# Adjusted
adj.p.edu.yr.par <- svyglm(acclevine~edu.c*fedu.c +
                           raceth + 
                           sex +
                           scale(H13ITOT) + 
                           smokestatus +
                           R13BMI,design=svy.edaa)
summary(adj.p.edu.yr.par)
adj.p.edu.yr.par.tab <- tidy(adj.p.edu.yr.par) %>% mutate(LL=sprintf(confint(adj.p.edu.yr.par)[,1],fmt='%#.3f'),
                                                      UL=sprintf(confint(adj.p.edu.yr.par)[,2],fmt='%#.3f')) %>%
  unite(col=CI,LL:UL, sep = ", ", remove=F)
  
names(adj.p.edu.yr.par.tab) <- gsub("^(.*?)","adj_\\2", names(adj.p.edu.yr.par.tab))

# Combine results into one table
p.edu.yr.par.LEVINE_DNAMAGE <- merge(p.edu.yr.par.tab,adj.p.edu.yr.par.tab,by.x='term',by.y='adj_term',all.y=T,sort=F) 
write.csv(p.edu.yr.par.LEVINE_DNAMAGE,'p.edu.yr.par.LEVINE_DNAMAGE.csv')

## Stratify by race/ethnicity
# NH Black
nhb.p.edu.yr.par <- svyglm(acclevine~edu.c*fedu.c +
                           sex +
                           scale(H13ITOT) + 
                           smokestatus +
                           R13BMI,design=subset(svy.edaa,raceth=="NHB"))
summary(nhb.p.edu.yr.par)
nhb.p.edu.yr.par.tab <- tidy(nhb.p.edu.yr.par) %>% mutate(LL=sprintf(confint(nhb.p.edu.yr.par)[,1],fmt='%#.3f'),
                                                      UL=sprintf(confint(nhb.p.edu.yr.par)[,2],fmt='%#.3f')) %>%
  unite(col=CI,LL:UL, sep = ", ", remove=F)
  
# Hispanic
h.p.edu.yr.par <- svyglm(acclevine~edu.c*fedu.c +
                         sex +
                         scale(H13ITOT) + 
                         smokestatus +
                         R13BMI,design=subset(svy.edaa,raceth=="Hispanic"))
summary(h.p.edu.yr.par)
h.p.edu.yr.par.tab <- tidy(h.p.edu.yr.par) %>% mutate(LL=sprintf(confint(h.p.edu.yr.par)[,1],fmt='%#.3f'),
                                                  UL=sprintf(confint(h.p.edu.yr.par)[,2],fmt='%#.3f')) %>%
  unite(col=CI,LL:UL, sep = ", ", remove=F)
  
#NH White
nhw.p.edu.yr.par <- svyglm(acclevine~edu.c*fedu.c +
                           sex +
                           scale(H13ITOT) + 
                           smokestatus +
                           R13BMI,design=subset(svy.edaa,raceth=="NHW"))
summary(nhw.p.edu.yr.par)
nhw.p.edu.yr.par.tab <- tidy(nhw.p.edu.yr.par) %>% mutate(LL=sprintf(confint(nhw.p.edu.yr.par)[,1],fmt='%#.3f'),
                                                      UL=sprintf(confint(nhw.p.edu.yr.par)[,2],fmt='%#.3f')) %>%
  unite(col=CI,LL:UL, sep = ", ", remove=F)
  

# Stack Columns 
re.p.edu.yr.par.LEVINE_DNAMAGE<- bind_rows("NH Black"=nhb.p.edu.yr.par.tab,"NH Hispanic"=h.p.edu.yr.par.tab,"NH White"=nhw.p.edu.yr.par.tab,.id='Subgroup')
write.csv(re.p.edu.yr.par.LEVINE_DNAMAGE,file='re.p.edu.yr.par.LEVINE_DNAMAGE.csv')

## HORVATHSKIN_DNAMAGE ----
# Unadjusted
p.edu.yr.par <- svyglm(acchorvsk~edu.c*fedu.c,design=svy.edaa)
summary(p.edu.yr.par)
p.edu.yr.par.tab <- tidy(p.edu.yr.par) %>% mutate(LL=sprintf(confint(p.edu.yr.par)[,1],fmt='%#.3f'),
                                              UL=sprintf(confint(p.edu.yr.par)[,2],fmt='%#.3f')) %>%
  unite(col=CI,LL:UL, sep = ", ", remove=F)
  

# Adjusted
adj.p.edu.yr.par <- svyglm(acchorvsk~edu.c*fedu.c +
                           raceth + 
                           sex +
                           scale(H13ITOT) + 
                           smokestatus +
                           R13BMI,design=svy.edaa)
summary(adj.p.edu.yr.par)
adj.p.edu.yr.par.tab <- tidy(adj.p.edu.yr.par) %>% mutate(LL=sprintf(confint(adj.p.edu.yr.par)[,1],fmt='%#.3f'),
                                                      UL=sprintf(confint(adj.p.edu.yr.par)[,2],fmt='%#.3f')) %>%
  unite(col=CI,LL:UL, sep = ", ", remove=F)
  
names(adj.p.edu.yr.par.tab) <- gsub("^(.*?)","adj_\\2", names(adj.p.edu.yr.par.tab))

# Combine results into one table
p.edu.yr.par.HORVATHSKIN_DNAMAGE<- merge(p.edu.yr.par.tab,adj.p.edu.yr.par.tab,by.x='term',by.y='adj_term',all.y=T,sort=F) 
write.csv(p.edu.yr.par.HORVATHSKIN_DNAMAGE,'p.edu.yr.par.HORVATHSKIN_DNAMAGE.csv')

## Stratify by race/ethnicity
# NH Black
nhb.p.edu.yr.par <- svyglm(acchorvsk~edu.c*fedu.c +
                           sex +
                           scale(H13ITOT) + 
                           smokestatus +
                           R13BMI,design=subset(svy.edaa,raceth=="NHB"))
summary(nhb.p.edu.yr.par)
nhb.p.edu.yr.par.tab <- tidy(nhb.p.edu.yr.par) %>% mutate(LL=sprintf(confint(nhb.p.edu.yr.par)[,1],fmt='%#.3f'),
                                                      UL=sprintf(confint(nhb.p.edu.yr.par)[,2],fmt='%#.3f')) %>%
  unite(col=CI,LL:UL, sep = ", ", remove=F)
  
# Hispanic
h.p.edu.yr.par <- svyglm(acchorvsk~edu.c*fedu.c +
                         sex +
                         scale(H13ITOT) + 
                         smokestatus +
                         R13BMI,design=subset(svy.edaa,raceth=="Hispanic"))
summary(h.p.edu.yr.par)
h.p.edu.yr.par.tab <- tidy(h.p.edu.yr.par) %>% mutate(LL=sprintf(confint(h.p.edu.yr.par)[,1],fmt='%#.3f'),
                                                  UL=sprintf(confint(h.p.edu.yr.par)[,2],fmt='%#.3f')) %>%
  unite(col=CI,LL:UL, sep = ", ", remove=F)
  
#NH White
nhw.p.edu.yr.par <- svyglm(acchorvsk~edu.c*fedu.c +
                           sex +
                           scale(H13ITOT) + 
                           smokestatus +
                           R13BMI,design=subset(svy.edaa,raceth=="NHW"))
summary(nhw.p.edu.yr.par)
nhw.p.edu.yr.par.tab <- tidy(nhw.p.edu.yr.par) %>% mutate(LL=sprintf(confint(nhw.p.edu.yr.par)[,1],fmt='%#.3f'),
                                                      UL=sprintf(confint(nhw.p.edu.yr.par)[,2],fmt='%#.3f')) %>%
  unite(col=CI,LL:UL, sep = ", ", remove=F)
  

# Stack Columns 
re.p.edu.yr.par.HORVATHSKIN_DNAMAGE<- bind_rows("NH Black"=nhb.p.edu.yr.par.tab,"NH Hispanic"=h.p.edu.yr.par.tab,"NH White"=nhw.p.edu.yr.par.tab,.id='Subgroup')
write.csv(re.p.edu.yr.par.HORVATHSKIN_DNAMAGE,file='re.p.edu.yr.par.HORVATHSKIN_DNAMAGE.csv')

## LIN_DNAMAGE ----
# Unadjusted
p.edu.yr.par <- svyglm(acclin~edu.c*fedu.c,design=svy.edaa)
summary(p.edu.yr.par)
p.edu.yr.par.tab <- tidy(p.edu.yr.par) %>% mutate(LL=sprintf(confint(p.edu.yr.par)[,1],fmt='%#.3f'),
                                              UL=sprintf(confint(p.edu.yr.par)[,2],fmt='%#.3f')) %>%
  unite(col=CI,LL:UL, sep = ", ", remove=F)
  

# Adjusted
adj.p.edu.yr.par <- svyglm(acclin~edu.c*fedu.c +
                           raceth + 
                           sex +
                           scale(H13ITOT) + 
                           smokestatus +
                           R13BMI,design=svy.edaa)
summary(adj.p.edu.yr.par)
adj.p.edu.yr.par.tab <- tidy(adj.p.edu.yr.par) %>% mutate(LL=sprintf(confint(adj.p.edu.yr.par)[,1],fmt='%#.3f'),
                                                      UL=sprintf(confint(adj.p.edu.yr.par)[,2],fmt='%#.3f')) %>%
  unite(col=CI,LL:UL, sep = ", ", remove=F)
  
names(adj.p.edu.yr.par.tab) <- gsub("^(.*?)","adj_\\2", names(adj.p.edu.yr.par.tab))

# Combine results into one table
p.edu.yr.par.LIN_DNAMAGE <- merge(p.edu.yr.par.tab,adj.p.edu.yr.par.tab,by.x='term',by.y='adj_term',all.y=T,sort=F) 
write.csv(p.edu.yr.par.LIN_DNAMAGE,'p.edu.yr.par.LIN_DNAMAGE.csv')

## Stratify by race/ethnicity
# NH Black
nhb.p.edu.yr.par <- svyglm(acclin~edu.c*fedu.c +
                           sex +
                           scale(H13ITOT) + 
                           smokestatus +
                           R13BMI,design=subset(svy.edaa,raceth=="NHB"))
summary(nhb.p.edu.yr.par)
nhb.p.edu.yr.par.tab <- tidy(nhb.p.edu.yr.par) %>% mutate(LL=sprintf(confint(nhb.p.edu.yr.par)[,1],fmt='%#.3f'),
                                                      UL=sprintf(confint(nhb.p.edu.yr.par)[,2],fmt='%#.3f')) %>%
  unite(col=CI,LL:UL, sep = ", ", remove=F)
  
# Hispanic
h.p.edu.yr.par <- svyglm(acclin~edu.c*fedu.c +
                         sex +
                         scale(H13ITOT) + 
                         smokestatus +
                         R13BMI,design=subset(svy.edaa,raceth=="Hispanic"))
summary(h.p.edu.yr.par)
h.p.edu.yr.par.tab <- tidy(h.p.edu.yr.par) %>% mutate(LL=sprintf(confint(h.p.edu.yr.par)[,1],fmt='%#.3f'),
                                                  UL=sprintf(confint(h.p.edu.yr.par)[,2],fmt='%#.3f')) %>%
  unite(col=CI,LL:UL, sep = ", ", remove=F)
  
#NH White
nhw.p.edu.yr.par <- svyglm(acclin~edu.c*fedu.c +
                           sex +
                           scale(H13ITOT) + 
                           smokestatus +
                           R13BMI,design=subset(svy.edaa,raceth=="NHW"))
summary(nhw.p.edu.yr.par)
nhw.p.edu.yr.par.tab <- tidy(nhw.p.edu.yr.par) %>% mutate(LL=sprintf(confint(nhw.p.edu.yr.par)[,1],fmt='%#.3f'),
                                                      UL=sprintf(confint(nhw.p.edu.yr.par)[,2],fmt='%#.3f')) %>%
  unite(col=CI,LL:UL, sep = ", ", remove=F)
  

# Stack Columns 
re.p.edu.yr.par.LIN_DNAMAGE<- bind_rows("NH Black"=nhb.p.edu.yr.par.tab,"NH Hispanic"=h.p.edu.yr.par.tab,"NH White"=nhw.p.edu.yr.par.tab,.id='Subgroup')
write.csv(re.p.edu.yr.par.LIN_DNAMAGE,file='re.p.edu.yr.par.LIN_DNAMAGE.csv')

## WEIDNER_DNAMAGE ----
# Unadjusted
p.edu.yr.par <- svyglm(accweid~edu.c*fedu.c,design=svy.edaa)
summary(p.edu.yr.par)
p.edu.yr.par.tab <- tidy(p.edu.yr.par) %>% mutate(LL=sprintf(confint(p.edu.yr.par)[,1],fmt='%#.3f'),
                                              UL=sprintf(confint(p.edu.yr.par)[,2],fmt='%#.3f')) %>%
  unite(col=CI,LL:UL, sep = ", ", remove=F)
  

# Adjusted
adj.p.edu.yr.par <- svyglm(accweid~edu.c*fedu.c +
                           raceth + 
                           sex +
                           scale(H13ITOT) + 
                           smokestatus +
                           R13BMI,design=svy.edaa)
summary(adj.p.edu.yr.par)
adj.p.edu.yr.par.tab <- tidy(adj.p.edu.yr.par) %>% mutate(LL=sprintf(confint(adj.p.edu.yr.par)[,1],fmt='%#.3f'),
                                                      UL=sprintf(confint(adj.p.edu.yr.par)[,2],fmt='%#.3f')) %>%
  unite(col=CI,LL:UL, sep = ", ", remove=F)
  
names(adj.p.edu.yr.par.tab) <- gsub("^(.*?)","adj_\\2", names(adj.p.edu.yr.par.tab))

# Combine results into one table
p.edu.yr.par.WEIDNER_DNAMAGE <- merge(p.edu.yr.par.tab,adj.p.edu.yr.par.tab,by.x='term',by.y='adj_term',all.y=T,sort=F) 
write.csv(p.edu.yr.par.WEIDNER_DNAMAGE,'p.edu.yr.par.WEIDNER_DNAMAGE.csv')

## Stratify by race/ethnicity
# NH Black
nhb.p.edu.yr.par <- svyglm(accweid~edu.c*fedu.c +
                           sex +
                           scale(H13ITOT) + 
                           smokestatus +
                           R13BMI,design=subset(svy.edaa,raceth=="NHB"))
summary(nhb.p.edu.yr.par)
nhb.p.edu.yr.par.tab <- tidy(nhb.p.edu.yr.par) %>% mutate(LL=sprintf(confint(nhb.p.edu.yr.par)[,1],fmt='%#.3f'),
                                                      UL=sprintf(confint(nhb.p.edu.yr.par)[,2],fmt='%#.3f')) %>%
  unite(col=CI,LL:UL, sep = ", ", remove=F)
  
# Hispanic
h.p.edu.yr.par <- svyglm(accweid~edu.c*fedu.c +
                         sex +
                         scale(H13ITOT) + 
                         smokestatus +
                         R13BMI,design=subset(svy.edaa,raceth=="Hispanic"))
summary(h.p.edu.yr.par)
h.p.edu.yr.par.tab <- tidy(h.p.edu.yr.par) %>% mutate(LL=sprintf(confint(h.p.edu.yr.par)[,1],fmt='%#.3f'),
                                                  UL=sprintf(confint(h.p.edu.yr.par)[,2],fmt='%#.3f')) %>%
  unite(col=CI,LL:UL, sep = ", ", remove=F)
  
#NH White
nhw.p.edu.yr.par <- svyglm(accweid~edu.c*fedu.c +
                           sex +
                           scale(H13ITOT) + 
                           smokestatus +
                           R13BMI,design=subset(svy.edaa,raceth=="NHW"))
summary(nhw.p.edu.yr.par)
nhw.p.edu.yr.par.tab <- tidy(nhw.p.edu.yr.par) %>% mutate(LL=sprintf(confint(nhw.p.edu.yr.par)[,1],fmt='%#.3f'),
                                                      UL=sprintf(confint(nhw.p.edu.yr.par)[,2],fmt='%#.3f')) %>%
  unite(col=CI,LL:UL, sep = ", ", remove=F)
  

# Stack Columns 
re.p.edu.yr.par.WEIDNER_DNAMAGE<- bind_rows("NH Black"=nhb.p.edu.yr.par.tab,"NH Hispanic"=h.p.edu.yr.par.tab,"NH White"=nhw.p.edu.yr.par.tab,.id='Subgroup')
write.csv(re.p.edu.yr.par.WEIDNER_DNAMAGE,file='re.p.edu.yr.par.WEIDNER_DNAMAGE.csv')

## VIDALBRALO_DNAMAGE ----
# Unadjusted
p.edu.yr.par <- svyglm(accvidalb~edu.c*fedu.c,design=svy.edaa)
summary(p.edu.yr.par)
p.edu.yr.par.tab <- tidy(p.edu.yr.par) %>% mutate(LL=sprintf(confint(p.edu.yr.par)[,1],fmt='%#.3f'),
                                              UL=sprintf(confint(p.edu.yr.par)[,2],fmt='%#.3f')) %>%
  unite(col=CI,LL:UL, sep = ", ", remove=F)
  

# Adjusted
adj.p.edu.yr.par <- svyglm(accvidalb~edu.c*fedu.c +
                           raceth + 
                           sex +
                           scale(H13ITOT) + 
                           smokestatus +
                           R13BMI,design=svy.edaa)
summary(adj.p.edu.yr.par)
adj.p.edu.yr.par.tab <- tidy(adj.p.edu.yr.par) %>% mutate(LL=sprintf(confint(adj.p.edu.yr.par)[,1],fmt='%#.3f'),
                                                      UL=sprintf(confint(adj.p.edu.yr.par)[,2],fmt='%#.3f')) %>%
  unite(col=CI,LL:UL, sep = ", ", remove=F)
  
names(adj.p.edu.yr.par.tab) <- gsub("^(.*?)","adj_\\2", names(adj.p.edu.yr.par.tab))

# Combine results into one table
p.edu.yr.par.VIDALBRALO_DNAMAGE <- merge(p.edu.yr.par.tab,adj.p.edu.yr.par.tab,by.x='term',by.y='adj_term',all.y=T,sort=F) 
write.csv(p.edu.yr.par.VIDALBRALO_DNAMAGE,'p.edu.yr.par.VIDALBRALO_DNAMAGE.csv')

## Stratify by race/ethnicity
# NH Black
nhb.p.edu.yr.par <- svyglm(accvidalb~edu.c*fedu.c +
                           sex +
                           scale(H13ITOT) + 
                           smokestatus +
                           R13BMI,design=subset(svy.edaa,raceth=="NHB"))
summary(nhb.p.edu.yr.par)
nhb.p.edu.yr.par.tab <- tidy(nhb.p.edu.yr.par) %>% mutate(LL=sprintf(confint(nhb.p.edu.yr.par)[,1],fmt='%#.3f'),
                                                      UL=sprintf(confint(nhb.p.edu.yr.par)[,2],fmt='%#.3f')) %>%
  unite(col=CI,LL:UL, sep = ", ", remove=F)
  
# Hispanic
h.p.edu.yr.par <- svyglm(accvidalb~edu.c*fedu.c +
                         sex +
                         scale(H13ITOT) + 
                         smokestatus +
                         R13BMI,design=subset(svy.edaa,raceth=="Hispanic"))
summary(h.p.edu.yr.par)
h.p.edu.yr.par.tab <- tidy(h.p.edu.yr.par) %>% mutate(LL=sprintf(confint(h.p.edu.yr.par)[,1],fmt='%#.3f'),
                                                  UL=sprintf(confint(h.p.edu.yr.par)[,2],fmt='%#.3f')) %>%
  unite(col=CI,LL:UL, sep = ", ", remove=F)
  
#NH White
nhw.p.edu.yr.par <- svyglm(accvidalb~edu.c*fedu.c +
                           sex +
                           scale(H13ITOT) + 
                           smokestatus +
                           R13BMI,design=subset(svy.edaa,raceth=="NHW"))
summary(nhw.p.edu.yr.par)
nhw.p.edu.yr.par.tab <- tidy(nhw.p.edu.yr.par) %>% mutate(LL=sprintf(confint(nhw.p.edu.yr.par)[,1],fmt='%#.3f'),
                                                      UL=sprintf(confint(nhw.p.edu.yr.par)[,2],fmt='%#.3f')) %>%
  unite(col=CI,LL:UL, sep = ", ", remove=F)
  

# Stack Columns 
re.p.edu.yr.par.VIDALBRALO_DNAMAGE<- bind_rows("NH Black"=nhb.p.edu.yr.par.tab,"NH Hispanic"=h.p.edu.yr.par.tab,"NH White"=nhw.p.edu.yr.par.tab,.id='Subgroup')
write.csv(re.p.edu.yr.par.VIDALBRALO_DNAMAGE,file='re.p.edu.yr.par.VIDALBRALO_DNAMAGE.csv')

## YANG_DNAMAGE ----
# Unadjusted
p.edu.yr.par <- svyglm(accyang~edu.c*fedu.c,design=svy.edaa)
summary(p.edu.yr.par)
p.edu.yr.par.tab <- tidy(p.edu.yr.par) %>% mutate(LL=sprintf(confint(p.edu.yr.par)[,1],fmt='%#.3f'),
                                              UL=sprintf(confint(p.edu.yr.par)[,2],fmt='%#.3f')) %>%
  unite(col=CI,LL:UL, sep = ", ", remove=F)
  

# Adjusted
adj.p.edu.yr.par <- svyglm(accyang~edu.c*fedu.c +
                           raceth + 
                           sex +
                           scale(H13ITOT) + 
                           smokestatus +
                           R13BMI,design=svy.edaa)
summary(adj.p.edu.yr.par)
adj.p.edu.yr.par.tab <- tidy(adj.p.edu.yr.par) %>% mutate(LL=sprintf(confint(adj.p.edu.yr.par)[,1],fmt='%#.3f'),
                                                      UL=sprintf(confint(adj.p.edu.yr.par)[,2],fmt='%#.3f')) %>%
  unite(col=CI,LL:UL, sep = ", ", remove=F)
  
names(adj.p.edu.yr.par.tab) <- gsub("^(.*?)","adj_\\2", names(adj.p.edu.yr.par.tab))

# Combine results into one table
p.edu.yr.par.YANG_DNAMAGE <- merge(p.edu.yr.par.tab,adj.p.edu.yr.par.tab,by.x='term',by.y='adj_term',all.y=T,sort=F) 
write.csv(p.edu.yr.par.YANG_DNAMAGE,'p.edu.yr.par.YANG_DNAMAGE.csv')

## Stratify by race/ethnicity
# NH Black
nhb.p.edu.yr.par <- svyglm(accyang~edu.c*fedu.c +
                           sex +
                           scale(H13ITOT) + 
                           smokestatus +
                           R13BMI,design=subset(svy.edaa,raceth=="NHB"))
summary(nhb.p.edu.yr.par)
nhb.p.edu.yr.par.tab <- tidy(nhb.p.edu.yr.par) %>% mutate(LL=sprintf(confint(nhb.p.edu.yr.par)[,1],fmt='%#.3f'),
                                                      UL=sprintf(confint(nhb.p.edu.yr.par)[,2],fmt='%#.3f')) %>%
  unite(col=CI,LL:UL, sep = ", ", remove=F)
  
# Hispanic
h.p.edu.yr.par <- svyglm(accyang~edu.c*fedu.c +
                         sex +
                         scale(H13ITOT) + 
                         smokestatus +
                         R13BMI,design=subset(svy.edaa,raceth=="Hispanic"))
summary(h.p.edu.yr.par)
h.p.edu.yr.par.tab <- tidy(h.p.edu.yr.par) %>% mutate(LL=sprintf(confint(h.p.edu.yr.par)[,1],fmt='%#.3f'),
                                                  UL=sprintf(confint(h.p.edu.yr.par)[,2],fmt='%#.3f')) %>%
  unite(col=CI,LL:UL, sep = ", ", remove=F)
  
#NH White
nhw.p.edu.yr.par <- svyglm(accyang~edu.c*fedu.c +
                           sex +
                           scale(H13ITOT) + 
                           smokestatus +
                           R13BMI,design=subset(svy.edaa,raceth=="NHW"))
summary(nhw.p.edu.yr.par)
nhw.p.edu.yr.par.tab <- tidy(nhw.p.edu.yr.par) %>% mutate(LL=sprintf(confint(nhw.p.edu.yr.par)[,1],fmt='%#.3f'),
                                                      UL=sprintf(confint(nhw.p.edu.yr.par)[,2],fmt='%#.3f')) %>%
  unite(col=CI,LL:UL, sep = ", ", remove=F)
  

# Stack Columns 
re.p.edu.yr.par.YANG_DNAMAGE<- bind_rows("NH Black"=nhb.p.edu.yr.par.tab,"NH Hispanic"=h.p.edu.yr.par.tab,"NH White"=nhw.p.edu.yr.par.tab,.id='Subgroup')
write.csv(re.p.edu.yr.par.YANG_DNAMAGE,file='re.p.edu.yr.par.YANG_DNAMAGE.csv')

## ZHANG_DNAMAGE ----
# Unadjusted
p.edu.yr.par <- svyglm(acczhang~edu.c*fedu.c,design=svy.edaa)
summary(p.edu.yr.par)
p.edu.yr.par.tab <- tidy(p.edu.yr.par) %>% mutate(LL=sprintf(confint(p.edu.yr.par)[,1],fmt='%#.3f'),
                                              UL=sprintf(confint(p.edu.yr.par)[,2],fmt='%#.3f')) %>%
  unite(col=CI,LL:UL, sep = ", ", remove=F)
  

# Adjusted
adj.p.edu.yr.par <- svyglm(acczhang~edu.c*fedu.c +
                           raceth + 
                           sex +
                           scale(H13ITOT) + 
                           smokestatus +
                           R13BMI,design=svy.edaa)
summary(adj.p.edu.yr.par)
adj.p.edu.yr.par.tab <- tidy(adj.p.edu.yr.par) %>% mutate(LL=sprintf(confint(adj.p.edu.yr.par)[,1],fmt='%#.3f'),
                                                      UL=sprintf(confint(adj.p.edu.yr.par)[,2],fmt='%#.3f')) %>%
  unite(col=CI,LL:UL, sep = ", ", remove=F)
  
names(adj.p.edu.yr.par.tab) <- gsub("^(.*?)","adj_\\2", names(adj.p.edu.yr.par.tab))

# Combine results into one table
p.edu.yr.par.ZHANG_DNAMAGE <- merge(p.edu.yr.par.tab,adj.p.edu.yr.par.tab,by.x='term',by.y='adj_term',all.y=T,sort=F) 
write.csv(p.edu.yr.par.ZHANG_DNAMAGE,'p.edu.yr.par.ZHANG_DNAMAGE.csv')

## Stratify by race/ethnicity
# NH Black
nhb.p.edu.yr.par <- svyglm(acczhang~edu.c*fedu.c +
                           sex +
                           scale(H13ITOT) + 
                           smokestatus +
                           R13BMI,design=subset(svy.edaa,raceth=="NHB"))
summary(nhb.p.edu.yr.par)
nhb.p.edu.yr.par.tab <- tidy(nhb.p.edu.yr.par) %>% mutate(LL=sprintf(confint(nhb.p.edu.yr.par)[,1],fmt='%#.3f'),
                                                      UL=sprintf(confint(nhb.p.edu.yr.par)[,2],fmt='%#.3f')) %>%
  unite(col=CI,LL:UL, sep = ", ", remove=F)
  
# Hispanic
h.p.edu.yr.par <- svyglm(acczhang~edu.c*fedu.c +
                         sex +
                         scale(H13ITOT) + 
                         smokestatus +
                         R13BMI,design=subset(svy.edaa,raceth=="Hispanic"))
summary(h.p.edu.yr.par)
h.p.edu.yr.par.tab <- tidy(h.p.edu.yr.par) %>% mutate(LL=sprintf(confint(h.p.edu.yr.par)[,1],fmt='%#.3f'),
                                                  UL=sprintf(confint(h.p.edu.yr.par)[,2],fmt='%#.3f')) %>%
  unite(col=CI,LL:UL, sep = ", ", remove=F)
  
#NH White
nhw.p.edu.yr.par <- svyglm(acczhang~edu.c*fedu.c +
                           sex +
                           scale(H13ITOT) + 
                           smokestatus +
                           R13BMI,design=subset(svy.edaa,raceth=="NHW"))
summary(nhw.p.edu.yr.par)
nhw.p.edu.yr.par.tab <- tidy(nhw.p.edu.yr.par) %>% mutate(LL=sprintf(confint(nhw.p.edu.yr.par)[,1],fmt='%#.3f'),
                                                      UL=sprintf(confint(nhw.p.edu.yr.par)[,2],fmt='%#.3f')) %>%
  unite(col=CI,LL:UL, sep = ", ", remove=F)
  

# Stack Columns 
re.p.edu.yr.par.ZHANG_DNAMAGE<- bind_rows("NH Black"=nhb.p.edu.yr.par.tab,"NH Hispanic"=h.p.edu.yr.par.tab,"NH White"=nhw.p.edu.yr.par.tab,.id='Subgroup')
write.csv(re.p.edu.yr.par.ZHANG_DNAMAGE,file='re.p.edu.yr.par.ZHANG_DNAMAGE.csv')

## BOCKLANDT_DNAMAGE ----
# Unadjusted
p.edu.yr.par <- svyglm(accbock~edu.c*fedu.c,design=svy.edaa)
summary(p.edu.yr.par)
p.edu.yr.par.tab <- tidy(p.edu.yr.par) %>% mutate(LL=sprintf(confint(p.edu.yr.par)[,1],fmt='%#.3f'),
                                              UL=sprintf(confint(p.edu.yr.par)[,2],fmt='%#.3f')) %>%
  unite(col=CI,LL:UL, sep = ", ", remove=F)
  

# Adjusted
adj.p.edu.yr.par <- svyglm(accbock~edu.c*fedu.c +
                           raceth + 
                           sex +
                           scale(H13ITOT) + 
                           smokestatus +
                           R13BMI,design=svy.edaa)
summary(adj.p.edu.yr.par)
adj.p.edu.yr.par.tab <- tidy(adj.p.edu.yr.par) %>% mutate(LL=sprintf(confint(adj.p.edu.yr.par)[,1],fmt='%#.3f'),
                                                      UL=sprintf(confint(adj.p.edu.yr.par)[,2],fmt='%#.3f')) %>%
  unite(col=CI,LL:UL, sep = ", ", remove=F)
  
names(adj.p.edu.yr.par.tab) <- gsub("^(.*?)","adj_\\2", names(adj.p.edu.yr.par.tab))

# Combine results into one table
p.edu.yr.par.BOCKLANDT_DNAMAGE <- merge(p.edu.yr.par.tab,adj.p.edu.yr.par.tab,by.x='term',by.y='adj_term',all.y=T,sort=F) 
write.csv(p.edu.yr.par.BOCKLANDT_DNAMAGE,'p.edu.yr.par.BOCKLANDT_DNAMAGE.csv')

## Stratify by race/ethnicity
# NH Black
nhb.p.edu.yr.par <- svyglm(accbock~edu.c*fedu.c +
                           sex +
                           scale(H13ITOT) + 
                           smokestatus +
                           R13BMI,design=subset(svy.edaa,raceth=="NHB"))
summary(nhb.p.edu.yr.par)
nhb.p.edu.yr.par.tab <- tidy(nhb.p.edu.yr.par) %>% mutate(LL=sprintf(confint(nhb.p.edu.yr.par)[,1],fmt='%#.3f'),
                                                      UL=sprintf(confint(nhb.p.edu.yr.par)[,2],fmt='%#.3f')) %>%
  unite(col=CI,LL:UL, sep = ", ", remove=F)
  
# Hispanic
h.p.edu.yr.par <- svyglm(accbock~edu.c*fedu.c +
                         sex +
                         scale(H13ITOT) + 
                         smokestatus +
                         R13BMI,design=subset(svy.edaa,raceth=="Hispanic"))
summary(h.p.edu.yr.par)
h.p.edu.yr.par.tab <- tidy(h.p.edu.yr.par) %>% mutate(LL=sprintf(confint(h.p.edu.yr.par)[,1],fmt='%#.3f'),
                                                  UL=sprintf(confint(h.p.edu.yr.par)[,2],fmt='%#.3f')) %>%
  unite(col=CI,LL:UL, sep = ", ", remove=F)
  
#NH White
nhw.p.edu.yr.par <- svyglm(accbock~edu.c*fedu.c +
                           sex +
                           scale(H13ITOT) + 
                           smokestatus +
                           R13BMI,design=subset(svy.edaa,raceth=="NHW"))
summary(nhw.p.edu.yr.par)
nhw.p.edu.yr.par.tab <- tidy(nhw.p.edu.yr.par) %>% mutate(LL=sprintf(confint(nhw.p.edu.yr.par)[,1],fmt='%#.3f'),
                                                      UL=sprintf(confint(nhw.p.edu.yr.par)[,2],fmt='%#.3f')) %>%
  unite(col=CI,LL:UL, sep = ", ", remove=F)
  

# Stack Columns 
re.p.edu.yr.par.BOCKLANDT_DNAMAGE<- bind_rows("NH Black"=nhb.p.edu.yr.par.tab,"NH Hispanic"=h.p.edu.yr.par.tab,"NH White"=nhw.p.edu.yr.par.tab,.id='Subgroup')
write.csv(re.p.edu.yr.par.BOCKLANDT_DNAMAGE,file='re.p.edu.yr.par.BOCKLANDT_DNAMAGE.csv')

## GARAGNANI_DNAMAGE ----
# Unadjusted
p.edu.yr.par <- svyglm(accgarag~edu.c*fedu.c,design=svy.edaa)
summary(p.edu.yr.par)
p.edu.yr.par.tab <- tidy(p.edu.yr.par) %>% mutate(LL=sprintf(confint(p.edu.yr.par)[,1],fmt='%#.3f'),
                                              UL=sprintf(confint(p.edu.yr.par)[,2],fmt='%#.3f')) %>%
  unite(col=CI,LL:UL, sep = ", ", remove=F)
  

# Adjusted
adj.p.edu.yr.par <- svyglm(accgarag~edu.c*fedu.c +
                           raceth + 
                           sex +
                           scale(H13ITOT) + 
                           smokestatus +
                           R13BMI,design=svy.edaa)
summary(adj.p.edu.yr.par)
adj.p.edu.yr.par.tab <- tidy(adj.p.edu.yr.par) %>% mutate(LL=sprintf(confint(adj.p.edu.yr.par)[,1],fmt='%#.3f'),
                                                      UL=sprintf(confint(adj.p.edu.yr.par)[,2],fmt='%#.3f')) %>%
  unite(col=CI,LL:UL, sep = ", ", remove=F)
  
names(adj.p.edu.yr.par.tab) <- gsub("^(.*?)","adj_\\2", names(adj.p.edu.yr.par.tab))

# Combine results into one table
p.edu.yr.par.GARAGNANI_DNAMAGE <- merge(p.edu.yr.par.tab,adj.p.edu.yr.par.tab,by.x='term',by.y='adj_term',all.y=T,sort=F) 
write.csv(p.edu.yr.par.GARAGNANI_DNAMAGE,'p.edu.yr.par.GARAGNANI_DNAMAGE.csv')

## Stratify by race/ethnicity
# NH Black
nhb.p.edu.yr.par <- svyglm(accgarag~edu.c*fedu.c +
                           sex +
                           scale(H13ITOT) + 
                           smokestatus +
                           R13BMI,design=subset(svy.edaa,raceth=="NHB"))
summary(nhb.p.edu.yr.par)
nhb.p.edu.yr.par.tab <- tidy(nhb.p.edu.yr.par) %>% mutate(LL=sprintf(confint(nhb.p.edu.yr.par)[,1],fmt='%#.3f'),
                                                      UL=sprintf(confint(nhb.p.edu.yr.par)[,2],fmt='%#.3f')) %>%
  unite(col=CI,LL:UL, sep = ", ", remove=F)
  
# Hispanic
h.p.edu.yr.par <- svyglm(accgarag~edu.c*fedu.c +
                         sex +
                         scale(H13ITOT) + 
                         smokestatus +
                         R13BMI,design=subset(svy.edaa,raceth=="Hispanic"))
summary(h.p.edu.yr.par)
h.p.edu.yr.par.tab <- tidy(h.p.edu.yr.par) %>% mutate(LL=sprintf(confint(h.p.edu.yr.par)[,1],fmt='%#.3f'),
                                                  UL=sprintf(confint(h.p.edu.yr.par)[,2],fmt='%#.3f')) %>%
  unite(col=CI,LL:UL, sep = ", ", remove=F)
  
#NH White
nhw.p.edu.yr.par <- svyglm(accgarag~edu.c*fedu.c +
                           sex +
                           scale(H13ITOT) + 
                           smokestatus +
                           R13BMI,design=subset(svy.edaa,raceth=="NHW"))
summary(nhw.p.edu.yr.par)
nhw.p.edu.yr.par.tab <- tidy(nhw.p.edu.yr.par) %>% mutate(LL=sprintf(confint(nhw.p.edu.yr.par)[,1],fmt='%#.3f'),
                                                      UL=sprintf(confint(nhw.p.edu.yr.par)[,2],fmt='%#.3f')) %>%
  unite(col=CI,LL:UL, sep = ", ", remove=F)
  

# Stack Columns 
re.p.edu.yr.par.GARAGNANI_DNAMAGE<- bind_rows("NH Black"=nhb.p.edu.yr.par.tab,"NH Hispanic"=h.p.edu.yr.par.tab,"NH White"=nhw.p.edu.yr.par.tab,.id='Subgroup')
write.csv(re.p.edu.yr.par.GARAGNANI_DNAMAGE,file='re.p.edu.yr.par.GARAGNANI_DNAMAGE.csv')

## DNAMGRIMAGE ----
# Unadjusted
p.edu.yr.par <- svyglm(accdnam~edu.c*fedu.c,design=svy.edaa)
summary(p.edu.yr.par)
p.edu.yr.par.tab <- tidy(p.edu.yr.par) %>% mutate(LL=sprintf(confint(p.edu.yr.par)[,1],fmt='%#.3f'),
                                              UL=sprintf(confint(p.edu.yr.par)[,2],fmt='%#.3f')) %>%
  unite(col=CI,LL:UL, sep = ", ", remove=F)
  

# Adjusted
adj.p.edu.yr.par <- svyglm(accdnam~edu.c*fedu.c +
                           raceth + 
                           sex +
                           scale(H13ITOT) + 
                           smokestatus +
                           R13BMI,design=svy.edaa)
summary(adj.p.edu.yr.par)
adj.p.edu.yr.par.tab <- tidy(adj.p.edu.yr.par) %>% mutate(LL=sprintf(confint(adj.p.edu.yr.par)[,1],fmt='%#.3f'),
                                                      UL=sprintf(confint(adj.p.edu.yr.par)[,2],fmt='%#.3f')) %>%
  unite(col=CI,LL:UL, sep = ", ", remove=F)
  
names(adj.p.edu.yr.par.tab) <- gsub("^(.*?)","adj_\\2", names(adj.p.edu.yr.par.tab))

# Combine results into one table
p.edu.yr.par.DNAMGRIMAGE <- merge(p.edu.yr.par.tab,adj.p.edu.yr.par.tab,by.x='term',by.y='adj_term',all.y=T,sort=F) 
write.csv(p.edu.yr.par.DNAMGRIMAGE,'p.edu.yr.par.DNAMGRIMAGE.csv')

## Stratify by race/ethnicity
# NH Black
nhb.p.edu.yr.par <- svyglm(accdnam~edu.c*fedu.c +
                           sex +
                           scale(H13ITOT) + 
                           smokestatus +
                           R13BMI,design=subset(svy.edaa,raceth=="NHB"))
summary(nhb.p.edu.yr.par)
nhb.p.edu.yr.par.tab <- tidy(nhb.p.edu.yr.par) %>% mutate(LL=sprintf(confint(nhb.p.edu.yr.par)[,1],fmt='%#.3f'),
                                                      UL=sprintf(confint(nhb.p.edu.yr.par)[,2],fmt='%#.3f')) %>%
  unite(col=CI,LL:UL, sep = ", ", remove=F)
  
# Hispanic
h.p.edu.yr.par <- svyglm(accdnam~edu.c*fedu.c +
                         sex +
                         scale(H13ITOT) + 
                         smokestatus +
                         R13BMI,design=subset(svy.edaa,raceth=="Hispanic"))
summary(h.p.edu.yr.par)
h.p.edu.yr.par.tab <- tidy(h.p.edu.yr.par) %>% mutate(LL=sprintf(confint(h.p.edu.yr.par)[,1],fmt='%#.3f'),
                                                  UL=sprintf(confint(h.p.edu.yr.par)[,2],fmt='%#.3f')) %>%
  unite(col=CI,LL:UL, sep = ", ", remove=F)
  
#NH White
nhw.p.edu.yr.par <- svyglm(accdnam~edu.c*fedu.c +
                           sex +
                           scale(H13ITOT) + 
                           smokestatus +
                           R13BMI,design=subset(svy.edaa,raceth=="NHW"))
summary(nhw.p.edu.yr.par)
nhw.p.edu.yr.par.tab <- tidy(nhw.p.edu.yr.par) %>% mutate(LL=sprintf(confint(nhw.p.edu.yr.par)[,1],fmt='%#.3f'),
                                                      UL=sprintf(confint(nhw.p.edu.yr.par)[,2],fmt='%#.3f')) %>%
  unite(col=CI,LL:UL, sep = ", ", remove=F)
  

# Stack Columns 
re.p.edu.yr.par.DNAMGRIMAGE<- bind_rows("NH Black"=nhb.p.edu.yr.par.tab,"NH Hispanic"=h.p.edu.yr.par.tab,"NH White"=nhw.p.edu.yr.par.tab,.id='Subgroup')
write.csv(re.p.edu.yr.par.DNAMGRIMAGE,file='re.p.edu.yr.par.DNAMGRIMAGE.csv')

## MPOA ----
# Unadjusted
p.edu.yr.par <- svyglm(accmpoa~edu.c*fedu.c,design=svy.edaa)
summary(p.edu.yr.par)
p.edu.yr.par.tab <- tidy(p.edu.yr.par) %>% mutate(LL=sprintf(confint(p.edu.yr.par)[,1],fmt='%#.3f'),
                                              UL=sprintf(confint(p.edu.yr.par)[,2],fmt='%#.3f')) %>%
  unite(col=CI,LL:UL, sep = ", ", remove=F)
  

# Adjusted
adj.p.edu.yr.par <- svyglm(accmpoa~edu.c*fedu.c +
                           raceth + 
                           sex +
                           scale(H13ITOT) + 
                           smokestatus +
                           R13BMI,design=svy.edaa)
summary(adj.p.edu.yr.par)
adj.p.edu.yr.par.tab <- tidy(adj.p.edu.yr.par) %>% mutate(LL=sprintf(confint(adj.p.edu.yr.par)[,1],fmt='%#.3f'),
                                                      UL=sprintf(confint(adj.p.edu.yr.par)[,2],fmt='%#.3f')) %>%
  unite(col=CI,LL:UL, sep = ", ", remove=F)
  
names(adj.p.edu.yr.par.tab) <- gsub("^(.*?)","adj_\\2", names(adj.p.edu.yr.par.tab))

# Combine results into one table
p.edu.yr.par.MPOA<- merge(p.edu.yr.par.tab,adj.p.edu.yr.par.tab,by.x='term',by.y='adj_term',all.y=T,sort=F) 
write.csv(p.edu.yr.par.MPOA,'p.edu.yr.par.MPOA.csv')

## Stratify by race/ethnicity
# NH Black
nhb.p.edu.yr.par <- svyglm(accmpoa~edu.c*fedu.c +
                           sex +
                           scale(H13ITOT) + 
                           smokestatus +
                           R13BMI,design=subset(svy.edaa,raceth=="NHB"))
summary(nhb.p.edu.yr.par)
nhb.p.edu.yr.par.tab <- tidy(nhb.p.edu.yr.par) %>% mutate(LL=sprintf(confint(nhb.p.edu.yr.par)[,1],fmt='%#.3f'),
                                                      UL=sprintf(confint(nhb.p.edu.yr.par)[,2],fmt='%#.3f')) %>%
  unite(col=CI,LL:UL, sep = ", ", remove=F)
  
# Hispanic
h.p.edu.yr.par <- svyglm(accmpoa~edu.c*fedu.c +
                         sex +
                         scale(H13ITOT) + 
                         smokestatus +
                         R13BMI,design=subset(svy.edaa,raceth=="Hispanic"))
summary(h.p.edu.yr.par)
h.p.edu.yr.par.tab <- tidy(h.p.edu.yr.par) %>% mutate(LL=sprintf(confint(h.p.edu.yr.par)[,1],fmt='%#.3f'),
                                                  UL=sprintf(confint(h.p.edu.yr.par)[,2],fmt='%#.3f')) %>%
  unite(col=CI,LL:UL, sep = ", ", remove=F)
  
#NH White
nhw.p.edu.yr.par <- svyglm(accmpoa~edu.c*fedu.c +
                           sex +
                           scale(H13ITOT) + 
                           smokestatus +
                           R13BMI,design=subset(svy.edaa,raceth=="NHW"))
summary(nhw.p.edu.yr.par)
nhw.p.edu.yr.par.tab <- tidy(nhw.p.edu.yr.par) %>% mutate(LL=sprintf(confint(nhw.p.edu.yr.par)[,1],fmt='%#.3f'),
                                                      UL=sprintf(confint(nhw.p.edu.yr.par)[,2],fmt='%#.3f')) %>%
  unite(col=CI,LL:UL, sep = ", ", remove=F)
  

# Stack Columns 
re.p.edu.yr.par.MPOA<- bind_rows("NH Black"=nhb.p.edu.yr.par.tab,"NH Hispanic"=h.p.edu.yr.par.tab,"NH White"=nhw.p.edu.yr.par.tab,.id='Subgroup')
write.csv(re.p.edu.yr.par.MPOA,file='re.p.edu.yr.par.MPOA.csv')

### Regression Models (Father Educational Attainment-Categorical) ----
## HORVATH_DNAMAGE ----
# Unadjusted
p.edu.c.par <- svyglm(acchorv~RAEDYRS_factor*RAFEDUC_bin,design=svy.edaa)
summary(p.edu.c.par)
p.edu.c.par.tab <- tidy(p.edu.c.par) %>% mutate(LL=sprintf(confint(p.edu.c.par)[,1],fmt='%#.3f'),
                                            UL=sprintf(confint(p.edu.c.par)[,2],fmt='%#.3f')) %>%
  unite(col=CI,LL:UL, sep = ", ", remove=F)
  

# Adjusted
adj.p.edu.c.par <- svyglm(acchorv~RAEDYRS_factor*RAFEDUC_bin +
                          raceth + 
                          sex +
                          scale(H13ITOT) + 
                          smokestatus +
                          R13BMI,design=svy.edaa)
summary(adj.p.edu.c.par)
adj.p.edu.c.par.tab <- tidy(adj.p.edu.c.par) %>% mutate(LL=sprintf(confint(adj.p.edu.c.par)[,1],fmt='%#.3f'),
                                                    UL=sprintf(confint(adj.p.edu.c.par)[,2],fmt='%#.3f')) %>%
  unite(col=CI,LL:UL, sep = ", ", remove=F)
  
names(adj.p.edu.c.par.tab) <- gsub("^(.*?)","adj_\\2", names(adj.p.edu.c.par.tab))

# Combine results into one table
p.edu.c.par.HORVATH_DNAMAGE <- merge(p.edu.c.par.tab,adj.p.edu.c.par.tab,by.x='term',by.y='adj_term',all.y=T,sort=F) 
write.csv(p.edu.c.par.HORVATH_DNAMAGE,'p.edu.c.par.HORVATH_DNAMAGE.csv')

## Marginal Means
# Unadjusted
HORVATH_DNAMAGE.MM <- emmeans(p.edu.c.par,pairwise~RAEDYRS_factor*RAFEDUC_bin,data=svy.edaa,adjust='mvt')

# Adjusted
adj.HORVATH_DNAMAGE.MM <- emmeans(adj.p.edu.c.par,pairwise~RAEDYRS_factor*RAFEDUC_bin,data=svy.edaa,adjust='mvt')

## Stratify by race/ethnicity
# NH Black
nhb.p.edu.c.par <- svyglm(acchorv~RAEDYRS_factor*RAFEDUC_bin +
                          sex +
                          scale(H13ITOT) + 
                          smokestatus +
                          R13BMI,design=subset(svy.edaa,raceth=="NHB"))
summary(nhb.p.edu.c.par)
nhb.p.edu.c.par.tab <- tidy(nhb.p.edu.c.par) %>% mutate(LL=sprintf(confint(nhb.p.edu.c.par)[,1],fmt='%#.3f'),
                                                    UL=sprintf(confint(nhb.p.edu.c.par)[,2],fmt='%#.3f')) %>%
  unite(col=CI,LL:UL, sep = ", ", remove=F)
  
# Hispanic
h.p.edu.c.par <- svyglm(acchorv~RAEDYRS_factor*RAFEDUC_bin +
                        sex +
                        scale(H13ITOT) + 
                        smokestatus +
                        R13BMI,design=subset(svy.edaa,raceth=="Hispanic"))
summary(h.p.edu.c.par)
h.p.edu.c.par.tab <- tidy(h.p.edu.c.par) %>% mutate(LL=sprintf(confint(h.p.edu.c.par)[,1],fmt='%#.3f'),
                                                UL=sprintf(confint(h.p.edu.c.par)[,2],fmt='%#.3f')) %>%
  unite(col=CI,LL:UL, sep = ", ", remove=F)
  
#NH White
nhw.p.edu.c.par <- svyglm(acchorv~RAEDYRS_factor*RAFEDUC_bin +
                          sex +
                          scale(H13ITOT) + 
                          smokestatus +
                          R13BMI,design=subset(svy.edaa,raceth=="NHW"))
summary(nhw.p.edu.c.par)
nhw.p.edu.c.par.tab <- tidy(nhw.p.edu.c.par) %>% mutate(LL=sprintf(confint(nhw.p.edu.c.par)[,1],fmt='%#.3f'),
                                                    UL=sprintf(confint(nhw.p.edu.c.par)[,2],fmt='%#.3f')) %>%
  unite(col=CI,LL:UL, sep = ", ", remove=F)
  

# Stack Columns 
re.p.edu.c.par.HORVATH_DNAMAGE <- bind_rows("NH Black"=nhb.p.edu.c.par.tab,"NH Hispanic"=h.p.edu.c.par.tab,"NH White"=nhw.p.edu.c.par.tab,.id='Subgroup')
write.csv(re.p.edu.c.par.HORVATH_DNAMAGE,file='re.p.edu.c.par.HORVATH_DNAMAGE.csv')

# Save ANOVA results
sink(file="p.aov.edu.c.HORVATH_DNAMAGE.txt")
print(aov<-list(anova(p.edu.c.par),anova(adj.p.edu.c.par)))
sink()

sink(file="re.p.aov.edu.c.HORVATH_DNAMAGE.txt")
print(aov<-list(anova(nhb.p.edu.c.par),anova(h.p.edu.c.par),anova(nhw.p.edu.c.par)))
sink()

## HANNUM_DNAMAGE ----
# Unadjusted
p.edu.c.par <- svyglm(acchannum~RAEDYRS_factor*RAFEDUC_bin,design=svy.edaa)
summary(p.edu.c.par)
p.edu.c.par.tab <- tidy(p.edu.c.par) %>% mutate(LL=sprintf(confint(p.edu.c.par)[,1],fmt='%#.3f'),
                                            UL=sprintf(confint(p.edu.c.par)[,2],fmt='%#.3f')) %>%
  unite(col=CI,LL:UL, sep = ", ", remove=F)
  

# Adjusted
adj.p.edu.c.par <- svyglm(acchannum~RAEDYRS_factor*RAFEDUC_bin +
                          raceth + 
                          sex +
                          scale(H13ITOT) + 
                          smokestatus +
                          R13BMI,design=svy.edaa)
summary(adj.p.edu.c.par)
adj.p.edu.c.par.tab <- tidy(adj.p.edu.c.par) %>% mutate(LL=sprintf(confint(adj.p.edu.c.par)[,1],fmt='%#.3f'),
                                                    UL=sprintf(confint(adj.p.edu.c.par)[,2],fmt='%#.3f')) %>%
  unite(col=CI,LL:UL, sep = ", ", remove=F)
  
names(adj.p.edu.c.par.tab) <- gsub("^(.*?)","adj_\\2", names(adj.p.edu.c.par.tab))

# Combine results into one table
p.edu.c.par.HANNUM_DNAMAGE <- merge(p.edu.c.par.tab,adj.p.edu.c.par.tab,by.x='term',by.y='adj_term',all.y=T,sort=F) 
write.csv(p.edu.c.par.HANNUM_DNAMAGE,'p.edu.c.par.HANNUM_DNAMAGE.csv')

## Marginal Means
# Unadjusted
HANNUM_DNAMAGE.MM <- emmeans(p.edu.c.par,pairwise~RAEDYRS_factor*RAFEDUC_bin,data=svy.edaa,adjust='mvt')

# Adjusted
adj.HANNUM_DNAMAGE.MM <- emmeans(adj.p.edu.c.par,pairwise~RAEDYRS_factor*RAFEDUC_bin,data=svy.edaa,adjust='mvt')

## Stratify by race/ethnicity
# NH Black
nhb.p.edu.c.par <- svyglm(acchannum~RAEDYRS_factor*RAFEDUC_bin +
                          sex +
                          scale(H13ITOT) + 
                          smokestatus +
                          R13BMI,design=subset(svy.edaa,raceth=="NHB"))
summary(nhb.p.edu.c.par)
nhb.p.edu.c.par.tab <- tidy(nhb.p.edu.c.par) %>% mutate(LL=sprintf(confint(nhb.p.edu.c.par)[,1],fmt='%#.3f'),
                                                    UL=sprintf(confint(nhb.p.edu.c.par)[,2],fmt='%#.3f')) %>%
  unite(col=CI,LL:UL, sep = ", ", remove=F)
  
# Hispanic
h.p.edu.c.par <- svyglm(acchannum~RAEDYRS_factor*RAFEDUC_bin +
                        sex +
                        scale(H13ITOT) + 
                        smokestatus +
                        R13BMI,design=subset(svy.edaa,raceth=="Hispanic"))
summary(h.p.edu.c.par)
h.p.edu.c.par.tab <- tidy(h.p.edu.c.par) %>% mutate(LL=sprintf(confint(h.p.edu.c.par)[,1],fmt='%#.3f'),
                                                UL=sprintf(confint(h.p.edu.c.par)[,2],fmt='%#.3f')) %>%
  unite(col=CI,LL:UL, sep = ", ", remove=F)
  
#NH White
nhw.p.edu.c.par <- svyglm(acchannum~RAEDYRS_factor*RAFEDUC_bin +
                          sex +
                          scale(H13ITOT) + 
                          smokestatus +
                          R13BMI,design=subset(svy.edaa,raceth=="NHW"))
summary(nhw.p.edu.c.par)
nhw.p.edu.c.par.tab <- tidy(nhw.p.edu.c.par) %>% mutate(LL=sprintf(confint(nhw.p.edu.c.par)[,1],fmt='%#.3f'),
                                                    UL=sprintf(confint(nhw.p.edu.c.par)[,2],fmt='%#.3f')) %>%
  unite(col=CI,LL:UL, sep = ", ", remove=F)
  

# Stack Columns 
re.p.edu.c.par.HANNUM_DNAMAGE<- bind_rows("NH Black"=nhb.p.edu.c.par.tab,"NH Hispanic"=h.p.edu.c.par.tab,"NH White"=nhw.p.edu.c.par.tab,.id='Subgroup')
write.csv(re.p.edu.c.par.HANNUM_DNAMAGE,file='re.p.edu.c.par.HANNUM_DNAMAGE.csv')

# Save ANOVA results
sink(file="p.aov.edu.c.HANNUM_DNAMAGE.txt")
print(aov<-list(anova(p.edu.c.par),anova(adj.p.edu.c.par)))
sink()

sink(file="re.p.aov.edu.c.HANNUM_DNAMAGE.txt")
print(aov<-list(anova(nhb.p.edu.c.par),anova(h.p.edu.c.par),anova(nhw.p.edu.c.par)))
sink()

## LEVINE_DNAMAGE ----
# Unadjusted
p.edu.c.par <- svyglm(acclevine~RAEDYRS_factor*RAFEDUC_bin,design=svy.edaa)
summary(p.edu.c.par)
p.edu.c.par.tab <- tidy(p.edu.c.par) %>% mutate(LL=sprintf(confint(p.edu.c.par)[,1],fmt='%#.3f'),
                                            UL=sprintf(confint(p.edu.c.par)[,2],fmt='%#.3f')) %>%
  unite(col=CI,LL:UL, sep = ", ", remove=F)
  

# Adjusted
adj.p.edu.c.par <- svyglm(acclevine~RAEDYRS_factor*RAFEDUC_bin +
                          raceth + 
                          sex +
                          scale(H13ITOT) + 
                          smokestatus +
                          R13BMI,design=svy.edaa)
summary(adj.p.edu.c.par)
adj.p.edu.c.par.tab <- tidy(adj.p.edu.c.par) %>% mutate(LL=sprintf(confint(adj.p.edu.c.par)[,1],fmt='%#.3f'),
                                                    UL=sprintf(confint(adj.p.edu.c.par)[,2],fmt='%#.3f')) %>%
  unite(col=CI,LL:UL, sep = ", ", remove=F)
  
names(adj.p.edu.c.par.tab) <- gsub("^(.*?)","adj_\\2", names(adj.p.edu.c.par.tab))

# Combine results into one table
p.edu.c.par.LEVINE_DNAMAGE <- merge(p.edu.c.par.tab,adj.p.edu.c.par.tab,by.x='term',by.y='adj_term',all.y=T,sort=F) 
write.csv(p.edu.c.par.LEVINE_DNAMAGE,'p.edu.c.par.LEVINE_DNAMAGE.csv')

## Marginal Means
# Unadjusted
LEVINE_DNAMAGE.MM <- emmeans(p.edu.c.par,pairwise~RAEDYRS_factor*RAFEDUC_bin,data=svy.edaa,adjust='mvt')

# Adjusted
adj.LEVINE_DNAMAGE.MM <- emmeans(adj.p.edu.c.par,pairwise~RAEDYRS_factor*RAFEDUC_bin,data=svy.edaa,adjust='mvt')

## Stratify by race/ethnicity
# NH Black
nhb.p.edu.c.par <- svyglm(acclevine~RAEDYRS_factor*RAFEDUC_bin +
                          sex +
                          scale(H13ITOT) + 
                          smokestatus +
                          R13BMI,design=subset(svy.edaa,raceth=="NHB"))
summary(nhb.p.edu.c.par)
nhb.p.edu.c.par.tab <- tidy(nhb.p.edu.c.par) %>% mutate(LL=sprintf(confint(nhb.p.edu.c.par)[,1],fmt='%#.3f'),
                                                    UL=sprintf(confint(nhb.p.edu.c.par)[,2],fmt='%#.3f')) %>%
  unite(col=CI,LL:UL, sep = ", ", remove=F)
  
# Hispanic
h.p.edu.c.par <- svyglm(acclevine~RAEDYRS_factor*RAFEDUC_bin +
                        sex +
                        scale(H13ITOT) + 
                        smokestatus +
                        R13BMI,design=subset(svy.edaa,raceth=="Hispanic"))
summary(h.p.edu.c.par)
h.p.edu.c.par.tab <- tidy(h.p.edu.c.par) %>% mutate(LL=sprintf(confint(h.p.edu.c.par)[,1],fmt='%#.3f'),
                                                UL=sprintf(confint(h.p.edu.c.par)[,2],fmt='%#.3f')) %>%
  unite(col=CI,LL:UL, sep = ", ", remove=F)
  
#NH White
nhw.p.edu.c.par <- svyglm(acclevine~RAEDYRS_factor*RAFEDUC_bin +
                          sex +
                          scale(H13ITOT) + 
                          smokestatus +
                          R13BMI,design=subset(svy.edaa,raceth=="NHW"))
summary(nhw.p.edu.c.par)
nhw.p.edu.c.par.tab <- tidy(nhw.p.edu.c.par) %>% mutate(LL=sprintf(confint(nhw.p.edu.c.par)[,1],fmt='%#.3f'),
                                                    UL=sprintf(confint(nhw.p.edu.c.par)[,2],fmt='%#.3f')) %>%
  unite(col=CI,LL:UL, sep = ", ", remove=F)
  

# Stack Columns 
re.p.edu.c.par.LEVINE_DNAMAGE<- bind_rows("NH Black"=nhb.p.edu.c.par.tab,"NH Hispanic"=h.p.edu.c.par.tab,"NH White"=nhw.p.edu.c.par.tab,.id='Subgroup')
write.csv(re.p.edu.c.par.LEVINE_DNAMAGE,file='re.p.edu.c.par.LEVINE_DNAMAGE.csv')

# Save ANOVA results
sink(file="p.aov.edu.c.LEVINE_DNAMAGE.txt")
print(aov<-list(anova(p.edu.c.par),anova(adj.p.edu.c.par)))
sink()

sink(file="re.p.aov.edu.c.LEVINE_DNAMAGE.txt")
print(aov<-list(anova(nhb.p.edu.c.par),anova(h.p.edu.c.par),anova(nhw.p.edu.c.par)))
sink()

## HORVATHSKIN_DNAMAGE ----
# Unadjusted
p.edu.c.par <- svyglm(acchorvsk~RAEDYRS_factor*RAFEDUC_bin,design=svy.edaa)
summary(p.edu.c.par)
p.edu.c.par.tab <- tidy(p.edu.c.par) %>% mutate(LL=sprintf(confint(p.edu.c.par)[,1],fmt='%#.3f'),
                                            UL=sprintf(confint(p.edu.c.par)[,2],fmt='%#.3f')) %>%
  unite(col=CI,LL:UL, sep = ", ", remove=F)
  

# Adjusted
adj.p.edu.c.par <- svyglm(acchorvsk~RAEDYRS_factor*RAFEDUC_bin +
                          raceth + 
                          sex +
                          scale(H13ITOT) + 
                          smokestatus +
                          R13BMI,design=svy.edaa)
summary(adj.p.edu.c.par)
adj.p.edu.c.par.tab <- tidy(adj.p.edu.c.par) %>% mutate(LL=sprintf(confint(adj.p.edu.c.par)[,1],fmt='%#.3f'),
                                                    UL=sprintf(confint(adj.p.edu.c.par)[,2],fmt='%#.3f')) %>%
  unite(col=CI,LL:UL, sep = ", ", remove=F)
  
names(adj.p.edu.c.par.tab) <- gsub("^(.*?)","adj_\\2", names(adj.p.edu.c.par.tab))

# Combine results into one table
p.edu.c.par.HORVATHSKIN_DNAMAGE<- merge(p.edu.c.par.tab,adj.p.edu.c.par.tab,by.x='term',by.y='adj_term',all.y=T,sort=F) 
write.csv(p.edu.c.par.HORVATHSKIN_DNAMAGE,'p.edu.c.par.HORVATHSKIN_DNAMAGE.csv')

## Marginal Means
# Unadjusted
HORVATHSKIN_DNAMAGE.MM <- emmeans(p.edu.c.par,pairwise~RAEDYRS_factor*RAFEDUC_bin,data=svy.edaa,adjust='mvt')

# Adjusted
adj.HORVATHSKIN_DNAMAGE.MM <- emmeans(adj.p.edu.c.par,pairwise~RAEDYRS_factor*RAFEDUC_bin,data=svy.edaa,adjust='mvt')

## Stratify by race/ethnicity
# NH Black
nhb.p.edu.c.par <- svyglm(acchorvsk~RAEDYRS_factor*RAFEDUC_bin +
                          sex +
                          scale(H13ITOT) + 
                          smokestatus +
                          R13BMI,design=subset(svy.edaa,raceth=="NHB"))
summary(nhb.p.edu.c.par)
nhb.p.edu.c.par.tab <- tidy(nhb.p.edu.c.par) %>% mutate(LL=sprintf(confint(nhb.p.edu.c.par)[,1],fmt='%#.3f'),
                                                    UL=sprintf(confint(nhb.p.edu.c.par)[,2],fmt='%#.3f')) %>%
  unite(col=CI,LL:UL, sep = ", ", remove=F)
  
# Hispanic
h.p.edu.c.par <- svyglm(acchorvsk~RAEDYRS_factor*RAFEDUC_bin +
                        sex +
                        scale(H13ITOT) + 
                        smokestatus +
                        R13BMI,design=subset(svy.edaa,raceth=="Hispanic"))
summary(h.p.edu.c.par)
h.p.edu.c.par.tab <- tidy(h.p.edu.c.par) %>% mutate(LL=sprintf(confint(h.p.edu.c.par)[,1],fmt='%#.3f'),
                                                UL=sprintf(confint(h.p.edu.c.par)[,2],fmt='%#.3f')) %>%
  unite(col=CI,LL:UL, sep = ", ", remove=F)
  
#NH White
nhw.p.edu.c.par <- svyglm(acchorvsk~RAEDYRS_factor*RAFEDUC_bin +
                          sex +
                          scale(H13ITOT) + 
                          smokestatus +
                          R13BMI,design=subset(svy.edaa,raceth=="NHW"))
summary(nhw.p.edu.c.par)
nhw.p.edu.c.par.tab <- tidy(nhw.p.edu.c.par) %>% mutate(LL=sprintf(confint(nhw.p.edu.c.par)[,1],fmt='%#.3f'),
                                                    UL=sprintf(confint(nhw.p.edu.c.par)[,2],fmt='%#.3f')) %>%
  unite(col=CI,LL:UL, sep = ", ", remove=F)
  

# Stack Columns 
re.p.edu.c.par.HORVATHSKIN_DNAMAGE<- bind_rows("NH Black"=nhb.p.edu.c.par.tab,"NH Hispanic"=h.p.edu.c.par.tab,"NH White"=nhw.p.edu.c.par.tab,.id='Subgroup')
write.csv(re.p.edu.c.par.HORVATHSKIN_DNAMAGE,file='re.p.edu.c.par.HORVATHSKIN_DNAMAGE.csv')

# Save ANOVA results
sink(file="p.aov.edu.c.HORVATHSKIN_DNAMAGE.txt")
print(aov<-list(anova(p.edu.c.par),anova(adj.p.edu.c.par)))
sink()

sink(file="re.p.aov.edu.c.HORVATHSKIN_DNAMAGE.txt")
print(aov<-list(anova(nhb.p.edu.c.par),anova(h.p.edu.c.par),anova(nhw.p.edu.c.par)))
sink()

## LIN_DNAMAGE ----
# Unadjusted
p.edu.c.par <- svyglm(acclin~RAEDYRS_factor*RAFEDUC_bin,design=svy.edaa)
summary(p.edu.c.par)
p.edu.c.par.tab <- tidy(p.edu.c.par) %>% mutate(LL=sprintf(confint(p.edu.c.par)[,1],fmt='%#.3f'),
                                            UL=sprintf(confint(p.edu.c.par)[,2],fmt='%#.3f')) %>%
  unite(col=CI,LL:UL, sep = ", ", remove=F)
  

# Adjusted
adj.p.edu.c.par <- svyglm(acclin~RAEDYRS_factor*RAFEDUC_bin +
                          raceth + 
                          sex +
                          scale(H13ITOT) + 
                          smokestatus +
                          R13BMI,design=svy.edaa)
summary(adj.p.edu.c.par)
adj.p.edu.c.par.tab <- tidy(adj.p.edu.c.par) %>% mutate(LL=sprintf(confint(adj.p.edu.c.par)[,1],fmt='%#.3f'),
                                                    UL=sprintf(confint(adj.p.edu.c.par)[,2],fmt='%#.3f')) %>%
  unite(col=CI,LL:UL, sep = ", ", remove=F)
  
names(adj.p.edu.c.par.tab) <- gsub("^(.*?)","adj_\\2", names(adj.p.edu.c.par.tab))

# Combine results into one table
p.edu.c.par.LIN_DNAMAGE <- merge(p.edu.c.par.tab,adj.p.edu.c.par.tab,by.x='term',by.y='adj_term',all.y=T,sort=F) 
write.csv(p.edu.c.par.LIN_DNAMAGE,'p.edu.c.par.LIN_DNAMAGE.csv')

## Marginal Means
# Unadjusted
LIN_DNAMAGE.MM <- emmeans(p.edu.c.par,pairwise~RAEDYRS_factor*RAFEDUC_bin,data=svy.edaa,adjust='mvt')

# Adjusted
adj.LIN_DNAMAGE.MM <- emmeans(adj.p.edu.c.par,pairwise~RAEDYRS_factor*RAFEDUC_bin,data=svy.edaa,adjust='mvt')

## Stratify by race/ethnicity
# NH Black
nhb.p.edu.c.par <- svyglm(acclin~RAEDYRS_factor*RAFEDUC_bin +
                          sex +
                          scale(H13ITOT) + 
                          smokestatus +
                          R13BMI,design=subset(svy.edaa,raceth=="NHB"))
summary(nhb.p.edu.c.par)
nhb.p.edu.c.par.tab <- tidy(nhb.p.edu.c.par) %>% mutate(LL=sprintf(confint(nhb.p.edu.c.par)[,1],fmt='%#.3f'),
                                                    UL=sprintf(confint(nhb.p.edu.c.par)[,2],fmt='%#.3f')) %>%
  unite(col=CI,LL:UL, sep = ", ", remove=F)
  
# Hispanic
h.p.edu.c.par <- svyglm(acclin~RAEDYRS_factor*RAFEDUC_bin +
                        sex +
                        scale(H13ITOT) + 
                        smokestatus +
                        R13BMI,design=subset(svy.edaa,raceth=="Hispanic"))
summary(h.p.edu.c.par)
h.p.edu.c.par.tab <- tidy(h.p.edu.c.par) %>% mutate(LL=sprintf(confint(h.p.edu.c.par)[,1],fmt='%#.3f'),
                                                UL=sprintf(confint(h.p.edu.c.par)[,2],fmt='%#.3f')) %>%
  unite(col=CI,LL:UL, sep = ", ", remove=F)
  
#NH White
nhw.p.edu.c.par <- svyglm(acclin~RAEDYRS_factor*RAFEDUC_bin +
                          sex +
                          scale(H13ITOT) + 
                          smokestatus +
                          R13BMI,design=subset(svy.edaa,raceth=="NHW"))
summary(nhw.p.edu.c.par)
nhw.p.edu.c.par.tab <- tidy(nhw.p.edu.c.par) %>% mutate(LL=sprintf(confint(nhw.p.edu.c.par)[,1],fmt='%#.3f'),
                                                    UL=sprintf(confint(nhw.p.edu.c.par)[,2],fmt='%#.3f')) %>%
  unite(col=CI,LL:UL, sep = ", ", remove=F)
  

# Stack Columns 
re.p.edu.c.par.LIN_DNAMAGE<- bind_rows("NH Black"=nhb.p.edu.c.par.tab,"NH Hispanic"=h.p.edu.c.par.tab,"NH White"=nhw.p.edu.c.par.tab,.id='Subgroup')
write.csv(re.p.edu.c.par.LIN_DNAMAGE,file='re.p.edu.c.par.LIN_DNAMAGE.csv')

# Save ANOVA results
sink(file="p.aov.edu.c.LIN_DNAMAGE.txt")
print(aov<-list(anova(p.edu.c.par),anova(adj.p.edu.c.par)))
sink()

sink(file="re.p.aov.edu.c.LIN_DNAMAGE.txt")
print(aov<-list(anova(nhb.p.edu.c.par),anova(h.p.edu.c.par),anova(nhw.p.edu.c.par)))
sink()

## WEIDNER_DNAMAGE ----
# Unadjusted
p.edu.c.par <- svyglm(accweid~RAEDYRS_factor*RAFEDUC_bin,design=svy.edaa)
summary(p.edu.c.par)
p.edu.c.par.tab <- tidy(p.edu.c.par) %>% mutate(LL=sprintf(confint(p.edu.c.par)[,1],fmt='%#.3f'),
                                            UL=sprintf(confint(p.edu.c.par)[,2],fmt='%#.3f')) %>%
  unite(col=CI,LL:UL, sep = ", ", remove=F)
  

# Adjusted
adj.p.edu.c.par <- svyglm(accweid~RAEDYRS_factor*RAFEDUC_bin +
                          raceth + 
                          sex +
                          scale(H13ITOT) + 
                          smokestatus +
                          R13BMI,design=svy.edaa)
summary(adj.p.edu.c.par)
adj.p.edu.c.par.tab <- tidy(adj.p.edu.c.par) %>% mutate(LL=sprintf(confint(adj.p.edu.c.par)[,1],fmt='%#.3f'),
                                                    UL=sprintf(confint(adj.p.edu.c.par)[,2],fmt='%#.3f')) %>%
  unite(col=CI,LL:UL, sep = ", ", remove=F)
  
names(adj.p.edu.c.par.tab) <- gsub("^(.*?)","adj_\\2", names(adj.p.edu.c.par.tab))

# Combine results into one table
p.edu.c.par.WEIDNER_DNAMAGE <- merge(p.edu.c.par.tab,adj.p.edu.c.par.tab,by.x='term',by.y='adj_term',all.y=T,sort=F) 
write.csv(p.edu.c.par.WEIDNER_DNAMAGE,'p.edu.c.par.WEIDNER_DNAMAGE.csv')

## Marginal Means
# Unadjusted
WEIDNER_DNAMAGE.MM <- emmeans(p.edu.c.par,pairwise~RAEDYRS_factor*RAFEDUC_bin,data=svy.edaa,adjust='mvt')

# Adjusted
adj.WEIDNER_DNAMAGE.MM <- emmeans(adj.p.edu.c.par,pairwise~RAEDYRS_factor*RAFEDUC_bin,data=svy.edaa,adjust='mvt')

## Stratify by race/ethnicity
# NH Black
nhb.p.edu.c.par <- svyglm(accweid~RAEDYRS_factor*RAFEDUC_bin +
                          sex +
                          scale(H13ITOT) + 
                          smokestatus +
                          R13BMI,design=subset(svy.edaa,raceth=="NHB"))
summary(nhb.p.edu.c.par)
nhb.p.edu.c.par.tab <- tidy(nhb.p.edu.c.par) %>% mutate(LL=sprintf(confint(nhb.p.edu.c.par)[,1],fmt='%#.3f'),
                                                    UL=sprintf(confint(nhb.p.edu.c.par)[,2],fmt='%#.3f')) %>%
  unite(col=CI,LL:UL, sep = ", ", remove=F)
  
# Hispanic
h.p.edu.c.par <- svyglm(accweid~RAEDYRS_factor*RAFEDUC_bin +
                        sex +
                        scale(H13ITOT) + 
                        smokestatus +
                        R13BMI,design=subset(svy.edaa,raceth=="Hispanic"))
summary(h.p.edu.c.par)
h.p.edu.c.par.tab <- tidy(h.p.edu.c.par) %>% mutate(LL=sprintf(confint(h.p.edu.c.par)[,1],fmt='%#.3f'),
                                                UL=sprintf(confint(h.p.edu.c.par)[,2],fmt='%#.3f')) %>%
  unite(col=CI,LL:UL, sep = ", ", remove=F)
  
#NH White
nhw.p.edu.c.par <- svyglm(accweid~RAEDYRS_factor*RAFEDUC_bin +
                          sex +
                          scale(H13ITOT) + 
                          smokestatus +
                          R13BMI,design=subset(svy.edaa,raceth=="NHW"))
summary(nhw.p.edu.c.par)
nhw.p.edu.c.par.tab <- tidy(nhw.p.edu.c.par) %>% mutate(LL=sprintf(confint(nhw.p.edu.c.par)[,1],fmt='%#.3f'),
                                                    UL=sprintf(confint(nhw.p.edu.c.par)[,2],fmt='%#.3f')) %>%
  unite(col=CI,LL:UL, sep = ", ", remove=F)
  

# Stack Columns 
re.p.edu.c.par.WEIDNER_DNAMAGE<- bind_rows("NH Black"=nhb.p.edu.c.par.tab,"NH Hispanic"=h.p.edu.c.par.tab,"NH White"=nhw.p.edu.c.par.tab,.id='Subgroup')
write.csv(re.p.edu.c.par.WEIDNER_DNAMAGE,file='re.p.edu.c.par.WEIDNER_DNAMAGE.csv')

# Save ANOVA results
sink(file="p.aov.edu.c.WEIDNER_DNAMAGE.txt")
print(aov<-list(anova(p.edu.c.par),anova(adj.p.edu.c.par)))
sink()

sink(file="re.p.aov.edu.c.WEIDNER_DNAMAGE.txt")
print(aov<-list(anova(nhb.p.edu.c.par),anova(h.p.edu.c.par),anova(nhw.p.edu.c.par)))
sink()

## VIDALBRALO_DNAMAGE ----
# Unadjusted
p.edu.c.par <- svyglm(accvidalb~RAEDYRS_factor*RAFEDUC_bin,design=svy.edaa)
summary(p.edu.c.par)
p.edu.c.par.tab <- tidy(p.edu.c.par) %>% mutate(LL=sprintf(confint(p.edu.c.par)[,1],fmt='%#.3f'),
                                            UL=sprintf(confint(p.edu.c.par)[,2],fmt='%#.3f')) %>%
  unite(col=CI,LL:UL, sep = ", ", remove=F)
  

# Adjusted
adj.p.edu.c.par <- svyglm(accvidalb~RAEDYRS_factor*RAFEDUC_bin +
                          raceth + 
                          sex +
                          scale(H13ITOT) + 
                          smokestatus +
                          R13BMI,design=svy.edaa)
summary(adj.p.edu.c.par)
adj.p.edu.c.par.tab <- tidy(adj.p.edu.c.par) %>% mutate(LL=sprintf(confint(adj.p.edu.c.par)[,1],fmt='%#.3f'),
                                                    UL=sprintf(confint(adj.p.edu.c.par)[,2],fmt='%#.3f')) %>%
  unite(col=CI,LL:UL, sep = ", ", remove=F)
  
names(adj.p.edu.c.par.tab) <- gsub("^(.*?)","adj_\\2", names(adj.p.edu.c.par.tab))

# Combine results into one table
p.edu.c.par.VIDALBRALO_DNAMAGE <- merge(p.edu.c.par.tab,adj.p.edu.c.par.tab,by.x='term',by.y='adj_term',all.y=T,sort=F) 
write.csv(p.edu.c.par.VIDALBRALO_DNAMAGE,'p.edu.c.par.VIDALBRALO_DNAMAGE.csv')

## Marginal Means
# Unadjusted
VIDALBRALO_DNAMAGE.MM <- emmeans(p.edu.c.par,pairwise~RAEDYRS_factor*RAFEDUC_bin,data=svy.edaa,adjust='mvt')

# Adjusted
adj.VIDALBRALO_DNAMAGE.MM <- emmeans(adj.p.edu.c.par,pairwise~RAEDYRS_factor*RAFEDUC_bin,data=svy.edaa,adjust='mvt')

## Stratify by race/ethnicity
# NH Black
nhb.p.edu.c.par <- svyglm(accvidalb~RAEDYRS_factor*RAFEDUC_bin +
                          sex +
                          scale(H13ITOT) + 
                          smokestatus +
                          R13BMI,design=subset(svy.edaa,raceth=="NHB"))
summary(nhb.p.edu.c.par)
nhb.p.edu.c.par.tab <- tidy(nhb.p.edu.c.par) %>% mutate(LL=sprintf(confint(nhb.p.edu.c.par)[,1],fmt='%#.3f'),
                                                    UL=sprintf(confint(nhb.p.edu.c.par)[,2],fmt='%#.3f')) %>%
  unite(col=CI,LL:UL, sep = ", ", remove=F)
  
# Hispanic
h.p.edu.c.par <- svyglm(accvidalb~RAEDYRS_factor*RAFEDUC_bin +
                        sex +
                        scale(H13ITOT) + 
                        smokestatus +
                        R13BMI,design=subset(svy.edaa,raceth=="Hispanic"))
summary(h.p.edu.c.par)
h.p.edu.c.par.tab <- tidy(h.p.edu.c.par) %>% mutate(LL=sprintf(confint(h.p.edu.c.par)[,1],fmt='%#.3f'),
                                                UL=sprintf(confint(h.p.edu.c.par)[,2],fmt='%#.3f')) %>%
  unite(col=CI,LL:UL, sep = ", ", remove=F)
  
#NH White
nhw.p.edu.c.par <- svyglm(accvidalb~RAEDYRS_factor*RAFEDUC_bin +
                          sex +
                          scale(H13ITOT) + 
                          smokestatus +
                          R13BMI,design=subset(svy.edaa,raceth=="NHW"))
summary(nhw.p.edu.c.par)
nhw.p.edu.c.par.tab <- tidy(nhw.p.edu.c.par) %>% mutate(LL=sprintf(confint(nhw.p.edu.c.par)[,1],fmt='%#.3f'),
                                                    UL=sprintf(confint(nhw.p.edu.c.par)[,2],fmt='%#.3f')) %>%
  unite(col=CI,LL:UL, sep = ", ", remove=F)
  

# Stack Columns 
re.p.edu.c.par.VIDALBRALO_DNAMAGE<- bind_rows("NH Black"=nhb.p.edu.c.par.tab,"NH Hispanic"=h.p.edu.c.par.tab,"NH White"=nhw.p.edu.c.par.tab,.id='Subgroup')
write.csv(re.p.edu.c.par.VIDALBRALO_DNAMAGE,file='re.p.edu.c.par.VIDALBRALO_DNAMAGE.csv')

# Save ANOVA results
sink(file="p.aov.edu.c.VIDALBRALO_DNAMAGE.txt")
print(aov<-list(anova(p.edu.c.par),anova(adj.p.edu.c.par)))
sink()

sink(file="re.p.aov.edu.c.VIDALBRALO_DNAMAGE.txt")
print(aov<-list(anova(nhb.p.edu.c.par),anova(h.p.edu.c.par),anova(nhw.p.edu.c.par)))
sink()

## YANG_DNAMAGE ----
# Unadjusted
p.edu.c.par <- svyglm(accyang~RAEDYRS_factor*RAFEDUC_bin,design=svy.edaa)
summary(p.edu.c.par)
p.edu.c.par.tab <- tidy(p.edu.c.par) %>% mutate(LL=sprintf(confint(p.edu.c.par)[,1],fmt='%#.3f'),
                                            UL=sprintf(confint(p.edu.c.par)[,2],fmt='%#.3f')) %>%
  unite(col=CI,LL:UL, sep = ", ", remove=F)
  

# Adjusted
adj.p.edu.c.par <- svyglm(accyang~RAEDYRS_factor*RAFEDUC_bin +
                          raceth + 
                          sex +
                          scale(H13ITOT) + 
                          smokestatus +
                          R13BMI,design=svy.edaa)
summary(adj.p.edu.c.par)
adj.p.edu.c.par.tab <- tidy(adj.p.edu.c.par) %>% mutate(LL=sprintf(confint(adj.p.edu.c.par)[,1],fmt='%#.3f'),
                                                    UL=sprintf(confint(adj.p.edu.c.par)[,2],fmt='%#.3f')) %>%
  unite(col=CI,LL:UL, sep = ", ", remove=F)
  
names(adj.p.edu.c.par.tab) <- gsub("^(.*?)","adj_\\2", names(adj.p.edu.c.par.tab))

# Combine results into one table
p.edu.c.par.YANG_DNAMAGE <- merge(p.edu.c.par.tab,adj.p.edu.c.par.tab,by.x='term',by.y='adj_term',all.y=T,sort=F) 
write.csv(p.edu.c.par.YANG_DNAMAGE,'p.edu.c.par.YANG_DNAMAGE.csv')

## Marginal Means
# Unadjusted
YANG_DNAMAGE.MM <- emmeans(p.edu.c.par,pairwise~RAEDYRS_factor*RAFEDUC_bin,data=svy.edaa,adjust='mvt')

# Adjusted
adj.YANG_DNAMAGE.MM <- emmeans(adj.p.edu.c.par,pairwise~RAEDYRS_factor*RAFEDUC_bin,data=svy.edaa,adjust='mvt')

## Stratify by race/ethnicity
# NH Black
nhb.p.edu.c.par <- svyglm(accyang~RAEDYRS_factor*RAFEDUC_bin +
                          sex +
                          scale(H13ITOT) + 
                          smokestatus +
                          R13BMI,design=subset(svy.edaa,raceth=="NHB"))
summary(nhb.p.edu.c.par)
nhb.p.edu.c.par.tab <- tidy(nhb.p.edu.c.par) %>% mutate(LL=sprintf(confint(nhb.p.edu.c.par)[,1],fmt='%#.3f'),
                                                    UL=sprintf(confint(nhb.p.edu.c.par)[,2],fmt='%#.3f')) %>%
  unite(col=CI,LL:UL, sep = ", ", remove=F)
  
# Hispanic
h.p.edu.c.par <- svyglm(accyang~RAEDYRS_factor*RAFEDUC_bin +
                        sex +
                        scale(H13ITOT) + 
                        smokestatus +
                        R13BMI,design=subset(svy.edaa,raceth=="Hispanic"))
summary(h.p.edu.c.par)
h.p.edu.c.par.tab <- tidy(h.p.edu.c.par) %>% mutate(LL=sprintf(confint(h.p.edu.c.par)[,1],fmt='%#.3f'),
                                                UL=sprintf(confint(h.p.edu.c.par)[,2],fmt='%#.3f')) %>%
  unite(col=CI,LL:UL, sep = ", ", remove=F)
  
#NH White
nhw.p.edu.c.par <- svyglm(accyang~RAEDYRS_factor*RAFEDUC_bin +
                          sex +
                          scale(H13ITOT) + 
                          smokestatus +
                          R13BMI,design=subset(svy.edaa,raceth=="NHW"))
summary(nhw.p.edu.c.par)
nhw.p.edu.c.par.tab <- tidy(nhw.p.edu.c.par) %>% mutate(LL=sprintf(confint(nhw.p.edu.c.par)[,1],fmt='%#.3f'),
                                                    UL=sprintf(confint(nhw.p.edu.c.par)[,2],fmt='%#.3f')) %>%
  unite(col=CI,LL:UL, sep = ", ", remove=F)
  

# Stack Columns 
re.p.edu.c.par.YANG_DNAMAGE<- bind_rows("NH Black"=nhb.p.edu.c.par.tab,"NH Hispanic"=h.p.edu.c.par.tab,"NH White"=nhw.p.edu.c.par.tab,.id='Subgroup')
write.csv(re.p.edu.c.par.YANG_DNAMAGE,file='re.p.edu.c.par.YANG_DNAMAGE.csv')

# Save ANOVA results
sink(file="p.aov.edu.c.YANG_DNAMAGE.txt")
print(aov<-list(anova(p.edu.c.par),anova(adj.p.edu.c.par)))
sink()

sink(file="re.p.aov.edu.c.YANG_DNAMAGE.txt")
print(aov<-list(anova(nhb.p.edu.c.par),anova(h.p.edu.c.par),anova(nhw.p.edu.c.par)))
sink()

## ZHANG_DNAMAGE ----
# Unadjusted
p.edu.c.par <- svyglm(acczhang~RAEDYRS_factor*RAFEDUC_bin,design=svy.edaa)
summary(p.edu.c.par)
p.edu.c.par.tab <- tidy(p.edu.c.par) %>% mutate(LL=sprintf(confint(p.edu.c.par)[,1],fmt='%#.3f'),
                                            UL=sprintf(confint(p.edu.c.par)[,2],fmt='%#.3f')) %>%
  unite(col=CI,LL:UL, sep = ", ", remove=F)
  

# Adjusted
adj.p.edu.c.par <- svyglm(acczhang~RAEDYRS_factor*RAFEDUC_bin +
                          raceth + 
                          sex +
                          scale(H13ITOT) + 
                          smokestatus +
                          R13BMI,design=svy.edaa)
summary(adj.p.edu.c.par)
adj.p.edu.c.par.tab <- tidy(adj.p.edu.c.par) %>% mutate(LL=sprintf(confint(adj.p.edu.c.par)[,1],fmt='%#.3f'),
                                                    UL=sprintf(confint(adj.p.edu.c.par)[,2],fmt='%#.3f')) %>%
  unite(col=CI,LL:UL, sep = ", ", remove=F)
  
names(adj.p.edu.c.par.tab) <- gsub("^(.*?)","adj_\\2", names(adj.p.edu.c.par.tab))

# Combine results into one table
p.edu.c.par.ZHANG_DNAMAGE <- merge(p.edu.c.par.tab,adj.p.edu.c.par.tab,by.x='term',by.y='adj_term',all.y=T,sort=F) 
write.csv(p.edu.c.par.ZHANG_DNAMAGE,'p.edu.c.par.ZHANG_DNAMAGE.csv')

## Marginal Means
# Unadjusted
ZHANG_DNAMAGE.MM <- emmeans(p.edu.c.par,pairwise~RAEDYRS_factor*RAFEDUC_bin,data=svy.edaa,adjust='mvt')

# Adjusted
adj.ZHANG_DNAMAGE.MM <- emmeans(adj.p.edu.c.par,pairwise~RAEDYRS_factor*RAFEDUC_bin,data=svy.edaa,adjust='mvt')

## Stratify by race/ethnicity
# NH Black
nhb.p.edu.c.par <- svyglm(acczhang~RAEDYRS_factor*RAFEDUC_bin +
                          sex +
                          scale(H13ITOT) + 
                          smokestatus +
                          R13BMI,design=subset(svy.edaa,raceth=="NHB"))
summary(nhb.p.edu.c.par)
nhb.p.edu.c.par.tab <- tidy(nhb.p.edu.c.par) %>% mutate(LL=sprintf(confint(nhb.p.edu.c.par)[,1],fmt='%#.3f'),
                                                    UL=sprintf(confint(nhb.p.edu.c.par)[,2],fmt='%#.3f')) %>%
  unite(col=CI,LL:UL, sep = ", ", remove=F)
  
# Hispanic
h.p.edu.c.par <- svyglm(acczhang~RAEDYRS_factor*RAFEDUC_bin +
                        sex +
                        scale(H13ITOT) + 
                        smokestatus +
                        R13BMI,design=subset(svy.edaa,raceth=="Hispanic"))
summary(h.p.edu.c.par)
h.p.edu.c.par.tab <- tidy(h.p.edu.c.par) %>% mutate(LL=sprintf(confint(h.p.edu.c.par)[,1],fmt='%#.3f'),
                                                UL=sprintf(confint(h.p.edu.c.par)[,2],fmt='%#.3f')) %>%
  unite(col=CI,LL:UL, sep = ", ", remove=F)
  
#NH White
nhw.p.edu.c.par <- svyglm(acczhang~RAEDYRS_factor*RAFEDUC_bin +
                          sex +
                          scale(H13ITOT) + 
                          smokestatus +
                          R13BMI,design=subset(svy.edaa,raceth=="NHW"))
summary(nhw.p.edu.c.par)
nhw.p.edu.c.par.tab <- tidy(nhw.p.edu.c.par) %>% mutate(LL=sprintf(confint(nhw.p.edu.c.par)[,1],fmt='%#.3f'),
                                                    UL=sprintf(confint(nhw.p.edu.c.par)[,2],fmt='%#.3f')) %>%
  unite(col=CI,LL:UL, sep = ", ", remove=F)
  

# Stack Columns 
re.p.edu.c.par.ZHANG_DNAMAGE<- bind_rows("NH Black"=nhb.p.edu.c.par.tab,"NH Hispanic"=h.p.edu.c.par.tab,"NH White"=nhw.p.edu.c.par.tab,.id='Subgroup')
write.csv(re.p.edu.c.par.ZHANG_DNAMAGE,file='re.p.edu.c.par.ZHANG_DNAMAGE.csv')

# Save ANOVA results
sink(file="p.aov.edu.c.ZHANG_DNAMAGE.txt")
print(aov<-list(anova(p.edu.c.par),anova(adj.p.edu.c.par)))
sink()

sink(file="re.p.aov.edu.c.ZHANG_DNAMAGE.txt")
print(aov<-list(anova(nhb.p.edu.c.par),anova(h.p.edu.c.par),anova(nhw.p.edu.c.par)))
sink()

## BOCKLANDT_DNAMAGE ----
# Unadjusted
p.edu.c.par <- svyglm(accbock~RAEDYRS_factor*RAFEDUC_bin,design=svy.edaa)
summary(p.edu.c.par)
p.edu.c.par.tab <- tidy(p.edu.c.par) %>% mutate(LL=sprintf(confint(p.edu.c.par)[,1],fmt='%#.3f'),
                                            UL=sprintf(confint(p.edu.c.par)[,2],fmt='%#.3f')) %>%
  unite(col=CI,LL:UL, sep = ", ", remove=F)
  

# Adjusted
adj.p.edu.c.par <- svyglm(accbock~RAEDYRS_factor*RAFEDUC_bin +
                          raceth + 
                          sex +
                          scale(H13ITOT) + 
                          smokestatus +
                          R13BMI,design=svy.edaa)
summary(adj.p.edu.c.par)
adj.p.edu.c.par.tab <- tidy(adj.p.edu.c.par) %>% mutate(LL=sprintf(confint(adj.p.edu.c.par)[,1],fmt='%#.3f'),
                                                    UL=sprintf(confint(adj.p.edu.c.par)[,2],fmt='%#.3f')) %>%
  unite(col=CI,LL:UL, sep = ", ", remove=F)
  
names(adj.p.edu.c.par.tab) <- gsub("^(.*?)","adj_\\2", names(adj.p.edu.c.par.tab))

# Combine results into one table
p.edu.c.par.BOCKLANDT_DNAMAGE <- merge(p.edu.c.par.tab,adj.p.edu.c.par.tab,by.x='term',by.y='adj_term',all.y=T,sort=F) 
write.csv(p.edu.c.par.BOCKLANDT_DNAMAGE,'p.edu.c.par.BOCKLANDT_DNAMAGE.csv')

## Marginal Means
# Unadjusted
BOCKLANDT_DNAMAGE.MM <- emmeans(p.edu.c.par,pairwise~RAEDYRS_factor*RAFEDUC_bin,data=svy.edaa,adjust='mvt')

# Adjusted
adj.BOCKLANDT_DNAMAGE.MM <- emmeans(adj.p.edu.c.par,pairwise~RAEDYRS_factor*RAFEDUC_bin,data=svy.edaa,adjust='mvt')

## Stratify by race/ethnicity
# NH Black
nhb.p.edu.c.par <- svyglm(accbock~RAEDYRS_factor*RAFEDUC_bin +
                          sex +
                          scale(H13ITOT) + 
                          smokestatus +
                          R13BMI,design=subset(svy.edaa,raceth=="NHB"))
summary(nhb.p.edu.c.par)
nhb.p.edu.c.par.tab <- tidy(nhb.p.edu.c.par) %>% mutate(LL=sprintf(confint(nhb.p.edu.c.par)[,1],fmt='%#.3f'),
                                                    UL=sprintf(confint(nhb.p.edu.c.par)[,2],fmt='%#.3f')) %>%
  unite(col=CI,LL:UL, sep = ", ", remove=F)
  
# Hispanic
h.p.edu.c.par <- svyglm(accbock~RAEDYRS_factor*RAFEDUC_bin +
                        sex +
                        scale(H13ITOT) + 
                        smokestatus +
                        R13BMI,design=subset(svy.edaa,raceth=="Hispanic"))
summary(h.p.edu.c.par)
h.p.edu.c.par.tab <- tidy(h.p.edu.c.par) %>% mutate(LL=sprintf(confint(h.p.edu.c.par)[,1],fmt='%#.3f'),
                                                UL=sprintf(confint(h.p.edu.c.par)[,2],fmt='%#.3f')) %>%
  unite(col=CI,LL:UL, sep = ", ", remove=F)
  
#NH White
nhw.p.edu.c.par <- svyglm(accbock~RAEDYRS_factor*RAFEDUC_bin +
                          sex +
                          scale(H13ITOT) + 
                          smokestatus +
                          R13BMI,design=subset(svy.edaa,raceth=="NHW"))
summary(nhw.p.edu.c.par)
nhw.p.edu.c.par.tab <- tidy(nhw.p.edu.c.par) %>% mutate(LL=sprintf(confint(nhw.p.edu.c.par)[,1],fmt='%#.3f'),
                                                    UL=sprintf(confint(nhw.p.edu.c.par)[,2],fmt='%#.3f')) %>%
  unite(col=CI,LL:UL, sep = ", ", remove=F)
  

# Stack Columns 
re.p.edu.c.par.BOCKLANDT_DNAMAGE<- bind_rows("NH Black"=nhb.p.edu.c.par.tab,"NH Hispanic"=h.p.edu.c.par.tab,"NH White"=nhw.p.edu.c.par.tab,.id='Subgroup')
write.csv(re.p.edu.c.par.BOCKLANDT_DNAMAGE,file='re.p.edu.c.par.BOCKLANDT_DNAMAGE.csv')

# Save ANOVA results
sink(file="p.aov.edu.c.BOCKLANDT_DNAMAGE.txt")
print(aov<-list(anova(p.edu.c.par),anova(adj.p.edu.c.par)))
sink()

sink(file="re.p.aov.edu.c.BOCKLANDT_DNAMAGE.txt")
print(aov<-list(anova(nhb.p.edu.c.par),anova(h.p.edu.c.par),anova(nhw.p.edu.c.par)))
sink()

## GARAGNANI_DNAMAGE ----
# Unadjusted
p.edu.c.par <- svyglm(accgarag~RAEDYRS_factor*RAFEDUC_bin,design=svy.edaa)
summary(p.edu.c.par)
p.edu.c.par.tab <- tidy(p.edu.c.par) %>% mutate(LL=sprintf(confint(p.edu.c.par)[,1],fmt='%#.3f'),
                                            UL=sprintf(confint(p.edu.c.par)[,2],fmt='%#.3f')) %>%
  unite(col=CI,LL:UL, sep = ", ", remove=F)
  

# Adjusted
adj.p.edu.c.par <- svyglm(accgarag~RAEDYRS_factor*RAFEDUC_bin +
                          raceth + 
                          sex +
                          scale(H13ITOT) + 
                          smokestatus +
                          R13BMI,design=svy.edaa)
summary(adj.p.edu.c.par)
adj.p.edu.c.par.tab <- tidy(adj.p.edu.c.par) %>% mutate(LL=sprintf(confint(adj.p.edu.c.par)[,1],fmt='%#.3f'),
                                                    UL=sprintf(confint(adj.p.edu.c.par)[,2],fmt='%#.3f')) %>%
  unite(col=CI,LL:UL, sep = ", ", remove=F)
  
names(adj.p.edu.c.par.tab) <- gsub("^(.*?)","adj_\\2", names(adj.p.edu.c.par.tab))

# Combine results into one table
p.edu.c.par.GARAGNANI_DNAMAGE <- merge(p.edu.c.par.tab,adj.p.edu.c.par.tab,by.x='term',by.y='adj_term',all.y=T,sort=F) 
write.csv(p.edu.c.par.GARAGNANI_DNAMAGE,'p.edu.c.par.GARAGNANI_DNAMAGE.csv')

## Marginal Means
# Unadjusted
GARAGNANI_DNAMAGE.MM <- emmeans(p.edu.c.par,pairwise~RAEDYRS_factor*RAFEDUC_bin,data=svy.edaa,adjust='mvt')

# Adjusted
adj.GARAGNANI_DNAMAGE.MM <- emmeans(adj.p.edu.c.par,pairwise~RAEDYRS_factor*RAFEDUC_bin,data=svy.edaa,adjust='mvt')

## Stratify by race/ethnicity
# NH Black
nhb.p.edu.c.par <- svyglm(accgarag~RAEDYRS_factor*RAFEDUC_bin +
                          sex +
                          scale(H13ITOT) + 
                          smokestatus +
                          R13BMI,design=subset(svy.edaa,raceth=="NHB"))
summary(nhb.p.edu.c.par)
nhb.p.edu.c.par.tab <- tidy(nhb.p.edu.c.par) %>% mutate(LL=sprintf(confint(nhb.p.edu.c.par)[,1],fmt='%#.3f'),
                                                    UL=sprintf(confint(nhb.p.edu.c.par)[,2],fmt='%#.3f')) %>%
  unite(col=CI,LL:UL, sep = ", ", remove=F)
  
# Hispanic
h.p.edu.c.par <- svyglm(accgarag~RAEDYRS_factor*RAFEDUC_bin +
                        sex +
                        scale(H13ITOT) + 
                        smokestatus +
                        R13BMI,design=subset(svy.edaa,raceth=="Hispanic"))
summary(h.p.edu.c.par)
h.p.edu.c.par.tab <- tidy(h.p.edu.c.par) %>% mutate(LL=sprintf(confint(h.p.edu.c.par)[,1],fmt='%#.3f'),
                                                UL=sprintf(confint(h.p.edu.c.par)[,2],fmt='%#.3f')) %>%
  unite(col=CI,LL:UL, sep = ", ", remove=F)
  
#NH White
nhw.p.edu.c.par <- svyglm(accgarag~RAEDYRS_factor*RAFEDUC_bin +
                          sex +
                          scale(H13ITOT) + 
                          smokestatus +
                          R13BMI,design=subset(svy.edaa,raceth=="NHW"))
summary(nhw.p.edu.c.par)
nhw.p.edu.c.par.tab <- tidy(nhw.p.edu.c.par) %>% mutate(LL=sprintf(confint(nhw.p.edu.c.par)[,1],fmt='%#.3f'),
                                                    UL=sprintf(confint(nhw.p.edu.c.par)[,2],fmt='%#.3f')) %>%
  unite(col=CI,LL:UL, sep = ", ", remove=F)
  

# Stack Columns 
re.p.edu.c.par.GARAGNANI_DNAMAGE<- bind_rows("NH Black"=nhb.p.edu.c.par.tab,"NH Hispanic"=h.p.edu.c.par.tab,"NH White"=nhw.p.edu.c.par.tab,.id='Subgroup')
write.csv(re.p.edu.c.par.GARAGNANI_DNAMAGE,file='re.p.edu.c.par.GARAGNANI_DNAMAGE.csv')

# Save ANOVA results
sink(file="p.aov.edu.c.GARAGNANI_DNAMAGE.txt")
print(aov<-list(anova(p.edu.c.par),anova(adj.p.edu.c.par)))
sink()

sink(file="re.p.aov.edu.c.GARAGNANI_DNAMAGE.txt")
print(aov<-list(anova(nhb.p.edu.c.par),anova(h.p.edu.c.par),anova(nhw.p.edu.c.par)))
sink()

## DNAMGRIMAGE ----
# Unadjusted
p.edu.c.par <- svyglm(accdnam~RAEDYRS_factor*RAFEDUC_bin,design=svy.edaa)
summary(p.edu.c.par)
p.edu.c.par.tab <- tidy(p.edu.c.par) %>% mutate(LL=sprintf(confint(p.edu.c.par)[,1],fmt='%#.3f'),
                                            UL=sprintf(confint(p.edu.c.par)[,2],fmt='%#.3f')) %>%
  unite(col=CI,LL:UL, sep = ", ", remove=F)
  

# Adjusted
adj.p.edu.c.par <- svyglm(accdnam~RAEDYRS_factor*RAFEDUC_bin +
                          raceth + 
                          sex +
                          scale(H13ITOT) + 
                          smokestatus +
                          R13BMI,design=svy.edaa)
summary(adj.p.edu.c.par)
adj.p.edu.c.par.tab <- tidy(adj.p.edu.c.par) %>% mutate(LL=sprintf(confint(adj.p.edu.c.par)[,1],fmt='%#.3f'),
                                                    UL=sprintf(confint(adj.p.edu.c.par)[,2],fmt='%#.3f')) %>%
  unite(col=CI,LL:UL, sep = ", ", remove=F)
  
names(adj.p.edu.c.par.tab) <- gsub("^(.*?)","adj_\\2", names(adj.p.edu.c.par.tab))

# Combine results into one table
p.edu.c.par.DNAMGRIMAGE <- merge(p.edu.c.par.tab,adj.p.edu.c.par.tab,by.x='term',by.y='adj_term',all.y=T,sort=F) 
write.csv(p.edu.c.par.DNAMGRIMAGE,'p.edu.c.par.DNAMGRIMAGE.csv')

## Marginal Means
# Unadjusted
DNAMGRIMAGE.MM <- emmeans(p.edu.c.par,pairwise~RAEDYRS_factor*RAFEDUC_bin,data=svy.edaa,adjust='mvt')

# Adjusted
adj.DNAMGRIMAGE.MM <- emmeans(adj.p.edu.c.par,pairwise~RAEDYRS_factor*RAFEDUC_bin,data=svy.edaa,adjust='mvt')

## Stratify by race/ethnicity
# NH Black
nhb.p.edu.c.par <- svyglm(accdnam~RAEDYRS_factor*RAFEDUC_bin +
                          sex +
                          scale(H13ITOT) + 
                          smokestatus +
                          R13BMI,design=subset(svy.edaa,raceth=="NHB"))
summary(nhb.p.edu.c.par)
nhb.p.edu.c.par.tab <- tidy(nhb.p.edu.c.par) %>% mutate(LL=sprintf(confint(nhb.p.edu.c.par)[,1],fmt='%#.3f'),
                                                    UL=sprintf(confint(nhb.p.edu.c.par)[,2],fmt='%#.3f')) %>%
  unite(col=CI,LL:UL, sep = ", ", remove=F)
  
# Hispanic
h.p.edu.c.par <- svyglm(accdnam~RAEDYRS_factor*RAFEDUC_bin +
                        sex +
                        scale(H13ITOT) + 
                        smokestatus +
                        R13BMI,design=subset(svy.edaa,raceth=="Hispanic"))
summary(h.p.edu.c.par)
h.p.edu.c.par.tab <- tidy(h.p.edu.c.par) %>% mutate(LL=sprintf(confint(h.p.edu.c.par)[,1],fmt='%#.3f'),
                                                UL=sprintf(confint(h.p.edu.c.par)[,2],fmt='%#.3f')) %>%
  unite(col=CI,LL:UL, sep = ", ", remove=F)
  
#NH White
nhw.p.edu.c.par <- svyglm(accdnam~RAEDYRS_factor*RAFEDUC_bin +
                          sex +
                          scale(H13ITOT) + 
                          smokestatus +
                          R13BMI,design=subset(svy.edaa,raceth=="NHW"))
summary(nhw.p.edu.c.par)
nhw.p.edu.c.par.tab <- tidy(nhw.p.edu.c.par) %>% mutate(LL=sprintf(confint(nhw.p.edu.c.par)[,1],fmt='%#.3f'),
                                                    UL=sprintf(confint(nhw.p.edu.c.par)[,2],fmt='%#.3f')) %>%
  unite(col=CI,LL:UL, sep = ", ", remove=F)
  

# Stack Columns 
re.p.edu.c.par.DNAMGRIMAGE<- bind_rows("NH Black"=nhb.p.edu.c.par.tab,"NH Hispanic"=h.p.edu.c.par.tab,"NH White"=nhw.p.edu.c.par.tab,.id='Subgroup')
write.csv(re.p.edu.c.par.DNAMGRIMAGE,file='re.p.edu.c.par.DNAMGRIMAGE.csv')

# Save ANOVA results
sink(file="p.aov.edu.c.DNAMGRIMAGE.txt")
print(aov<-list(anova(p.edu.c.par),anova(adj.p.edu.c.par)))
sink()

sink(file="re.p.aov.edu.c.DNAMGRIMAGE.txt")
print(aov<-list(anova(nhb.p.edu.c.par),anova(h.p.edu.c.par),anova(nhw.p.edu.c.par)))
sink()

## MPOA ----
# Unadjusted
p.edu.c.par <- svyglm(accmpoa~RAEDYRS_factor*RAFEDUC_bin,design=svy.edaa)
summary(p.edu.c.par)
p.edu.c.par.tab <- tidy(p.edu.c.par) %>% mutate(LL=sprintf(confint(p.edu.c.par)[,1],fmt='%#.3f'),
                                            UL=sprintf(confint(p.edu.c.par)[,2],fmt='%#.3f')) %>%
  unite(col=CI,LL:UL, sep = ", ", remove=F)
  

# Adjusted
adj.p.edu.c.par <- svyglm(accmpoa~RAEDYRS_factor*RAFEDUC_bin +
                          raceth + 
                          sex +
                          scale(H13ITOT) + 
                          smokestatus +
                          R13BMI,design=svy.edaa)
summary(adj.p.edu.c.par)
adj.p.edu.c.par.tab <- tidy(adj.p.edu.c.par) %>% mutate(LL=sprintf(confint(adj.p.edu.c.par)[,1],fmt='%#.3f'),
                                                    UL=sprintf(confint(adj.p.edu.c.par)[,2],fmt='%#.3f')) %>%
  unite(col=CI,LL:UL, sep = ", ", remove=F)
  
names(adj.p.edu.c.par.tab) <- gsub("^(.*?)","adj_\\2", names(adj.p.edu.c.par.tab))

# Combine results into one table
p.edu.c.par.MPOA<- merge(p.edu.c.par.tab,adj.p.edu.c.par.tab,by.x='term',by.y='adj_term',all.y=T,sort=F) 
write.csv(p.edu.c.par.MPOA,'p.edu.c.par.MPOA.csv')

## Marginal Means
# Unadjusted
MPOA.MM <- emmeans(p.edu.c.par,pairwise~RAEDYRS_factor*RAFEDUC_bin,data=svy.edaa,adjust='mvt')

# Adjusted
adj.MPOA.MM <- emmeans(adj.p.edu.c.par,pairwise~RAEDYRS_factor*RAFEDUC_bin,data=svy.edaa,adjust='mvt')

## Stratify by race/ethnicity
# NH Black
nhb.p.edu.c.par <- svyglm(accmpoa~RAEDYRS_factor*RAFEDUC_bin +
                          sex +
                          scale(H13ITOT) + 
                          smokestatus +
                          R13BMI,design=subset(svy.edaa,raceth=="NHB"))
summary(nhb.p.edu.c.par)
nhb.p.edu.c.par.tab <- tidy(nhb.p.edu.c.par) %>% mutate(LL=sprintf(confint(nhb.p.edu.c.par)[,1],fmt='%#.3f'),
                                                    UL=sprintf(confint(nhb.p.edu.c.par)[,2],fmt='%#.3f')) %>%
  unite(col=CI,LL:UL, sep = ", ", remove=F)
  
# Hispanic
h.p.edu.c.par <- svyglm(accmpoa~RAEDYRS_factor*RAFEDUC_bin +
                        sex +
                        scale(H13ITOT) + 
                        smokestatus +
                        R13BMI,design=subset(svy.edaa,raceth=="Hispanic"))
summary(h.p.edu.c.par)
h.p.edu.c.par.tab <- tidy(h.p.edu.c.par) %>% mutate(LL=sprintf(confint(h.p.edu.c.par)[,1],fmt='%#.3f'),
                                                UL=sprintf(confint(h.p.edu.c.par)[,2],fmt='%#.3f')) %>%
  unite(col=CI,LL:UL, sep = ", ", remove=F)
  
#NH White
nhw.p.edu.c.par <- svyglm(accmpoa~RAEDYRS_factor*RAFEDUC_bin +
                          sex +
                          scale(H13ITOT) + 
                          smokestatus +
                          R13BMI,design=subset(svy.edaa,raceth=="NHW"))
summary(nhw.p.edu.c.par)
nhw.p.edu.c.par.tab <- tidy(nhw.p.edu.c.par) %>% mutate(LL=sprintf(confint(nhw.p.edu.c.par)[,1],fmt='%#.3f'),
                                                    UL=sprintf(confint(nhw.p.edu.c.par)[,2],fmt='%#.3f')) %>%
  unite(col=CI,LL:UL, sep = ", ", remove=F)
  

# Stack Columns 
re.p.edu.c.par.MPOA<- bind_rows("NH Black"=nhb.p.edu.c.par.tab,"NH Hispanic"=h.p.edu.c.par.tab,"NH White"=nhw.p.edu.c.par.tab,.id='Subgroup')
write.csv(re.p.edu.c.par.MPOA,file='re.p.edu.c.par.MPOA.csv')

# Save ANOVA results
sink(file="p.aov.edu.c.MPOA.txt")
print(aov<-list(anova(p.edu.c.par),anova(adj.p.edu.c.par)))
sink()

sink(file="re.p.aov.edu.c.MPOA.txt")
print(aov<-list(anova(nhb.p.edu.c.par),anova(h.p.edu.c.par),anova(nhw.p.edu.c.par)))
sink()

## Save marginal means ----
save(HORVATH_DNAMAGE.MM,adj.HORVATH_DNAMAGE.MM,HANNUM_DNAMAGE.MM,adj.HANNUM_DNAMAGE.MM,
     LEVINE_DNAMAGE.MM,adj.LEVINE_DNAMAGE.MM,HORVATHSKIN_DNAMAGE.MM,adj.HORVATHSKIN_DNAMAGE.MM,
     LIN_DNAMAGE.MM,adj.LIN_DNAMAGE.MM,WEIDNER_DNAMAGE.MM,adj.WEIDNER_DNAMAGE.MM,
     VIDALBRALO_DNAMAGE.MM,adj.VIDALBRALO_DNAMAGE.MM,YANG_DNAMAGE.MM,adj.YANG_DNAMAGE.MM,
     ZHANG_DNAMAGE.MM,adj.ZHANG_DNAMAGE.MM,BOCKLANDT_DNAMAGE.MM,adj.BOCKLANDT_DNAMAGE.MM,
     GARAGNANI_DNAMAGE.MM,adj.GARAGNANI_DNAMAGE.MM,DNAMGRIMAGE.MM,adj.DNAMGRIMAGE.MM,
     MPOA.MM,adj.MPOA.MM,file='Father_MMs.RData')

### Regression Models (Mother Years of Education) ----
## HORVATH_DNAMAGE ----
# Unadjusted
m.edu.yr.par <- svyglm(acchorv~edu.c*medu.c,design=svy.edaa)
summary(m.edu.yr.par)
m.edu.yr.par.tab <- tidy(m.edu.yr.par) %>% mutate(LL=sprintf(confint(m.edu.yr.par)[,1],fmt='%#.3f'),
                                              UL=sprintf(confint(m.edu.yr.par)[,2],fmt='%#.3f')) %>%
  unite(col=CI,LL:UL, sep = ", ", remove=F)
  

# Adjusted
adj.m.edu.yr.par <- svyglm(acchorv~edu.c*medu.c +
                           raceth + 
                           sex +
                           scale(H13ITOT) + 
                           smokestatus +
                           R13BMI,design=svy.edaa)
summary(adj.m.edu.yr.par)
adj.m.edu.yr.par.tab <- tidy(adj.m.edu.yr.par) %>% mutate(LL=sprintf(confint(adj.m.edu.yr.par)[,1],fmt='%#.3f'),
                                                      UL=sprintf(confint(adj.m.edu.yr.par)[,2],fmt='%#.3f')) %>%
  unite(col=CI,LL:UL, sep = ", ", remove=F)
  
names(adj.m.edu.yr.par.tab) <- gsub("^(.*?)","adj_\\2", names(adj.m.edu.yr.par.tab))

# Combine results into one table
m.edu.yr.par.HORVATH_DNAMAGE <- merge(m.edu.yr.par.tab,adj.m.edu.yr.par.tab,by.x='term',by.y='adj_term',all.y=T,sort=F) 
write.csv(m.edu.yr.par.HORVATH_DNAMAGE,'m.edu.yr.par.HORVATH_DNAMAGE.csv')

## Stratify by race/ethnicity
# NH Black
nhb.m.edu.yr.par <- svyglm(acchorv~edu.c*medu.c +
                           sex +
                           scale(H13ITOT) + 
                           smokestatus +
                           R13BMI,design=subset(svy.edaa,raceth=="NHB"))
summary(nhb.m.edu.yr.par)
nhb.m.edu.yr.par.tab <- tidy(nhb.m.edu.yr.par) %>% mutate(LL=sprintf(confint(nhb.m.edu.yr.par)[,1],fmt='%#.3f'),
                                                      UL=sprintf(confint(nhb.m.edu.yr.par)[,2],fmt='%#.3f')) %>%
  unite(col=CI,LL:UL, sep = ", ", remove=F)
  
# Hispanic
h.m.edu.yr.par <- svyglm(acchorv~edu.c*medu.c +
                         sex +
                         scale(H13ITOT) + 
                         smokestatus +
                         R13BMI,design=subset(svy.edaa,raceth=="Hispanic"))
summary(h.m.edu.yr.par)
h.m.edu.yr.par.tab <- tidy(h.m.edu.yr.par) %>% mutate(LL=sprintf(confint(h.m.edu.yr.par)[,1],fmt='%#.3f'),
                                                  UL=sprintf(confint(h.m.edu.yr.par)[,2],fmt='%#.3f')) %>%
  unite(col=CI,LL:UL, sep = ", ", remove=F)
  
#NH White
nhw.m.edu.yr.par <- svyglm(acchorv~edu.c*medu.c +
                           sex +
                           scale(H13ITOT) + 
                           smokestatus +
                           R13BMI,design=subset(svy.edaa,raceth=="NHW"))
summary(nhw.m.edu.yr.par)
nhw.m.edu.yr.par.tab <- tidy(nhw.m.edu.yr.par) %>% mutate(LL=sprintf(confint(nhw.m.edu.yr.par)[,1],fmt='%#.3f'),
                                                      UL=sprintf(confint(nhw.m.edu.yr.par)[,2],fmt='%#.3f')) %>%
  unite(col=CI,LL:UL, sep = ", ", remove=F)
  

# Stack Columns 
re.m.edu.yr.par.HORVATH_DNAMAGE <- bind_rows("NH Black"=nhb.m.edu.yr.par.tab,"NH Hispanic"=h.m.edu.yr.par.tab,"NH White"=nhw.m.edu.yr.par.tab,.id='Subgroup')
write.csv(re.m.edu.yr.par.HORVATH_DNAMAGE,file='re.m.edu.yr.par.HORVATH_DNAMAGE.csv')

## HANNUM_DNAMAGE ----
# Unadjusted
m.edu.yr.par <- svyglm(acchannum~edu.c*medu.c,design=svy.edaa)
summary(m.edu.yr.par)
m.edu.yr.par.tab <- tidy(m.edu.yr.par) %>% mutate(LL=sprintf(confint(m.edu.yr.par)[,1],fmt='%#.3f'),
                                              UL=sprintf(confint(m.edu.yr.par)[,2],fmt='%#.3f')) %>%
  unite(col=CI,LL:UL, sep = ", ", remove=F)
  

# Adjusted
adj.m.edu.yr.par <- svyglm(acchannum~edu.c*medu.c +
                           raceth + 
                           sex +
                           scale(H13ITOT) + 
                           smokestatus +
                           R13BMI,design=svy.edaa)
summary(adj.m.edu.yr.par)
adj.m.edu.yr.par.tab <- tidy(adj.m.edu.yr.par) %>% mutate(LL=sprintf(confint(adj.m.edu.yr.par)[,1],fmt='%#.3f'),
                                                      UL=sprintf(confint(adj.m.edu.yr.par)[,2],fmt='%#.3f')) %>%
  unite(col=CI,LL:UL, sep = ", ", remove=F)
  
names(adj.m.edu.yr.par.tab) <- gsub("^(.*?)","adj_\\2", names(adj.m.edu.yr.par.tab))

# Combine results into one table
m.edu.yr.par.HANNUM_DNAMAGE <- merge(m.edu.yr.par.tab,adj.m.edu.yr.par.tab,by.x='term',by.y='adj_term',all.y=T,sort=F) 
write.csv(m.edu.yr.par.HANNUM_DNAMAGE,'m.edu.yr.par.HANNUM_DNAMAGE.csv')

## Stratify by race/ethnicity
# NH Black
nhb.m.edu.yr.par <- svyglm(acchannum~edu.c*medu.c +
                           sex +
                           scale(H13ITOT) + 
                           smokestatus +
                           R13BMI,design=subset(svy.edaa,raceth=="NHB"))
summary(nhb.m.edu.yr.par)
nhb.m.edu.yr.par.tab <- tidy(nhb.m.edu.yr.par) %>% mutate(LL=sprintf(confint(nhb.m.edu.yr.par)[,1],fmt='%#.3f'),
                                                      UL=sprintf(confint(nhb.m.edu.yr.par)[,2],fmt='%#.3f')) %>%
  unite(col=CI,LL:UL, sep = ", ", remove=F)
  
# Hispanic
h.m.edu.yr.par <- svyglm(acchannum~edu.c*medu.c +
                         sex +
                         scale(H13ITOT) + 
                         smokestatus +
                         R13BMI,design=subset(svy.edaa,raceth=="Hispanic"))
summary(h.m.edu.yr.par)
h.m.edu.yr.par.tab <- tidy(h.m.edu.yr.par) %>% mutate(LL=sprintf(confint(h.m.edu.yr.par)[,1],fmt='%#.3f'),
                                                  UL=sprintf(confint(h.m.edu.yr.par)[,2],fmt='%#.3f')) %>%
  unite(col=CI,LL:UL, sep = ", ", remove=F)
  
#NH White
nhw.m.edu.yr.par <- svyglm(acchannum~edu.c*medu.c +
                           sex +
                           scale(H13ITOT) + 
                           smokestatus +
                           R13BMI,design=subset(svy.edaa,raceth=="NHW"))
summary(nhw.m.edu.yr.par)
nhw.m.edu.yr.par.tab <- tidy(nhw.m.edu.yr.par) %>% mutate(LL=sprintf(confint(nhw.m.edu.yr.par)[,1],fmt='%#.3f'),
                                                      UL=sprintf(confint(nhw.m.edu.yr.par)[,2],fmt='%#.3f')) %>%
  unite(col=CI,LL:UL, sep = ", ", remove=F)
  

# Stack Columns 
re.m.edu.yr.par.HANNUM_DNAMAGE<- bind_rows("NH Black"=nhb.m.edu.yr.par.tab,"NH Hispanic"=h.m.edu.yr.par.tab,"NH White"=nhw.m.edu.yr.par.tab,.id='Subgroup')
write.csv(re.m.edu.yr.par.HANNUM_DNAMAGE,file='re.m.edu.yr.par.HANNUM_DNAMAGE.csv')

## LEVINE_DNAMAGE ----
# Unadjusted
m.edu.yr.par <- svyglm(acclevine~edu.c*medu.c,design=svy.edaa)
summary(m.edu.yr.par)
m.edu.yr.par.tab <- tidy(m.edu.yr.par) %>% mutate(LL=sprintf(confint(m.edu.yr.par)[,1],fmt='%#.3f'),
                                              UL=sprintf(confint(m.edu.yr.par)[,2],fmt='%#.3f')) %>%
  unite(col=CI,LL:UL, sep = ", ", remove=F)
  

# Adjusted
adj.m.edu.yr.par <- svyglm(acclevine~edu.c*medu.c +
                           raceth + 
                           sex +
                           scale(H13ITOT) + 
                           smokestatus +
                           R13BMI,design=svy.edaa)
summary(adj.m.edu.yr.par)
adj.m.edu.yr.par.tab <- tidy(adj.m.edu.yr.par) %>% mutate(LL=sprintf(confint(adj.m.edu.yr.par)[,1],fmt='%#.3f'),
                                                      UL=sprintf(confint(adj.m.edu.yr.par)[,2],fmt='%#.3f')) %>%
  unite(col=CI,LL:UL, sep = ", ", remove=F)
  
names(adj.m.edu.yr.par.tab) <- gsub("^(.*?)","adj_\\2", names(adj.m.edu.yr.par.tab))

# Combine results into one table
m.edu.yr.par.LEVINE_DNAMAGE <- merge(m.edu.yr.par.tab,adj.m.edu.yr.par.tab,by.x='term',by.y='adj_term',all.y=T,sort=F) 
write.csv(m.edu.yr.par.LEVINE_DNAMAGE,'m.edu.yr.par.LEVINE_DNAMAGE.csv')

## Stratify by race/ethnicity
# NH Black
nhb.m.edu.yr.par <- svyglm(acclevine~edu.c*medu.c +
                           sex +
                           scale(H13ITOT) + 
                           smokestatus +
                           R13BMI,design=subset(svy.edaa,raceth=="NHB"))
summary(nhb.m.edu.yr.par)
nhb.m.edu.yr.par.tab <- tidy(nhb.m.edu.yr.par) %>% mutate(LL=sprintf(confint(nhb.m.edu.yr.par)[,1],fmt='%#.3f'),
                                                      UL=sprintf(confint(nhb.m.edu.yr.par)[,2],fmt='%#.3f')) %>%
  unite(col=CI,LL:UL, sep = ", ", remove=F)
  
# Hispanic
h.m.edu.yr.par <- svyglm(acclevine~edu.c*medu.c +
                         sex +
                         scale(H13ITOT) + 
                         smokestatus +
                         R13BMI,design=subset(svy.edaa,raceth=="Hispanic"))
summary(h.m.edu.yr.par)
h.m.edu.yr.par.tab <- tidy(h.m.edu.yr.par) %>% mutate(LL=sprintf(confint(h.m.edu.yr.par)[,1],fmt='%#.3f'),
                                                  UL=sprintf(confint(h.m.edu.yr.par)[,2],fmt='%#.3f')) %>%
  unite(col=CI,LL:UL, sep = ", ", remove=F)
  
#NH White
nhw.m.edu.yr.par <- svyglm(acclevine~edu.c*medu.c +
                           sex +
                           scale(H13ITOT) + 
                           smokestatus +
                           R13BMI,design=subset(svy.edaa,raceth=="NHW"))
summary(nhw.m.edu.yr.par)
nhw.m.edu.yr.par.tab <- tidy(nhw.m.edu.yr.par) %>% mutate(LL=sprintf(confint(nhw.m.edu.yr.par)[,1],fmt='%#.3f'),
                                                      UL=sprintf(confint(nhw.m.edu.yr.par)[,2],fmt='%#.3f')) %>%
  unite(col=CI,LL:UL, sep = ", ", remove=F)
  

# Stack Columns 
re.m.edu.yr.par.LEVINE_DNAMAGE<- bind_rows("NH Black"=nhb.m.edu.yr.par.tab,"NH Hispanic"=h.m.edu.yr.par.tab,"NH White"=nhw.m.edu.yr.par.tab,.id='Subgroup')
write.csv(re.m.edu.yr.par.LEVINE_DNAMAGE,file='re.m.edu.yr.par.LEVINE_DNAMAGE.csv')

## HORVATHSKIN_DNAMAGE ----
# Unadjusted
m.edu.yr.par <- svyglm(acchorvsk~edu.c*medu.c,design=svy.edaa)
summary(m.edu.yr.par)
m.edu.yr.par.tab <- tidy(m.edu.yr.par) %>% mutate(LL=sprintf(confint(m.edu.yr.par)[,1],fmt='%#.3f'),
                                              UL=sprintf(confint(m.edu.yr.par)[,2],fmt='%#.3f')) %>%
  unite(col=CI,LL:UL, sep = ", ", remove=F)
  

# Adjusted
adj.m.edu.yr.par <- svyglm(acchorvsk~edu.c*medu.c +
                           raceth + 
                           sex +
                           scale(H13ITOT) + 
                           smokestatus +
                           R13BMI,design=svy.edaa)
summary(adj.m.edu.yr.par)
adj.m.edu.yr.par.tab <- tidy(adj.m.edu.yr.par) %>% mutate(LL=sprintf(confint(adj.m.edu.yr.par)[,1],fmt='%#.3f'),
                                                      UL=sprintf(confint(adj.m.edu.yr.par)[,2],fmt='%#.3f')) %>%
  unite(col=CI,LL:UL, sep = ", ", remove=F)
  
names(adj.m.edu.yr.par.tab) <- gsub("^(.*?)","adj_\\2", names(adj.m.edu.yr.par.tab))

# Combine results into one table
m.edu.yr.par.HORVATHSKIN_DNAMAGE<- merge(m.edu.yr.par.tab,adj.m.edu.yr.par.tab,by.x='term',by.y='adj_term',all.y=T,sort=F) 
write.csv(m.edu.yr.par.HORVATHSKIN_DNAMAGE,'m.edu.yr.par.HORVATHSKIN_DNAMAGE.csv')

## Stratify by race/ethnicity
# NH Black
nhb.m.edu.yr.par <- svyglm(acchorvsk~edu.c*medu.c +
                           sex +
                           scale(H13ITOT) + 
                           smokestatus +
                           R13BMI,design=subset(svy.edaa,raceth=="NHB"))
summary(nhb.m.edu.yr.par)
nhb.m.edu.yr.par.tab <- tidy(nhb.m.edu.yr.par) %>% mutate(LL=sprintf(confint(nhb.m.edu.yr.par)[,1],fmt='%#.3f'),
                                                      UL=sprintf(confint(nhb.m.edu.yr.par)[,2],fmt='%#.3f')) %>%
  unite(col=CI,LL:UL, sep = ", ", remove=F)
  
# Hispanic
h.m.edu.yr.par <- svyglm(acchorvsk~edu.c*medu.c +
                         sex +
                         scale(H13ITOT) + 
                         smokestatus +
                         R13BMI,design=subset(svy.edaa,raceth=="Hispanic"))
summary(h.m.edu.yr.par)
h.m.edu.yr.par.tab <- tidy(h.m.edu.yr.par) %>% mutate(LL=sprintf(confint(h.m.edu.yr.par)[,1],fmt='%#.3f'),
                                                  UL=sprintf(confint(h.m.edu.yr.par)[,2],fmt='%#.3f')) %>%
  unite(col=CI,LL:UL, sep = ", ", remove=F)
  
#NH White
nhw.m.edu.yr.par <- svyglm(acchorvsk~edu.c*medu.c +
                           sex +
                           scale(H13ITOT) + 
                           smokestatus +
                           R13BMI,design=subset(svy.edaa,raceth=="NHW"))
summary(nhw.m.edu.yr.par)
nhw.m.edu.yr.par.tab <- tidy(nhw.m.edu.yr.par) %>% mutate(LL=sprintf(confint(nhw.m.edu.yr.par)[,1],fmt='%#.3f'),
                                                      UL=sprintf(confint(nhw.m.edu.yr.par)[,2],fmt='%#.3f')) %>%
  unite(col=CI,LL:UL, sep = ", ", remove=F)
  

# Stack Columns 
re.m.edu.yr.par.HORVATHSKIN_DNAMAGE<- bind_rows("NH Black"=nhb.m.edu.yr.par.tab,"NH Hispanic"=h.m.edu.yr.par.tab,"NH White"=nhw.m.edu.yr.par.tab,.id='Subgroup')
write.csv(re.m.edu.yr.par.HORVATHSKIN_DNAMAGE,file='re.m.edu.yr.par.HORVATHSKIN_DNAMAGE.csv')

## LIN_DNAMAGE ----
# Unadjusted
m.edu.yr.par <- svyglm(acclin~edu.c*medu.c,design=svy.edaa)
summary(m.edu.yr.par)
m.edu.yr.par.tab <- tidy(m.edu.yr.par) %>% mutate(LL=sprintf(confint(m.edu.yr.par)[,1],fmt='%#.3f'),
                                              UL=sprintf(confint(m.edu.yr.par)[,2],fmt='%#.3f')) %>%
  unite(col=CI,LL:UL, sep = ", ", remove=F)
  

# Adjusted
adj.m.edu.yr.par <- svyglm(acclin~edu.c*medu.c +
                           raceth + 
                           sex +
                           scale(H13ITOT) + 
                           smokestatus +
                           R13BMI,design=svy.edaa)
summary(adj.m.edu.yr.par)
adj.m.edu.yr.par.tab <- tidy(adj.m.edu.yr.par) %>% mutate(LL=sprintf(confint(adj.m.edu.yr.par)[,1],fmt='%#.3f'),
                                                      UL=sprintf(confint(adj.m.edu.yr.par)[,2],fmt='%#.3f')) %>%
  unite(col=CI,LL:UL, sep = ", ", remove=F)
  
names(adj.m.edu.yr.par.tab) <- gsub("^(.*?)","adj_\\2", names(adj.m.edu.yr.par.tab))

# Combine results into one table
m.edu.yr.par.LIN_DNAMAGE <- merge(m.edu.yr.par.tab,adj.m.edu.yr.par.tab,by.x='term',by.y='adj_term',all.y=T,sort=F) 
write.csv(m.edu.yr.par.LIN_DNAMAGE,'m.edu.yr.par.LIN_DNAMAGE.csv')

## Stratify by race/ethnicity
# NH Black
nhb.m.edu.yr.par <- svyglm(acclin~edu.c*medu.c +
                           sex +
                           scale(H13ITOT) + 
                           smokestatus +
                           R13BMI,design=subset(svy.edaa,raceth=="NHB"))
summary(nhb.m.edu.yr.par)
nhb.m.edu.yr.par.tab <- tidy(nhb.m.edu.yr.par) %>% mutate(LL=sprintf(confint(nhb.m.edu.yr.par)[,1],fmt='%#.3f'),
                                                      UL=sprintf(confint(nhb.m.edu.yr.par)[,2],fmt='%#.3f')) %>%
  unite(col=CI,LL:UL, sep = ", ", remove=F)
  
# Hispanic
h.m.edu.yr.par <- svyglm(acclin~edu.c*medu.c +
                         sex +
                         scale(H13ITOT) + 
                         smokestatus +
                         R13BMI,design=subset(svy.edaa,raceth=="Hispanic"))
summary(h.m.edu.yr.par)
h.m.edu.yr.par.tab <- tidy(h.m.edu.yr.par) %>% mutate(LL=sprintf(confint(h.m.edu.yr.par)[,1],fmt='%#.3f'),
                                                  UL=sprintf(confint(h.m.edu.yr.par)[,2],fmt='%#.3f')) %>%
  unite(col=CI,LL:UL, sep = ", ", remove=F)
  
#NH White
nhw.m.edu.yr.par <- svyglm(acclin~edu.c*medu.c +
                           sex +
                           scale(H13ITOT) + 
                           smokestatus +
                           R13BMI,design=subset(svy.edaa,raceth=="NHW"))
summary(nhw.m.edu.yr.par)
nhw.m.edu.yr.par.tab <- tidy(nhw.m.edu.yr.par) %>% mutate(LL=sprintf(confint(nhw.m.edu.yr.par)[,1],fmt='%#.3f'),
                                                      UL=sprintf(confint(nhw.m.edu.yr.par)[,2],fmt='%#.3f')) %>%
  unite(col=CI,LL:UL, sep = ", ", remove=F)
  

# Stack Columns 
re.m.edu.yr.par.LIN_DNAMAGE<- bind_rows("NH Black"=nhb.m.edu.yr.par.tab,"NH Hispanic"=h.m.edu.yr.par.tab,"NH White"=nhw.m.edu.yr.par.tab,.id='Subgroup')
write.csv(re.m.edu.yr.par.LIN_DNAMAGE,file='re.m.edu.yr.par.LIN_DNAMAGE.csv')

## WEIDNER_DNAMAGE ----
# Unadjusted
m.edu.yr.par <- svyglm(accweid~edu.c*medu.c,design=svy.edaa)
summary(m.edu.yr.par)
m.edu.yr.par.tab <- tidy(m.edu.yr.par) %>% mutate(LL=sprintf(confint(m.edu.yr.par)[,1],fmt='%#.3f'),
                                              UL=sprintf(confint(m.edu.yr.par)[,2],fmt='%#.3f')) %>%
  unite(col=CI,LL:UL, sep = ", ", remove=F)
  

# Adjusted
adj.m.edu.yr.par <- svyglm(accweid~edu.c*medu.c +
                           raceth + 
                           sex +
                           scale(H13ITOT) + 
                           smokestatus +
                           R13BMI,design=svy.edaa)
summary(adj.m.edu.yr.par)
adj.m.edu.yr.par.tab <- tidy(adj.m.edu.yr.par) %>% mutate(LL=sprintf(confint(adj.m.edu.yr.par)[,1],fmt='%#.3f'),
                                                      UL=sprintf(confint(adj.m.edu.yr.par)[,2],fmt='%#.3f')) %>%
  unite(col=CI,LL:UL, sep = ", ", remove=F)
  
names(adj.m.edu.yr.par.tab) <- gsub("^(.*?)","adj_\\2", names(adj.m.edu.yr.par.tab))

# Combine results into one table
m.edu.yr.par.WEIDNER_DNAMAGE <- merge(m.edu.yr.par.tab,adj.m.edu.yr.par.tab,by.x='term',by.y='adj_term',all.y=T,sort=F) 
write.csv(m.edu.yr.par.WEIDNER_DNAMAGE,'m.edu.yr.par.WEIDNER_DNAMAGE.csv')

## Stratify by race/ethnicity
# NH Black
nhb.m.edu.yr.par <- svyglm(accweid~edu.c*medu.c +
                           sex +
                           scale(H13ITOT) + 
                           smokestatus +
                           R13BMI,design=subset(svy.edaa,raceth=="NHB"))
summary(nhb.m.edu.yr.par)
nhb.m.edu.yr.par.tab <- tidy(nhb.m.edu.yr.par) %>% mutate(LL=sprintf(confint(nhb.m.edu.yr.par)[,1],fmt='%#.3f'),
                                                      UL=sprintf(confint(nhb.m.edu.yr.par)[,2],fmt='%#.3f')) %>%
  unite(col=CI,LL:UL, sep = ", ", remove=F)
  
# Hispanic
h.m.edu.yr.par <- svyglm(accweid~edu.c*medu.c +
                         sex +
                         scale(H13ITOT) + 
                         smokestatus +
                         R13BMI,design=subset(svy.edaa,raceth=="Hispanic"))
summary(h.m.edu.yr.par)
h.m.edu.yr.par.tab <- tidy(h.m.edu.yr.par) %>% mutate(LL=sprintf(confint(h.m.edu.yr.par)[,1],fmt='%#.3f'),
                                                  UL=sprintf(confint(h.m.edu.yr.par)[,2],fmt='%#.3f')) %>%
  unite(col=CI,LL:UL, sep = ", ", remove=F)
  
#NH White
nhw.m.edu.yr.par <- svyglm(accweid~edu.c*medu.c +
                           sex +
                           scale(H13ITOT) + 
                           smokestatus +
                           R13BMI,design=subset(svy.edaa,raceth=="NHW"))
summary(nhw.m.edu.yr.par)
nhw.m.edu.yr.par.tab <- tidy(nhw.m.edu.yr.par) %>% mutate(LL=sprintf(confint(nhw.m.edu.yr.par)[,1],fmt='%#.3f'),
                                                      UL=sprintf(confint(nhw.m.edu.yr.par)[,2],fmt='%#.3f')) %>%
  unite(col=CI,LL:UL, sep = ", ", remove=F)
  

# Stack Columns 
re.m.edu.yr.par.WEIDNER_DNAMAGE<- bind_rows("NH Black"=nhb.m.edu.yr.par.tab,"NH Hispanic"=h.m.edu.yr.par.tab,"NH White"=nhw.m.edu.yr.par.tab,.id='Subgroup')
write.csv(re.m.edu.yr.par.WEIDNER_DNAMAGE,file='re.m.edu.yr.par.WEIDNER_DNAMAGE.csv')

## VIDALBRALO_DNAMAGE ----
# Unadjusted
m.edu.yr.par <- svyglm(accvidalb~edu.c*medu.c,design=svy.edaa)
summary(m.edu.yr.par)
m.edu.yr.par.tab <- tidy(m.edu.yr.par) %>% mutate(LL=sprintf(confint(m.edu.yr.par)[,1],fmt='%#.3f'),
                                              UL=sprintf(confint(m.edu.yr.par)[,2],fmt='%#.3f')) %>%
  unite(col=CI,LL:UL, sep = ", ", remove=F)
  

# Adjusted
adj.m.edu.yr.par <- svyglm(accvidalb~edu.c*medu.c +
                           raceth + 
                           sex +
                           scale(H13ITOT) + 
                           smokestatus +
                           R13BMI,design=svy.edaa)
summary(adj.m.edu.yr.par)
adj.m.edu.yr.par.tab <- tidy(adj.m.edu.yr.par) %>% mutate(LL=sprintf(confint(adj.m.edu.yr.par)[,1],fmt='%#.3f'),
                                                      UL=sprintf(confint(adj.m.edu.yr.par)[,2],fmt='%#.3f')) %>%
  unite(col=CI,LL:UL, sep = ", ", remove=F)
  
names(adj.m.edu.yr.par.tab) <- gsub("^(.*?)","adj_\\2", names(adj.m.edu.yr.par.tab))

# Combine results into one table
m.edu.yr.par.VIDALBRALO_DNAMAGE <- merge(m.edu.yr.par.tab,adj.m.edu.yr.par.tab,by.x='term',by.y='adj_term',all.y=T,sort=F) 
write.csv(m.edu.yr.par.VIDALBRALO_DNAMAGE,'m.edu.yr.par.VIDALBRALO_DNAMAGE.csv')

## Stratify by race/ethnicity
# NH Black
nhb.m.edu.yr.par <- svyglm(accvidalb~edu.c*medu.c +
                           sex +
                           scale(H13ITOT) + 
                           smokestatus +
                           R13BMI,design=subset(svy.edaa,raceth=="NHB"))
summary(nhb.m.edu.yr.par)
nhb.m.edu.yr.par.tab <- tidy(nhb.m.edu.yr.par) %>% mutate(LL=sprintf(confint(nhb.m.edu.yr.par)[,1],fmt='%#.3f'),
                                                      UL=sprintf(confint(nhb.m.edu.yr.par)[,2],fmt='%#.3f')) %>%
  unite(col=CI,LL:UL, sep = ", ", remove=F)
  
# Hispanic
h.m.edu.yr.par <- svyglm(accvidalb~edu.c*medu.c +
                         sex +
                         scale(H13ITOT) + 
                         smokestatus +
                         R13BMI,design=subset(svy.edaa,raceth=="Hispanic"))
summary(h.m.edu.yr.par)
h.m.edu.yr.par.tab <- tidy(h.m.edu.yr.par) %>% mutate(LL=sprintf(confint(h.m.edu.yr.par)[,1],fmt='%#.3f'),
                                                  UL=sprintf(confint(h.m.edu.yr.par)[,2],fmt='%#.3f')) %>%
  unite(col=CI,LL:UL, sep = ", ", remove=F)
  
#NH White
nhw.m.edu.yr.par <- svyglm(accvidalb~edu.c*medu.c +
                           sex +
                           scale(H13ITOT) + 
                           smokestatus +
                           R13BMI,design=subset(svy.edaa,raceth=="NHW"))
summary(nhw.m.edu.yr.par)
nhw.m.edu.yr.par.tab <- tidy(nhw.m.edu.yr.par) %>% mutate(LL=sprintf(confint(nhw.m.edu.yr.par)[,1],fmt='%#.3f'),
                                                      UL=sprintf(confint(nhw.m.edu.yr.par)[,2],fmt='%#.3f')) %>%
  unite(col=CI,LL:UL, sep = ", ", remove=F)
  

# Stack Columns 
re.m.edu.yr.par.VIDALBRALO_DNAMAGE<- bind_rows("NH Black"=nhb.m.edu.yr.par.tab,"NH Hispanic"=h.m.edu.yr.par.tab,"NH White"=nhw.m.edu.yr.par.tab,.id='Subgroup')
write.csv(re.m.edu.yr.par.VIDALBRALO_DNAMAGE,file='re.m.edu.yr.par.VIDALBRALO_DNAMAGE.csv')

## YANG_DNAMAGE ----
# Unadjusted
m.edu.yr.par <- svyglm(accyang~edu.c*medu.c,design=svy.edaa)
summary(m.edu.yr.par)
m.edu.yr.par.tab <- tidy(m.edu.yr.par) %>% mutate(LL=sprintf(confint(m.edu.yr.par)[,1],fmt='%#.3f'),
                                              UL=sprintf(confint(m.edu.yr.par)[,2],fmt='%#.3f')) %>%
  unite(col=CI,LL:UL, sep = ", ", remove=F)
  

# Adjusted
adj.m.edu.yr.par <- svyglm(accyang~edu.c*medu.c +
                           raceth + 
                           sex +
                           scale(H13ITOT) + 
                           smokestatus +
                           R13BMI,design=svy.edaa)
summary(adj.m.edu.yr.par)
adj.m.edu.yr.par.tab <- tidy(adj.m.edu.yr.par) %>% mutate(LL=sprintf(confint(adj.m.edu.yr.par)[,1],fmt='%#.3f'),
                                                      UL=sprintf(confint(adj.m.edu.yr.par)[,2],fmt='%#.3f')) %>%
  unite(col=CI,LL:UL, sep = ", ", remove=F)
  
names(adj.m.edu.yr.par.tab) <- gsub("^(.*?)","adj_\\2", names(adj.m.edu.yr.par.tab))

# Combine results into one table
m.edu.yr.par.YANG_DNAMAGE <- merge(m.edu.yr.par.tab,adj.m.edu.yr.par.tab,by.x='term',by.y='adj_term',all.y=T,sort=F) 
write.csv(m.edu.yr.par.YANG_DNAMAGE,'m.edu.yr.par.YANG_DNAMAGE.csv')

## Stratify by race/ethnicity
# NH Black
nhb.m.edu.yr.par <- svyglm(accyang~edu.c*medu.c +
                           sex +
                           scale(H13ITOT) + 
                           smokestatus +
                           R13BMI,design=subset(svy.edaa,raceth=="NHB"))
summary(nhb.m.edu.yr.par)
nhb.m.edu.yr.par.tab <- tidy(nhb.m.edu.yr.par) %>% mutate(LL=sprintf(confint(nhb.m.edu.yr.par)[,1],fmt='%#.3f'),
                                                      UL=sprintf(confint(nhb.m.edu.yr.par)[,2],fmt='%#.3f')) %>%
  unite(col=CI,LL:UL, sep = ", ", remove=F)
  
# Hispanic
h.m.edu.yr.par <- svyglm(accyang~edu.c*medu.c +
                         sex +
                         scale(H13ITOT) + 
                         smokestatus +
                         R13BMI,design=subset(svy.edaa,raceth=="Hispanic"))
summary(h.m.edu.yr.par)
h.m.edu.yr.par.tab <- tidy(h.m.edu.yr.par) %>% mutate(LL=sprintf(confint(h.m.edu.yr.par)[,1],fmt='%#.3f'),
                                                  UL=sprintf(confint(h.m.edu.yr.par)[,2],fmt='%#.3f')) %>%
  unite(col=CI,LL:UL, sep = ", ", remove=F)
  
#NH White
nhw.m.edu.yr.par <- svyglm(accyang~edu.c*medu.c +
                           sex +
                           scale(H13ITOT) + 
                           smokestatus +
                           R13BMI,design=subset(svy.edaa,raceth=="NHW"))
summary(nhw.m.edu.yr.par)
nhw.m.edu.yr.par.tab <- tidy(nhw.m.edu.yr.par) %>% mutate(LL=sprintf(confint(nhw.m.edu.yr.par)[,1],fmt='%#.3f'),
                                                      UL=sprintf(confint(nhw.m.edu.yr.par)[,2],fmt='%#.3f')) %>%
  unite(col=CI,LL:UL, sep = ", ", remove=F)
  

# Stack Columns 
re.m.edu.yr.par.YANG_DNAMAGE<- bind_rows("NH Black"=nhb.m.edu.yr.par.tab,"NH Hispanic"=h.m.edu.yr.par.tab,"NH White"=nhw.m.edu.yr.par.tab,.id='Subgroup')
write.csv(re.m.edu.yr.par.YANG_DNAMAGE,file='re.m.edu.yr.par.YANG_DNAMAGE.csv')

## ZHANG_DNAMAGE ----
# Unadjusted
m.edu.yr.par <- svyglm(acczhang~edu.c*medu.c,design=svy.edaa)
summary(m.edu.yr.par)
m.edu.yr.par.tab <- tidy(m.edu.yr.par) %>% mutate(LL=sprintf(confint(m.edu.yr.par)[,1],fmt='%#.3f'),
                                              UL=sprintf(confint(m.edu.yr.par)[,2],fmt='%#.3f')) %>%
  unite(col=CI,LL:UL, sep = ", ", remove=F)
  

# Adjusted
adj.m.edu.yr.par <- svyglm(acczhang~edu.c*medu.c +
                           raceth + 
                           sex +
                           scale(H13ITOT) + 
                           smokestatus +
                           R13BMI,design=svy.edaa)
summary(adj.m.edu.yr.par)
adj.m.edu.yr.par.tab <- tidy(adj.m.edu.yr.par) %>% mutate(LL=sprintf(confint(adj.m.edu.yr.par)[,1],fmt='%#.3f'),
                                                      UL=sprintf(confint(adj.m.edu.yr.par)[,2],fmt='%#.3f')) %>%
  unite(col=CI,LL:UL, sep = ", ", remove=F)
  
names(adj.m.edu.yr.par.tab) <- gsub("^(.*?)","adj_\\2", names(adj.m.edu.yr.par.tab))

# Combine results into one table
m.edu.yr.par.ZHANG_DNAMAGE <- merge(m.edu.yr.par.tab,adj.m.edu.yr.par.tab,by.x='term',by.y='adj_term',all.y=T,sort=F) 
write.csv(m.edu.yr.par.ZHANG_DNAMAGE,'m.edu.yr.par.ZHANG_DNAMAGE.csv')

## Stratify by race/ethnicity
# NH Black
nhb.m.edu.yr.par <- svyglm(acczhang~edu.c*medu.c +
                           sex +
                           scale(H13ITOT) + 
                           smokestatus +
                           R13BMI,design=subset(svy.edaa,raceth=="NHB"))
summary(nhb.m.edu.yr.par)
nhb.m.edu.yr.par.tab <- tidy(nhb.m.edu.yr.par) %>% mutate(LL=sprintf(confint(nhb.m.edu.yr.par)[,1],fmt='%#.3f'),
                                                      UL=sprintf(confint(nhb.m.edu.yr.par)[,2],fmt='%#.3f')) %>%
  unite(col=CI,LL:UL, sep = ", ", remove=F)
  
# Hispanic
h.m.edu.yr.par <- svyglm(acczhang~edu.c*medu.c +
                         sex +
                         scale(H13ITOT) + 
                         smokestatus +
                         R13BMI,design=subset(svy.edaa,raceth=="Hispanic"))
summary(h.m.edu.yr.par)
h.m.edu.yr.par.tab <- tidy(h.m.edu.yr.par) %>% mutate(LL=sprintf(confint(h.m.edu.yr.par)[,1],fmt='%#.3f'),
                                                  UL=sprintf(confint(h.m.edu.yr.par)[,2],fmt='%#.3f')) %>%
  unite(col=CI,LL:UL, sep = ", ", remove=F)
  
#NH White
nhw.m.edu.yr.par <- svyglm(acczhang~edu.c*medu.c +
                           sex +
                           scale(H13ITOT) + 
                           smokestatus +
                           R13BMI,design=subset(svy.edaa,raceth=="NHW"))
summary(nhw.m.edu.yr.par)
nhw.m.edu.yr.par.tab <- tidy(nhw.m.edu.yr.par) %>% mutate(LL=sprintf(confint(nhw.m.edu.yr.par)[,1],fmt='%#.3f'),
                                                      UL=sprintf(confint(nhw.m.edu.yr.par)[,2],fmt='%#.3f')) %>%
  unite(col=CI,LL:UL, sep = ", ", remove=F)
  

# Stack Columns 
re.m.edu.yr.par.ZHANG_DNAMAGE<- bind_rows("NH Black"=nhb.m.edu.yr.par.tab,"NH Hispanic"=h.m.edu.yr.par.tab,"NH White"=nhw.m.edu.yr.par.tab,.id='Subgroup')
write.csv(re.m.edu.yr.par.ZHANG_DNAMAGE,file='re.m.edu.yr.par.ZHANG_DNAMAGE.csv')

## BOCKLANDT_DNAMAGE ----
# Unadjusted
m.edu.yr.par <- svyglm(accbock~edu.c*medu.c,design=svy.edaa)
summary(m.edu.yr.par)
m.edu.yr.par.tab <- tidy(m.edu.yr.par) %>% mutate(LL=sprintf(confint(m.edu.yr.par)[,1],fmt='%#.3f'),
                                              UL=sprintf(confint(m.edu.yr.par)[,2],fmt='%#.3f')) %>%
  unite(col=CI,LL:UL, sep = ", ", remove=F)
  

# Adjusted
adj.m.edu.yr.par <- svyglm(accbock~edu.c*medu.c +
                           raceth + 
                           sex +
                           scale(H13ITOT) + 
                           smokestatus +
                           R13BMI,design=svy.edaa)
summary(adj.m.edu.yr.par)
adj.m.edu.yr.par.tab <- tidy(adj.m.edu.yr.par) %>% mutate(LL=sprintf(confint(adj.m.edu.yr.par)[,1],fmt='%#.3f'),
                                                      UL=sprintf(confint(adj.m.edu.yr.par)[,2],fmt='%#.3f')) %>%
  unite(col=CI,LL:UL, sep = ", ", remove=F)
  
names(adj.m.edu.yr.par.tab) <- gsub("^(.*?)","adj_\\2", names(adj.m.edu.yr.par.tab))

# Combine results into one table
m.edu.yr.par.BOCKLANDT_DNAMAGE <- merge(m.edu.yr.par.tab,adj.m.edu.yr.par.tab,by.x='term',by.y='adj_term',all.y=T,sort=F) 
write.csv(m.edu.yr.par.BOCKLANDT_DNAMAGE,'m.edu.yr.par.BOCKLANDT_DNAMAGE.csv')

## Stratify by race/ethnicity
# NH Black
nhb.m.edu.yr.par <- svyglm(accbock~edu.c*medu.c +
                           sex +
                           scale(H13ITOT) + 
                           smokestatus +
                           R13BMI,design=subset(svy.edaa,raceth=="NHB"))
summary(nhb.m.edu.yr.par)
nhb.m.edu.yr.par.tab <- tidy(nhb.m.edu.yr.par) %>% mutate(LL=sprintf(confint(nhb.m.edu.yr.par)[,1],fmt='%#.3f'),
                                                      UL=sprintf(confint(nhb.m.edu.yr.par)[,2],fmt='%#.3f')) %>%
  unite(col=CI,LL:UL, sep = ", ", remove=F)
  
# Hispanic
h.m.edu.yr.par <- svyglm(accbock~edu.c*medu.c +
                         sex +
                         scale(H13ITOT) + 
                         smokestatus +
                         R13BMI,design=subset(svy.edaa,raceth=="Hispanic"))
summary(h.m.edu.yr.par)
h.m.edu.yr.par.tab <- tidy(h.m.edu.yr.par) %>% mutate(LL=sprintf(confint(h.m.edu.yr.par)[,1],fmt='%#.3f'),
                                                  UL=sprintf(confint(h.m.edu.yr.par)[,2],fmt='%#.3f')) %>%
  unite(col=CI,LL:UL, sep = ", ", remove=F)
  
#NH White
nhw.m.edu.yr.par <- svyglm(accbock~edu.c*medu.c +
                           sex +
                           scale(H13ITOT) + 
                           smokestatus +
                           R13BMI,design=subset(svy.edaa,raceth=="NHW"))
summary(nhw.m.edu.yr.par)
nhw.m.edu.yr.par.tab <- tidy(nhw.m.edu.yr.par) %>% mutate(LL=sprintf(confint(nhw.m.edu.yr.par)[,1],fmt='%#.3f'),
                                                      UL=sprintf(confint(nhw.m.edu.yr.par)[,2],fmt='%#.3f')) %>%
  unite(col=CI,LL:UL, sep = ", ", remove=F)
  

# Stack Columns 
re.m.edu.yr.par.BOCKLANDT_DNAMAGE<- bind_rows("NH Black"=nhb.m.edu.yr.par.tab,"NH Hispanic"=h.m.edu.yr.par.tab,"NH White"=nhw.m.edu.yr.par.tab,.id='Subgroup')
write.csv(re.m.edu.yr.par.BOCKLANDT_DNAMAGE,file='re.m.edu.yr.par.BOCKLANDT_DNAMAGE.csv')

## GARAGNANI_DNAMAGE ----
# Unadjusted
m.edu.yr.par <- svyglm(accgarag~edu.c*medu.c,design=svy.edaa)
summary(m.edu.yr.par)
m.edu.yr.par.tab <- tidy(m.edu.yr.par) %>% mutate(LL=sprintf(confint(m.edu.yr.par)[,1],fmt='%#.3f'),
                                              UL=sprintf(confint(m.edu.yr.par)[,2],fmt='%#.3f')) %>%
  unite(col=CI,LL:UL, sep = ", ", remove=F)
  

# Adjusted
adj.m.edu.yr.par <- svyglm(accgarag~edu.c*medu.c +
                           raceth + 
                           sex +
                           scale(H13ITOT) + 
                           smokestatus +
                           R13BMI,design=svy.edaa)
summary(adj.m.edu.yr.par)
adj.m.edu.yr.par.tab <- tidy(adj.m.edu.yr.par) %>% mutate(LL=sprintf(confint(adj.m.edu.yr.par)[,1],fmt='%#.3f'),
                                                      UL=sprintf(confint(adj.m.edu.yr.par)[,2],fmt='%#.3f')) %>%
  unite(col=CI,LL:UL, sep = ", ", remove=F)
  
names(adj.m.edu.yr.par.tab) <- gsub("^(.*?)","adj_\\2", names(adj.m.edu.yr.par.tab))

# Combine results into one table
m.edu.yr.par.GARAGNANI_DNAMAGE <- merge(m.edu.yr.par.tab,adj.m.edu.yr.par.tab,by.x='term',by.y='adj_term',all.y=T,sort=F) 
write.csv(m.edu.yr.par.GARAGNANI_DNAMAGE,'m.edu.yr.par.GARAGNANI_DNAMAGE.csv')

## Stratify by race/ethnicity
# NH Black
nhb.m.edu.yr.par <- svyglm(accgarag~edu.c*medu.c +
                           sex +
                           scale(H13ITOT) + 
                           smokestatus +
                           R13BMI,design=subset(svy.edaa,raceth=="NHB"))
summary(nhb.m.edu.yr.par)
nhb.m.edu.yr.par.tab <- tidy(nhb.m.edu.yr.par) %>% mutate(LL=sprintf(confint(nhb.m.edu.yr.par)[,1],fmt='%#.3f'),
                                                      UL=sprintf(confint(nhb.m.edu.yr.par)[,2],fmt='%#.3f')) %>%
  unite(col=CI,LL:UL, sep = ", ", remove=F)
  
# Hispanic
h.m.edu.yr.par <- svyglm(accgarag~edu.c*medu.c +
                         sex +
                         scale(H13ITOT) + 
                         smokestatus +
                         R13BMI,design=subset(svy.edaa,raceth=="Hispanic"))
summary(h.m.edu.yr.par)
h.m.edu.yr.par.tab <- tidy(h.m.edu.yr.par) %>% mutate(LL=sprintf(confint(h.m.edu.yr.par)[,1],fmt='%#.3f'),
                                                  UL=sprintf(confint(h.m.edu.yr.par)[,2],fmt='%#.3f')) %>%
  unite(col=CI,LL:UL, sep = ", ", remove=F)
  
#NH White
nhw.m.edu.yr.par <- svyglm(accgarag~edu.c*medu.c +
                           sex +
                           scale(H13ITOT) + 
                           smokestatus +
                           R13BMI,design=subset(svy.edaa,raceth=="NHW"))
summary(nhw.m.edu.yr.par)
nhw.m.edu.yr.par.tab <- tidy(nhw.m.edu.yr.par) %>% mutate(LL=sprintf(confint(nhw.m.edu.yr.par)[,1],fmt='%#.3f'),
                                                      UL=sprintf(confint(nhw.m.edu.yr.par)[,2],fmt='%#.3f')) %>%
  unite(col=CI,LL:UL, sep = ", ", remove=F)
  

# Stack Columns 
re.m.edu.yr.par.GARAGNANI_DNAMAGE<- bind_rows("NH Black"=nhb.m.edu.yr.par.tab,"NH Hispanic"=h.m.edu.yr.par.tab,"NH White"=nhw.m.edu.yr.par.tab,.id='Subgroup')
write.csv(re.m.edu.yr.par.GARAGNANI_DNAMAGE,file='re.m.edu.yr.par.GARAGNANI_DNAMAGE.csv')

## DNAMGRIMAGE ----
# Unadjusted
m.edu.yr.par <- svyglm(accdnam~edu.c*medu.c,design=svy.edaa)
summary(m.edu.yr.par)
m.edu.yr.par.tab <- tidy(m.edu.yr.par) %>% mutate(LL=sprintf(confint(m.edu.yr.par)[,1],fmt='%#.3f'),
                                              UL=sprintf(confint(m.edu.yr.par)[,2],fmt='%#.3f')) %>%
  unite(col=CI,LL:UL, sep = ", ", remove=F)
  

# Adjusted
adj.m.edu.yr.par <- svyglm(accdnam~edu.c*medu.c +
                           raceth + 
                           sex +
                           scale(H13ITOT) + 
                           smokestatus +
                           R13BMI,design=svy.edaa)
summary(adj.m.edu.yr.par)
adj.m.edu.yr.par.tab <- tidy(adj.m.edu.yr.par) %>% mutate(LL=sprintf(confint(adj.m.edu.yr.par)[,1],fmt='%#.3f'),
                                                      UL=sprintf(confint(adj.m.edu.yr.par)[,2],fmt='%#.3f')) %>%
  unite(col=CI,LL:UL, sep = ", ", remove=F)
  
names(adj.m.edu.yr.par.tab) <- gsub("^(.*?)","adj_\\2", names(adj.m.edu.yr.par.tab))

# Combine results into one table
m.edu.yr.par.DNAMGRIMAGE <- merge(m.edu.yr.par.tab,adj.m.edu.yr.par.tab,by.x='term',by.y='adj_term',all.y=T,sort=F) 
write.csv(m.edu.yr.par.DNAMGRIMAGE,'m.edu.yr.par.DNAMGRIMAGE.csv')

## Stratify by race/ethnicity
# NH Black
nhb.m.edu.yr.par <- svyglm(accdnam~edu.c*medu.c +
                           sex +
                           scale(H13ITOT) + 
                           smokestatus +
                           R13BMI,design=subset(svy.edaa,raceth=="NHB"))
summary(nhb.m.edu.yr.par)
nhb.m.edu.yr.par.tab <- tidy(nhb.m.edu.yr.par) %>% mutate(LL=sprintf(confint(nhb.m.edu.yr.par)[,1],fmt='%#.3f'),
                                                      UL=sprintf(confint(nhb.m.edu.yr.par)[,2],fmt='%#.3f')) %>%
  unite(col=CI,LL:UL, sep = ", ", remove=F)
  
# Hispanic
h.m.edu.yr.par <- svyglm(accdnam~edu.c*medu.c +
                         sex +
                         scale(H13ITOT) + 
                         smokestatus +
                         R13BMI,design=subset(svy.edaa,raceth=="Hispanic"))
summary(h.m.edu.yr.par)
h.m.edu.yr.par.tab <- tidy(h.m.edu.yr.par) %>% mutate(LL=sprintf(confint(h.m.edu.yr.par)[,1],fmt='%#.3f'),
                                                  UL=sprintf(confint(h.m.edu.yr.par)[,2],fmt='%#.3f')) %>%
  unite(col=CI,LL:UL, sep = ", ", remove=F)
  
#NH White
nhw.m.edu.yr.par <- svyglm(accdnam~edu.c*medu.c +
                           sex +
                           scale(H13ITOT) + 
                           smokestatus +
                           R13BMI,design=subset(svy.edaa,raceth=="NHW"))
summary(nhw.m.edu.yr.par)
nhw.m.edu.yr.par.tab <- tidy(nhw.m.edu.yr.par) %>% mutate(LL=sprintf(confint(nhw.m.edu.yr.par)[,1],fmt='%#.3f'),
                                                      UL=sprintf(confint(nhw.m.edu.yr.par)[,2],fmt='%#.3f')) %>%
  unite(col=CI,LL:UL, sep = ", ", remove=F)
  

# Stack Columns 
re.m.edu.yr.par.DNAMGRIMAGE<- bind_rows("NH Black"=nhb.m.edu.yr.par.tab,"NH Hispanic"=h.m.edu.yr.par.tab,"NH White"=nhw.m.edu.yr.par.tab,.id='Subgroup')
write.csv(re.m.edu.yr.par.DNAMGRIMAGE,file='re.m.edu.yr.par.DNAMGRIMAGE.csv')

## MPOA ----
# Unadjusted
m.edu.yr.par <- svyglm(accmpoa~edu.c*medu.c,design=svy.edaa)
summary(m.edu.yr.par)
m.edu.yr.par.tab <- tidy(m.edu.yr.par) %>% mutate(LL=sprintf(confint(m.edu.yr.par)[,1],fmt='%#.3f'),
                                              UL=sprintf(confint(m.edu.yr.par)[,2],fmt='%#.3f')) %>%
  unite(col=CI,LL:UL, sep = ", ", remove=F)
  

# Adjusted
adj.m.edu.yr.par <- svyglm(accmpoa~edu.c*medu.c +
                           raceth + 
                           sex +
                           scale(H13ITOT) + 
                           smokestatus +
                           R13BMI,design=svy.edaa)
summary(adj.m.edu.yr.par)
adj.m.edu.yr.par.tab <- tidy(adj.m.edu.yr.par) %>% mutate(LL=sprintf(confint(adj.m.edu.yr.par)[,1],fmt='%#.3f'),
                                                      UL=sprintf(confint(adj.m.edu.yr.par)[,2],fmt='%#.3f')) %>%
  unite(col=CI,LL:UL, sep = ", ", remove=F)
  
names(adj.m.edu.yr.par.tab) <- gsub("^(.*?)","adj_\\2", names(adj.m.edu.yr.par.tab))

# Combine results into one table
m.edu.yr.par.MPOA<- merge(m.edu.yr.par.tab,adj.m.edu.yr.par.tab,by.x='term',by.y='adj_term',all.y=T,sort=F) 
write.csv(m.edu.yr.par.MPOA,'m.edu.yr.par.MPOA.csv')

## Stratify by race/ethnicity
# NH Black
nhb.m.edu.yr.par <- svyglm(accmpoa~edu.c*medu.c +
                           sex +
                           scale(H13ITOT) + 
                           smokestatus +
                           R13BMI,design=subset(svy.edaa,raceth=="NHB"))
summary(nhb.m.edu.yr.par)
nhb.m.edu.yr.par.tab <- tidy(nhb.m.edu.yr.par) %>% mutate(LL=sprintf(confint(nhb.m.edu.yr.par)[,1],fmt='%#.3f'),
                                                      UL=sprintf(confint(nhb.m.edu.yr.par)[,2],fmt='%#.3f')) %>%
  unite(col=CI,LL:UL, sep = ", ", remove=F)
  
# Hispanic
h.m.edu.yr.par <- svyglm(accmpoa~edu.c*medu.c +
                         sex +
                         scale(H13ITOT) + 
                         smokestatus +
                         R13BMI,design=subset(svy.edaa,raceth=="Hispanic"))
summary(h.m.edu.yr.par)
h.m.edu.yr.par.tab <- tidy(h.m.edu.yr.par) %>% mutate(LL=sprintf(confint(h.m.edu.yr.par)[,1],fmt='%#.3f'),
                                                  UL=sprintf(confint(h.m.edu.yr.par)[,2],fmt='%#.3f')) %>%
  unite(col=CI,LL:UL, sep = ", ", remove=F)
  
#NH White
nhw.m.edu.yr.par <- svyglm(accmpoa~edu.c*medu.c +
                           sex +
                           scale(H13ITOT) + 
                           smokestatus +
                           R13BMI,design=subset(svy.edaa,raceth=="NHW"))
summary(nhw.m.edu.yr.par)
nhw.m.edu.yr.par.tab <- tidy(nhw.m.edu.yr.par) %>% mutate(LL=sprintf(confint(nhw.m.edu.yr.par)[,1],fmt='%#.3f'),
                                                      UL=sprintf(confint(nhw.m.edu.yr.par)[,2],fmt='%#.3f')) %>%
  unite(col=CI,LL:UL, sep = ", ", remove=F)
  

# Stack Columns 
re.m.edu.yr.par.MPOA<- bind_rows("NH Black"=nhb.m.edu.yr.par.tab,"NH Hispanic"=h.m.edu.yr.par.tab,"NH White"=nhw.m.edu.yr.par.tab,.id='Subgroup')
write.csv(re.m.edu.yr.par.MPOA,file='re.m.edu.yr.par.MPOA.csv')

### Regression Models (Mother Educational Attainment-Categorical) ----
## HORVATH_DNAMAGE ----
# Unadjusted
m.edu.c.par <- svyglm(acchorv~RAEDYRS_factor*RAMEDUC_bin,design=svy.edaa)
summary(m.edu.c.par)
m.edu.c.par.tab <- tidy(m.edu.c.par) %>% mutate(LL=sprintf(confint(m.edu.c.par)[,1],fmt='%#.3f'),
                                            UL=sprintf(confint(m.edu.c.par)[,2],fmt='%#.3f')) %>%
  unite(col=CI,LL:UL, sep = ", ", remove=F)
  

# Adjusted
adj.m.edu.c.par <- svyglm(acchorv~RAEDYRS_factor*RAMEDUC_bin +
                          raceth + 
                          sex +
                          scale(H13ITOT) + 
                          smokestatus +
                          R13BMI,design=svy.edaa)
summary(adj.m.edu.c.par)
adj.m.edu.c.par.tab <- tidy(adj.m.edu.c.par) %>% mutate(LL=sprintf(confint(adj.m.edu.c.par)[,1],fmt='%#.3f'),
                                                    UL=sprintf(confint(adj.m.edu.c.par)[,2],fmt='%#.3f')) %>%
  unite(col=CI,LL:UL, sep = ", ", remove=F)
  
names(adj.m.edu.c.par.tab) <- gsub("^(.*?)","adj_\\2", names(adj.m.edu.c.par.tab))

# Combine results into one table
m.edu.c.par.HORVATH_DNAMAGE <- merge(m.edu.c.par.tab,adj.m.edu.c.par.tab,by.x='term',by.y='adj_term',all.y=T,sort=F) 
write.csv(m.edu.c.par.HORVATH_DNAMAGE,'m.edu.c.par.HORVATH_DNAMAGE.csv')

## Marginal Means
# Unadjusted
HORVATH_DNAMAGE.MM <- emmeans(m.edu.c.par,pairwise~RAEDYRS_factor*RAMEDUC_bin,data=svy.edaa,adjust='mvt')

# Adjusted
adj.HORVATH_DNAMAGE.MM <- emmeans(adj.m.edu.c.par,pairwise~RAEDYRS_factor*RAMEDUC_bin,data=svy.edaa,adjust='mvt')

## Stratify by race/ethnicity
# NH Black
nhb.m.edu.c.par <- svyglm(acchorv~RAEDYRS_factor*RAMEDUC_bin +
                          sex +
                          scale(H13ITOT) + 
                          smokestatus +
                          R13BMI,design=subset(svy.edaa,raceth=="NHB"))
summary(nhb.m.edu.c.par)
nhb.m.edu.c.par.tab <- tidy(nhb.m.edu.c.par) %>% mutate(LL=sprintf(confint(nhb.m.edu.c.par)[,1],fmt='%#.3f'),
                                                    UL=sprintf(confint(nhb.m.edu.c.par)[,2],fmt='%#.3f')) %>%
  unite(col=CI,LL:UL, sep = ", ", remove=F)
  
# Hispanic
h.m.edu.c.par <- svyglm(acchorv~RAEDYRS_factor*RAMEDUC_bin +
                        sex +
                        scale(H13ITOT) + 
                        smokestatus +
                        R13BMI,design=subset(svy.edaa,raceth=="Hispanic"))
summary(h.m.edu.c.par)
h.m.edu.c.par.tab <- tidy(h.m.edu.c.par) %>% mutate(LL=sprintf(confint(h.m.edu.c.par)[,1],fmt='%#.3f'),
                                                UL=sprintf(confint(h.m.edu.c.par)[,2],fmt='%#.3f')) %>%
  unite(col=CI,LL:UL, sep = ", ", remove=F)
  
#NH White
nhw.m.edu.c.par <- svyglm(acchorv~RAEDYRS_factor*RAMEDUC_bin +
                          sex +
                          scale(H13ITOT) + 
                          smokestatus +
                          R13BMI,design=subset(svy.edaa,raceth=="NHW"))
summary(nhw.m.edu.c.par)
nhw.m.edu.c.par.tab <- tidy(nhw.m.edu.c.par) %>% mutate(LL=sprintf(confint(nhw.m.edu.c.par)[,1],fmt='%#.3f'),
                                                    UL=sprintf(confint(nhw.m.edu.c.par)[,2],fmt='%#.3f')) %>%
  unite(col=CI,LL:UL, sep = ", ", remove=F)
  

# Stack Columns 
re.m.edu.c.par.HORVATH_DNAMAGE <- bind_rows("NH Black"=nhb.m.edu.c.par.tab,"NH Hispanic"=h.m.edu.c.par.tab,"NH White"=nhw.m.edu.c.par.tab,.id='Subgroup')
write.csv(re.m.edu.c.par.HORVATH_DNAMAGE,file='re.m.edu.c.par.HORVATH_DNAMAGE.csv')

# Save ANOVA results
sink(file="m.aov.edu.c.HORVATH_DNAMAGE.txt")
print(aov<-list(anova(m.edu.c.par),anova(adj.m.edu.c.par)))
sink()

sink(file="re.m.aov.edu.c.HORVATH_DNAMAGE.txt")
print(aov<-list(anova(nhb.m.edu.c.par),anova(h.m.edu.c.par),anova(nhw.m.edu.c.par)))
sink()

## HANNUM_DNAMAGE ----
# Unadjusted
m.edu.c.par <- svyglm(acchannum~RAEDYRS_factor*RAMEDUC_bin,design=svy.edaa)
summary(m.edu.c.par)
m.edu.c.par.tab <- tidy(m.edu.c.par) %>% mutate(LL=sprintf(confint(m.edu.c.par)[,1],fmt='%#.3f'),
                                            UL=sprintf(confint(m.edu.c.par)[,2],fmt='%#.3f')) %>%
  unite(col=CI,LL:UL, sep = ", ", remove=F)
  

# Adjusted
adj.m.edu.c.par <- svyglm(acchannum~RAEDYRS_factor*RAMEDUC_bin +
                          raceth + 
                          sex +
                          scale(H13ITOT) + 
                          smokestatus +
                          R13BMI,design=svy.edaa)
summary(adj.m.edu.c.par)
adj.m.edu.c.par.tab <- tidy(adj.m.edu.c.par) %>% mutate(LL=sprintf(confint(adj.m.edu.c.par)[,1],fmt='%#.3f'),
                                                    UL=sprintf(confint(adj.m.edu.c.par)[,2],fmt='%#.3f')) %>%
  unite(col=CI,LL:UL, sep = ", ", remove=F)
  
names(adj.m.edu.c.par.tab) <- gsub("^(.*?)","adj_\\2", names(adj.m.edu.c.par.tab))

# Combine results into one table
m.edu.c.par.HANNUM_DNAMAGE <- merge(m.edu.c.par.tab,adj.m.edu.c.par.tab,by.x='term',by.y='adj_term',all.y=T,sort=F) 
write.csv(m.edu.c.par.HANNUM_DNAMAGE,'m.edu.c.par.HANNUM_DNAMAGE.csv')

## Marginal Means
# Unadjusted
HANNUM_DNAMAGE.MM <- emmeans(m.edu.c.par,pairwise~RAEDYRS_factor*RAMEDUC_bin,data=svy.edaa,adjust='mvt')

# Adjusted
adj.HANNUM_DNAMAGE.MM <- emmeans(adj.m.edu.c.par,pairwise~RAEDYRS_factor*RAMEDUC_bin,data=svy.edaa,adjust='mvt')

## Stratify by race/ethnicity
# NH Black
nhb.m.edu.c.par <- svyglm(acchannum~RAEDYRS_factor*RAMEDUC_bin +
                          sex +
                          scale(H13ITOT) + 
                          smokestatus +
                          R13BMI,design=subset(svy.edaa,raceth=="NHB"))
summary(nhb.m.edu.c.par)
nhb.m.edu.c.par.tab <- tidy(nhb.m.edu.c.par) %>% mutate(LL=sprintf(confint(nhb.m.edu.c.par)[,1],fmt='%#.3f'),
                                                    UL=sprintf(confint(nhb.m.edu.c.par)[,2],fmt='%#.3f')) %>%
  unite(col=CI,LL:UL, sep = ", ", remove=F)
  
# Hispanic
h.m.edu.c.par <- svyglm(acchannum~RAEDYRS_factor*RAMEDUC_bin +
                        sex +
                        scale(H13ITOT) + 
                        smokestatus +
                        R13BMI,design=subset(svy.edaa,raceth=="Hispanic"))
summary(h.m.edu.c.par)
h.m.edu.c.par.tab <- tidy(h.m.edu.c.par) %>% mutate(LL=sprintf(confint(h.m.edu.c.par)[,1],fmt='%#.3f'),
                                                UL=sprintf(confint(h.m.edu.c.par)[,2],fmt='%#.3f')) %>%
  unite(col=CI,LL:UL, sep = ", ", remove=F)
  
#NH White
nhw.m.edu.c.par <- svyglm(acchannum~RAEDYRS_factor*RAMEDUC_bin +
                          sex +
                          scale(H13ITOT) + 
                          smokestatus +
                          R13BMI,design=subset(svy.edaa,raceth=="NHW"))
summary(nhw.m.edu.c.par)
nhw.m.edu.c.par.tab <- tidy(nhw.m.edu.c.par) %>% mutate(LL=sprintf(confint(nhw.m.edu.c.par)[,1],fmt='%#.3f'),
                                                    UL=sprintf(confint(nhw.m.edu.c.par)[,2],fmt='%#.3f')) %>%
  unite(col=CI,LL:UL, sep = ", ", remove=F)
  

# Stack Columns 
re.m.edu.c.par.HANNUM_DNAMAGE<- bind_rows("NH Black"=nhb.m.edu.c.par.tab,"NH Hispanic"=h.m.edu.c.par.tab,"NH White"=nhw.m.edu.c.par.tab,.id='Subgroup')
write.csv(re.m.edu.c.par.HANNUM_DNAMAGE,file='re.m.edu.c.par.HANNUM_DNAMAGE.csv')

# Save ANOVA results
sink(file="m.aov.edu.c.HANNUM_DNAMAGE.txt")
print(aov<-list(anova(m.edu.c.par),anova(adj.m.edu.c.par)))
sink()

sink(file="re.m.aov.edu.c.HANNUM_DNAMAGE.txt")
print(aov<-list(anova(nhb.m.edu.c.par),anova(h.m.edu.c.par),anova(nhw.m.edu.c.par)))
sink()

## LEVINE_DNAMAGE ----
# Unadjusted
m.edu.c.par <- svyglm(acclevine~RAEDYRS_factor*RAMEDUC_bin,design=svy.edaa)
summary(m.edu.c.par)
m.edu.c.par.tab <- tidy(m.edu.c.par) %>% mutate(LL=sprintf(confint(m.edu.c.par)[,1],fmt='%#.3f'),
                                            UL=sprintf(confint(m.edu.c.par)[,2],fmt='%#.3f')) %>%
  unite(col=CI,LL:UL, sep = ", ", remove=F)
  

# Adjusted
adj.m.edu.c.par <- svyglm(acclevine~RAEDYRS_factor*RAMEDUC_bin +
                          raceth + 
                          sex +
                          scale(H13ITOT) + 
                          smokestatus +
                          R13BMI,design=svy.edaa)
summary(adj.m.edu.c.par)
adj.m.edu.c.par.tab <- tidy(adj.m.edu.c.par) %>% mutate(LL=sprintf(confint(adj.m.edu.c.par)[,1],fmt='%#.3f'),
                                                    UL=sprintf(confint(adj.m.edu.c.par)[,2],fmt='%#.3f')) %>%
  unite(col=CI,LL:UL, sep = ", ", remove=F)
  
names(adj.m.edu.c.par.tab) <- gsub("^(.*?)","adj_\\2", names(adj.m.edu.c.par.tab))

# Combine results into one table
m.edu.c.par.LEVINE_DNAMAGE <- merge(m.edu.c.par.tab,adj.m.edu.c.par.tab,by.x='term',by.y='adj_term',all.y=T,sort=F) 
write.csv(m.edu.c.par.LEVINE_DNAMAGE,'m.edu.c.par.LEVINE_DNAMAGE.csv')

## Marginal Means
# Unadjusted
LEVINE_DNAMAGE.MM <- emmeans(m.edu.c.par,pairwise~RAEDYRS_factor*RAMEDUC_bin,data=svy.edaa,adjust='mvt')

# Adjusted
adj.LEVINE_DNAMAGE.MM <- emmeans(adj.m.edu.c.par,pairwise~RAEDYRS_factor*RAMEDUC_bin,data=svy.edaa,adjust='mvt')

## Stratify by race/ethnicity
# NH Black
nhb.m.edu.c.par <- svyglm(acclevine~RAEDYRS_factor*RAMEDUC_bin +
                          sex +
                          scale(H13ITOT) + 
                          smokestatus +
                          R13BMI,design=subset(svy.edaa,raceth=="NHB"))
summary(nhb.m.edu.c.par)
nhb.m.edu.c.par.tab <- tidy(nhb.m.edu.c.par) %>% mutate(LL=sprintf(confint(nhb.m.edu.c.par)[,1],fmt='%#.3f'),
                                                    UL=sprintf(confint(nhb.m.edu.c.par)[,2],fmt='%#.3f')) %>%
  unite(col=CI,LL:UL, sep = ", ", remove=F)
  
# Hispanic
h.m.edu.c.par <- svyglm(acclevine~RAEDYRS_factor*RAMEDUC_bin +
                        sex +
                        scale(H13ITOT) + 
                        smokestatus +
                        R13BMI,design=subset(svy.edaa,raceth=="Hispanic"))
summary(h.m.edu.c.par)
h.m.edu.c.par.tab <- tidy(h.m.edu.c.par) %>% mutate(LL=sprintf(confint(h.m.edu.c.par)[,1],fmt='%#.3f'),
                                                UL=sprintf(confint(h.m.edu.c.par)[,2],fmt='%#.3f')) %>%
  unite(col=CI,LL:UL, sep = ", ", remove=F)
  
#NH White
nhw.m.edu.c.par <- svyglm(acclevine~RAEDYRS_factor*RAMEDUC_bin +
                          sex +
                          scale(H13ITOT) + 
                          smokestatus +
                          R13BMI,design=subset(svy.edaa,raceth=="NHW"))
summary(nhw.m.edu.c.par)
nhw.m.edu.c.par.tab <- tidy(nhw.m.edu.c.par) %>% mutate(LL=sprintf(confint(nhw.m.edu.c.par)[,1],fmt='%#.3f'),
                                                    UL=sprintf(confint(nhw.m.edu.c.par)[,2],fmt='%#.3f')) %>%
  unite(col=CI,LL:UL, sep = ", ", remove=F)
  

# Stack Columns 
re.m.edu.c.par.LEVINE_DNAMAGE<- bind_rows("NH Black"=nhb.m.edu.c.par.tab,"NH Hispanic"=h.m.edu.c.par.tab,"NH White"=nhw.m.edu.c.par.tab,.id='Subgroup')
write.csv(re.m.edu.c.par.LEVINE_DNAMAGE,file='re.m.edu.c.par.LEVINE_DNAMAGE.csv')

# Save ANOVA results
sink(file="m.aov.edu.c.LEVINE_DNAMAGE.txt")
print(aov<-list(anova(m.edu.c.par),anova(adj.m.edu.c.par)))
sink()

sink(file="re.m.aov.edu.c.LEVINE_DNAMAGE.txt")
print(aov<-list(anova(nhb.m.edu.c.par),anova(h.m.edu.c.par),anova(nhw.m.edu.c.par)))
sink()

## HORVATHSKIN_DNAMAGE ----
# Unadjusted
m.edu.c.par <- svyglm(acchorvsk~RAEDYRS_factor*RAMEDUC_bin,design=svy.edaa)
summary(m.edu.c.par)
m.edu.c.par.tab <- tidy(m.edu.c.par) %>% mutate(LL=sprintf(confint(m.edu.c.par)[,1],fmt='%#.3f'),
                                            UL=sprintf(confint(m.edu.c.par)[,2],fmt='%#.3f')) %>%
  unite(col=CI,LL:UL, sep = ", ", remove=F)
  

# Adjusted
adj.m.edu.c.par <- svyglm(acchorvsk~RAEDYRS_factor*RAMEDUC_bin +
                          raceth + 
                          sex +
                          scale(H13ITOT) + 
                          smokestatus +
                          R13BMI,design=svy.edaa)
summary(adj.m.edu.c.par)
adj.m.edu.c.par.tab <- tidy(adj.m.edu.c.par) %>% mutate(LL=sprintf(confint(adj.m.edu.c.par)[,1],fmt='%#.3f'),
                                                    UL=sprintf(confint(adj.m.edu.c.par)[,2],fmt='%#.3f')) %>%
  unite(col=CI,LL:UL, sep = ", ", remove=F)
  
names(adj.m.edu.c.par.tab) <- gsub("^(.*?)","adj_\\2", names(adj.m.edu.c.par.tab))

# Combine results into one table
m.edu.c.par.HORVATHSKIN_DNAMAGE<- merge(m.edu.c.par.tab,adj.m.edu.c.par.tab,by.x='term',by.y='adj_term',all.y=T,sort=F) 
write.csv(m.edu.c.par.HORVATHSKIN_DNAMAGE,'m.edu.c.par.HORVATHSKIN_DNAMAGE.csv')

## Marginal Means
# Unadjusted
HORVATHSKIN_DNAMAGE.MM <- emmeans(m.edu.c.par,pairwise~RAEDYRS_factor*RAMEDUC_bin,data=svy.edaa,adjust='mvt')

# Adjusted
adj.HORVATHSKIN_DNAMAGE.MM <- emmeans(adj.m.edu.c.par,pairwise~RAEDYRS_factor*RAMEDUC_bin,data=svy.edaa,adjust='mvt')

## Stratify by race/ethnicity
# NH Black
nhb.m.edu.c.par <- svyglm(acchorvsk~RAEDYRS_factor*RAMEDUC_bin +
                          sex +
                          scale(H13ITOT) + 
                          smokestatus +
                          R13BMI,design=subset(svy.edaa,raceth=="NHB"))
summary(nhb.m.edu.c.par)
nhb.m.edu.c.par.tab <- tidy(nhb.m.edu.c.par) %>% mutate(LL=sprintf(confint(nhb.m.edu.c.par)[,1],fmt='%#.3f'),
                                                    UL=sprintf(confint(nhb.m.edu.c.par)[,2],fmt='%#.3f')) %>%
  unite(col=CI,LL:UL, sep = ", ", remove=F)
  
# Hispanic
h.m.edu.c.par <- svyglm(acchorvsk~RAEDYRS_factor*RAMEDUC_bin +
                        sex +
                        scale(H13ITOT) + 
                        smokestatus +
                        R13BMI,design=subset(svy.edaa,raceth=="Hispanic"))
summary(h.m.edu.c.par)
h.m.edu.c.par.tab <- tidy(h.m.edu.c.par) %>% mutate(LL=sprintf(confint(h.m.edu.c.par)[,1],fmt='%#.3f'),
                                                UL=sprintf(confint(h.m.edu.c.par)[,2],fmt='%#.3f')) %>%
  unite(col=CI,LL:UL, sep = ", ", remove=F)
  
#NH White
nhw.m.edu.c.par <- svyglm(acchorvsk~RAEDYRS_factor*RAMEDUC_bin +
                          sex +
                          scale(H13ITOT) + 
                          smokestatus +
                          R13BMI,design=subset(svy.edaa,raceth=="NHW"))
summary(nhw.m.edu.c.par)
nhw.m.edu.c.par.tab <- tidy(nhw.m.edu.c.par) %>% mutate(LL=sprintf(confint(nhw.m.edu.c.par)[,1],fmt='%#.3f'),
                                                    UL=sprintf(confint(nhw.m.edu.c.par)[,2],fmt='%#.3f')) %>%
  unite(col=CI,LL:UL, sep = ", ", remove=F)
  

# Stack Columns 
re.m.edu.c.par.HORVATHSKIN_DNAMAGE<- bind_rows("NH Black"=nhb.m.edu.c.par.tab,"NH Hispanic"=h.m.edu.c.par.tab,"NH White"=nhw.m.edu.c.par.tab,.id='Subgroup')
write.csv(re.m.edu.c.par.HORVATHSKIN_DNAMAGE,file='re.m.edu.c.par.HORVATHSKIN_DNAMAGE.csv')

# Save ANOVA results
sink(file="m.aov.edu.c.HORVATHSKIN_DNAMAGE.txt")
print(aov<-list(anova(m.edu.c.par),anova(adj.m.edu.c.par)))
sink()

sink(file="re.m.aov.edu.c.HORVATHSKIN_DNAMAGE.txt")
print(aov<-list(anova(nhb.m.edu.c.par),anova(h.m.edu.c.par),anova(nhw.m.edu.c.par)))
sink()

## LIN_DNAMAGE ----
# Unadjusted
m.edu.c.par <- svyglm(acclin~RAEDYRS_factor*RAMEDUC_bin,design=svy.edaa)
summary(m.edu.c.par)
m.edu.c.par.tab <- tidy(m.edu.c.par) %>% mutate(LL=sprintf(confint(m.edu.c.par)[,1],fmt='%#.3f'),
                                            UL=sprintf(confint(m.edu.c.par)[,2],fmt='%#.3f')) %>%
  unite(col=CI,LL:UL, sep = ", ", remove=F)
  

# Adjusted
adj.m.edu.c.par <- svyglm(acclin~RAEDYRS_factor*RAMEDUC_bin +
                          raceth + 
                          sex +
                          scale(H13ITOT) + 
                          smokestatus +
                          R13BMI,design=svy.edaa)
summary(adj.m.edu.c.par)
adj.m.edu.c.par.tab <- tidy(adj.m.edu.c.par) %>% mutate(LL=sprintf(confint(adj.m.edu.c.par)[,1],fmt='%#.3f'),
                                                    UL=sprintf(confint(adj.m.edu.c.par)[,2],fmt='%#.3f')) %>%
  unite(col=CI,LL:UL, sep = ", ", remove=F)
  
names(adj.m.edu.c.par.tab) <- gsub("^(.*?)","adj_\\2", names(adj.m.edu.c.par.tab))

# Combine results into one table
m.edu.c.par.LIN_DNAMAGE <- merge(m.edu.c.par.tab,adj.m.edu.c.par.tab,by.x='term',by.y='adj_term',all.y=T,sort=F) 
write.csv(m.edu.c.par.LIN_DNAMAGE,'m.edu.c.par.LIN_DNAMAGE.csv')

## Marginal Means
# Unadjusted
LIN_DNAMAGE.MM <- emmeans(m.edu.c.par,pairwise~RAEDYRS_factor*RAMEDUC_bin,data=svy.edaa,adjust='mvt')

# Adjusted
adj.LIN_DNAMAGE.MM <- emmeans(adj.m.edu.c.par,pairwise~RAEDYRS_factor*RAMEDUC_bin,data=svy.edaa,adjust='mvt')

## Stratify by race/ethnicity
# NH Black
nhb.m.edu.c.par <- svyglm(acclin~RAEDYRS_factor*RAMEDUC_bin +
                          sex +
                          scale(H13ITOT) + 
                          smokestatus +
                          R13BMI,design=subset(svy.edaa,raceth=="NHB"))
summary(nhb.m.edu.c.par)
nhb.m.edu.c.par.tab <- tidy(nhb.m.edu.c.par) %>% mutate(LL=sprintf(confint(nhb.m.edu.c.par)[,1],fmt='%#.3f'),
                                                    UL=sprintf(confint(nhb.m.edu.c.par)[,2],fmt='%#.3f')) %>%
  unite(col=CI,LL:UL, sep = ", ", remove=F)
  
# Hispanic
h.m.edu.c.par <- svyglm(acclin~RAEDYRS_factor*RAMEDUC_bin +
                        sex +
                        scale(H13ITOT) + 
                        smokestatus +
                        R13BMI,design=subset(svy.edaa,raceth=="Hispanic"))
summary(h.m.edu.c.par)
h.m.edu.c.par.tab <- tidy(h.m.edu.c.par) %>% mutate(LL=sprintf(confint(h.m.edu.c.par)[,1],fmt='%#.3f'),
                                                UL=sprintf(confint(h.m.edu.c.par)[,2],fmt='%#.3f')) %>%
  unite(col=CI,LL:UL, sep = ", ", remove=F)
  
#NH White
nhw.m.edu.c.par <- svyglm(acclin~RAEDYRS_factor*RAMEDUC_bin +
                          sex +
                          scale(H13ITOT) + 
                          smokestatus +
                          R13BMI,design=subset(svy.edaa,raceth=="NHW"))
summary(nhw.m.edu.c.par)
nhw.m.edu.c.par.tab <- tidy(nhw.m.edu.c.par) %>% mutate(LL=sprintf(confint(nhw.m.edu.c.par)[,1],fmt='%#.3f'),
                                                    UL=sprintf(confint(nhw.m.edu.c.par)[,2],fmt='%#.3f')) %>%
  unite(col=CI,LL:UL, sep = ", ", remove=F)
  

# Stack Columns 
re.m.edu.c.par.LIN_DNAMAGE<- bind_rows("NH Black"=nhb.m.edu.c.par.tab,"NH Hispanic"=h.m.edu.c.par.tab,"NH White"=nhw.m.edu.c.par.tab,.id='Subgroup')
write.csv(re.m.edu.c.par.LIN_DNAMAGE,file='re.m.edu.c.par.LIN_DNAMAGE.csv')

# Save ANOVA results
sink(file="m.aov.edu.c.LIN_DNAMAGE.txt")
print(aov<-list(anova(m.edu.c.par),anova(adj.m.edu.c.par)))
sink()

sink(file="re.m.aov.edu.c.LIN_DNAMAGE.txt")
print(aov<-list(anova(nhb.m.edu.c.par),anova(h.m.edu.c.par),anova(nhw.m.edu.c.par)))
sink()

## WEIDNER_DNAMAGE ----
# Unadjusted
m.edu.c.par <- svyglm(accweid~RAEDYRS_factor*RAMEDUC_bin,design=svy.edaa)
summary(m.edu.c.par)
m.edu.c.par.tab <- tidy(m.edu.c.par) %>% mutate(LL=sprintf(confint(m.edu.c.par)[,1],fmt='%#.3f'),
                                            UL=sprintf(confint(m.edu.c.par)[,2],fmt='%#.3f')) %>%
  unite(col=CI,LL:UL, sep = ", ", remove=F)
  

# Adjusted
adj.m.edu.c.par <- svyglm(accweid~RAEDYRS_factor*RAMEDUC_bin +
                          raceth + 
                          sex +
                          scale(H13ITOT) + 
                          smokestatus +
                          R13BMI,design=svy.edaa)
summary(adj.m.edu.c.par)
adj.m.edu.c.par.tab <- tidy(adj.m.edu.c.par) %>% mutate(LL=sprintf(confint(adj.m.edu.c.par)[,1],fmt='%#.3f'),
                                                    UL=sprintf(confint(adj.m.edu.c.par)[,2],fmt='%#.3f')) %>%
  unite(col=CI,LL:UL, sep = ", ", remove=F)
  
names(adj.m.edu.c.par.tab) <- gsub("^(.*?)","adj_\\2", names(adj.m.edu.c.par.tab))

# Combine results into one table
m.edu.c.par.WEIDNER_DNAMAGE <- merge(m.edu.c.par.tab,adj.m.edu.c.par.tab,by.x='term',by.y='adj_term',all.y=T,sort=F) 
write.csv(m.edu.c.par.WEIDNER_DNAMAGE,'m.edu.c.par.WEIDNER_DNAMAGE.csv')

## Marginal Means
# Unadjusted
WEIDNER_DNAMAGE.MM <- emmeans(m.edu.c.par,pairwise~RAEDYRS_factor*RAMEDUC_bin,data=svy.edaa,adjust='mvt')

# Adjusted
adj.WEIDNER_DNAMAGE.MM <- emmeans(adj.m.edu.c.par,pairwise~RAEDYRS_factor*RAMEDUC_bin,data=svy.edaa,adjust='mvt')

## Stratify by race/ethnicity
# NH Black
nhb.m.edu.c.par <- svyglm(accweid~RAEDYRS_factor*RAMEDUC_bin +
                          sex +
                          scale(H13ITOT) + 
                          smokestatus +
                          R13BMI,design=subset(svy.edaa,raceth=="NHB"))
summary(nhb.m.edu.c.par)
nhb.m.edu.c.par.tab <- tidy(nhb.m.edu.c.par) %>% mutate(LL=sprintf(confint(nhb.m.edu.c.par)[,1],fmt='%#.3f'),
                                                    UL=sprintf(confint(nhb.m.edu.c.par)[,2],fmt='%#.3f')) %>%
  unite(col=CI,LL:UL, sep = ", ", remove=F)
  
# Hispanic
h.m.edu.c.par <- svyglm(accweid~RAEDYRS_factor*RAMEDUC_bin +
                        sex +
                        scale(H13ITOT) + 
                        smokestatus +
                        R13BMI,design=subset(svy.edaa,raceth=="Hispanic"))
summary(h.m.edu.c.par)
h.m.edu.c.par.tab <- tidy(h.m.edu.c.par) %>% mutate(LL=sprintf(confint(h.m.edu.c.par)[,1],fmt='%#.3f'),
                                                UL=sprintf(confint(h.m.edu.c.par)[,2],fmt='%#.3f')) %>%
  unite(col=CI,LL:UL, sep = ", ", remove=F)
  
#NH White
nhw.m.edu.c.par <- svyglm(accweid~RAEDYRS_factor*RAMEDUC_bin +
                          sex +
                          scale(H13ITOT) + 
                          smokestatus +
                          R13BMI,design=subset(svy.edaa,raceth=="NHW"))
summary(nhw.m.edu.c.par)
nhw.m.edu.c.par.tab <- tidy(nhw.m.edu.c.par) %>% mutate(LL=sprintf(confint(nhw.m.edu.c.par)[,1],fmt='%#.3f'),
                                                    UL=sprintf(confint(nhw.m.edu.c.par)[,2],fmt='%#.3f')) %>%
  unite(col=CI,LL:UL, sep = ", ", remove=F)
  

# Stack Columns 
re.m.edu.c.par.WEIDNER_DNAMAGE<- bind_rows("NH Black"=nhb.m.edu.c.par.tab,"NH Hispanic"=h.m.edu.c.par.tab,"NH White"=nhw.m.edu.c.par.tab,.id='Subgroup')
write.csv(re.m.edu.c.par.WEIDNER_DNAMAGE,file='re.m.edu.c.par.WEIDNER_DNAMAGE.csv')

# Save ANOVA results
sink(file="m.aov.edu.c.WEIDNER_DNAMAGE.txt")
print(aov<-list(anova(m.edu.c.par),anova(adj.m.edu.c.par)))
sink()

sink(file="re.m.aov.edu.c.WEIDNER_DNAMAGE.txt")
print(aov<-list(anova(nhb.m.edu.c.par),anova(h.m.edu.c.par),anova(nhw.m.edu.c.par)))
sink()

## VIDALBRALO_DNAMAGE ----
# Unadjusted
m.edu.c.par <- svyglm(accvidalb~RAEDYRS_factor*RAMEDUC_bin,design=svy.edaa)
summary(m.edu.c.par)
m.edu.c.par.tab <- tidy(m.edu.c.par) %>% mutate(LL=sprintf(confint(m.edu.c.par)[,1],fmt='%#.3f'),
                                            UL=sprintf(confint(m.edu.c.par)[,2],fmt='%#.3f')) %>%
  unite(col=CI,LL:UL, sep = ", ", remove=F)
  

# Adjusted
adj.m.edu.c.par <- svyglm(accvidalb~RAEDYRS_factor*RAMEDUC_bin +
                          raceth + 
                          sex +
                          scale(H13ITOT) + 
                          smokestatus +
                          R13BMI,design=svy.edaa)
summary(adj.m.edu.c.par)
adj.m.edu.c.par.tab <- tidy(adj.m.edu.c.par) %>% mutate(LL=sprintf(confint(adj.m.edu.c.par)[,1],fmt='%#.3f'),
                                                    UL=sprintf(confint(adj.m.edu.c.par)[,2],fmt='%#.3f')) %>%
  unite(col=CI,LL:UL, sep = ", ", remove=F)
  
names(adj.m.edu.c.par.tab) <- gsub("^(.*?)","adj_\\2", names(adj.m.edu.c.par.tab))

# Combine results into one table
m.edu.c.par.VIDALBRALO_DNAMAGE <- merge(m.edu.c.par.tab,adj.m.edu.c.par.tab,by.x='term',by.y='adj_term',all.y=T,sort=F) 
write.csv(m.edu.c.par.VIDALBRALO_DNAMAGE,'m.edu.c.par.VIDALBRALO_DNAMAGE.csv')

## Marginal Means
# Unadjusted
VIDALBRALO_DNAMAGE.MM <- emmeans(m.edu.c.par,pairwise~RAEDYRS_factor*RAMEDUC_bin,data=svy.edaa,adjust='mvt')

# Adjusted
adj.VIDALBRALO_DNAMAGE.MM <- emmeans(adj.m.edu.c.par,pairwise~RAEDYRS_factor*RAMEDUC_bin,data=svy.edaa,adjust='mvt')

## Stratify by race/ethnicity
# NH Black
nhb.m.edu.c.par <- svyglm(accvidalb~RAEDYRS_factor*RAMEDUC_bin +
                          sex +
                          scale(H13ITOT) + 
                          smokestatus +
                          R13BMI,design=subset(svy.edaa,raceth=="NHB"))
summary(nhb.m.edu.c.par)
nhb.m.edu.c.par.tab <- tidy(nhb.m.edu.c.par) %>% mutate(LL=sprintf(confint(nhb.m.edu.c.par)[,1],fmt='%#.3f'),
                                                    UL=sprintf(confint(nhb.m.edu.c.par)[,2],fmt='%#.3f')) %>%
  unite(col=CI,LL:UL, sep = ", ", remove=F)
  
# Hispanic
h.m.edu.c.par <- svyglm(accvidalb~RAEDYRS_factor*RAMEDUC_bin +
                        sex +
                        scale(H13ITOT) + 
                        smokestatus +
                        R13BMI,design=subset(svy.edaa,raceth=="Hispanic"))
summary(h.m.edu.c.par)
h.m.edu.c.par.tab <- tidy(h.m.edu.c.par) %>% mutate(LL=sprintf(confint(h.m.edu.c.par)[,1],fmt='%#.3f'),
                                                UL=sprintf(confint(h.m.edu.c.par)[,2],fmt='%#.3f')) %>%
  unite(col=CI,LL:UL, sep = ", ", remove=F)
  
#NH White
nhw.m.edu.c.par <- svyglm(accvidalb~RAEDYRS_factor*RAMEDUC_bin +
                          sex +
                          scale(H13ITOT) + 
                          smokestatus +
                          R13BMI,design=subset(svy.edaa,raceth=="NHW"))
summary(nhw.m.edu.c.par)
nhw.m.edu.c.par.tab <- tidy(nhw.m.edu.c.par) %>% mutate(LL=sprintf(confint(nhw.m.edu.c.par)[,1],fmt='%#.3f'),
                                                    UL=sprintf(confint(nhw.m.edu.c.par)[,2],fmt='%#.3f')) %>%
  unite(col=CI,LL:UL, sep = ", ", remove=F)
  

# Stack Columns 
re.m.edu.c.par.VIDALBRALO_DNAMAGE<- bind_rows("NH Black"=nhb.m.edu.c.par.tab,"NH Hispanic"=h.m.edu.c.par.tab,"NH White"=nhw.m.edu.c.par.tab,.id='Subgroup')
write.csv(re.m.edu.c.par.VIDALBRALO_DNAMAGE,file='re.m.edu.c.par.VIDALBRALO_DNAMAGE.csv')

# Save ANOVA results
sink(file="m.aov.edu.c.VIDALBRALO_DNAMAGE.txt")
print(aov<-list(anova(m.edu.c.par),anova(adj.m.edu.c.par)))
sink()

sink(file="re.m.aov.edu.c.VIDALBRALO_DNAMAGE.txt")
print(aov<-list(anova(nhb.m.edu.c.par),anova(h.m.edu.c.par),anova(nhw.m.edu.c.par)))
sink()

## YANG_DNAMAGE ----
# Unadjusted
m.edu.c.par <- svyglm(accyang~RAEDYRS_factor*RAMEDUC_bin,design=svy.edaa)
summary(m.edu.c.par)
m.edu.c.par.tab <- tidy(m.edu.c.par) %>% mutate(LL=sprintf(confint(m.edu.c.par)[,1],fmt='%#.3f'),
                                            UL=sprintf(confint(m.edu.c.par)[,2],fmt='%#.3f')) %>%
  unite(col=CI,LL:UL, sep = ", ", remove=F)
  

# Adjusted
adj.m.edu.c.par <- svyglm(accyang~RAEDYRS_factor*RAMEDUC_bin +
                          raceth + 
                          sex +
                          scale(H13ITOT) + 
                          smokestatus +
                          R13BMI,design=svy.edaa)
summary(adj.m.edu.c.par)
adj.m.edu.c.par.tab <- tidy(adj.m.edu.c.par) %>% mutate(LL=sprintf(confint(adj.m.edu.c.par)[,1],fmt='%#.3f'),
                                                    UL=sprintf(confint(adj.m.edu.c.par)[,2],fmt='%#.3f')) %>%
  unite(col=CI,LL:UL, sep = ", ", remove=F)
  
names(adj.m.edu.c.par.tab) <- gsub("^(.*?)","adj_\\2", names(adj.m.edu.c.par.tab))

# Combine results into one table
m.edu.c.par.YANG_DNAMAGE <- merge(m.edu.c.par.tab,adj.m.edu.c.par.tab,by.x='term',by.y='adj_term',all.y=T,sort=F) 
write.csv(m.edu.c.par.YANG_DNAMAGE,'m.edu.c.par.YANG_DNAMAGE.csv')

## Marginal Means
# Unadjusted
YANG_DNAMAGE.MM <- emmeans(m.edu.c.par,pairwise~RAEDYRS_factor*RAMEDUC_bin,data=svy.edaa,adjust='mvt')

# Adjusted
adj.YANG_DNAMAGE.MM <- emmeans(adj.m.edu.c.par,pairwise~RAEDYRS_factor*RAMEDUC_bin,data=svy.edaa,adjust='mvt')

## Stratify by race/ethnicity
# NH Black
nhb.m.edu.c.par <- svyglm(accyang~RAEDYRS_factor*RAMEDUC_bin +
                          sex +
                          scale(H13ITOT) + 
                          smokestatus +
                          R13BMI,design=subset(svy.edaa,raceth=="NHB"))
summary(nhb.m.edu.c.par)
nhb.m.edu.c.par.tab <- tidy(nhb.m.edu.c.par) %>% mutate(LL=sprintf(confint(nhb.m.edu.c.par)[,1],fmt='%#.3f'),
                                                    UL=sprintf(confint(nhb.m.edu.c.par)[,2],fmt='%#.3f')) %>%
  unite(col=CI,LL:UL, sep = ", ", remove=F)
  
# Hispanic
h.m.edu.c.par <- svyglm(accyang~RAEDYRS_factor*RAMEDUC_bin +
                        sex +
                        scale(H13ITOT) + 
                        smokestatus +
                        R13BMI,design=subset(svy.edaa,raceth=="Hispanic"))
summary(h.m.edu.c.par)
h.m.edu.c.par.tab <- tidy(h.m.edu.c.par) %>% mutate(LL=sprintf(confint(h.m.edu.c.par)[,1],fmt='%#.3f'),
                                                UL=sprintf(confint(h.m.edu.c.par)[,2],fmt='%#.3f')) %>%
  unite(col=CI,LL:UL, sep = ", ", remove=F)
  
#NH White
nhw.m.edu.c.par <- svyglm(accyang~RAEDYRS_factor*RAMEDUC_bin +
                          sex +
                          scale(H13ITOT) + 
                          smokestatus +
                          R13BMI,design=subset(svy.edaa,raceth=="NHW"))
summary(nhw.m.edu.c.par)
nhw.m.edu.c.par.tab <- tidy(nhw.m.edu.c.par) %>% mutate(LL=sprintf(confint(nhw.m.edu.c.par)[,1],fmt='%#.3f'),
                                                    UL=sprintf(confint(nhw.m.edu.c.par)[,2],fmt='%#.3f')) %>%
  unite(col=CI,LL:UL, sep = ", ", remove=F)
  

# Stack Columns 
re.m.edu.c.par.YANG_DNAMAGE<- bind_rows("NH Black"=nhb.m.edu.c.par.tab,"NH Hispanic"=h.m.edu.c.par.tab,"NH White"=nhw.m.edu.c.par.tab,.id='Subgroup')
write.csv(re.m.edu.c.par.YANG_DNAMAGE,file='re.m.edu.c.par.YANG_DNAMAGE.csv')

# Save ANOVA results
sink(file="m.aov.edu.c.YANG_DNAMAGE.txt")
print(aov<-list(anova(m.edu.c.par),anova(adj.m.edu.c.par)))
sink()

sink(file="re.m.aov.edu.c.YANG_DNAMAGE.txt")
print(aov<-list(anova(nhb.m.edu.c.par),anova(h.m.edu.c.par),anova(nhw.m.edu.c.par)))
sink()

## ZHANG_DNAMAGE ----
# Unadjusted
m.edu.c.par <- svyglm(acczhang~RAEDYRS_factor*RAMEDUC_bin,design=svy.edaa)
summary(m.edu.c.par)
m.edu.c.par.tab <- tidy(m.edu.c.par) %>% mutate(LL=sprintf(confint(m.edu.c.par)[,1],fmt='%#.3f'),
                                            UL=sprintf(confint(m.edu.c.par)[,2],fmt='%#.3f')) %>%
  unite(col=CI,LL:UL, sep = ", ", remove=F)
  

# Adjusted
adj.m.edu.c.par <- svyglm(acczhang~RAEDYRS_factor*RAMEDUC_bin +
                          raceth + 
                          sex +
                          scale(H13ITOT) + 
                          smokestatus +
                          R13BMI,design=svy.edaa)
summary(adj.m.edu.c.par)
adj.m.edu.c.par.tab <- tidy(adj.m.edu.c.par) %>% mutate(LL=sprintf(confint(adj.m.edu.c.par)[,1],fmt='%#.3f'),
                                                    UL=sprintf(confint(adj.m.edu.c.par)[,2],fmt='%#.3f')) %>%
  unite(col=CI,LL:UL, sep = ", ", remove=F)
  
names(adj.m.edu.c.par.tab) <- gsub("^(.*?)","adj_\\2", names(adj.m.edu.c.par.tab))

# Combine results into one table
m.edu.c.par.ZHANG_DNAMAGE <- merge(m.edu.c.par.tab,adj.m.edu.c.par.tab,by.x='term',by.y='adj_term',all.y=T,sort=F) 
write.csv(m.edu.c.par.ZHANG_DNAMAGE,'m.edu.c.par.ZHANG_DNAMAGE.csv')

## Marginal Means
# Unadjusted
ZHANG_DNAMAGE.MM <- emmeans(m.edu.c.par,pairwise~RAEDYRS_factor*RAMEDUC_bin,data=svy.edaa,adjust='mvt')

# Adjusted
adj.ZHANG_DNAMAGE.MM <- emmeans(adj.m.edu.c.par,pairwise~RAEDYRS_factor*RAMEDUC_bin,data=svy.edaa,adjust='mvt')

## Stratify by race/ethnicity
# NH Black
nhb.m.edu.c.par <- svyglm(acczhang~RAEDYRS_factor*RAMEDUC_bin +
                          sex +
                          scale(H13ITOT) + 
                          smokestatus +
                          R13BMI,design=subset(svy.edaa,raceth=="NHB"))
summary(nhb.m.edu.c.par)
nhb.m.edu.c.par.tab <- tidy(nhb.m.edu.c.par) %>% mutate(LL=sprintf(confint(nhb.m.edu.c.par)[,1],fmt='%#.3f'),
                                                    UL=sprintf(confint(nhb.m.edu.c.par)[,2],fmt='%#.3f')) %>%
  unite(col=CI,LL:UL, sep = ", ", remove=F)
  
# Hispanic
h.m.edu.c.par <- svyglm(acczhang~RAEDYRS_factor*RAMEDUC_bin +
                        sex +
                        scale(H13ITOT) + 
                        smokestatus +
                        R13BMI,design=subset(svy.edaa,raceth=="Hispanic"))
summary(h.m.edu.c.par)
h.m.edu.c.par.tab <- tidy(h.m.edu.c.par) %>% mutate(LL=sprintf(confint(h.m.edu.c.par)[,1],fmt='%#.3f'),
                                                UL=sprintf(confint(h.m.edu.c.par)[,2],fmt='%#.3f')) %>%
  unite(col=CI,LL:UL, sep = ", ", remove=F)
  
#NH White
nhw.m.edu.c.par <- svyglm(acczhang~RAEDYRS_factor*RAMEDUC_bin +
                          sex +
                          scale(H13ITOT) + 
                          smokestatus +
                          R13BMI,design=subset(svy.edaa,raceth=="NHW"))
summary(nhw.m.edu.c.par)
nhw.m.edu.c.par.tab <- tidy(nhw.m.edu.c.par) %>% mutate(LL=sprintf(confint(nhw.m.edu.c.par)[,1],fmt='%#.3f'),
                                                    UL=sprintf(confint(nhw.m.edu.c.par)[,2],fmt='%#.3f')) %>%
  unite(col=CI,LL:UL, sep = ", ", remove=F)
  

# Stack Columns 
re.m.edu.c.par.ZHANG_DNAMAGE<- bind_rows("NH Black"=nhb.m.edu.c.par.tab,"NH Hispanic"=h.m.edu.c.par.tab,"NH White"=nhw.m.edu.c.par.tab,.id='Subgroup')
write.csv(re.m.edu.c.par.ZHANG_DNAMAGE,file='re.m.edu.c.par.ZHANG_DNAMAGE.csv')

# Save ANOVA results
sink(file="m.aov.edu.c.ZHANG_DNAMAGE.txt")
print(aov<-list(anova(m.edu.c.par),anova(adj.m.edu.c.par)))
sink()

sink(file="re.m.aov.edu.c.ZHANG_DNAMAGE.txt")
print(aov<-list(anova(nhb.m.edu.c.par),anova(h.m.edu.c.par),anova(nhw.m.edu.c.par)))
sink()

## BOCKLANDT_DNAMAGE ----
# Unadjusted
m.edu.c.par <- svyglm(accbock~RAEDYRS_factor*RAMEDUC_bin,design=svy.edaa)
summary(m.edu.c.par)
m.edu.c.par.tab <- tidy(m.edu.c.par) %>% mutate(LL=sprintf(confint(m.edu.c.par)[,1],fmt='%#.3f'),
                                            UL=sprintf(confint(m.edu.c.par)[,2],fmt='%#.3f')) %>%
  unite(col=CI,LL:UL, sep = ", ", remove=F)
  

# Adjusted
adj.m.edu.c.par <- svyglm(accbock~RAEDYRS_factor*RAMEDUC_bin +
                          raceth + 
                          sex +
                          scale(H13ITOT) + 
                          smokestatus +
                          R13BMI,design=svy.edaa)
summary(adj.m.edu.c.par)
adj.m.edu.c.par.tab <- tidy(adj.m.edu.c.par) %>% mutate(LL=sprintf(confint(adj.m.edu.c.par)[,1],fmt='%#.3f'),
                                                    UL=sprintf(confint(adj.m.edu.c.par)[,2],fmt='%#.3f')) %>%
  unite(col=CI,LL:UL, sep = ", ", remove=F)
  
names(adj.m.edu.c.par.tab) <- gsub("^(.*?)","adj_\\2", names(adj.m.edu.c.par.tab))

# Combine results into one table
m.edu.c.par.BOCKLANDT_DNAMAGE <- merge(m.edu.c.par.tab,adj.m.edu.c.par.tab,by.x='term',by.y='adj_term',all.y=T,sort=F) 
write.csv(m.edu.c.par.BOCKLANDT_DNAMAGE,'m.edu.c.par.BOCKLANDT_DNAMAGE.csv')

## Marginal Means
# Unadjusted
BOCKLANDT_DNAMAGE.MM <- emmeans(m.edu.c.par,pairwise~RAEDYRS_factor*RAMEDUC_bin,data=svy.edaa,adjust='mvt')

# Adjusted
adj.BOCKLANDT_DNAMAGE.MM <- emmeans(adj.m.edu.c.par,pairwise~RAEDYRS_factor*RAMEDUC_bin,data=svy.edaa,adjust='mvt')

## Stratify by race/ethnicity
# NH Black
nhb.m.edu.c.par <- svyglm(accbock~RAEDYRS_factor*RAMEDUC_bin +
                          sex +
                          scale(H13ITOT) + 
                          smokestatus +
                          R13BMI,design=subset(svy.edaa,raceth=="NHB"))
summary(nhb.m.edu.c.par)
nhb.m.edu.c.par.tab <- tidy(nhb.m.edu.c.par) %>% mutate(LL=sprintf(confint(nhb.m.edu.c.par)[,1],fmt='%#.3f'),
                                                    UL=sprintf(confint(nhb.m.edu.c.par)[,2],fmt='%#.3f')) %>%
  unite(col=CI,LL:UL, sep = ", ", remove=F)
  
# Hispanic
h.m.edu.c.par <- svyglm(accbock~RAEDYRS_factor*RAMEDUC_bin +
                        sex +
                        scale(H13ITOT) + 
                        smokestatus +
                        R13BMI,design=subset(svy.edaa,raceth=="Hispanic"))
summary(h.m.edu.c.par)
h.m.edu.c.par.tab <- tidy(h.m.edu.c.par) %>% mutate(LL=sprintf(confint(h.m.edu.c.par)[,1],fmt='%#.3f'),
                                                UL=sprintf(confint(h.m.edu.c.par)[,2],fmt='%#.3f')) %>%
  unite(col=CI,LL:UL, sep = ", ", remove=F)
  
#NH White
nhw.m.edu.c.par <- svyglm(accbock~RAEDYRS_factor*RAMEDUC_bin +
                          sex +
                          scale(H13ITOT) + 
                          smokestatus +
                          R13BMI,design=subset(svy.edaa,raceth=="NHW"))
summary(nhw.m.edu.c.par)
nhw.m.edu.c.par.tab <- tidy(nhw.m.edu.c.par) %>% mutate(LL=sprintf(confint(nhw.m.edu.c.par)[,1],fmt='%#.3f'),
                                                    UL=sprintf(confint(nhw.m.edu.c.par)[,2],fmt='%#.3f')) %>%
  unite(col=CI,LL:UL, sep = ", ", remove=F)
  

# Stack Columns 
re.m.edu.c.par.BOCKLANDT_DNAMAGE<- bind_rows("NH Black"=nhb.m.edu.c.par.tab,"NH Hispanic"=h.m.edu.c.par.tab,"NH White"=nhw.m.edu.c.par.tab,.id='Subgroup')
write.csv(re.m.edu.c.par.BOCKLANDT_DNAMAGE,file='re.m.edu.c.par.BOCKLANDT_DNAMAGE.csv')

# Save ANOVA results
sink(file="m.aov.edu.c.BOCKLANDT_DNAMAGE.txt")
print(aov<-list(anova(m.edu.c.par),anova(adj.m.edu.c.par)))
sink()

sink(file="re.m.aov.edu.c.BOCKLANDT_DNAMAGE.txt")
print(aov<-list(anova(nhb.m.edu.c.par),anova(h.m.edu.c.par),anova(nhw.m.edu.c.par)))
sink()

## GARAGNANI_DNAMAGE ----
# Unadjusted
m.edu.c.par <- svyglm(accgarag~RAEDYRS_factor*RAMEDUC_bin,design=svy.edaa)
summary(m.edu.c.par)
m.edu.c.par.tab <- tidy(m.edu.c.par) %>% mutate(LL=sprintf(confint(m.edu.c.par)[,1],fmt='%#.3f'),
                                            UL=sprintf(confint(m.edu.c.par)[,2],fmt='%#.3f')) %>%
  unite(col=CI,LL:UL, sep = ", ", remove=F)
  

# Adjusted
adj.m.edu.c.par <- svyglm(accgarag~RAEDYRS_factor*RAMEDUC_bin +
                          raceth + 
                          sex +
                          scale(H13ITOT) + 
                          smokestatus +
                          R13BMI,design=svy.edaa)
summary(adj.m.edu.c.par)
adj.m.edu.c.par.tab <- tidy(adj.m.edu.c.par) %>% mutate(LL=sprintf(confint(adj.m.edu.c.par)[,1],fmt='%#.3f'),
                                                    UL=sprintf(confint(adj.m.edu.c.par)[,2],fmt='%#.3f')) %>%
  unite(col=CI,LL:UL, sep = ", ", remove=F)
  
names(adj.m.edu.c.par.tab) <- gsub("^(.*?)","adj_\\2", names(adj.m.edu.c.par.tab))

# Combine results into one table
m.edu.c.par.GARAGNANI_DNAMAGE <- merge(m.edu.c.par.tab,adj.m.edu.c.par.tab,by.x='term',by.y='adj_term',all.y=T,sort=F) 
write.csv(m.edu.c.par.GARAGNANI_DNAMAGE,'m.edu.c.par.GARAGNANI_DNAMAGE.csv')

## Marginal Means
# Unadjusted
GARAGNANI_DNAMAGE.MM <- emmeans(m.edu.c.par,pairwise~RAEDYRS_factor*RAMEDUC_bin,data=svy.edaa,adjust='mvt')

# Adjusted
adj.GARAGNANI_DNAMAGE.MM <- emmeans(adj.m.edu.c.par,pairwise~RAEDYRS_factor*RAMEDUC_bin,data=svy.edaa,adjust='mvt')

## Stratify by race/ethnicity
# NH Black
nhb.m.edu.c.par <- svyglm(accgarag~RAEDYRS_factor*RAMEDUC_bin +
                          sex +
                          scale(H13ITOT) + 
                          smokestatus +
                          R13BMI,design=subset(svy.edaa,raceth=="NHB"))
summary(nhb.m.edu.c.par)
nhb.m.edu.c.par.tab <- tidy(nhb.m.edu.c.par) %>% mutate(LL=sprintf(confint(nhb.m.edu.c.par)[,1],fmt='%#.3f'),
                                                    UL=sprintf(confint(nhb.m.edu.c.par)[,2],fmt='%#.3f')) %>%
  unite(col=CI,LL:UL, sep = ", ", remove=F)
  
# Hispanic
h.m.edu.c.par <- svyglm(accgarag~RAEDYRS_factor*RAMEDUC_bin +
                        sex +
                        scale(H13ITOT) + 
                        smokestatus +
                        R13BMI,design=subset(svy.edaa,raceth=="Hispanic"))
summary(h.m.edu.c.par)
h.m.edu.c.par.tab <- tidy(h.m.edu.c.par) %>% mutate(LL=sprintf(confint(h.m.edu.c.par)[,1],fmt='%#.3f'),
                                                UL=sprintf(confint(h.m.edu.c.par)[,2],fmt='%#.3f')) %>%
  unite(col=CI,LL:UL, sep = ", ", remove=F)
  
#NH White
nhw.m.edu.c.par <- svyglm(accgarag~RAEDYRS_factor*RAMEDUC_bin +
                          sex +
                          scale(H13ITOT) + 
                          smokestatus +
                          R13BMI,design=subset(svy.edaa,raceth=="NHW"))
summary(nhw.m.edu.c.par)
nhw.m.edu.c.par.tab <- tidy(nhw.m.edu.c.par) %>% mutate(LL=sprintf(confint(nhw.m.edu.c.par)[,1],fmt='%#.3f'),
                                                    UL=sprintf(confint(nhw.m.edu.c.par)[,2],fmt='%#.3f')) %>%
  unite(col=CI,LL:UL, sep = ", ", remove=F)
  

# Stack Columns 
re.m.edu.c.par.GARAGNANI_DNAMAGE<- bind_rows("NH Black"=nhb.m.edu.c.par.tab,"NH Hispanic"=h.m.edu.c.par.tab,"NH White"=nhw.m.edu.c.par.tab,.id='Subgroup')
write.csv(re.m.edu.c.par.GARAGNANI_DNAMAGE,file='re.m.edu.c.par.GARAGNANI_DNAMAGE.csv')

# Save ANOVA results
sink(file="m.aov.edu.c.GARAGNANI_DNAMAGE.txt")
print(aov<-list(anova(m.edu.c.par),anova(adj.m.edu.c.par)))
sink()

sink(file="re.m.aov.edu.c.GARAGNANI_DNAMAGE.txt")
print(aov<-list(anova(nhb.m.edu.c.par),anova(h.m.edu.c.par),anova(nhw.m.edu.c.par)))
sink()

## DNAMGRIMAGE ----
# Unadjusted
m.edu.c.par <- svyglm(accdnam~RAEDYRS_factor*RAMEDUC_bin,design=svy.edaa)
summary(m.edu.c.par)
m.edu.c.par.tab <- tidy(m.edu.c.par) %>% mutate(LL=sprintf(confint(m.edu.c.par)[,1],fmt='%#.3f'),
                                            UL=sprintf(confint(m.edu.c.par)[,2],fmt='%#.3f')) %>%
  unite(col=CI,LL:UL, sep = ", ", remove=F)
  

# Adjusted
adj.m.edu.c.par <- svyglm(accdnam~RAEDYRS_factor*RAMEDUC_bin +
                          raceth + 
                          sex +
                          scale(H13ITOT) + 
                          smokestatus +
                          R13BMI,design=svy.edaa)
summary(adj.m.edu.c.par)
adj.m.edu.c.par.tab <- tidy(adj.m.edu.c.par) %>% mutate(LL=sprintf(confint(adj.m.edu.c.par)[,1],fmt='%#.3f'),
                                                    UL=sprintf(confint(adj.m.edu.c.par)[,2],fmt='%#.3f')) %>%
  unite(col=CI,LL:UL, sep = ", ", remove=F)
  
names(adj.m.edu.c.par.tab) <- gsub("^(.*?)","adj_\\2", names(adj.m.edu.c.par.tab))

# Combine results into one table
m.edu.c.par.DNAMGRIMAGE <- merge(m.edu.c.par.tab,adj.m.edu.c.par.tab,by.x='term',by.y='adj_term',all.y=T,sort=F) 
write.csv(m.edu.c.par.DNAMGRIMAGE,'m.edu.c.par.DNAMGRIMAGE.csv')

## Marginal Means
# Unadjusted
DNAMGRIMAGE.MM <- emmeans(m.edu.c.par,pairwise~RAEDYRS_factor*RAMEDUC_bin,data=svy.edaa,adjust='mvt')

# Adjusted
adj.DNAMGRIMAGE.MM <- emmeans(adj.m.edu.c.par,pairwise~RAEDYRS_factor*RAMEDUC_bin,data=svy.edaa,adjust='mvt')

## Stratify by race/ethnicity
# NH Black
nhb.m.edu.c.par <- svyglm(accdnam~RAEDYRS_factor*RAMEDUC_bin +
                          sex +
                          scale(H13ITOT) + 
                          smokestatus +
                          R13BMI,design=subset(svy.edaa,raceth=="NHB"))
summary(nhb.m.edu.c.par)
nhb.m.edu.c.par.tab <- tidy(nhb.m.edu.c.par) %>% mutate(LL=sprintf(confint(nhb.m.edu.c.par)[,1],fmt='%#.3f'),
                                                    UL=sprintf(confint(nhb.m.edu.c.par)[,2],fmt='%#.3f')) %>%
  unite(col=CI,LL:UL, sep = ", ", remove=F)
  
# Hispanic
h.m.edu.c.par <- svyglm(accdnam~RAEDYRS_factor*RAMEDUC_bin +
                        sex +
                        scale(H13ITOT) + 
                        smokestatus +
                        R13BMI,design=subset(svy.edaa,raceth=="Hispanic"))
summary(h.m.edu.c.par)
h.m.edu.c.par.tab <- tidy(h.m.edu.c.par) %>% mutate(LL=sprintf(confint(h.m.edu.c.par)[,1],fmt='%#.3f'),
                                                UL=sprintf(confint(h.m.edu.c.par)[,2],fmt='%#.3f')) %>%
  unite(col=CI,LL:UL, sep = ", ", remove=F)
  
#NH White
nhw.m.edu.c.par <- svyglm(accdnam~RAEDYRS_factor*RAMEDUC_bin +
                          sex +
                          scale(H13ITOT) + 
                          smokestatus +
                          R13BMI,design=subset(svy.edaa,raceth=="NHW"))
summary(nhw.m.edu.c.par)
nhw.m.edu.c.par.tab <- tidy(nhw.m.edu.c.par) %>% mutate(LL=sprintf(confint(nhw.m.edu.c.par)[,1],fmt='%#.3f'),
                                                    UL=sprintf(confint(nhw.m.edu.c.par)[,2],fmt='%#.3f')) %>%
  unite(col=CI,LL:UL, sep = ", ", remove=F)
  

# Stack Columns 
re.m.edu.c.par.DNAMGRIMAGE<- bind_rows("NH Black"=nhb.m.edu.c.par.tab,"NH Hispanic"=h.m.edu.c.par.tab,"NH White"=nhw.m.edu.c.par.tab,.id='Subgroup')
write.csv(re.m.edu.c.par.DNAMGRIMAGE,file='re.m.edu.c.par.DNAMGRIMAGE.csv')

# Save ANOVA results
sink(file="m.aov.edu.c.DNAMGRIMAGE.txt")
print(aov<-list(anova(m.edu.c.par),anova(adj.m.edu.c.par)))
sink()

sink(file="re.m.aov.edu.c.DNAMGRIMAGE.txt")
print(aov<-list(anova(nhb.m.edu.c.par),anova(h.m.edu.c.par),anova(nhw.m.edu.c.par)))
sink()

## MPOA ----
# Unadjusted
m.edu.c.par <- svyglm(accmpoa~RAEDYRS_factor*RAMEDUC_bin,design=svy.edaa)
summary(m.edu.c.par)
m.edu.c.par.tab <- tidy(m.edu.c.par) %>% mutate(LL=sprintf(confint(m.edu.c.par)[,1],fmt='%#.3f'),
                                            UL=sprintf(confint(m.edu.c.par)[,2],fmt='%#.3f')) %>%
  unite(col=CI,LL:UL, sep = ", ", remove=F)
  

# Adjusted
adj.m.edu.c.par <- svyglm(accmpoa~RAEDYRS_factor*RAMEDUC_bin +
                          raceth + 
                          sex +
                          scale(H13ITOT) + 
                          smokestatus +
                          R13BMI,design=svy.edaa)
summary(adj.m.edu.c.par)
adj.m.edu.c.par.tab <- tidy(adj.m.edu.c.par) %>% mutate(LL=sprintf(confint(adj.m.edu.c.par)[,1],fmt='%#.3f'),
                                                    UL=sprintf(confint(adj.m.edu.c.par)[,2],fmt='%#.3f')) %>%
  unite(col=CI,LL:UL, sep = ", ", remove=F)
  
names(adj.m.edu.c.par.tab) <- gsub("^(.*?)","adj_\\2", names(adj.m.edu.c.par.tab))

# Combine results into one table
m.edu.c.par.MPOA<- merge(m.edu.c.par.tab,adj.m.edu.c.par.tab,by.x='term',by.y='adj_term',all.y=T,sort=F) 
write.csv(m.edu.c.par.MPOA,'m.edu.c.par.MPOA.csv')

## Marginal Means
# Unadjusted
MPOA.MM <- emmeans(m.edu.c.par,pairwise~RAEDYRS_factor*RAMEDUC_bin,data=svy.edaa,adjust='mvt')

# Adjusted
adj.MPOA.MM <- emmeans(adj.m.edu.c.par,pairwise~RAEDYRS_factor*RAMEDUC_bin,data=svy.edaa,adjust='mvt')

## Stratify by race/ethnicity
# NH Black
nhb.m.edu.c.par <- svyglm(accmpoa~RAEDYRS_factor*RAMEDUC_bin +
                          sex +
                          scale(H13ITOT) + 
                          smokestatus +
                          R13BMI,design=subset(svy.edaa,raceth=="NHB"))
summary(nhb.m.edu.c.par)
nhb.m.edu.c.par.tab <- tidy(nhb.m.edu.c.par) %>% mutate(LL=sprintf(confint(nhb.m.edu.c.par)[,1],fmt='%#.3f'),
                                                    UL=sprintf(confint(nhb.m.edu.c.par)[,2],fmt='%#.3f')) %>%
  unite(col=CI,LL:UL, sep = ", ", remove=F)
  
# Hispanic
h.m.edu.c.par <- svyglm(accmpoa~RAEDYRS_factor*RAMEDUC_bin +
                        sex +
                        scale(H13ITOT) + 
                        smokestatus +
                        R13BMI,design=subset(svy.edaa,raceth=="Hispanic"))
summary(h.m.edu.c.par)
h.m.edu.c.par.tab <- tidy(h.m.edu.c.par) %>% mutate(LL=sprintf(confint(h.m.edu.c.par)[,1],fmt='%#.3f'),
                                                UL=sprintf(confint(h.m.edu.c.par)[,2],fmt='%#.3f')) %>%
  unite(col=CI,LL:UL, sep = ", ", remove=F)
  
#NH White
nhw.m.edu.c.par <- svyglm(accmpoa~RAEDYRS_factor*RAMEDUC_bin +
                          sex +
                          scale(H13ITOT) + 
                          smokestatus +
                          R13BMI,design=subset(svy.edaa,raceth=="NHW"))
summary(nhw.m.edu.c.par)
nhw.m.edu.c.par.tab <- tidy(nhw.m.edu.c.par) %>% mutate(LL=sprintf(confint(nhw.m.edu.c.par)[,1],fmt='%#.3f'),
                                                    UL=sprintf(confint(nhw.m.edu.c.par)[,2],fmt='%#.3f')) %>%
  unite(col=CI,LL:UL, sep = ", ", remove=F)
  

# Stack Columns 
re.m.edu.c.par.MPOA<- bind_rows("NH Black"=nhb.m.edu.c.par.tab,"NH Hispanic"=h.m.edu.c.par.tab,"NH White"=nhw.m.edu.c.par.tab,.id='Subgroup')
write.csv(re.m.edu.c.par.MPOA,file='re.m.edu.c.par.MPOA.csv')

# Save ANOVA results
sink(file="m.aov.edu.c.MPOA.txt")
print(aov<-list(anova(m.edu.c.par),anova(adj.m.edu.c.par)))
sink()

sink(file="re.m.aov.edu.c.MPOA.txt")
print(aov<-list(anova(nhb.m.edu.c.par),anova(h.m.edu.c.par),anova(nhw.m.edu.c.par)))
sink()

## Save marginal means ----
save(HORVATH_DNAMAGE.MM,adj.HORVATH_DNAMAGE.MM,HANNUM_DNAMAGE.MM,adj.HANNUM_DNAMAGE.MM,
     LEVINE_DNAMAGE.MM,adj.LEVINE_DNAMAGE.MM,HORVATHSKIN_DNAMAGE.MM,adj.HORVATHSKIN_DNAMAGE.MM,
     LIN_DNAMAGE.MM,adj.LIN_DNAMAGE.MM,WEIDNER_DNAMAGE.MM,adj.WEIDNER_DNAMAGE.MM,
     VIDALBRALO_DNAMAGE.MM,adj.VIDALBRALO_DNAMAGE.MM,YANG_DNAMAGE.MM,adj.YANG_DNAMAGE.MM,
     ZHANG_DNAMAGE.MM,adj.ZHANG_DNAMAGE.MM,BOCKLANDT_DNAMAGE.MM,adj.BOCKLANDT_DNAMAGE.MM,
     GARAGNANI_DNAMAGE.MM,adj.GARAGNANI_DNAMAGE.MM,DNAMGRIMAGE.MM,adj.DNAMGRIMAGE.MM,
     MPOA.MM,adj.MPOA.MM,file='Mother_MMs.RData')

## Marginal Means Plots ----
# ZHANG_DNAMAGE
zhang.mm.plotdat<-plot(adj.ZHANG_DNAMAGE.MM$emmeans,comparisons=T,adjust='mvt',CIs=T,plotit=F)

zhang.mm.plot<-ggplot(zhang.mm.plotdat,(aes(x=the.emmean,y=factor(par_edu_bin,levels=c("No Degree","High School or More")),
                 shape=factor(RAEDYRS_factor,levels=c("No Degree","High School or GED","Associates","Bachelors or Higher")))))+
  geom_vline(xintercept=0.00,lty=2)+
  geom_linerange(aes(xmin=ifelse(is.na(lcmpl),the.emmean,lcmpl),xmax=ifelse(is.na(rcmpl),the.emmean,rcmpl)),position=position_dodge(width=.5),show.legend = F,size=5,color='lightgrey')+
  geom_point(aes(shape=factor(RAEDYRS_factor,levels=c("No Degree","High School or GED","Associates","Bachelors or Higher"))),position=position_dodge(width=.5),size=3)+
  geom_errorbar(aes(xmin=lower.CL,xmax=upper.CL),width=.25,position=position_dodge(width=.5))+
  scale_shape_manual(values=c(18, 17, 16, 15))+
  scale_x_continuous(labels = function(x) format(x, nsmall = 2),limits=c(-0.11,0.30),breaks=c(-0.10,0.00,0.10,0.20,0.30))+
  labs(x="Adjusted Maringal Mean",y='Parental Education',shape='Personal Education',title='Zhang')+
  theme_apa(legend.use.title=T,legend.pos='bottom') + theme(plot.title = element_text(hjust = 0.5))
zhang.mm.plot

# LEVINE_DNAMAGE
levine.mm.plotdat<-plot(adj.LEVINE_DNAMAGE.MM$emmeans,comparisons=T,adjust='mvt',CIs=T,plotit=F)

levine.mm.plot<-ggplot(levine.mm.plotdat,(aes(x=the.emmean,y=factor(par_edu_bin,levels=c("No Degree","High School or More")),
                 shape=factor(RAEDYRS_factor,levels=c("No Degree","High School or GED","Associates","Bachelors or Higher")))))+
  geom_vline(xintercept=0.00,lty=2)+
  geom_linerange(aes(xmin=ifelse(is.na(lcmpl),the.emmean,lcmpl),xmax=ifelse(is.na(rcmpl),the.emmean,rcmpl)),position=position_dodge(width=.5),show.legend = F,size=5,color='lightgrey')+
  geom_point(aes(shape=factor(RAEDYRS_factor,levels=c("No Degree","High School or GED","Associates","Bachelors or Higher"))),position=position_dodge(width=.5),size=3)+
  geom_errorbar(aes(xmin=lower.CL,xmax=upper.CL),width=.25,position=position_dodge(width=.5))+
  scale_shape_manual(values=c(18, 17, 16, 15))+
  scale_x_continuous(labels = function(x) format(x, nsmall = 2))+
  labs(x="Adjusted Maringal Mean",y='Parental Education',shape='Personal Education',title='Levine-PhenoAge')+
  theme_apa(legend.use.title=T,legend.pos='bottom') + theme(plot.title = element_text(hjust = 0.5))
levine.mm.plot

# DNAMGRIMAGE
grimage.mm.plotdat<-plot(adj.DNAMGRIMAGE.MM$emmeans,comparisons=T,adjust='mvt',CIs=T,plotit=F)

grimage.mm.plot<-ggplot(grimage.mm.plotdat,(aes(x=the.emmean,y=factor(par_edu_bin,levels=c("No Degree","High School or More")),
                 shape=factor(RAEDYRS_factor,levels=c("No Degree","High School or GED","Associates","Bachelors or Higher")))))+
  geom_vline(xintercept=0.00,lty=2)+
  geom_linerange(aes(xmin=ifelse(is.na(lcmpl),the.emmean,lcmpl),xmax=ifelse(is.na(rcmpl),the.emmean,rcmpl)),position=position_dodge(width=.5),show.legend = F,size=5,color='lightgrey')+
  geom_point(aes(shape=factor(RAEDYRS_factor,levels=c("No Degree","High School or GED","Associates","Bachelors or Higher"))),position=position_dodge(width=.5),size=3)+
  geom_errorbar(aes(xmin=lower.CL,xmax=upper.CL),width=.25,position=position_dodge(width=.5))+
  scale_shape_manual(values=c(18, 17, 16, 15))+
  scale_x_continuous(labels = function(x) format(x, nsmall = 2))+
  labs(x="Adjusted Maringal Mean",y='Parental Education',shape='Personal Education',title='Lu-GrimAge')+
  theme_apa(legend.use.title=T,legend.pos='bottom') + theme(plot.title = element_text(hjust = 0.5))
grimage.mm.plot

# MPOA
mpoa.mm.plotdat<-plot(adj.MPOA.MM$emmeans,comparisons=T,adjust='mvt',CIs=T,plotit=F)

mpoa.mm.plot<-ggplot(mpoa.mm.plotdat,(aes(x=the.emmean,y=factor(par_edu_bin,levels=c("No Degree","High School or More")),
                 shape=factor(RAEDYRS_factor,levels=c("No Degree","High School or GED","Associates","Bachelors or Higher")))))+
  geom_vline(xintercept=0.00,lty=2)+
  geom_linerange(aes(xmin=ifelse(is.na(lcmpl),the.emmean,lcmpl),xmax=ifelse(is.na(rcmpl),the.emmean,rcmpl)),position=position_dodge(width=.5),show.legend = F,size=5,color='lightgrey')+
  geom_point(aes(shape=factor(RAEDYRS_factor,levels=c("No Degree","High School or GED","Associates","Bachelors or Higher"))),position=position_dodge(width=.5),size=3)+
  geom_errorbar(aes(xmin=lower.CL,xmax=upper.CL),width=.25,position=position_dodge(width=.5))+
  scale_shape_manual(values=c(18, 17, 16, 15))+
  scale_x_continuous(labels = function(x) format(x, nsmall = 2),limits=c(0,.08),breaks=c(0.0,0.02,0.04,0.06,0.08))+
  labs(x="Adjusted Maringal Mean",y='Parental Education',shape='Personal Education',title='DunedinPoAm38')+
  theme_apa(legend.use.title=T,legend.pos='bottom') + theme(plot.title = element_text(hjust = 0.5))
mpoa.mm.plot

save(zhang.mm.plot,levine.mm.plot,grimage.mm.plot,mpoa.mm.plot,file='maringalmeansplot.RData')
ggpubr::ggarrange(zhang.mm.plot,levine.mm.plot,grimage.mm.plot,mpoa.mm.plot,
                  ncol=2,nrow=2,common.legend=T,legend="right")
## Descriptive Information (Weighted) ----
# Continuous variables (means and SEs)
ddat.wcont <- svy.edaa %>% summarise(
                           weighted_tot=survey_total(),
                           agem=survey_mean(R13AGEY_E,vartype='se'),
                           edum=survey_mean(RAEDYRS,vartype='se',na.rm=T),
                           pedum=survey_mean(par_edu_yr,vartype='se',na.rm=T),
                           medum=survey_mean(RAMEDUC,vartype='se',na.rm=T),
                           fedum=survey_mean(RAFEDUC,vartype='se',na.rm=T),
                           incm=survey_median(H13ITOT,vartype='se',na.rm=T),
                           bmim=survey_mean(R13BMI,vartype='se',na.rm=T),
                           acchorv.m=survey_mean(acchorv,vartype = 'se'),
                           acchannum.m=survey_mean(acchannum,vartype = 'se'),
                           acclevine.m=survey_mean(acclevine,vartype = 'se'),
                           acchorvsk.m=survey_mean(acchorvsk,vartype = 'se'),
                           acclin.m=survey_mean(acclin,vartype = 'se'),
                           accweid.m=survey_mean(accweid,vartype = 'se'),
                           accvidalb.m=survey_mean(accvidalb,vartype = 'se'),
                           accyang.m=survey_mean(accyang,vartype = 'se'),
                           acczhang.m=survey_mean(acczhang,vartype = 'se'),
                           accbock.m=survey_mean(accbock,vartype = 'se'),
                           accgarag.m=survey_mean(accgarag,vartype = 'se'),
                           accdnam.m=survey_mean(accdnam,vartype = 'se'),
                           accmpoa.m=survey_mean(accmpoa,vartype = 'se'))
view(ddat.wcont)
write.csv(ddat.wcont,'descriptive_c.csv')

# Categorical variables (Frequncy [%] and SEs)
sink('descriptive_cat.txt')
print(list(
# Females and Males
dsex=svy.edaa %>% group_by(sex) %>% summarise(weighted_tot=survey_total(),pct=survey_prop(vartype = "se")),

# Race/Ethnicity
dre=svy.edaa %>% group_by(raceth) %>% summarise(weighted_tot=survey_total(),pct=survey_prop(vartype = "se")),

# Educational attainment
dedu=svy.edaa %>% group_by(RAEDYRS_factor) %>% summarise(weighted_tot=survey_total(),pct=survey_prop(vartype = "se")),
dpedu=svy.edaa %>% group_by(par_edu_bin) %>% summarise(weighted_tot=survey_total(),pct=survey_prop(vartype = "se")),
dmedu=svy.edaa %>% group_by(RAMEDUC_bin) %>% summarise(weighted_tot=survey_total(),pct=survey_prop(vartype = "se")),
dfedu=svy.edaa %>% group_by(RAFEDUC_bin) %>% summarise(weighted_tot=survey_total(),pct=survey_prop(vartype = "se")),

dedubypedu=svytable(~RAEDYRS_factor+par_edu_bin,design=svy.edaa),
dedupeduchisq=svychisq(~RAEDYRS_factor+par_edu_bin,design=svy.edaa),
coredupedu=jtools::svycor(~RAEDYRS+par_edu_yr,svy.edaa,na.rm=T,sig.stats=T)[c('cors','p.values')],
coredumedu=jtools::svycor(~RAEDYRS+RAMEDUC,svy.edaa,na.rm=T,sig.stats=T)[c('cors','p.values')],
coredufedu=jtools::svycor(~RAEDYRS+RAFEDUC,svy.edaa,na.rm=T,sig.stats=T)[c('cors','p.values')],

# Smoking status
dsmoke=svy.edaa %>% group_by(smokestatus) %>% summarise(weighted_tot=survey_total(),pct=survey_prop(vartype = "se"))))
sink()
