# Packages
library(tidyverse)
library(survey)
library(srvyr)
library(broom)
library(emmeans)
library(jtools)

# Load Data ----
## Clean data from main analysis ----
edaa <- readRDS('pared_epi_analysis.RDS')
## Childhood and family health data ----
hrs_childhealth <- asciiSetupReader::read_ascii_setup(data="AGGCHLDFH2016A_R.da",
                                                      setup_file = "AGGCHLDFH2016A_R.sas",
                                                      use_value_labels = F,
                                                      use_clean_names = F,
                                                      coerce_numeric = F)

# Merge data ----
eda_ch <- hrs_childhealth %>% 
  select(HHID, PN, RTHLTHCH, DEPRESS, CHDISABL, CHLEARN) %>% 
  mutate(RAHHIDPN = paste(HHID,PN,sep = "")) %>%
  right_join(edaa, by = 'RAHHIDPN') %>% 
  select(RAHHIDPN, everything(),-c(HHID,PN))

# Recode ----
eda_ch <- eda_ch %>% 
  mutate(
    RTHLTHCH = as.numeric(recode(RTHLTHCH, '5' = '1', '4' = '2', '3' = '3', '2' = '4', '1' = '5')),
    DEPRESS = recode(DEPRESS, '1' = '1', '5' = '0', '8' = NA_character_),
    CHDISABL = recode(CHDISABL, '1' = '1', '5' = '0', '8' = NA_character_),
    CHLEARN = recode(CHLEARN, '1' = '1', '5' = '0', '8' = NA_character_),
  )

# Create Survey Object ----
svy.edaa <- eda_ch %>% as_survey_design(ids=RAEHSAMP,
                                      strata=RAESTRAT,
                                      weights=VBSI16WGTRA,nest=T)

# Calculate Age Accelerating Variables ----
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
# Minimally adjusted
edu.yr.par <- svyglm(acchorv~edu.c*pedu.c +
                       raceth +
                       sex,
                     design=svy.edaa)
summary(edu.yr.par)
edu.yr.par.tab <- tidy(edu.yr.par) %>% 
  mutate(
    LL=sprintf(confint(edu.yr.par)[,1],fmt='%#.3f'),
    UL=sprintf(confint(edu.yr.par)[,2],fmt='%#.3f')) %>%
    unite(col=CI,LL:UL, sep = ", ", remove=F) %>% 
    filter(term %in% c('(Intercept)', 'edu.c','pedu.c','edu.c:pedu.c'))

# Adjusted with Childhood Health Indicators
adj.edu.yr.par <- svyglm(acchorv~edu.c*pedu.c +
                                 raceth + 
                                 sex +
                                 scale(H13ITOT) + 
                                 smokestatus +
                                 R13BMI +
                                 RTHLTHCH +
                                 DEPRESS +
                                 CHDISABL +
                                 CHLEARN,design=svy.edaa)
summary(adj.edu.yr.par)
adj.edu.yr.par.tab <- tidy(adj.edu.yr.par) %>% mutate(LL=sprintf(confint(adj.edu.yr.par)[,1],fmt='%#.3f'),
                                                      UL=sprintf(confint(adj.edu.yr.par)[,2],fmt='%#.3f')) %>%
                                                      unite(col=CI,LL:UL, sep = ", ", remove=F) %>% 
      filter(term %in% c('(Intercept)', 'edu.c','pedu.c','edu.c:pedu.c'))

names(adj.edu.yr.par.tab) <- gsub("^(.*?)","adj_\\2", names(adj.edu.yr.par.tab))

# Combine results into one table
edu.yr.par.HORVATH_DNAMAGE <- merge(edu.yr.par.tab,adj.edu.yr.par.tab,by.x='term',by.y='adj_term',all.y=T,sort=F) 
write.csv(edu.yr.par.HORVATH_DNAMAGE,'ph_edu.yr.par.HORVATH_DNAMAGE.csv')


## HANNUM_DNAMAGE ----
# Minimally adjusted
edu.yr.par <- svyglm(acchannum~edu.c*pedu.c +
                       raceth +
                       sex,
                     design=svy.edaa)
summary(edu.yr.par)
edu.yr.par.tab <- tidy(edu.yr.par) %>% 
  mutate(
    LL=sprintf(confint(edu.yr.par)[,1],fmt='%#.3f'),
    UL=sprintf(confint(edu.yr.par)[,2],fmt='%#.3f')) %>%
    unite(col=CI,LL:UL, sep = ", ", remove=F) %>% 
    filter(term %in% c('(Intercept)', 'edu.c','pedu.c','edu.c:pedu.c'))

# Adjusted with Childhood Health Indicators
adj.edu.yr.par <- svyglm(acchannum~edu.c*pedu.c +
                                 raceth + 
                                 sex +
                                 scale(H13ITOT) + 
                                 smokestatus +
                                 R13BMI +
                                 RTHLTHCH +
                                 DEPRESS +
                                 CHDISABL +
                                 CHLEARN,design=svy.edaa)
summary(adj.edu.yr.par)
adj.edu.yr.par.tab <- tidy(adj.edu.yr.par) %>% mutate(LL=sprintf(confint(adj.edu.yr.par)[,1],fmt='%#.3f'),
                                                      UL=sprintf(confint(adj.edu.yr.par)[,2],fmt='%#.3f')) %>%
                                                      unite(col=CI,LL:UL, sep = ", ", remove=F) %>% 
      filter(term %in% c('(Intercept)', 'edu.c','pedu.c','edu.c:pedu.c'))

names(adj.edu.yr.par.tab) <- gsub("^(.*?)","adj_\\2", names(adj.edu.yr.par.tab))

# Combine results into one table
edu.yr.par.HANNUM_DNAMAGE <- merge(edu.yr.par.tab,adj.edu.yr.par.tab,by.x='term',by.y='adj_term',all.y=T,sort=F) 
write.csv(edu.yr.par.HANNUM_DNAMAGE,'ph_edu.yr.par.HANNUM_DNAMAGE.csv')

## LEVINE_DNAMAGE ----
# Minimally adjusted
edu.yr.par <- svyglm(acclevine~edu.c*pedu.c +
                       raceth +
                       sex,
                     design=svy.edaa)
summary(edu.yr.par)
edu.yr.par.tab <- tidy(edu.yr.par) %>% 
  mutate(
    LL=sprintf(confint(edu.yr.par)[,1],fmt='%#.3f'),
    UL=sprintf(confint(edu.yr.par)[,2],fmt='%#.3f')) %>%
    unite(col=CI,LL:UL, sep = ", ", remove=F) %>% 
    filter(term %in% c('(Intercept)', 'edu.c','pedu.c','edu.c:pedu.c'))

# Adjusted with Childhood Health Indicators
adj.edu.yr.par <- svyglm(acclevine~edu.c*pedu.c +
                                 raceth + 
                                 sex +
                                 scale(H13ITOT) + 
                                 smokestatus +
                                 R13BMI +
                                 RTHLTHCH +
                                 DEPRESS +
                                 CHDISABL +
                                 CHLEARN,design=svy.edaa)
summary(adj.edu.yr.par)
adj.edu.yr.par.tab <- tidy(adj.edu.yr.par) %>% mutate(LL=sprintf(confint(adj.edu.yr.par)[,1],fmt='%#.3f'),
                                                      UL=sprintf(confint(adj.edu.yr.par)[,2],fmt='%#.3f')) %>%
                                                      unite(col=CI,LL:UL, sep = ", ", remove=F) %>% 
      filter(term %in% c('(Intercept)', 'edu.c','pedu.c','edu.c:pedu.c'))

names(adj.edu.yr.par.tab) <- gsub("^(.*?)","adj_\\2", names(adj.edu.yr.par.tab))

# Combine results into one table
edu.yr.par.LEVINE_DNAMAGE <- merge(edu.yr.par.tab,adj.edu.yr.par.tab,by.x='term',by.y='adj_term',all.y=T,sort=F) 
write.csv(edu.yr.par.LEVINE_DNAMAGE,'ph_edu.yr.par.LEVINE_DNAMAGE.csv')

## HORVATHSKIN_DNAMAGE ----
# Minimally adjusted
edu.yr.par <- svyglm(acchorvsk~edu.c*pedu.c +
                       raceth +
                       sex,
                     design=svy.edaa)
summary(edu.yr.par)
edu.yr.par.tab <- tidy(edu.yr.par) %>% 
  mutate(
    LL=sprintf(confint(edu.yr.par)[,1],fmt='%#.3f'),
    UL=sprintf(confint(edu.yr.par)[,2],fmt='%#.3f')) %>%
    unite(col=CI,LL:UL, sep = ", ", remove=F) %>% 
    filter(term %in% c('(Intercept)', 'edu.c','pedu.c','edu.c:pedu.c'))

# Adjusted with Childhood Health Indicators
adj.edu.yr.par <- svyglm(acchorvsk~edu.c*pedu.c +
                                 raceth + 
                                 sex +
                                 scale(H13ITOT) + 
                                 smokestatus +
                                 R13BMI +
                                 RTHLTHCH +
                                 DEPRESS +
                                 CHDISABL +
                                 CHLEARN,design=svy.edaa)
summary(adj.edu.yr.par)
adj.edu.yr.par.tab <- tidy(adj.edu.yr.par) %>% mutate(LL=sprintf(confint(adj.edu.yr.par)[,1],fmt='%#.3f'),
                                                      UL=sprintf(confint(adj.edu.yr.par)[,2],fmt='%#.3f')) %>%
                                                      unite(col=CI,LL:UL, sep = ", ", remove=F) %>% 
      filter(term %in% c('(Intercept)', 'edu.c','pedu.c','edu.c:pedu.c'))

names(adj.edu.yr.par.tab) <- gsub("^(.*?)","adj_\\2", names(adj.edu.yr.par.tab))

# Combine results into one table
edu.yr.par.HORVATHSKIN_DNAMAGE<- merge(edu.yr.par.tab,adj.edu.yr.par.tab,by.x='term',by.y='adj_term',all.y=T,sort=F) 
write.csv(edu.yr.par.HORVATHSKIN_DNAMAGE,'ph_edu.yr.par.HORVATHSKIN_DNAMAGE.csv')

## LIN_DNAMAGE ----
# Minimally adjusted
edu.yr.par <- svyglm(acclin~edu.c*pedu.c +
                       raceth +
                       sex,
                     design=svy.edaa)
summary(edu.yr.par)
edu.yr.par.tab <- tidy(edu.yr.par) %>% 
  mutate(
    LL=sprintf(confint(edu.yr.par)[,1],fmt='%#.3f'),
    UL=sprintf(confint(edu.yr.par)[,2],fmt='%#.3f')) %>%
    unite(col=CI,LL:UL, sep = ", ", remove=F) %>% 
    filter(term %in% c('(Intercept)', 'edu.c','pedu.c','edu.c:pedu.c'))

# Adjusted with Childhood Health Indicators
adj.edu.yr.par <- svyglm(acclin~edu.c*pedu.c +
                                 raceth + 
                                 sex +
                                 scale(H13ITOT) + 
                                 smokestatus +
                                 R13BMI +
                                 RTHLTHCH +
                                 DEPRESS +
                                 CHDISABL +
                                 CHLEARN,design=svy.edaa)
summary(adj.edu.yr.par)
adj.edu.yr.par.tab <- tidy(adj.edu.yr.par) %>% mutate(LL=sprintf(confint(adj.edu.yr.par)[,1],fmt='%#.3f'),
                                                      UL=sprintf(confint(adj.edu.yr.par)[,2],fmt='%#.3f')) %>%
                                                      unite(col=CI,LL:UL, sep = ", ", remove=F) %>% 
      filter(term %in% c('(Intercept)', 'edu.c','pedu.c','edu.c:pedu.c'))

names(adj.edu.yr.par.tab) <- gsub("^(.*?)","adj_\\2", names(adj.edu.yr.par.tab))

# Combine results into one table
edu.yr.par.LIN_DNAMAGE <- merge(edu.yr.par.tab,adj.edu.yr.par.tab,by.x='term',by.y='adj_term',all.y=T,sort=F) 
write.csv(edu.yr.par.LIN_DNAMAGE,'ph_edu.yr.par.LIN_DNAMAGE.csv')

## WEIDNER_DNAMAGE ----
# Minimally adjusted
edu.yr.par <- svyglm(accweid~edu.c*pedu.c +
                       raceth +
                       sex,
                     design=svy.edaa)
summary(edu.yr.par)
edu.yr.par.tab <- tidy(edu.yr.par) %>% 
  mutate(
    LL=sprintf(confint(edu.yr.par)[,1],fmt='%#.3f'),
    UL=sprintf(confint(edu.yr.par)[,2],fmt='%#.3f')) %>%
    unite(col=CI,LL:UL, sep = ", ", remove=F) %>% 
    filter(term %in% c('(Intercept)', 'edu.c','pedu.c','edu.c:pedu.c'))

# Adjusted with Childhood Health Indicators
adj.edu.yr.par <- svyglm(accweid~edu.c*pedu.c +
                                 raceth + 
                                 sex +
                                 scale(H13ITOT) + 
                                 smokestatus +
                                 R13BMI +
                                 RTHLTHCH +
                                 DEPRESS +
                                 CHDISABL +
                                 CHLEARN,design=svy.edaa)
summary(adj.edu.yr.par)
adj.edu.yr.par.tab <- tidy(adj.edu.yr.par) %>% mutate(LL=sprintf(confint(adj.edu.yr.par)[,1],fmt='%#.3f'),
                                                      UL=sprintf(confint(adj.edu.yr.par)[,2],fmt='%#.3f')) %>%
                                                      unite(col=CI,LL:UL, sep = ", ", remove=F) %>% 
      filter(term %in% c('(Intercept)', 'edu.c','pedu.c','edu.c:pedu.c'))

names(adj.edu.yr.par.tab) <- gsub("^(.*?)","adj_\\2", names(adj.edu.yr.par.tab))

# Combine results into one table
edu.yr.par.WEIDNER_DNAMAGE <- merge(edu.yr.par.tab,adj.edu.yr.par.tab,by.x='term',by.y='adj_term',all.y=T,sort=F) 
write.csv(edu.yr.par.WEIDNER_DNAMAGE,'ph_edu.yr.par.WEIDNER_DNAMAGE.csv')

## VIDALBRALO_DNAMAGE ----
# Minimally adjusted
edu.yr.par <- svyglm(accvidalb~edu.c*pedu.c +
                       raceth +
                       sex,
                     design=svy.edaa)
summary(edu.yr.par)
edu.yr.par.tab <- tidy(edu.yr.par) %>% 
  mutate(
    LL=sprintf(confint(edu.yr.par)[,1],fmt='%#.3f'),
    UL=sprintf(confint(edu.yr.par)[,2],fmt='%#.3f')) %>%
    unite(col=CI,LL:UL, sep = ", ", remove=F) %>% 
    filter(term %in% c('(Intercept)', 'edu.c','pedu.c','edu.c:pedu.c'))

# Adjusted with Childhood Health Indicators
adj.edu.yr.par <- svyglm(accvidalb~edu.c*pedu.c +
                                 raceth + 
                                 sex +
                                 scale(H13ITOT) + 
                                 smokestatus +
                                 R13BMI +
                                 RTHLTHCH +
                                 DEPRESS +
                                 CHDISABL +
                                 CHLEARN,design=svy.edaa)
summary(adj.edu.yr.par)
adj.edu.yr.par.tab <- tidy(adj.edu.yr.par) %>% mutate(LL=sprintf(confint(adj.edu.yr.par)[,1],fmt='%#.3f'),
                                                      UL=sprintf(confint(adj.edu.yr.par)[,2],fmt='%#.3f')) %>%
                                                      unite(col=CI,LL:UL, sep = ", ", remove=F) %>% 
      filter(term %in% c('(Intercept)', 'edu.c','pedu.c','edu.c:pedu.c'))

names(adj.edu.yr.par.tab) <- gsub("^(.*?)","adj_\\2", names(adj.edu.yr.par.tab))

# Combine results into one table
edu.yr.par.VIDALBRALO_DNAMAGE <- merge(edu.yr.par.tab,adj.edu.yr.par.tab,by.x='term',by.y='adj_term',all.y=T,sort=F) 
write.csv(edu.yr.par.VIDALBRALO_DNAMAGE,'ph_edu.yr.par.VIDALBRALO_DNAMAGE.csv')

## YANG_DNAMAGE ----
# Minimally adjusted
edu.yr.par <- svyglm(accyang~edu.c*pedu.c +
                       raceth +
                       sex,
                     design=svy.edaa)
summary(edu.yr.par)
edu.yr.par.tab <- tidy(edu.yr.par) %>% 
  mutate(
    LL=sprintf(confint(edu.yr.par)[,1],fmt='%#.3f'),
    UL=sprintf(confint(edu.yr.par)[,2],fmt='%#.3f')) %>%
    unite(col=CI,LL:UL, sep = ", ", remove=F) %>% 
    filter(term %in% c('(Intercept)', 'edu.c','pedu.c','edu.c:pedu.c'))

# Adjusted with Childhood Health Indicators
adj.edu.yr.par <- svyglm(accyang~edu.c*pedu.c +
                                 raceth + 
                                 sex +
                                 scale(H13ITOT) + 
                                 smokestatus +
                                 R13BMI +
                                 RTHLTHCH +
                                 DEPRESS +
                                 CHDISABL +
                                 CHLEARN,design=svy.edaa)
summary(adj.edu.yr.par)
adj.edu.yr.par.tab <- tidy(adj.edu.yr.par) %>% mutate(LL=sprintf(confint(adj.edu.yr.par)[,1],fmt='%#.3f'),
                                                      UL=sprintf(confint(adj.edu.yr.par)[,2],fmt='%#.3f')) %>%
                                                      unite(col=CI,LL:UL, sep = ", ", remove=F) %>% 
      filter(term %in% c('(Intercept)', 'edu.c','pedu.c','edu.c:pedu.c'))

names(adj.edu.yr.par.tab) <- gsub("^(.*?)","adj_\\2", names(adj.edu.yr.par.tab))

# Combine results into one table
edu.yr.par.YANG_DNAMAGE <- merge(edu.yr.par.tab,adj.edu.yr.par.tab,by.x='term',by.y='adj_term',all.y=T,sort=F) 
write.csv(edu.yr.par.YANG_DNAMAGE,'ph_edu.yr.par.YANG_DNAMAGE.csv')

## ZHANG_DNAMAGE ----
# Minimally adjusted
edu.yr.par <- svyglm(acczhang~edu.c*pedu.c +
                       raceth +
                       sex,
                     design=svy.edaa)
summary(edu.yr.par)
edu.yr.par.tab <- tidy(edu.yr.par) %>% 
  mutate(
    LL=sprintf(confint(edu.yr.par)[,1],fmt='%#.3f'),
    UL=sprintf(confint(edu.yr.par)[,2],fmt='%#.3f')) %>%
    unite(col=CI,LL:UL, sep = ", ", remove=F) %>% 
    filter(term %in% c('(Intercept)', 'edu.c','pedu.c','edu.c:pedu.c'))

# Adjusted with Childhood Health Indicators
adj.edu.yr.par <- svyglm(acczhang~edu.c*pedu.c +
                                 raceth + 
                                 sex +
                                 scale(H13ITOT) + 
                                 smokestatus +
                                 R13BMI +
                                 RTHLTHCH +
                                 DEPRESS +
                                 CHDISABL +
                                 CHLEARN,design=svy.edaa)
summary(adj.edu.yr.par)
adj.edu.yr.par.tab <- tidy(adj.edu.yr.par) %>% mutate(LL=sprintf(confint(adj.edu.yr.par)[,1],fmt='%#.3f'),
                                                      UL=sprintf(confint(adj.edu.yr.par)[,2],fmt='%#.3f')) %>%
                                                      unite(col=CI,LL:UL, sep = ", ", remove=F) %>% 
      filter(term %in% c('(Intercept)', 'edu.c','pedu.c','edu.c:pedu.c'))

names(adj.edu.yr.par.tab) <- gsub("^(.*?)","adj_\\2", names(adj.edu.yr.par.tab))

# Combine results into one table
edu.yr.par.ZHANG_DNAMAGE <- merge(edu.yr.par.tab,adj.edu.yr.par.tab,by.x='term',by.y='adj_term',all.y=T,sort=F) 
write.csv(edu.yr.par.ZHANG_DNAMAGE,'ph_edu.yr.par.ZHANG_DNAMAGE.csv')

## BOCKLANDT_DNAMAGE ----
# Minimally adjusted
edu.yr.par <- svyglm(accbock~edu.c*pedu.c +
                       raceth +
                       sex,
                     design=svy.edaa)
summary(edu.yr.par)
edu.yr.par.tab <- tidy(edu.yr.par) %>% 
  mutate(
    LL=sprintf(confint(edu.yr.par)[,1],fmt='%#.3f'),
    UL=sprintf(confint(edu.yr.par)[,2],fmt='%#.3f')) %>%
    unite(col=CI,LL:UL, sep = ", ", remove=F) %>% 
    filter(term %in% c('(Intercept)', 'edu.c','pedu.c','edu.c:pedu.c'))

# Adjusted with Childhood Health Indicators
adj.edu.yr.par <- svyglm(accbock~edu.c*pedu.c +
                                 raceth + 
                                 sex +
                                 scale(H13ITOT) + 
                                 smokestatus +
                                 R13BMI +
                                 RTHLTHCH +
                                 DEPRESS +
                                 CHDISABL +
                                 CHLEARN,design=svy.edaa)
summary(adj.edu.yr.par)
adj.edu.yr.par.tab <- tidy(adj.edu.yr.par) %>% mutate(LL=sprintf(confint(adj.edu.yr.par)[,1],fmt='%#.3f'),
                                                      UL=sprintf(confint(adj.edu.yr.par)[,2],fmt='%#.3f')) %>%
                                                      unite(col=CI,LL:UL, sep = ", ", remove=F) %>% 
      filter(term %in% c('(Intercept)', 'edu.c','pedu.c','edu.c:pedu.c'))

names(adj.edu.yr.par.tab) <- gsub("^(.*?)","adj_\\2", names(adj.edu.yr.par.tab))

# Combine results into one table
edu.yr.par.BOCKLANDT_DNAMAGE <- merge(edu.yr.par.tab,adj.edu.yr.par.tab,by.x='term',by.y='adj_term',all.y=T,sort=F) 
write.csv(edu.yr.par.BOCKLANDT_DNAMAGE,'ph_edu.yr.par.BOCKLANDT_DNAMAGE.csv')

## GARAGNANI_DNAMAGE ----
# Minimally adjusted
edu.yr.par <- svyglm(accgarag~edu.c*pedu.c +
                       raceth +
                       sex,
                     design=svy.edaa)
summary(edu.yr.par)
edu.yr.par.tab <- tidy(edu.yr.par) %>% 
  mutate(
    LL=sprintf(confint(edu.yr.par)[,1],fmt='%#.3f'),
    UL=sprintf(confint(edu.yr.par)[,2],fmt='%#.3f')) %>%
    unite(col=CI,LL:UL, sep = ", ", remove=F) %>% 
    filter(term %in% c('(Intercept)', 'edu.c','pedu.c','edu.c:pedu.c'))

# Adjusted with Childhood Health Indicators
adj.edu.yr.par <- svyglm(accgarag~edu.c*pedu.c +
                                 raceth + 
                                 sex +
                                 scale(H13ITOT) + 
                                 smokestatus +
                                 R13BMI +
                                 RTHLTHCH +
                                 DEPRESS +
                                 CHDISABL +
                                 CHLEARN,design=svy.edaa)
summary(adj.edu.yr.par)
adj.edu.yr.par.tab <- tidy(adj.edu.yr.par) %>% mutate(LL=sprintf(confint(adj.edu.yr.par)[,1],fmt='%#.3f'),
                                                      UL=sprintf(confint(adj.edu.yr.par)[,2],fmt='%#.3f')) %>%
                                                      unite(col=CI,LL:UL, sep = ", ", remove=F) %>% 
      filter(term %in% c('(Intercept)', 'edu.c','pedu.c','edu.c:pedu.c'))

names(adj.edu.yr.par.tab) <- gsub("^(.*?)","adj_\\2", names(adj.edu.yr.par.tab))

# Combine results into one table
edu.yr.par.GARAGNANI_DNAMAGE <- merge(edu.yr.par.tab,adj.edu.yr.par.tab,by.x='term',by.y='adj_term',all.y=T,sort=F) 
write.csv(edu.yr.par.GARAGNANI_DNAMAGE,'ph_edu.yr.par.GARAGNANI_DNAMAGE.csv')

## DNAMGRIMAGE ----
# Minimally adjusted
edu.yr.par <- svyglm(accdnam~edu.c*pedu.c +
                       raceth +
                       sex,
                     design=svy.edaa)
summary(edu.yr.par)
edu.yr.par.tab <- tidy(edu.yr.par) %>% 
  mutate(
    LL=sprintf(confint(edu.yr.par)[,1],fmt='%#.3f'),
    UL=sprintf(confint(edu.yr.par)[,2],fmt='%#.3f')) %>%
    unite(col=CI,LL:UL, sep = ", ", remove=F) %>% 
    filter(term %in% c('(Intercept)', 'edu.c','pedu.c','edu.c:pedu.c'))

# Adjusted with Childhood Health Indicators
adj.edu.yr.par <- svyglm(accdnam~edu.c*pedu.c +
                                 raceth + 
                                 sex +
                                 scale(H13ITOT) + 
                                 smokestatus +
                                 R13BMI +
                                 RTHLTHCH +
                                 DEPRESS +
                                 CHDISABL +
                                 CHLEARN,design=svy.edaa)
summary(adj.edu.yr.par)
adj.edu.yr.par.tab <- tidy(adj.edu.yr.par) %>% mutate(LL=sprintf(confint(adj.edu.yr.par)[,1],fmt='%#.3f'),
                                                      UL=sprintf(confint(adj.edu.yr.par)[,2],fmt='%#.3f')) %>%
                                                      unite(col=CI,LL:UL, sep = ", ", remove=F) %>% 
      filter(term %in% c('(Intercept)', 'edu.c','pedu.c','edu.c:pedu.c'))

names(adj.edu.yr.par.tab) <- gsub("^(.*?)","adj_\\2", names(adj.edu.yr.par.tab))

# Combine results into one table
edu.yr.par.DNAMGRIMAGE <- merge(edu.yr.par.tab,adj.edu.yr.par.tab,by.x='term',by.y='adj_term',all.y=T,sort=F) 
write.csv(edu.yr.par.DNAMGRIMAGE,'ph_edu.yr.par.DNAMGRIMAGE.csv')

## MPOA ----
# Minimally adjusted
edu.yr.par <- svyglm(accmpoa~edu.c*pedu.c +
                       raceth +
                       sex,
                     design=svy.edaa)
summary(edu.yr.par)
edu.yr.par.tab <- tidy(edu.yr.par) %>% 
  mutate(
    LL=sprintf(confint(edu.yr.par)[,1],fmt='%#.3f'),
    UL=sprintf(confint(edu.yr.par)[,2],fmt='%#.3f')) %>%
    unite(col=CI,LL:UL, sep = ", ", remove=F) %>% 
    filter(term %in% c('(Intercept)', 'edu.c','pedu.c','edu.c:pedu.c'))

# Adjusted with Childhood Health Indicators
adj.edu.yr.par <- svyglm(accmpoa~edu.c*pedu.c +
                                 raceth + 
                                 sex +
                                 scale(H13ITOT) + 
                                 smokestatus +
                                 R13BMI +
                                 RTHLTHCH +
                                 DEPRESS +
                                 CHDISABL +
                                 CHLEARN,design=svy.edaa)
summary(adj.edu.yr.par)
adj.edu.yr.par.tab <- tidy(adj.edu.yr.par) %>% mutate(LL=sprintf(confint(adj.edu.yr.par)[,1],fmt='%#.3f'),
                                                      UL=sprintf(confint(adj.edu.yr.par)[,2],fmt='%#.3f')) %>%
                                                      unite(col=CI,LL:UL, sep = ", ", remove=F) %>% 
      filter(term %in% c('(Intercept)', 'edu.c','pedu.c','edu.c:pedu.c'))

names(adj.edu.yr.par.tab) <- gsub("^(.*?)","adj_\\2", names(adj.edu.yr.par.tab))

# Combine results into one table
edu.yr.par.MPOA<- merge(edu.yr.par.tab,adj.edu.yr.par.tab,by.x='term',by.y='adj_term',all.y=T,sort=F) 
write.csv(edu.yr.par.MPOA,'ph_edu.yr.par.MPOA.csv')

### Regression Models (Parental Educational Attainment-Categorical) ----
## Re-level personal education ----
svy.edaa$variables$RAEDYRS_factor <- relevel(svy.edaa$variables$RAEDYRS_factor,ref='Bachelors or Higher')
## HORVATH_DNAMAGE ----
# Minimally adjusted
edu.c.par <- svyglm(acchorv~RAEDYRS_factor*par_edu_bin +
                    raceth +
                    sex,design=svy.edaa)
summary(edu.c.par)
edu.c.par.aov <- anova(edu.c.par,test="Chisq",method='LRT')
edu.c.par.tab <- tidy(edu.c.par) %>% 
  mutate(
    LL=sprintf(confint(edu.c.par)[,1],fmt='%#.3f'),
    UL=sprintf(confint(edu.c.par)[,2],fmt='%#.3f')) %>%
    unite(col=CI,LL:UL, sep = ", ", remove=F) %>% 
    filter(!(term %in% c('racethNHB','racethHispanic','racethNHOTH', 'sexFemale')))
                                       

# Adjusted with Childhood Health Indicators
adj.edu.c.par <- svyglm(acchorv~RAEDYRS_factor*par_edu_bin +
                                 raceth + 
                                 sex +
                                 scale(H13ITOT) + 
                                 smokestatus +
                                 R13BMI +
                                 RTHLTHCH +
                                 DEPRESS +
                                 CHDISABL +
                                 CHLEARN,design=svy.edaa)
summary(adj.edu.c.par)

adj.edu.c.par.tab <- tidy(adj.edu.c.par) %>% mutate(LL=sprintf(confint(adj.edu.c.par)[,1],fmt='%#.3f'),
                                               UL=sprintf(confint(adj.edu.c.par)[,2],fmt='%#.3f')) %>%
                                               unite(col=CI,LL:UL, sep = ", ", remove=F) %>% 
  filter((term %in% c('(Intercept)','RAEDYRS_factorNo Degree','RAEDYRS_factorHigh School or GED', 'RAEDYRS_factorAssociates',
                      'par_edu_binNo Degree','RAEDYRS_factorNo Degree:par_edu_binNo Degree',
                      'RAEDYRS_factorHigh School or GED:par_edu_binNo Degree','RAEDYRS_factorAssociates:par_edu_binNo Degree')))
                                               
names(adj.edu.c.par.tab) <- gsub("^(.*?)","adj_\\2", names(adj.edu.c.par.tab))

# Combine results into one table
edu.c.par.HORVATH_DNAMAGE <- merge(edu.c.par.tab,adj.edu.c.par.tab,by.x='term',by.y='adj_term',all.y=T,sort=F) 
write.csv(edu.c.par.HORVATH_DNAMAGE,'edu.c.par.HORVATH_DNAMAGE.csv')

# Save ANOVA results
sink(file="aov.edu.c.HORVATH_DNAMAGE.txt")
print(aov<-list(anova(edu.c.par),anova(adj.edu.c.par)))
sink()

## Marginal Means
HORVATH_DNAMAGE.MM <- emmeans(edu.c.par,pairwise~RAEDYRS_factor*par_edu_bin,data=svy.edaa,adjust='mvt')

adj.HORVATH_DNAMAGE.MM <- emmeans(adj.edu.c.par,pairwise~RAEDYRS_factor*par_edu_bin,data=svy.edaa,adjust='mvt')

## HANNUM_DNAMAGE ----
# Minimally adjusted
edu.c.par <- svyglm(acchannum~RAEDYRS_factor*par_edu_bin +
                    raceth +
                    sex,design=svy.edaa)
summary(edu.c.par)
edu.c.par.aov <- anova(edu.c.par,test="Chisq",method='LRT')
edu.c.par.tab <- tidy(edu.c.par) %>% 
  mutate(
    LL=sprintf(confint(edu.c.par)[,1],fmt='%#.3f'),
    UL=sprintf(confint(edu.c.par)[,2],fmt='%#.3f')) %>%
    unite(col=CI,LL:UL, sep = ", ", remove=F) %>% 
    filter(!(term %in% c('racethNHB','racethHispanic','racethNHOTH', 'sexFemale')))
                                       

# Adjusted with Childhood Health Indicators
adj.edu.c.par <- svyglm(acchannum~RAEDYRS_factor*par_edu_bin +
                                 raceth + 
                                 sex +
                                 scale(H13ITOT) + 
                                 smokestatus +
                                 R13BMI +
                                 RTHLTHCH +
                                 DEPRESS +
                                 CHDISABL +
                                 CHLEARN,design=svy.edaa)
summary(adj.edu.c.par)

adj.edu.c.par.tab <- tidy(adj.edu.c.par) %>% mutate(LL=sprintf(confint(adj.edu.c.par)[,1],fmt='%#.3f'),
                                               UL=sprintf(confint(adj.edu.c.par)[,2],fmt='%#.3f')) %>%
                                               unite(col=CI,LL:UL, sep = ", ", remove=F) %>% 
  filter((term %in% c('(Intercept)','RAEDYRS_factorNo Degree','RAEDYRS_factorHigh School or GED', 'RAEDYRS_factorAssociates',
                      'par_edu_binNo Degree','RAEDYRS_factorNo Degree:par_edu_binNo Degree',
                      'RAEDYRS_factorHigh School or GED:par_edu_binNo Degree','RAEDYRS_factorAssociates:par_edu_binNo Degree')))
                             
names(adj.edu.c.par.tab) <- gsub("^(.*?)","adj_\\2", names(adj.edu.c.par.tab))

# Combine results into one table
edu.c.par.HANNUM_DNAMAGE <- merge(edu.c.par.tab,adj.edu.c.par.tab,by.x='term',by.y='adj_term',all.y=T,sort=F) 
write.csv(edu.c.par.HANNUM_DNAMAGE,'edu.c.par.HANNUM_DNAMAGE.csv')

# Save ANOVA results
sink(file="aov.edu.c.HANNUM_DNAMAGE.txt")
print(aov<-list(anova(edu.c.par),anova(adj.edu.c.par)))
sink()

## Marginal Means
HANNUM_DNAMAGE.MM <- emmeans(edu.c.par,pairwise~RAEDYRS_factor*par_edu_bin,data=svy.edaa,adjust='mvt')

adj.HANNUM_DNAMAGE.MM <- emmeans(adj.edu.c.par,pairwise~RAEDYRS_factor*par_edu_bin,data=svy.edaa,adjust='mvt')

## LEVINE_DNAMAGE ----
# Minimally adjusted
edu.c.par <- svyglm(acclevine~RAEDYRS_factor*par_edu_bin +
                    raceth +
                    sex,design=svy.edaa)
summary(edu.c.par)
edu.c.par.aov <- anova(edu.c.par,test="Chisq",method='LRT')
edu.c.par.tab <- tidy(edu.c.par) %>% 
  mutate(
    LL=sprintf(confint(edu.c.par)[,1],fmt='%#.3f'),
    UL=sprintf(confint(edu.c.par)[,2],fmt='%#.3f')) %>%
    unite(col=CI,LL:UL, sep = ", ", remove=F) %>% 
    filter(!(term %in% c('racethNHB','racethHispanic','racethNHOTH', 'sexFemale')))
                                       

# Adjusted with Childhood Health Indicators
adj.edu.c.par <- svyglm(acclevine~RAEDYRS_factor*par_edu_bin +
                                 raceth + 
                                 sex +
                                 scale(H13ITOT) + 
                                 smokestatus +
                                 R13BMI +
                                 RTHLTHCH +
                                 DEPRESS +
                                 CHDISABL +
                                 CHLEARN,design=svy.edaa)
summary(adj.edu.c.par)

adj.edu.c.par.tab <- tidy(adj.edu.c.par) %>% mutate(LL=sprintf(confint(adj.edu.c.par)[,1],fmt='%#.3f'),
                                               UL=sprintf(confint(adj.edu.c.par)[,2],fmt='%#.3f')) %>%
                                               unite(col=CI,LL:UL, sep = ", ", remove=F) %>% 
  filter((term %in% c('(Intercept)','RAEDYRS_factorNo Degree','RAEDYRS_factorHigh School or GED', 'RAEDYRS_factorAssociates',
                      'par_edu_binNo Degree','RAEDYRS_factorNo Degree:par_edu_binNo Degree',
                      'RAEDYRS_factorHigh School or GED:par_edu_binNo Degree','RAEDYRS_factorAssociates:par_edu_binNo Degree')))
                                                     
names(adj.edu.c.par.tab) <- gsub("^(.*?)","adj_\\2", names(adj.edu.c.par.tab))

# Combine results into one table
edu.c.par.LEVINE_DNAMAGE <- merge(edu.c.par.tab,adj.edu.c.par.tab,by.x='term',by.y='adj_term',all.y=T,sort=F) 
write.csv(edu.c.par.LEVINE_DNAMAGE,'edu.c.par.LEVINE_DNAMAGE.csv')

# Save ANOVA results
sink(file="aov.edu.c.LEVINE_DNAMAGE.txt")
print(aov<-list(anova(edu.c.par),anova(adj.edu.c.par)))
sink()

## Marginal Means
LEVINE_DNAMAGE.MM <- emmeans(edu.c.par,pairwise~RAEDYRS_factor*par_edu_bin,data=svy.edaa,adjust='mvt')

adj.LEVINE_DNAMAGE.MM <- emmeans(adj.edu.c.par,pairwise~RAEDYRS_factor*par_edu_bin,data=svy.edaa,adjust='mvt')

## HORVATHSKIN_DNAMAGE ----
# Minimally adjusted
edu.c.par <- svyglm(acchorvsk~RAEDYRS_factor*par_edu_bin +
                    raceth +
                    sex,design=svy.edaa)
summary(edu.c.par)
edu.c.par.aov <- anova(edu.c.par,test="Chisq",method='LRT')
edu.c.par.tab <- tidy(edu.c.par) %>% 
  mutate(
    LL=sprintf(confint(edu.c.par)[,1],fmt='%#.3f'),
    UL=sprintf(confint(edu.c.par)[,2],fmt='%#.3f')) %>%
    unite(col=CI,LL:UL, sep = ", ", remove=F) %>% 
    filter(!(term %in% c('racethNHB','racethHispanic','racethNHOTH', 'sexFemale')))
                                       
# Adjusted with Childhood Health Indicators
adj.edu.c.par <- svyglm(acchorvsk~RAEDYRS_factor*par_edu_bin +
                                 raceth + 
                                 sex +
                                 scale(H13ITOT) + 
                                 smokestatus +
                                 R13BMI +
                                 RTHLTHCH +
                                 DEPRESS +
                                 CHDISABL +
                                 CHLEARN,design=svy.edaa)
summary(adj.edu.c.par)

adj.edu.c.par.tab <- tidy(adj.edu.c.par) %>% mutate(LL=sprintf(confint(adj.edu.c.par)[,1],fmt='%#.3f'),
                                               UL=sprintf(confint(adj.edu.c.par)[,2],fmt='%#.3f')) %>%
                                               unite(col=CI,LL:UL, sep = ", ", remove=F) %>% 
  filter((term %in% c('(Intercept)','RAEDYRS_factorNo Degree','RAEDYRS_factorHigh School or GED', 'RAEDYRS_factorAssociates',
                      'par_edu_binNo Degree','RAEDYRS_factorNo Degree:par_edu_binNo Degree',
                      'RAEDYRS_factorHigh School or GED:par_edu_binNo Degree','RAEDYRS_factorAssociates:par_edu_binNo Degree')))
                                                   
names(adj.edu.c.par.tab) <- gsub("^(.*?)","adj_\\2", names(adj.edu.c.par.tab))

# Combine results into one table
edu.c.par.HORVATHSKIN_DNAMAGE<- merge(edu.c.par.tab,adj.edu.c.par.tab,by.x='term',by.y='adj_term',all.y=T,sort=F) 
write.csv(edu.c.par.HORVATHSKIN_DNAMAGE,'edu.c.par.HORVATHSKIN_DNAMAGE.csv')

# Save ANOVA results
sink(file="aov.edu.c.HORVATHSKIN_DNAMAGE.txt")
print(aov<-list(anova(edu.c.par),anova(adj.edu.c.par)))
sink()

## Marginal Means
HORVATHSKIN_DNAMAGE.MM <- emmeans(edu.c.par,pairwise~RAEDYRS_factor*par_edu_bin,data=svy.edaa,adjust='mvt')

adj.HORVATHSKIN_DNAMAGE.MM <- emmeans(adj.edu.c.par,pairwise~RAEDYRS_factor*par_edu_bin,data=svy.edaa,adjust='mvt')

## LIN_DNAMAGE ----
# Minimally adjusted
edu.c.par <- svyglm(acclin~RAEDYRS_factor*par_edu_bin +
                    raceth +
                    sex,design=svy.edaa)
summary(edu.c.par)
edu.c.par.aov <- anova(edu.c.par,test="Chisq",method='LRT')
edu.c.par.tab <- tidy(edu.c.par) %>% 
  mutate(
    LL=sprintf(confint(edu.c.par)[,1],fmt='%#.3f'),
    UL=sprintf(confint(edu.c.par)[,2],fmt='%#.3f')) %>%
    unite(col=CI,LL:UL, sep = ", ", remove=F) %>% 
    filter(!(term %in% c('racethNHB','racethHispanic','racethNHOTH', 'sexFemale')))
                                       
# Adjusted with Childhood Health Indicators
adj.edu.c.par <- svyglm(acclin~RAEDYRS_factor*par_edu_bin +
                                 raceth + 
                                 sex +
                                 scale(H13ITOT) + 
                                 smokestatus +
                                 R13BMI +
                                 RTHLTHCH +
                                 DEPRESS +
                                 CHDISABL +
                                 CHLEARN,design=svy.edaa)
summary(adj.edu.c.par)

adj.edu.c.par.tab <- tidy(adj.edu.c.par) %>% mutate(LL=sprintf(confint(adj.edu.c.par)[,1],fmt='%#.3f'),
                                               UL=sprintf(confint(adj.edu.c.par)[,2],fmt='%#.3f')) %>%
                                               unite(col=CI,LL:UL, sep = ", ", remove=F) %>% 
  filter((term %in% c('(Intercept)','RAEDYRS_factorNo Degree','RAEDYRS_factorHigh School or GED', 'RAEDYRS_factorAssociates',
                      'par_edu_binNo Degree','RAEDYRS_factorNo Degree:par_edu_binNo Degree',
                      'RAEDYRS_factorHigh School or GED:par_edu_binNo Degree','RAEDYRS_factorAssociates:par_edu_binNo Degree')))
                                                  
names(adj.edu.c.par.tab) <- gsub("^(.*?)","adj_\\2", names(adj.edu.c.par.tab))

# Combine results into one table
edu.c.par.LIN_DNAMAGE <- merge(edu.c.par.tab,adj.edu.c.par.tab,by.x='term',by.y='adj_term',all.y=T,sort=F) 
write.csv(edu.c.par.LIN_DNAMAGE,'edu.c.par.LIN_DNAMAGE.csv')

# Save ANOVA results
sink(file="aov.edu.c.LIN_DNAMAGE.txt")
print(aov<-list(anova(edu.c.par),anova(adj.edu.c.par)))
sink()

## Marginal Means
LIN_DNAMAGE.MM <- emmeans(edu.c.par,pairwise~RAEDYRS_factor*par_edu_bin,data=svy.edaa,adjust='mvt')

adj.LIN_DNAMAGE.MM <- emmeans(adj.edu.c.par,pairwise~RAEDYRS_factor*par_edu_bin,data=svy.edaa,adjust='mvt')

## WEIDNER_DNAMAGE ----
# Minimally adjusted
edu.c.par <- svyglm(accweid~RAEDYRS_factor*par_edu_bin +
                    raceth +
                    sex,design=svy.edaa)
summary(edu.c.par)
edu.c.par.aov <- anova(edu.c.par,test="Chisq",method='LRT')
edu.c.par.tab <- tidy(edu.c.par) %>% 
  mutate(
    LL=sprintf(confint(edu.c.par)[,1],fmt='%#.3f'),
    UL=sprintf(confint(edu.c.par)[,2],fmt='%#.3f')) %>%
    unite(col=CI,LL:UL, sep = ", ", remove=F) %>% 
    filter(!(term %in% c('racethNHB','racethHispanic','racethNHOTH', 'sexFemale')))
                                       
# Adjusted with Childhood Health Indicators
adj.edu.c.par <- svyglm(accweid~RAEDYRS_factor*par_edu_bin +
                                 raceth + 
                                 sex +
                                 scale(H13ITOT) + 
                                 smokestatus +
                                 R13BMI +
                                 RTHLTHCH +
                                 DEPRESS +
                                 CHDISABL +
                                 CHLEARN,design=svy.edaa)
summary(adj.edu.c.par)

adj.edu.c.par.tab <- tidy(adj.edu.c.par) %>% mutate(LL=sprintf(confint(adj.edu.c.par)[,1],fmt='%#.3f'),
                                               UL=sprintf(confint(adj.edu.c.par)[,2],fmt='%#.3f')) %>%
                                               unite(col=CI,LL:UL, sep = ", ", remove=F) %>% 
  filter((term %in% c('(Intercept)','RAEDYRS_factorNo Degree','RAEDYRS_factorHigh School or GED', 'RAEDYRS_factorAssociates',
                      'par_edu_binNo Degree','RAEDYRS_factorNo Degree:par_edu_binNo Degree',
                      'RAEDYRS_factorHigh School or GED:par_edu_binNo Degree','RAEDYRS_factorAssociates:par_edu_binNo Degree')))
                                           
names(adj.edu.c.par.tab) <- gsub("^(.*?)","adj_\\2", names(adj.edu.c.par.tab))

# Combine results into one table
edu.c.par.WEIDNER_DNAMAGE <- merge(edu.c.par.tab,adj.edu.c.par.tab,by.x='term',by.y='adj_term',all.y=T,sort=F) 
write.csv(edu.c.par.WEIDNER_DNAMAGE,'edu.c.par.WEIDNER_DNAMAGE.csv')

# Save ANOVA results
sink(file="aov.edu.c.WEIDNER_DNAMAGE.txt")
print(aov<-list(anova(edu.c.par),anova(adj.edu.c.par)))
sink()

## Marginal Means
WEIDNER_DNAMAGE.MM <- emmeans(edu.c.par,pairwise~RAEDYRS_factor*par_edu_bin,data=svy.edaa,adjust='mvt')

adj.WEIDNER_DNAMAGE.MM <- emmeans(adj.edu.c.par,pairwise~RAEDYRS_factor*par_edu_bin,data=svy.edaa,adjust='mvt')

## VIDALBRALO_DNAMAGE ----
# Minimally adjusted
edu.c.par <- svyglm(accvidalb~RAEDYRS_factor*par_edu_bin +
                    raceth +
                    sex,design=svy.edaa)
summary(edu.c.par)
edu.c.par.aov <- anova(edu.c.par,test="Chisq",method='LRT')
edu.c.par.tab <- tidy(edu.c.par) %>% 
  mutate(
    LL=sprintf(confint(edu.c.par)[,1],fmt='%#.3f'),
    UL=sprintf(confint(edu.c.par)[,2],fmt='%#.3f')) %>%
    unite(col=CI,LL:UL, sep = ", ", remove=F) %>% 
    filter(!(term %in% c('racethNHB','racethHispanic','racethNHOTH', 'sexFemale')))
                                       
# Adjusted with Childhood Health Indicators
adj.edu.c.par <- svyglm(accvidalb~RAEDYRS_factor*par_edu_bin +
                                 raceth + 
                                 sex +
                                 scale(H13ITOT) + 
                                 smokestatus +
                                 R13BMI +
                                 RTHLTHCH +
                                 DEPRESS +
                                 CHDISABL +
                                 CHLEARN,design=svy.edaa)
summary(adj.edu.c.par)

adj.edu.c.par.tab <- tidy(adj.edu.c.par) %>% mutate(LL=sprintf(confint(adj.edu.c.par)[,1],fmt='%#.3f'),
                                               UL=sprintf(confint(adj.edu.c.par)[,2],fmt='%#.3f')) %>%
                                               unite(col=CI,LL:UL, sep = ", ", remove=F) %>% 
  filter((term %in% c('(Intercept)','RAEDYRS_factorNo Degree','RAEDYRS_factorHigh School or GED', 'RAEDYRS_factorAssociates',
                      'par_edu_binNo Degree','RAEDYRS_factorNo Degree:par_edu_binNo Degree',
                      'RAEDYRS_factorHigh School or GED:par_edu_binNo Degree','RAEDYRS_factorAssociates:par_edu_binNo Degree')))
                                                         
names(adj.edu.c.par.tab) <- gsub("^(.*?)","adj_\\2", names(adj.edu.c.par.tab))

# Combine results into one table
edu.c.par.VIDALBRALO_DNAMAGE <- merge(edu.c.par.tab,adj.edu.c.par.tab,by.x='term',by.y='adj_term',all.y=T,sort=F) 
write.csv(edu.c.par.VIDALBRALO_DNAMAGE,'edu.c.par.VIDALBRALO_DNAMAGE.csv')

# Save ANOVA results
sink(file="aov.edu.c.VIDALBRALO_DNAMAGE.txt")
print(aov<-list(anova(edu.c.par),anova(adj.edu.c.par)))
sink()

## Marginal Means
VIDALBRALO_DNAMAGE.MM <- emmeans(edu.c.par,pairwise~RAEDYRS_factor*par_edu_bin,data=svy.edaa,adjust='mvt')

adj.VIDALBRALO_DNAMAGE.MM <- emmeans(adj.edu.c.par,pairwise~RAEDYRS_factor*par_edu_bin,data=svy.edaa,adjust='mvt')

## YANG_DNAMAGE ----
# Minimally adjusted
edu.c.par <- svyglm(accyang~RAEDYRS_factor*par_edu_bin +
                    raceth +
                    sex,design=svy.edaa)
summary(edu.c.par)
edu.c.par.aov <- anova(edu.c.par,test="Chisq",method='LRT')
edu.c.par.tab <- tidy(edu.c.par) %>% 
  mutate(
    LL=sprintf(confint(edu.c.par)[,1],fmt='%#.3f'),
    UL=sprintf(confint(edu.c.par)[,2],fmt='%#.3f')) %>%
    unite(col=CI,LL:UL, sep = ", ", remove=F) %>% 
    filter(!(term %in% c('racethNHB','racethHispanic','racethNHOTH', 'sexFemale')))
                                       
# Adjusted with Childhood Health Indicators
adj.edu.c.par <- svyglm(accyang~RAEDYRS_factor*par_edu_bin +
                                 raceth + 
                                 sex +
                                 scale(H13ITOT) + 
                                 smokestatus +
                                 R13BMI +
                                 RTHLTHCH +
                                 DEPRESS +
                                 CHDISABL +
                                 CHLEARN,design=svy.edaa)
summary(adj.edu.c.par)

adj.edu.c.par.tab <- tidy(adj.edu.c.par) %>% mutate(LL=sprintf(confint(adj.edu.c.par)[,1],fmt='%#.3f'),
                                               UL=sprintf(confint(adj.edu.c.par)[,2],fmt='%#.3f')) %>%
                                               unite(col=CI,LL:UL, sep = ", ", remove=F) %>% 
  filter((term %in% c('(Intercept)','RAEDYRS_factorNo Degree','RAEDYRS_factorHigh School or GED', 'RAEDYRS_factorAssociates',
                      'par_edu_binNo Degree','RAEDYRS_factorNo Degree:par_edu_binNo Degree',
                      'RAEDYRS_factorHigh School or GED:par_edu_binNo Degree','RAEDYRS_factorAssociates:par_edu_binNo Degree')))
                                                 
names(adj.edu.c.par.tab) <- gsub("^(.*?)","adj_\\2", names(adj.edu.c.par.tab))

# Combine results into one table
edu.c.par.YANG_DNAMAGE <- merge(edu.c.par.tab,adj.edu.c.par.tab,by.x='term',by.y='adj_term',all.y=T,sort=F) 
write.csv(edu.c.par.YANG_DNAMAGE,'edu.c.par.YANG_DNAMAGE.csv')

# Save ANOVA results
sink(file="aov.edu.c.YANG_DNAMAGE.txt")
print(aov<-list(anova(edu.c.par),anova(adj.edu.c.par)))
sink()

## Marginal Means
YANG_DNAMAGE.MM <- emmeans(edu.c.par,pairwise~RAEDYRS_factor*par_edu_bin,data=svy.edaa,adjust='mvt')

adj.YANG_DNAMAGE.MM <- emmeans(adj.edu.c.par,pairwise~RAEDYRS_factor*par_edu_bin,data=svy.edaa,adjust='mvt')

## ZHANG_DNAMAGE ----
# Minimally adjusted
edu.c.par <- svyglm(acczhang~RAEDYRS_factor*par_edu_bin +
                    raceth +
                    sex,design=svy.edaa)
summary(edu.c.par)
edu.c.par.aov <- anova(edu.c.par,test="Chisq",method='LRT')
edu.c.par.tab <- tidy(edu.c.par) %>% 
  mutate(
    LL=sprintf(confint(edu.c.par)[,1],fmt='%#.3f'),
    UL=sprintf(confint(edu.c.par)[,2],fmt='%#.3f')) %>%
    unite(col=CI,LL:UL, sep = ", ", remove=F) %>% 
    filter(!(term %in% c('racethNHB','racethHispanic','racethNHOTH', 'sexFemale')))
                                       
# Adjusted with Childhood Health Indicators
adj.edu.c.par <- svyglm(acczhang~RAEDYRS_factor*par_edu_bin +
                                 raceth + 
                                 sex +
                                 scale(H13ITOT) + 
                                 smokestatus +
                                 R13BMI +
                                 RTHLTHCH +
                                 DEPRESS +
                                 CHDISABL +
                                 CHLEARN,design=svy.edaa)
summary(adj.edu.c.par)

adj.edu.c.par.tab <- tidy(adj.edu.c.par) %>% mutate(LL=sprintf(confint(adj.edu.c.par)[,1],fmt='%#.3f'),
                                               UL=sprintf(confint(adj.edu.c.par)[,2],fmt='%#.3f')) %>%
                                               unite(col=CI,LL:UL, sep = ", ", remove=F) %>% 
  filter((term %in% c('(Intercept)','RAEDYRS_factorNo Degree','RAEDYRS_factorHigh School or GED', 'RAEDYRS_factorAssociates',
                      'par_edu_binNo Degree','RAEDYRS_factorNo Degree:par_edu_binNo Degree',
                      'RAEDYRS_factorHigh School or GED:par_edu_binNo Degree','RAEDYRS_factorAssociates:par_edu_binNo Degree')))
                                                      
names(adj.edu.c.par.tab) <- gsub("^(.*?)","adj_\\2", names(adj.edu.c.par.tab))

# Combine results into one table
edu.c.par.ZHANG_DNAMAGE <- merge(edu.c.par.tab,adj.edu.c.par.tab,by.x='term',by.y='adj_term',all.y=T,sort=F) 
write.csv(edu.c.par.ZHANG_DNAMAGE,'edu.c.par.ZHANG_DNAMAGE.csv')

# Save ANOVA results
sink(file="aov.edu.c.ZHANG_DNAMAGE.txt")
print(aov<-list(anova(edu.c.par),anova(adj.edu.c.par)))
sink()

## Marginal Means
ZHANG_DNAMAGE.MM <- emmeans(edu.c.par,pairwise~RAEDYRS_factor*par_edu_bin,data=svy.edaa,adjust='mvt')

adj.ZHANG_DNAMAGE.MM <- emmeans(adj.edu.c.par,pairwise~RAEDYRS_factor*par_edu_bin,data=svy.edaa,adjust='mvt')

## BOCKLANDT_DNAMAGE ----
# Minimally adjusted
edu.c.par <- svyglm(accbock~RAEDYRS_factor*par_edu_bin +
                    raceth +
                    sex,design=svy.edaa)
summary(edu.c.par)
edu.c.par.aov <- anova(edu.c.par,test="Chisq",method='LRT')
edu.c.par.tab <- tidy(edu.c.par) %>% 
  mutate(
    LL=sprintf(confint(edu.c.par)[,1],fmt='%#.3f'),
    UL=sprintf(confint(edu.c.par)[,2],fmt='%#.3f')) %>%
    unite(col=CI,LL:UL, sep = ", ", remove=F) %>% 
    filter(!(term %in% c('racethNHB','racethHispanic','racethNHOTH', 'sexFemale')))
                                       
# Adjusted with Childhood Health Indicators
adj.edu.c.par <- svyglm(accbock~RAEDYRS_factor*par_edu_bin +
                                 raceth + 
                                 sex +
                                 scale(H13ITOT) + 
                                 smokestatus +
                                 R13BMI +
                                 RTHLTHCH +
                                 DEPRESS +
                                 CHDISABL +
                                 CHLEARN,design=svy.edaa)
summary(adj.edu.c.par)

adj.edu.c.par.tab <- tidy(adj.edu.c.par) %>% mutate(LL=sprintf(confint(adj.edu.c.par)[,1],fmt='%#.3f'),
                                               UL=sprintf(confint(adj.edu.c.par)[,2],fmt='%#.3f')) %>%
                                               unite(col=CI,LL:UL, sep = ", ", remove=F) %>% 
  filter((term %in% c('(Intercept)','RAEDYRS_factorNo Degree','RAEDYRS_factorHigh School or GED', 'RAEDYRS_factorAssociates',
                      'par_edu_binNo Degree','RAEDYRS_factorNo Degree:par_edu_binNo Degree',
                      'RAEDYRS_factorHigh School or GED:par_edu_binNo Degree','RAEDYRS_factorAssociates:par_edu_binNo Degree')))
                                            
names(adj.edu.c.par.tab) <- gsub("^(.*?)","adj_\\2", names(adj.edu.c.par.tab))

# Combine results into one table
edu.c.par.BOCKLANDT_DNAMAGE <- merge(edu.c.par.tab,adj.edu.c.par.tab,by.x='term',by.y='adj_term',all.y=T,sort=F) 
write.csv(edu.c.par.BOCKLANDT_DNAMAGE,'edu.c.par.BOCKLANDT_DNAMAGE.csv')

# Save ANOVA results
sink(file="aov.edu.c.BOCKLANDT_DNAMAGE.txt")
print(aov<-list(anova(edu.c.par),anova(adj.edu.c.par)))
sink()

## Marginal Means
BOCKLANDT_DNAMAGE.MM <- emmeans(edu.c.par,pairwise~RAEDYRS_factor*par_edu_bin,data=svy.edaa,adjust='mvt')

adj.BOCKLANDT_DNAMAGE.MM <- emmeans(adj.edu.c.par,pairwise~RAEDYRS_factor*par_edu_bin,data=svy.edaa,adjust='mvt')

## GARAGNANI_DNAMAGE ----
# Minimally adjusted
edu.c.par <- svyglm(accgarag~RAEDYRS_factor*par_edu_bin +
                    raceth +
                    sex,design=svy.edaa)
summary(edu.c.par)
edu.c.par.aov <- anova(edu.c.par,test="Chisq",method='LRT')
edu.c.par.tab <- tidy(edu.c.par) %>% 
  mutate(
    LL=sprintf(confint(edu.c.par)[,1],fmt='%#.3f'),
    UL=sprintf(confint(edu.c.par)[,2],fmt='%#.3f')) %>%
    unite(col=CI,LL:UL, sep = ", ", remove=F) %>% 
    filter(!(term %in% c('racethNHB','racethHispanic','racethNHOTH', 'sexFemale')))
                                       
# Adjusted with Childhood Health Indicators
adj.edu.c.par <- svyglm(accgarag~RAEDYRS_factor*par_edu_bin +
                                 raceth + 
                                 sex +
                                 scale(H13ITOT) + 
                                 smokestatus +
                                 R13BMI +
                                 RTHLTHCH +
                                 DEPRESS +
                                 CHDISABL +
                                 CHLEARN,design=svy.edaa)
summary(adj.edu.c.par)

adj.edu.c.par.tab <- tidy(adj.edu.c.par) %>% mutate(LL=sprintf(confint(adj.edu.c.par)[,1],fmt='%#.3f'),
                                               UL=sprintf(confint(adj.edu.c.par)[,2],fmt='%#.3f')) %>%
                                               unite(col=CI,LL:UL, sep = ", ", remove=F) %>% 
  filter((term %in% c('(Intercept)','RAEDYRS_factorNo Degree','RAEDYRS_factorHigh School or GED', 'RAEDYRS_factorAssociates',
                      'par_edu_binNo Degree','RAEDYRS_factorNo Degree:par_edu_binNo Degree',
                      'RAEDYRS_factorHigh School or GED:par_edu_binNo Degree','RAEDYRS_factorAssociates:par_edu_binNo Degree')))
                                           
names(adj.edu.c.par.tab) <- gsub("^(.*?)","adj_\\2", names(adj.edu.c.par.tab))

# Combine results into one table
edu.c.par.GARAGNANI_DNAMAGE <- merge(edu.c.par.tab,adj.edu.c.par.tab,by.x='term',by.y='adj_term',all.y=T,sort=F) 
write.csv(edu.c.par.GARAGNANI_DNAMAGE,'edu.c.par.GARAGNANI_DNAMAGE.csv')

# Save ANOVA results
sink(file="aov.edu.c.GARAGNANI_DNAMAGE.txt")
print(aov<-list(anova(edu.c.par),anova(adj.edu.c.par)))
sink()

## Marginal Means
GARAGNANI_DNAMAGE.MM <- emmeans(edu.c.par,pairwise~RAEDYRS_factor*par_edu_bin,data=svy.edaa,adjust='mvt')

adj.GARAGNANI_DNAMAGE.MM <- emmeans(adj.edu.c.par,pairwise~RAEDYRS_factor*par_edu_bin,data=svy.edaa,adjust='mvt')

## DNAMGRIMAGE ----
# Minimally adjusted
edu.c.par <- svyglm(accdnam~RAEDYRS_factor*par_edu_bin +
                    raceth +
                    sex,design=svy.edaa)
summary(edu.c.par)
edu.c.par.aov <- anova(edu.c.par,test="Chisq",method='LRT')
edu.c.par.tab <- tidy(edu.c.par) %>% 
  mutate(
    LL=sprintf(confint(edu.c.par)[,1],fmt='%#.3f'),
    UL=sprintf(confint(edu.c.par)[,2],fmt='%#.3f')) %>%
    unite(col=CI,LL:UL, sep = ", ", remove=F) %>% 
    filter(!(term %in% c('racethNHB','racethHispanic','racethNHOTH', 'sexFemale')))
                                       
# Adjusted with Childhood Health Indicators
adj.edu.c.par <- svyglm(accdnam~RAEDYRS_factor*par_edu_bin +
                                 raceth + 
                                 sex +
                                 scale(H13ITOT) + 
                                 smokestatus +
                                 R13BMI +
                                 RTHLTHCH +
                                 DEPRESS +
                                 CHDISABL +
                                 CHLEARN,design=svy.edaa)
summary(adj.edu.c.par)

adj.edu.c.par.tab <- tidy(adj.edu.c.par) %>% mutate(LL=sprintf(confint(adj.edu.c.par)[,1],fmt='%#.3f'),
                                               UL=sprintf(confint(adj.edu.c.par)[,2],fmt='%#.3f')) %>%
                                               unite(col=CI,LL:UL, sep = ", ", remove=F) %>% 
  filter((term %in% c('(Intercept)','RAEDYRS_factorNo Degree','RAEDYRS_factorHigh School or GED', 'RAEDYRS_factorAssociates',
                      'par_edu_binNo Degree','RAEDYRS_factorNo Degree:par_edu_binNo Degree',
                      'RAEDYRS_factorHigh School or GED:par_edu_binNo Degree','RAEDYRS_factorAssociates:par_edu_binNo Degree')))
                                         
names(adj.edu.c.par.tab) <- gsub("^(.*?)","adj_\\2", names(adj.edu.c.par.tab))

# Combine results into one table
edu.c.par.DNAMGRIMAGE <- merge(edu.c.par.tab,adj.edu.c.par.tab,by.x='term',by.y='adj_term',all.y=T,sort=F) 
write.csv(edu.c.par.DNAMGRIMAGE,'edu.c.par.DNAMGRIMAGE.csv')

# Save ANOVA results
sink(file="aov.edu.c.DNAMGRIMAGE.txt")
print(aov<-list(anova(edu.c.par),anova(adj.edu.c.par)))
sink()

## Marginal Means
DNAMGRIMAGE.MM <- emmeans(edu.c.par,pairwise~RAEDYRS_factor*par_edu_bin,data=svy.edaa,adjust='mvt')

adj.DNAMGRIMAGE.MM <- emmeans(adj.edu.c.par,pairwise~RAEDYRS_factor*par_edu_bin,data=svy.edaa,adjust='mvt')

## MPOA ----
# Minimally adjusted
edu.c.par <- svyglm(accmpoa~RAEDYRS_factor*par_edu_bin +
                    raceth +
                    sex,design=svy.edaa)
summary(edu.c.par)
edu.c.par.aov <- anova(edu.c.par,test="Chisq",method='LRT')
edu.c.par.tab <- tidy(edu.c.par) %>% 
  mutate(
    LL=sprintf(confint(edu.c.par)[,1],fmt='%#.3f'),
    UL=sprintf(confint(edu.c.par)[,2],fmt='%#.3f')) %>%
    unite(col=CI,LL:UL, sep = ", ", remove=F) %>% 
    filter(!(term %in% c('racethNHB','racethHispanic','racethNHOTH', 'sexFemale')))
                                       
# Adjusted with Childhood Health Indicators
adj.edu.c.par <- svyglm(accmpoa~RAEDYRS_factor*par_edu_bin +
                                 raceth + 
                                 sex +
                                 scale(H13ITOT) + 
                                 smokestatus +
                                 R13BMI +
                                 RTHLTHCH +
                                 DEPRESS +
                                 CHDISABL +
                                 CHLEARN,design=svy.edaa)
summary(adj.edu.c.par)

adj.edu.c.par.tab <- tidy(adj.edu.c.par) %>% mutate(LL=sprintf(confint(adj.edu.c.par)[,1],fmt='%#.3f'),
                                               UL=sprintf(confint(adj.edu.c.par)[,2],fmt='%#.3f')) %>%
                                               unite(col=CI,LL:UL, sep = ", ", remove=F) %>% 
  filter((term %in% c('(Intercept)','RAEDYRS_factorNo Degree','RAEDYRS_factorHigh School or GED', 'RAEDYRS_factorAssociates',
                      'par_edu_binNo Degree','RAEDYRS_factorNo Degree:par_edu_binNo Degree',
                      'RAEDYRS_factorHigh School or GED:par_edu_binNo Degree','RAEDYRS_factorAssociates:par_edu_binNo Degree')))
                                                
names(adj.edu.c.par.tab) <- gsub("^(.*?)","adj_\\2", names(adj.edu.c.par.tab))

# Combine results into one table
edu.c.par.MPOA<- merge(edu.c.par.tab,adj.edu.c.par.tab,by.x='term',by.y='adj_term',all.y=T,sort=F) 
write.csv(edu.c.par.MPOA,'edu.c.par.MPOA.csv')

# Save ANOVA results
sink(file="aov.edu.c.MPOA.txt")
print(aov<-list(anova(edu.c.par),anova(adj.edu.c.par)))
sink()

## Marginal Means
MPOA.MM <- emmeans(edu.c.par,pairwise~RAEDYRS_factor*par_edu_bin,data=svy.edaa,adjust='mvt')

adj.MPOA.MM <- emmeans(adj.edu.c.par,pairwise~RAEDYRS_factor*par_edu_bin,data=svy.edaa,adjust='mvt')

## Save marginal means ----
save(HORVATH_DNAMAGE.MM,adj.HORVATH_DNAMAGE.MM,HANNUM_DNAMAGE.MM,adj.HANNUM_DNAMAGE.MM,
     LEVINE_DNAMAGE.MM,adj.LEVINE_DNAMAGE.MM,HORVATHSKIN_DNAMAGE.MM,adj.HORVATHSKIN_DNAMAGE.MM,
     LIN_DNAMAGE.MM,adj.LIN_DNAMAGE.MM,WEIDNER_DNAMAGE.MM,adj.WEIDNER_DNAMAGE.MM,
     VIDALBRALO_DNAMAGE.MM,adj.VIDALBRALO_DNAMAGE.MM,YANG_DNAMAGE.MM,adj.YANG_DNAMAGE.MM,
     ZHANG_DNAMAGE.MM,adj.ZHANG_DNAMAGE.MM,BOCKLANDT_DNAMAGE.MM,adj.BOCKLANDT_DNAMAGE.MM,
     GARAGNANI_DNAMAGE.MM,adj.GARAGNANI_DNAMAGE.MM,DNAMGRIMAGE.MM,adj.DNAMGRIMAGE.MM,
     MPOA.MM,adj.MPOA.MM,file='PH_Parental_MMs.RData')

