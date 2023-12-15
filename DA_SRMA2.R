
#######################....Data Analysis for SRMA_PPD......#####################
setwd('type the path of the working directory')

#######################....    dependencies       ......########################
library(meta) # for conducting meta-analysis.
library(metafor) # for conducting influence analysis.
library(metasens) # for doi plot
library(tidyverse) # for data processing

############....       load the data file                       ....############
df<-read.csv("file name.csv") # type the file name in your directory

############....       meta object class creation               ....############
m.prop<-metaprop (event = Event,
                 n = Total,
                 studlab = Author,
                 data = df,
                 method = "GLMM",
                 sm = "PLOGIT",
                 fixed = F,
                 random = T,
                 hakn = T,
                 title = "Prevalence of Postpartum Depression in India")
summary(m.prop)

###################...Forest plot with JAMA layout.....#########################
forest.meta(m.prop,
            sortvar = Author,
            layout = "JAMA",
            prediction = T,
            print.tau2 = T,
            leftlabs = c("Author","Events","N"))

#####################...Doi plot and LFK index   ...############################
doiplot(m.prop, xlab = "Logit transformed prevalence of Postpartum Depression in India")

#######################...    Funnel plot        ...############################

funnel.meta (m.prop, xlab = "Logit transformed prevalence of postpartum depression in India")
title("Funnel Plot (postpartum depression in India)")

############...    Contour enhanced Funnel plot        ...######################

# Define fill colours for contour
col.contour = c("gray75", "gray85", "gray95")

# Generate funnel plot (we do not include study labels here)
funnel.meta(m.prop, xlim = c(-3.5, 2.5),
            contour = c(0.9, 0.95, 0.99),
            col.contour = col.contour)
# Add a title
title("Contour-Enhanced Funnel Plot (Prevalence of PPD in India)")

#############################...    Eggers test  ...############################
m.prop$data %>% 
  mutate(y = m.prop$TE/m.prop$seTE, x = 1/m.prop$seTE) %>% 
  lm(y ~ x, data = .) %>% 
  summary()

#####################...Sensitivity anlaysis    ...#############################
#############..SA1: exclusion of inadequate sample size   ...###################
SA1<- read.csv ("filename.csv") #type the data file name for sensitivity analysis in your directory
m.propSA1<-metaprop (event = Event,
                  n = Total,
                  studlab = Author,
                  data = SA1,
                  method = "GLMM",
                  sm = "PLOGIT",
                  fixed = F,
                  random = T,
                  hakn = T,
                  title = "Prevalence of Postpartum Depression in India after
                  excluding those studies with inadequate sample size")
summary(m.propSA1)
forest.meta(m.propSA1,
            sortvar = Author,
            layout = "JAMA",
            digits = 3,
            prediction = T,
            print.tau2 = T,
            leftlabs = c("Author","Events","N"))

#############..SA2: influence analysis using metainf in meta ..#################
# influence analysis
data<-read.csv("file name.csv")
#object class using meta package
m.prop<-metaprop (event = Event,
                     n = Total,
                     studlab = Author,
                     data = data,
                     method = "GLMM",
                     sm = "PLOGIT",
                     fixed = F,
                     random = T,
                     hakn = T,
                     title = "Prevalence of Postpartum Depression in India")

#influence analysis
metainf (m.prop)

SA2<- read.csv ("filename.csv") #type the data file name for sensitivity analysis in your directory
m.propSA2<-metaprop (event = Event,
                     n = Total,
                     studlab = Author,
                     data = SA2,
                     method = "GLMM",
                     sm = "PLOGIT",
                     fixed = F,
                     random = T,
                     hakn = T,
                     title = "Prevalence of Postpartum Depression in India after
                  excluding study after influence analysis")
summary(m.propSA2)
forest.meta(m.propSA2,
            sortvar = Author,
            layout = "JAMA",
            digits = 3,
            prediction = T,
            print.tau2 = T,
            leftlabs = c("Author","Events","N"))

#############..SA3: omitting studies without using EPDS tool ..#################

SA3<- read.csv ("filename.csv") #type the data file name for sensitivity analysis in your directory
m.propSA3<-metaprop (event = Event,
                     n = Total,
                     studlab = Author,
                     data = SA3,
                     method = "GLMM",
                     sm = "PLOGIT",
                     fixed = F,
                     random = T,
                     hakn = T,
                     title = "Prevalence of Postpartum Depression in India after
                  excluding study using assessment tool other than EPDS")
summary(m.propSA3)
forest.meta(m.propSA3,
            sortvar = Author,
            layout = "JAMA",
            digits = 3,
            prediction = T,
            print.tau2 = T,
            leftlabs = c("Author","Events","N"))

#############..SA3: Excluding studies assessed the participants for PPD beyond 6 weeks postpartum ..#################

SA4<- read.csv ("filename.csv") #type the data file name for sensitivity analysis in your directory
m.propSA4<-metaprop (event = Event,
                     n = Total,
                     studlab = Author,
                     data = SA4,
                     method = "GLMM",
                     sm = "PLOGIT",
                     fixed = F,
                     random = T,
                     hakn = T,
                     title = "Prevalence of Postpartum Depression in India after
                  excluding studies which have assessed participatns after 6 week 
                     postnatal period")
summary(m.propSA4)
forest.meta(m.propSA4,
            sortvar = Author,
            layout = "JAMA",
            digits = 3,
            prediction = T,
            print.tau2 = T,
            leftlabs = c("Author","Events","N"))

###########################...Subgroup Analysis...##############################
#################...Subgroup Analysis-Region (SG1)...###########################
SG1<-read.csv("file name.csv")
m.propSG1<-metaprop(event = Event,
                 n = Total,
                 studlab = Author,
                 data = SG1,
                 byvar = Region,
                 method = "GLMM",
                 sm = "PLOGIT",
                 fixed = F,
                 random = T,
                 hakn = T,
                 title = "Prevalence of Postpartum Depression in India by Region")
forest.meta(m.propSG1,layout = "JAMA",col.subgroup = "RED",digits = 3)
summary(m.propSG1)
###################...Subgroup Analysis-States (SG2)...#########################

SG2<-read.csv("file name.csv")
m.propSG2<-metaprop(event = Event,
                 n = Total,
                 studlab = Author,
                 data = SG2,
                 byvar = state,
                 method = "GLMM",
                 sm = "PLOGIT",
                 fixed = F,
                 random = T,
                 hakn = T,
                 title = "Prevalence of Postpartum Depression in India by study states")
summary(m.propSG2)
forest.meta(m.propSG2,layout = "JAMA",col.subgroup = "RED", digits = 3)
##################...Subgroup Analysis-Setting (SG3)...#########################
SG3<-read.csv("file name.csv")
m.propSG3<-metaprop(event = Event,
                 n = Total,
                 studlab = Author,
                 data = SG3,
                 byvar = Setting,
                 method = "GLMM",
                 sm = "PLOGIT",
                 fixed = F,
                 random = T,
                 hakn = T,
                 title = "Prevalence of Postpartum Depression in India by study setting")

summary(m.propSG3)

forest.meta(m.propSG3,layout = "JAMA",col.subgroup = "RED")

####################...Subgroup Analysis-Area (SG4)...##########################

SG4<-read.csv("file name.csv")
m.propSG4<-metaprop(event = Event,
                 n = Total,
                 studlab = Author,
                 data = SG4,
                 byvar = Area,
                 method = "GLMM",
                 sm = "PLOGIT",
                 fixed = F,
                 random = T,
                 hakn = T,
                 title = "Prevalence of Postpartum Depression in India by study area")
summary(m.propSG4)

forest.meta(m.propSG4,layout = "JAMA",col.subgroup = "RED", digits = 3)

##############...Subgroup Analysis-Quality score (SG5)...#######################

SG5<-read.csv("file namey.csv")
m.propSG5<-metaprop(event = Event,
                    n = Total,
                    studlab = Author,
                    data = SG5,
                    byvar = SG5$QS,
                    method = "GLMM",
                    sm = "PLOGIT",
                    fixed = F,
                    random = T,
                    hakn = T,
                    title = "Prevalence of Postpartum Depression in India 
                    by quality score")

summary(m.propSG5)

forest.meta(m.propSG5,layout = "JAMA",col.subgroup = "RED", digits = 3)

#######################...Metaregression analysis...############################

df<-read.csv("file name.csv")

reg_multi <- rma(yi = Prevalence, 
              sei = SE, 
              data = df, 
              method = "ML", 
              mods = ~ Year + Region_code + Setting_code + Area_code + 
                Time_code + QS + Q7 + SP, 
              test = "knha")
################################################################################
