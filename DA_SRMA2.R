##############################....06-06-2023......##############################
#######################....Data Analysis for SRMA2......########################

setwd('D:/Publication Plans/#MyFirstSRMA/SRMA2_PPD/5_Data analysis_SRMA2')
data<-read.csv("data.csv")

#######################....    dependencies       ......########################

library(meta)
library(metafor) # for conducting influence analysis
library(metasens) # for doi plot
library(dplyr)
library(tidyverse)
library(readxl)
library(gtsummary)
library(officer)
library(flextable)
library(gt)
library(writexl)
library(forcats)
##################....             Descriptive               ....###############
metab
# basic summary table (Frequencies)
data<-read_excel("Total literture list_V2.xlsx", sheet = "Descriptive")

# Sum of the econtinuous variable has been given
tbl<-data %>% select(!c(1:2)) %>%
  tbl_summary(
    statistic = list(all_continuous() ~ "{sum}"))
tbl
# Convert gtsummary table to a flextable
gt_tbl <- as_gt(tbl)


# Save the result in doc and excel files
gtsave(gt_tbl, file = "DraftDescriptive_SRMA2.docx")

############....       meta object class creation               ....############

df<-read.csv("data.csv")
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


reg_ <- metareg(m.prop, ~Year)
summary(reg_)

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

# Define fill colors for contour
col.contour = c("gray75", "gray85", "gray95")

# Generate funnel plot (we do not include study labels here)
funnel.meta(m.prop, xlim = c(-3.5, 2.5),
            contour = c(0.9, 0.95, 0.99),
            col.contour = col.contour)
# Add a title
title("Contour-Enhanced Funnel Plot (Third Wave Psychotherapies)")

#############################...    Eggers test  ...############################

#use dmeta package for the following
m.prop$data %>% 
  mutate(y = m.prop$TE/m.prop$seTE, x = 1/m.prop$seTE) %>% 
  lm(y ~ x, data = .) %>% 
  summary()
#In the results, we see that the intercept of our regression model is β= 0.2402.
#This is not significantly larger than zero (t= 0.135, p=0.893), and indicates that
#the data in the funnel plot is not asymmetrical. Overall, this corroborates our
#initial findings that there are negligible small-study effects. 
#Yet, to reiterate, it is uncertain if this pattern has been caused by publication bias.

#####################...Sensitivity anlaysis    ...#############################

#############..SA1: exclusion of inadequate sample size   ...###################

df<-read_excel("sensitivity_data.xlsx")
df[,colnames(df[,4:16])]<-lapply(df[, colnames(df[,4:16])], as.factor)

summary(df)

#to create SA1 data
SA1<-df[df$SA1==1,]
write.csv(SA1,"SA1.csv", row.names = F)

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
data<-read.csv("data.csv")
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
?metainf
#to create SA2 data
SA2<-df[df$SA2==1,]
write.csv(SA1,"SA2.csv", row.names = F)

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

#############..SA3: influence analysis using metainf in meta ..#################

SA3<-df[df$SA3==1,]
write.csv(SA1,"SA3.csv", row.names = F)

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
                  excluding study after influence analysis")
summary(m.propSA3)
forest.meta(m.propSA3,
            sortvar = Author,
            layout = "JAMA",
            digits = 3,
            prediction = T,
            print.tau2 = T,
            leftlabs = c("Author","Events","N"))

#############..SA3: influence analysis using metainf in meta ..#################

SA4<-read_excel ("sensitivity_data2.xlsx")
SA4$SA4<-as.factor(SA4$SA4)
summary(SA4$SA4)

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

#################...Subgroup Analysis-Region (SG1)...###########################

setwd('D:/Publication Plans/#MyFirstSRMA/SRMA2_PPD/5_Data analysis_SRMA2')
library(tidyverse)
library(meta)
SG1<-read.csv("data_SG_Region.csv")
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

reg_SG1 <- metareg(m.propSG1, ~Year)
summary(reg_SG1)

data<-SG1[,c(4,7)]
result <- aggregate(Total ~ Region, data = data, sum)
###################...Subgroup Analysis-States (SG2)...#########################

SG2<-read.csv("data_SG_states.csv")
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
reg_SG2 <- metareg(m.propSG2, ~state)
summary(reg_SG2)
bubble(reg_SG2, studlab = TRUE)
forest.meta(m.propSG2,layout = "JAMA",col.subgroup = "RED", digits = 3)

data<-SG2[,c(4,6)]
result <- aggregate(Total ~ state, data = data, sum)
write.csv(result,"result.csv")
##################...Subgroup Analysis-Setting (SG3)...#########################

setwd('D:/Publication Plans/#MyFirstSRMA/SRMA2_PPD/5_Data analysis_SRMA2')
library(tidyverse)
library(meta)
SG3<-read.csv("data_SG_setting.csv")
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
reg_SG3 <- metareg(m.propSG3, ~Setting)
summary(reg_SG3)

forest.meta(m.propSG3,layout = "JAMA",col.subgroup = "RED")
data<-SG3[,c(6,11)]
result <- aggregate(Total ~ Setting, data = data, sum)
####################...Subgroup Analysis-Area (SG4)...##########################

SG4<-read.csv("data_SG_area.csv")
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
reg_SG4 <- metareg(m.propSG4, ~Area)
summary(reg_SG4)
forest.meta(m.propSG4,layout = "JAMA",col.subgroup = "RED", digits = 3)

data<-SG4[,c(3,4)]
result <- aggregate(Total ~ Area, data = data, sum)
##############...Subgroup Analysis-Quality score (SG5)...#######################

SG5<-read.csv("data_SG_quality.csv")
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

result <- aggregate(Total ~ QS, data = SG5, sum)
colnames(data)<-c("QS","Total")
#####################...       07-05-2023        ...############################
##############...User oslo Meta analysis using metafor   ...####################
library(metafor)
df<-read.csv("data.csv")
df
df1<-escalc(measure = "PFT", xi = Event, ni = Total, data = df)
?escalc
res <- rma(yi, vi, data = df1, method = "REML")
forest(m.prop)
dev.off()
funnel (m.prop, ylim=c(0,.8), las=1, digits=list(1L,1))
funnel.default(m.prop)
doiplot(m.prop)

m.prop<-metaprop(event = Event,
                 n = Total,
                 studlab = Author,
                 data = df,
                 method = "inverse",
                 sm = "PFT",
                 fixed = F,
                 random = T,
                 hakn = T,
                 title = "Prevalence of Postpartum Depression in India by the time of assessment")
funnel(m.prop)
dat<-escalc(measure = "PFT", xi = Event, ni = Total, data = df)
res <- rma(yi, vi, data=dat)
forest(res, )
funnel(res)
funnel(dat$yi, dat$vi, yaxis="seinv",
       xlim=c(-3,2), ylim=c(.00001,8), xaxs="i", yaxs="i", las=1,
       level=c(.10, .05, .01), shade=c("white", "gray55", "gray75"),
       legend=TRUE, back="gray90", hlines=NULL, ylab="Precision (1/se)")
# trim and fill method
funnel(trimfill(m.prop))
# outlier/influence diagnostics
par(mar=c(5,6,4,2))
plot(influence(res), cex=0.8, las=1)

radial(res)
labbe(res)
baujat(res)
sav <- gosh(res, subset=20000)
plot(sav, out=6, xlim=c(0.4,0.7), breaks=100, hh=0.2)
?gosh

#####################...       08-05-2023        ...############################
#######################...Sensitivity analysis   ...############################
1.	Funnel plot and eggers test
2.	Trim and fill method
3.	Influence plot
4.	Leave one out analysis
#####################...       09-05-2023        ...############################
#######################...    Funnel plot        ...############################
setwd('D:/Publication Plans/#MyFirstSRMA/SRMA2_PPD/5_Data analysis_SRMA2')
library(meta)
df<-read.csv("data.csv")
m.prop<-metaprop(event = Event,
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
funnel.meta (m.prop, xlab = "Logit transformed prevalence of postpartum depression in India")
title("Funnel Plot (postpartum depression in India)")

############...    Contour enhanced Funnel plot        ...######################
# Define fill colors for contour
col.contour = c("gray75", "gray85", "gray95")

# Generate funnel plot (we do not include study labels here)
funnel.meta(m.prop, xlim = c(-3.5, 2.5),
            contour = c(0.9, 0.95, 0.99),
            col.contour = col.contour)
# Add a title
title("Contour-Enhanced Funnel Plot (Third Wave Psychotherapies)")
############...    Eggers test        ...######################
library(metafor)
m.propMF<-rma.uni (xi = Event, ni = Total, data = df, measure = "PLO")

library(dmeta)
m.prop$data %>% 
  mutate(y = m.prop$TE/m.prop$seTE, x = 1/m.prop$seTE) %>% 
  lm(y ~ x, data = .) %>% 
  summary()
#In the results, we see that the intercept of our regression model is β= 0.2402.
#This is not significantly larger than zero (t= 0.135, p>0.05), and indicates that
#the data in the funnel plot is not asymmetrical. Overall, this corroborates our
#initial findings that there are negligible small-study effects. 
#Yet, to reiterate, it is uncertain if this pattern has been caused by publication bias.
m.prop$I2

tf<-trimfill(m.prop)
summary(tf)

#####################...       04-06-2023        ...############################
#######################... Influence analysis    ...############################
library(stats)
m.propMF<-rma.glmm (xi = Event, ni = Total, data = df, measure = "PLO")
influence (m.propMF)
?metainf(m.propMF)
?rma.uni
influence(m.propMF)
plot(influence(m.propMF))
forest(m.propMF)
cooks.distance(metainf (m.prop))
infl<-metainf (m.prop)
infl$lev
?influence
infl_


#####################...       17-11-2023        ...############################
#######################...Metaregression analysis...############################

df<-read.csv("Metaregression_data.csv")

df$Region_code<-as.factor(df$Region_code)
df$Setting_code<-as.factor(df$Setting_code)
df$Area_code<-as.factor(df$Area_code)
df$Time_code<-as.factor(df$Time_code)
df$Q7<-as.factor(df$Q7)
df$SP<-as.factor(df$SP)

colnames(df)
summary(df$Q7)

reg_multi <- rma(yi = Prevalence, 
              sei = SE, 
              data = df, 
              method = "ML", 
              mods = ~ Year + Region_code + Setting_code + Area_code + 
                Time_code + QS + Q7 + SP, 
              test = "knha")

