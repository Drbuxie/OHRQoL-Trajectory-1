# OHRQoL-Trajectory-1
Predicting group-based trajectories of Oral Health-Related Quality of Life using K-means clustering algorithm
---

### ABSTRACT

Objectives
This study aimed to identify and analyze the longitudinal trajectories of Oral Health-Related Quality of Life (OHRQoL) from late adolescence to early adulthood using an unsupervised machine learning approach, K-means for Longitudinal data (KmL). 
We hypothesized the existence of distinct OHRQoL trajectory groups with varying slopes over time.

Methods
This study utilized data from the Iowa Fluoride Study, a prospective cohort study assessing oral health outcomes. 
OHRQoL was measured at ages 17, 19, and 23 using three validated instruments: the Child Perception Questionnaire (CPQ), Global Oral Health Rating (GOHR), and Visual Analog Rating of Quality of Life (VisQoL). 
KmL, an unsupervised clustering algorithm, was applied to identify trajectory groups based on OHRQoL scores. 
The optimal number of clusters was determined using the Calinski-Harabasz criterion. 
Sociodemographic predictors of trajectory group membership were assessed using logistic regression.

Results
Two distinct OHRQoL trajectories were identified for each measure: a “Good” trajectory group and a “Poor” trajectory group. 
For CPQ, 84.8% of participants belonged to the “Good” trajectory, while 15.2% followed a “Poor” trajectory. 
GOHR trajectories had 57.2% in the “Good” group and 42.8% in the “Poor” group. 
VisQoL trajectories showed 67.5% in the “Good” group and 32.5% in the “Poor” group. 
Higher socioeconomic status was significantly associated with better OHRQoL trajectories (p < 0.05), while sex differences were not statistically significant (p > 0.05).

Conclusion
Machine learning effectively identified distinct OHRQoL trajectories, emphasizing the role of socioeconomic factors in oral health disparities. Future research should explore predictive modeling to enhance personalized oral healthcare strategies.


```{r packages}
#install.packages("installr")
#library(installr)
#updateR()
# install.packages("recipes")
# install.packages("arsenal")
# install.packages("PerformanceAnalytics")
# install.packages("pander")
# install.packages("haven")
# install.packages("MachineShop")
# install.packages("tidyverse")
# #install.packages("tableone")
# #install.packages("pastecs")
# install.packages("coin")
# install.packages("car")
# #install.packages("AER")
# install.packages("doParallel")
# install.packages("magrittr")
# #install.packages("recipes")
# #install.packages("Hmisc")
# install.packages("psych")
# install.packages("gbm")
# install.packages("imputeTS")
# install.packages("nnet")
# install.packages("stats")
# install.packages("MASS")
# install.packages("ResourceSelection")
# install.packages("glmnet")
# install.packages("corrplot")
# install.packages("VGAM")
library(themis)
library(corrplot)
library(readr)
library(pander)
library(Hmisc)
library(pastecs)
library(coin)
library(car)
library(AER)
library(arsenal)
library(PerformanceAnalytics)
require(knitr)
library(MachineShop)
library(nnet)
library(stats)
library(ResourceSelection)
library(MachineShop)
library(kml3d)
library(kml)
library(rgl)
library(dplyr)
library(psych)
library(tableone)
library(haven)
require(MASS)
suppressPackageStartupMessages(library(doParallel))
library(recipes)
library(magrittr)
library(gbm)
## Allocate cores for parallel processing
registerDoParallel(cores = 6)
```

```{r}
##loading the CSV file containing All OHRQOL variables at 17, 19 and 23
#setwd("/Users/User/OneDrive - Temple University/IFS Data - Dr. Ogwo/Wide Datasets")
setwd("/Users/cho379/OneDrive - Harvard University/IFS Data - Dr. Ogwo/Wide Datasets")
OHRQoL<- read_csv("OHQoL_Wide.csv")
OHRQoL2<- read_csv("GOHR_all.csv")
OHRQoL
OHRQoL2

summary(OHRQoL)
summary(OHRQoL2)

```

```{r}
###Descrptive univariate statistic for OHRQOL variables
describe(OHRQoL)
describe(OHRQoL2)
stat.desc(OHRQoL)
stat.desc(OHRQoL2)
table(ifelse (OHRQoL$OverallQoLScore_6==0,"zero",ifelse (OHRQoL$OverallQoLScore_6>0,"positive", "negative")))
table(ifelse (OHRQoL$OverallQoLScore_7==0,"zero",ifelse (OHRQoL$OverallQoLScore_6>0,"positive", "negative")))
table(ifelse (OHRQoL$OverallQoLScore_9==0,"zero",ifelse (OHRQoL$OverallQoLScore_6>0,"positive", "negative")))
```

## Including Plots
```{r Longitudinal data clustering }
### 1a. Main analysis### Longitudinal clustering for Trajectory analysis for Overall OHRQoL
Ebtraj<-cld(traj=OHRQoL,
    idAll=as.character(OHRQoL$SUBJECT_ID),
    time=c(17,19,23),
    timeInData=c(17,18,19), maxNA=1)
 kml(Ebtraj,nbClusters = 2:4, nbRedrawing=20,toPlot="both")
```


```{r Partition selection}
 ### 1b. Get and choose best partions for Overall OHRQoL
X11(type = "Xlib")
choice(Ebtraj, typeGraph = "png") ### To see the best partition  
plotAllCriterion(Ebtraj)          ### To check the best's cluster numbers##
print(Ebtraj)
mean.trajectories <- calculTrajMean(Ebtraj["traj"], Ebtraj['c3'][[1]]['clusters'])
print(mean.trajectories)
```
  
```{r Longitudinal data clustering }
### 2a. Main analysis### Longitudinal clustering for Trajectory analysis for VISQoL
Ebtraj2<-cld(traj=OHRQoL,
    idAll=as.character(OHRQoL$SUBJECT_ID),
    time=c(17,19,23),
    timeInData=c(2,3,4), maxNA=1)
 kml(Ebtraj2,nbClusters = 2:4, nbRedrawing=20,toPlot="both")
```

```{r Partition selection}
 ### 2b. Get and choose best partitions for VISQOL
X11(type = "Xlib")
choice(Ebtraj2, typeGraph = "png") ### To see the best partition  
plotAllCriterion(Ebtraj2)          ### To check the best's cluster numbers##
print(Ebtraj2)
mean.trajectories2 <- calculTrajMean(Ebtraj2["traj"], Ebtraj2['c2'][[1]]['clusters'])
print(mean.trajectories2)
```


```{r Longitudinal data clustering }
### 3a. Main analysis### Longitudinal clustering for Trajectory analysis for GOHR
Ebtraj3<-cld(traj=OHRQoL2,
    idAll=as.character(OHRQoL2$id),
    time=c(17,19,23),
    timeInData=c(2,3,4), maxNA=1)
 kml(Ebtraj3,nbClusters = 2:4, nbRedrawing=20,toPlot="both")
```

```{r Partition selection}
 ### 3b. Get and choose best partitions for GOHR
X11(type = "Xlib")
choice(Ebtraj3, typeGraph = "png") ### To see the best partition  
plotAllCriterion(Ebtraj3)          ### To check the best's cluster numbers##
print(Ebtraj3)
mean.trajectories3 <- calculTrajMean(Ebtraj3["traj"], Ebtraj3['c2'][[1]]['clusters'])
print(mean.trajectories3)
```

```{r Longitudinal data clustering }
### 4a. Main analysis### Longitudinal clustering for Trajectory analysis for Oral symptoms
Ebtraj4<-cld(traj=OHRQoL,
    idAll=as.character(OHRQoL$SUBJECT_ID),
    time=c(17,19,23),
    timeInData=c(5,6,7), maxNA=1)
 kml(Ebtraj4,nbClusters = 2:4, nbRedrawing=20,toPlot="both")
```

```{r Partition selection}
 ### 4b. Get and choose best partitions for Oral symptoms
X11(type = "Xlib")
choice(Ebtraj4, typeGraph = "png") ### To see the best partition  
plotAllCriterion(Ebtraj4)          ### To check the best's cluster numbers##
print(Ebtraj4)
mean.trajectories4 <- calculTrajMean(Ebtraj4["traj"], Ebtraj3['c2'][[1]]['clusters'])
print(mean.trajectories4)
```

```{r Longitudinal data clustering }
### 5a. Main analysis### Longitudinal clustering for Trajectory analysis for functional limit
Ebtraj5<-cld(traj=OHRQoL,
    idAll=as.character(OHRQoL$SUBJECT_ID),
    time=c(17,19,23),
    timeInData=c(8,9,10), maxNA=1)
 kml(Ebtraj5,nbClusters = 2:4, nbRedrawing=20,toPlot="both")
```

```{r Partition selection}
 ### 5b. Get and choose best partitions for functional limit
X11(type = "Xlib")
choice(Ebtraj5, typeGraph = "png") ### To see the best partition  
plotAllCriterion(Ebtraj5)          ### To check the best's cluster numbers##
print(Ebtraj5)
mean.trajectories5 <- calculTrajMean(Ebtraj5["traj"], Ebtraj5['c2'][[1]]['clusters'])
print(mean.trajectories5)
```

```{r Longitudinal data clustering }
### 6a. Main analysis### Longitudinal clustering for Trajectory analysis for Emotional wellbeing
Ebtraj6<-cld(traj=OHRQoL,
    idAll=as.character(OHRQoL$SUBJECT_ID),
    time=c(17,19,23),
    timeInData=c(11,12,13), maxNA=1)
 kml(Ebtraj6,nbClusters = 2:4, nbRedrawing=20,toPlot="both")
```

```{r Partition selection}
 ### 6b. Get and choose best partitions for emotional well being
X11(type = "Xlib")
choice(Ebtraj6, typeGraph = "png") ### To see the best partition  
plotAllCriterion(Ebtraj6)          ### To check the best's cluster numbers##
print(Ebtraj6)
mean.trajectories6 <- calculTrajMean(Ebtraj6["traj"], Ebtraj6['c2'][[1]]['clusters'])
print(mean.trajectories6)
```

```{r Longitudinal data clustering }
### 7a. Main analysis### Longitudinal clustering for Trajectory analysis for Social wellbeing
Ebtraj7<-cld(traj=OHRQoL,
    idAll=as.character(OHRQoL$SUBJECT_ID),
    time=c(17,19,23),
    timeInData=c(14,15,16), maxNA=1)
 kml(Ebtraj7,nbClusters = 2:4, nbRedrawing=20,toPlot="both")
```

```{r Partition selection}
 ### 7b. Get and choose best partitions for social wellbeing
X11(type = "Xlib")
choice(Ebtraj7, typeGraph = "png") ### To see the best partition  
plotAllCriterion(Ebtraj7)          ### To check the best's cluster numbers##
print(Ebtraj7)
mean.trajectories7 <- calculTrajMean(Ebtraj7["traj"], Ebtraj7['c2'][[1]]['clusters'])
print(mean.trajectories7)
```

### DESCRIPTIVE STATISTICS ###
```{r}
A1<- read_csv("Bivariate data grp A vs grp B.csv")
t.test(A1$`CPQ_Trajectory A`, A1$`CPQ_Trajectory B`)

A2<- read_csv("Bivariate data grp A vs grp B.csv")
t.test(A2$`GOHR_Trajectory A`, A2$`GOHR_Trajectory B`)

A3<- read_csv("Bivariate data grp A vs grp B.csv")
t.test(A3$`VisQoL_Trajectory A`, A3$`VisQoL_Trajectory B`)


B1<- read_csv("Bivariate data CPQ 17 vs 19 vs 23.csv")
t.test(B1$CPQ_17, B1$CPQ_23)

B2<- read_csv("Bivariate data GOHR 17 vs 19 vs 23.csv")
t.test(B2$GOHR_19, B2$GOHR_23)

B3<- read_csv("Bivariate data VisQol 17 vs 19 vs 23.csv")
t.test(B3$VisQoL_17, B3$VisQoL_19)


```


```{r Data export and merging}
### 4. Export the clusters and create a new dataset that includes the clusters -CPQ
OHRQoL$clustercpq <- getClusters(Ebtraj,2)
View (OHRQoL)
#write.csv(OHRQoL, 'CPQtraj.csv')
```

```{r Data export and merging}
### 4. Export the clusters and create a new dataset that includes the clusters -VisQoL
OHRQoL$clustervis <- getClusters(Ebtraj2,2)
View (OHRQoL)
#write.csv(OHRQoL, 'VISQOLtraj.csv')
```

```{r Data export and merging}
### 4. Export the clusters and create a new dataset that includes the clusters -GOHR
OHRQoL2$clustergohr <- getClusters(Ebtraj3,2)
View (OHRQoL2)
#write.csv(OHRQoL2, 'GOHRtraj.csv')
```


```{r}
##load the data
#setwd("/Users/User/OneDrive - Harvard University/IFS Data - Dr. Ogwo/Wide Datasets")

CPQ_traj<- read_csv("CPQtraj.csv")
VIS_traj<- read_csv("VISQOLtraj.csv")
GOHR_traj<- read_csv("GOHRtraj.csv")
predictors <- read_csv("Overall_Wide_Data modified.csv")

#merge each dependent variable data with the independent variable
CPQtraj_comb_data<- merge(predictors, CPQ_traj, by= "SUBJECT_ID")
VIStraj_comb_data<- merge(predictors, VIS_traj, by= "SUBJECT_ID")
GOHRtraj_comb_data<- merge(predictors, GOHR_traj, by= "SUBJECT_ID")


###Keep rows with at least 31 non missing values
CPQtraj_comb_data<-CPQtraj_comb_data[rowSums(is.na(CPQtraj_comb_data))<(length(CPQtraj_comb_data)-20),]
VIStraj_comb_data<-VIStraj_comb_data[rowSums(is.na(VIStraj_comb_data))<(length(VIStraj_comb_data)-20),]
GOHRtraj_comb_data<-GOHRtraj_comb_data[rowSums(is.na(GOHRtraj_comb_data))<(length(GOHRtraj_comb_data)-20),]

CPQtraj_comb_data
VIStraj_comb_data
GOHRtraj_comb_data

#Export
# write.csv(CPQtraj_comb_data, 'CPQtraj_comb_data.csv')
# write.csv(VIStraj_comb_data, 'VIStraj_comb_data.csv')
# write.csv(GOHRtraj_comb_data, 'GOHRtraj_comb_data.csv')

```


```{r}
#### CPQ - Data cleaning and preparation ####

CPQ_traj<- read_csv("Wide Datasets/CPQtraj_comb_data.csv")

###Subset specific variables 1
CPQtraj1<- subset(CPQtraj_comb_data, select= -c(14:16, 29:31))
##convert the character variables to factor
CPQtraj1[sapply(CPQtraj1, is.character)] <- lapply(CPQtraj1[sapply(CPQtraj1, is.character)], as.factor)
##convert the numeric variables to factor
CPQtraj1[sapply(CPQtraj1, is.numeric)] <- lapply(CPQtraj1[sapply(CPQtraj1, is.numeric)], as.factor)

###Subset specific variables 2
CPQtraj2<- subset(CPQtraj_comb_data, select= c(1,14:16,29:31))
#CPQtraj2

###Merge data
CPQ_traj_final <- merge(CPQtraj2,CPQtraj1, by= "SUBJECT_ID" )

##convert the ID variable to character
CPQ_traj_final$SUBJECT_ID<- as.character(CPQ_traj_final$SUBJECT_ID)

CPQ_traj_final
```



```{r}
###GOHR - Data cleaning and preparation ###

GOHR_traj<- read_csv("Wide Datasets/GOHRtraj_comb_data.csv")
###Subset specific variables 1
GOHRtraj1<- subset(GOHRtraj_comb_data, select= -c(14:16, 29:31))
#GOHRtraj1
##convert the character variables to factor
GOHRtraj1[sapply(GOHRtraj1, is.character)] <- lapply(GOHRtraj1[sapply(GOHRtraj1, is.character)], as.factor)
##convert the numeric variables to factor
GOHRtraj1[sapply(GOHRtraj1, is.numeric)] <- lapply(GOHRtraj1[sapply(GOHRtraj1, is.numeric)], as.factor)

###Subset specific variables 2
GOHRtraj2<- subset(GOHRtraj_comb_data, select= c(1,14:16,29:31))
#GOHRtraj2

###Merge data
GOHR_traj_final <- merge(GOHRtraj2,GOHRtraj1, by= "SUBJECT_ID" )

##convert the ID variable to character
GOHR_traj_final$SUBJECT_ID<- as.character(GOHR_traj_final$SUBJECT_ID)

GOHR_traj_final

```

```{r}
###VISQoL###
VIS_traj<- read_csv("Wide Datasets/VIStraj_comb_data.csv")

###Subset specific variables 1
VIStraj1<- subset(VIStraj_comb_data, select= -c(14:16, 29:31))
#VIStraj1
##convert the character variables to factor
VIStraj1[sapply(VIStraj1, is.character)] <- lapply(VIStraj1[sapply(VIStraj1, is.character)], as.factor)
##convert the character variables to factor
VIStraj1[sapply(VIStraj1, is.numeric)] <- lapply(VIStraj1[sapply(VIStraj1, is.numeric)], as.factor)

###Subset specific variables 2
VIStraj2<- subset(VIStraj_comb_data, select= c(1,14:16,29:31))
#VIStraj2

##Merge data
VIS_traj_final <- merge(VIStraj2,VIStraj1, by= "SUBJECT_ID" )

##convert the ID variable to character
VIS_traj_final$SUBJECT_ID<- as.character(VIS_traj_final$SUBJECT_ID)

VIS_traj_final
```


### CPQ ASSOCIATIVE ANALYSIS WITH DEMOGRAPHICS ###
```{r}

###Univariate
table(CPQ_traj_final$clustercpq,CPQ_traj_final$female)
table(CPQ_traj_final$clustercpq,CPQ_traj_final$SES_2007)


##Bivariate
chisq.test(CPQ_traj_final$female, CPQ_traj_final$clustercpq)
chisq.test(CPQ_traj_final$SES_2007, CPQ_traj_final$clustercpq)

#Multivariable
Reg_CPQ<- glm(clustercpq~female+SES_2007, family = "binomial", data = CPQ_traj_final)
summary(Reg_CPQ)
stepAIC(Reg_CPQ)
```

### GOHR ASSOCIATIVE ANALYSIS WITH DEMOGRAPHICS ###
```{r}

###Univariate
table(GOHR_traj_final$clustergohr,GOHR_traj_final$female)
table(GOHR_traj_final$clustergohr,GOHR_traj_final$SES_2007)


###Bivariate
chisq.test(GOHR_traj_final$female, GOHR_traj_final$clustergohr)
chisq.test(GOHR_traj_final$SES_2007, GOHR_traj_final$clustergohr)


###Multivariate
Reg_GOHR<- glm(clustergohr~female+SES_2007, family = "binomial", data = GOHR_traj_final)
summary(Reg_GOHR)
stepAIC(Reg_GOHR)

```

### VISQoL ASSOCIATIVE ANALYSIS WITH DEMOGRAPHICS ###
```{r}

###Univariate
table(VIS_traj_final$clustervis,VIS_traj_final$female)
table(VIS_traj_final$clustervis,VIS_traj_final$SES_2007)

###Bivariate
chisq.test(VIS_traj_final$female, VIS_traj_final$clustervis)
chisq.test(VIS_traj_final$SES_2007, VIS_traj_final$clustervis)


##Multivariate
Reg_VIS<- glm(clustervis~female+SES_2007, family = "binomial", data = VIS_traj_final)
summary(Reg_VIS)
stepAIC(Reg_VIS)
```
