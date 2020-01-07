###################################################################################################################
################################### IDS 462 MIDTERM PROJECT #######################################################
###################################################################################################################

library(car)
library(gmodels) 
library(tidyverse) 
library(psych) 
library(lubridate)
library(stringr)
library(ggplot2) # already intalled
library(gridExtra)
library(plotly)
library(RColorBrewer)
library(vcd) # already intalled
library(corrplot)
library(ggfortify)

###################################################################################################################
############################## DATA LOADING AND MISSING VALUE IMPUTATION ##########################################
###################################################################################################################

#Loading the data
data_raw_2018 <- read_csv("FEVS_2018_PRDF.csv")
head(data_raw_2018)
str(data_raw_2018)
summary(data_raw_2018)

#Removing unwanted weight variable and replacing missing value in Minority with "OTHER"
data_raw_2018 <- data_raw_2018[ , -c(80)]
#data_raw_2018$DMINORITY[is.na(data_raw_2018$DMINORITY)] <- "Other"

#finding percentage of missing variables for each column and sorting it 
missing_list18 <- colSums(is.na(data_raw_2018))/598003 *100
sort(missing_list18)
max(missing_list18) 
#Highest percentage of missing value is for column "DSEX" and it is only 13.22%. 
#Since all variables have missing values less than 70% we shall retain all the variables in the dataset

data_2018 <- data_raw_2018 %>% drop_na()
nrow(data_2018)/nrow(data_raw_2018)
#if we drop all the NA valued rows, we still retain 70.6% of our data. Hence dropping NA rows

#COLUMNS/QUESTIONS WHICH HAVE SIXTH CATEGORY AS "DON'T KNOW" = c(17:27, 29:35, 37:47, 49:55, 61:67, 69,70 )]
#These "X" values will be replaced with a zero value
data_2018[data_2018 == "X"] <- "0"

data_2018[ ,c(9:80)] <- sapply(data_2018[ ,c(9:80)], as.numeric)
str(data_2018)

for (a in colnames(data_2018)) {
  b <- (0 %in% data_2018[[a]])
  if(b==TRUE) {print(a)}
}
#columns which have zero according to the Excel sheet "2018 PRDF CODEBOOK" are as follows
#Q9:Q19, Q21:Q27, Q29:Q47, Q53:Q62

#HOWEVER......

#columns which have zero according to the dataset are as follows
#Q10	Q11	Q12	Q13	Q14	Q15	Q16	Q17	Q18	Q19	Q21	Q22	Q23	Q24	Q25	Q26	Q27	Q29	Q30	Q31	Q32	Q33	Q34	Q35	
#Q36	Q37	Q38	Q39	Q41	Q42	Q43	Q44	Q45	Q46	Q47	Q53	Q54	Q55	Q56	Q57	Q58	Q59	Q60	Q61	Q62

########## THERE IS A DISPARITY BETWEEN THE BOTH => Q60 is said to have no X values but it does!
#This is imporant while trying to normalize the new variables formed by merging these question variables


######################## repeating the same for 2015 data  #########################################

data_raw_2015 <- read_csv("evs2015_PRDF.csv")

#removing the columns/question which are absent in the 2018 dataset and making the two same dimensional
data_raw_2015 <- data_raw_2015[ , -c(1,4, 76:88, 94:97)]

#imputing the missing values in variable "PLEVEL1" and "DMINORITY" with "Others" to avoid data loss
data_raw_2015$PLEVEL1[is.na(data_raw_2015$PLEVEL1)] <- "Other"
#data_raw_2015$DMINORITY[is.na(data_raw_2015$DMINORITY)] <- "Other"

missing_list15 <- colSums(is.na(data_raw_2015))/421748 * 100
sort(missing_list15)
max(missing_list15)
#Highest percentage of missing value is for column "DSEX" and it is only 7%. 
#Since all variables have missing values less than 70% we shall retain all the variables in the dataset

data_2015 <- data_raw_2015 %>% drop_na()
nrow(data_2015)/nrow(data_raw_2015)
#if we drop all the NA valued rows, we still retain 65.4% of our data. Hence we drop the NA rows

#COLUMNS/QUESTIONS WHICH HAVE SIXTH CATEGORY AS "DON'T KNOW" = c(17:27, 29:35, 37:47, 49:55, 61:67, 69,70 )]
#These "X" values will be replaced with a zero value
data_2015[data_2015 == "X"] <- "0"

data_2015[ ,c(3:73)] <- sapply(data_2015[ ,c(3:73)], as.numeric)
str(data_2015)

for (a in colnames(data_2015)) {
  #print(a)
  b <- (0 %in% data_2015[[a]])
  #print(b)
  if(b==TRUE) {print(a)}
}

#columns which have zero according to the Excel sheet "2018 PRDF CODEBOOK" are as follows
#Q9:Q19, Q21:Q27, Q29:Q47, Q53:Q62

#HOWEVER......

#columns which have zero according to the dataset are as follows
#Q9	Q11	Q12	Q14	Q15	Q16	Q17	Q18	Q19	Q21	Q22	Q23	Q24	Q25	Q26	Q27	Q29	Q30	Q31	Q32	Q33	Q34	Q35
#Q36	Q37	Q38	Q39	Q41	Q42	Q43	Q44	Q45	Q46	Q47	Q53	Q54	Q55	Q56	Q57	Q58	Q59	Q60	Q61	Q62

########## THERE IS A DISPARITY BETWEEN THE BOTH => Q60 is said to have no X values but it does!
#This is imporant while trying to normalize the new variables formed by merging these question variables

#Equalizing the two datasets with variables in the same order, and same names
d2015 <- data_2015[ , c(1,2, 75,77,78,74,76,79,3:73, 80)]
names(d2015)[names(d2015)== "agency"] <- "AGENCY"
names(d2015)[names(d2015)== "PLEVEL1"] <- "LEVEL1"

d2018 <- data_2018

summary(d2015)
summary(d2018)

###################################################################################################################
################################### VARIABLE CREATION BY MERGING Q1 TO Q71 ########################################
###################################################################################################################

#Adding corresponding variables to form new ones 

d2015$PERSONAL <- d2015$Q1+d2015$Q2+d2015$Q3+d2015$Q4+d2015$Q5+d2015$Q6+d2015$Q7+d2015$Q8+d2015$Q9+d2015$Q10+d2015$Q11+
  d2015$Q12+d2015$Q13+d2015$Q14+d2015$Q16+d2015$Q17+d2015$Q18 - 8

d2015$REWARDS <- d2015$Q15+d2015$Q19+d2015$Q22+d2015$Q25+d2015$Q33 -0 

d2015$WORKUNIT <- d2015$Q20+d2015$Q21+d2015$Q23+d2015$Q24+d2015$Q26+d2015$Q27+d2015$Q28 - 1

d2015$AGENCYLEVEL <- d2015$Q29+d2015$Q30+d2015$Q31+d2015$Q32+d2015$Q34+d2015$Q35+d2015$Q36+d2015$Q37+d2015$Q38+d2015$Q39+d2015$Q40+d2015$Q41 -0  

d2015$SUPERVISOR <- d2015$Q42+d2015$Q43+d2015$Q44+d2015$Q45+d2015$Q46+d2015$Q47+d2015$Q48+d2015$Q49+d2015$Q50+d2015$Q51+d2015$Q52 -5

d2015$LEADERSHIP <- d2015$Q53+d2015$Q54+d2015$Q55+d2015$Q56+d2015$Q57+d2015$Q58+d2015$Q59+d2015$Q60+d2015$Q61+d2015$Q62 - 0

d2015$TARGET_SATISFACTION <- d2015$Q63+d2015$Q64+d2015$Q65+d2015$Q66+d2015$Q67+d2015$Q68+d2015$Q69+d2015$Q70+d2015$Q71 - 9

final2015 <- d2015[ ,-c(9:79)] #finally removing old QUESTION variables 

str(final2015)
summary(final2015)


######################################### Repeating for 2018 ################################

#Adding corresponding variables to form new ones 

d2018$PERSONAL <- d2018$Q1+d2018$Q2+d2018$Q3+d2018$Q4+d2018$Q5+d2018$Q6+d2018$Q7+d2018$Q8+d2018$Q9+d2018$Q10+d2018$Q11+
  d2018$Q12+d2018$Q13+d2018$Q14+d2018$Q16+d2018$Q17+d2018$Q18 - 8

d2018$REWARDS <- d2018$Q15+d2018$Q19+d2018$Q22+d2018$Q25+d2018$Q33 -0 

d2018$WORKUNIT <- d2018$Q20+d2018$Q21+d2018$Q23+d2018$Q24+d2018$Q26+d2018$Q27+d2018$Q28 - 1

d2018$AGENCYLEVEL <- d2018$Q29+d2018$Q30+d2018$Q31+d2018$Q32+d2018$Q34+d2018$Q35+d2018$Q36+d2018$Q37+d2018$Q38+d2018$Q39+d2018$Q40+d2018$Q41 -0  

d2018$SUPERVISOR <- d2018$Q42+d2018$Q43+d2018$Q44+d2018$Q45+d2018$Q46+d2018$Q47+d2018$Q48+d2018$Q49+d2018$Q50+d2018$Q51+d2018$Q52 -5

d2018$LEADERSHIP <- d2018$Q53+d2018$Q54+d2018$Q55+d2018$Q56+d2018$Q57+d2018$Q58+d2018$Q59+d2018$Q60+d2018$Q61+d2018$Q62 - 0

d2018$TARGET_SATISFACTION <- d2018$Q63+d2018$Q64+d2018$Q65+d2018$Q66+d2018$Q67+d2018$Q68+d2018$Q69+d2018$Q70+d2018$Q71 - 9

final2018 <- d2018[ ,-c(9:79)] #finally removing old QUESTION variables 

str(final2018)
summary(final2018)

#############################################################################################################
####################################### SELECTING AGENCIES FOR FURTHER ANALYSIS #############################
#############################################################################################################

# Converting the demographic and AGENCY variables into FACTORS 
final2015[ ,c(1:8)] <- lapply(final2015[ ,c(1:8)], as.factor) 
final2018[ ,c(1:8)] <- lapply(final2018[ ,c(1:8)], as.factor)

f18$DLEAVING <-  recode_factor(f18$DLEAVING, "C"="B", "D"="B")
f15$DLEAVING <-  recode_factor(f18$DLEAVING, "C"="B", "D"="B")

# A list of the factors within AGENCY variable that are common for both 2015 and 2018 data
agencies <- c('AF','AG','AM','AR','CM','DD','DJ','DL','DN','ED','EP','FC','GS','HE','HS','HU','IN','NN','NU','NV','OM','SE','ST','SZ','TD','TR')

# creating the subset of data where only the agency factors common to both 2015 and 2018 are present 
final15 <- final2015 %>% group_by(AGENCY) %>% filter(AGENCY ==agencies) %>% droplevels()
final18 <- final2018 %>% group_by(AGENCY) %>% filter(AGENCY ==agencies) %>% droplevels()

#number of levels within agencies for both years is now equal 
levels(final15$AGENCY)
levels(final18$AGENCY)

#this shows the distribution of data within the agency factors 
summary(final15$AGENCY)
summary(final18$AGENCY)

#THIS IS THE FINAL DATA SET WHERE THE NUMBER OF ROWS IS 9480 for 2015 and 15926 for 2018 data
dim(final15)
dim(final18)

#> summary(final15$AGENCY)
# AF   AG   AM   AR   CM   DD   DJ   DL   DN   ED   EP   FC   GS   HE   HS   HU   IN   NN   NU   NV   OM   SE 
# 468  516   48  536  263  397  496  273  217   70  116   11  195  878 1258  126  679  224   74  461   84   41 
# ST   SZ   TD   TR 
# 110  271  404 1264 
# > summary(final18$AGENCY)
# AF   AG   AM   AR   CM   DD   DJ   DL   DN   ED   EP   FC   GS   HE   HS   HU   IN   NN   NU   NV   OM   SE 
# 896 1216   46 2004  597  820  816  185  236   55  184   11  198 1214 2054  109  795  310   60 1352   72   89 
# ST   SZ   TD   TR 
# 172  714  603 1118

final15$AGENCY <- reorder(final15$AGENCY, final15$AGENCY, FUN = length)
levels(final15$AGENCY)
summary(final15$AGENCY)

final18$AGENCY <- reorder(final18$AGENCY, final18$AGENCY, FUN = length)
levels(final18$AGENCY)
summary(final18$AGENCY)


############ next step - choose the agencies and move ahead with the analysis 
########Comparing the 2018 datasets and 2015 datasets we are selecting the same agencies that
####### have high responses on both the datasets. We choose Agencies 'TR', 'HS', 'HE', 'AR' that 
####### belongs to the 'Very Large Agency' category and 'IN' that belongs to the 'Large Agencies'
###### category so that we can obtain a balanced dataset for both 2015 and 2018

#Subsetting based on the top 5 agencies in 2018
f18 <- final18 %>% filter(AGENCY=='TR' | AGENCY=='HS' |AGENCY=='HE'| AGENCY=='AR'|AGENCY=='IN') %>% droplevels()
dim(f18) #We are left with 7185 rows for 2018

#Subsetting based on the top 5 agencies in 2015
f15 <- final15 %>% filter(AGENCY=='TR' | AGENCY=='HS' |AGENCY=='HE'| AGENCY=='AR'|AGENCY=='IN') %>% droplevels()
dim(f15) #We are left with 4615 rows for year 2015



############################################### UNIVARIATE ANALYSIS ################################################

########UNIVARIATE ANALYSIS FOR Numerical Variables


#########2018 Dataset

#1)Analysis for Variable TARGET_SATISFACTION
#STEP 1: Univariate Analysis for TARGET_SATISFACTION

par(mfrow=c(2,2))

hist(f18$TARGET_SATISFACTION, 
     freq=F, #Frequency as False
     col="steelblue", 
     breaks=10, 
     xlab="Satisfaction",
     main="Target Satisfaction 2018")

rug(jitter(f18$TARGET_SATISFACTION), col="darkgrey")
lines(density(f18$TARGET_SATISFACTION), col="red", lwd=3) 
box(lwd=2)

boxplot(f18$TARGET_SATISFACTION, col="orange", main="Satisfaction Level in 2018")
#We observe almost normal distribution with the Target Variable, hence we are proceeding with
#this distribution for the target variable TARGET_SATISFACTION



#2)Analysis for Variable PERSONAL

#STEP 1: Plotting the distribution of the variable PERSONAL

par(mfrow=c(2,2))

hist(f18$PERSONAL, main="Distr of PERSONAL 2018", freq=F, col="grey") 
lines(density(f18$PERSONAL), main="Distribution of PERSONAL", col="yellow", lwd=2)
boxplot(f18$PERSONAL, main="Boxplot of PERSONAL", col="coral")
#We see that the Distribution of variable PERSONAL is having a left tail 
#We considered removing the outliers for making the distribution normal, but it 
#results in elimination of around 100 rows
#By Variable Transformation, we see a normal distribution for this variable
#Therefore, we will be Squaring this variable and use this for our analysis


f18$sqrPERSONAL <- f18$PERSONAL * f18$PERSONAL

#Observing the distribution using Histogram and Barplot
hist(f18$sqrPERSONAL, main="Distr of sqrPERSONAL'18", freq=F, col="grey") 
lines(density(f18$sqrPERSONAL), main="Distribution of sqrPERSONAL", col="yellow", lwd=2)
boxplot(f18$sqrPERSONAL, main="Boxplot of sqrPERSONAL", col="coral")

#We see a normal distribution for the variable sqrPERSONAL and will be using this for further analysis
#No outliers present in the data

#3)Analysis for Variable REWARDS

#STEP 1: Plotting the distribution of the variable REWARDS

#REWARDS
#Checking for outliers for Reward
reward_lower_outliers <- fivenum(f18$REWARDS)[2]- IQR(f18$REWARDS)*1.5
reward_lower_outliers
which(f18$REWARDS<=reward_lower_outliers) #We find that there are 7 Lower Outliers
reward_higher_outliers <- fivenum(f18$REWARDS)[4]+IQR(f18$REWARDS)*1.5
reward_higher_outliers
which(f18$REWARDS>=reward_higher_outliers) #No outliers

#Removing the Outliers from Reward
f18 <- f18 %>% filter(REWARDS> reward_lower_outliers)

hist(f18$REWARDS, main="Distr of REWARDS 2018", freq=F, col="steelblue") # the ylim argument allows the peak of the curve to show. 
lines(density(f18$REWARDS), main="Distr of REWARDS 2018", col="red", lwd=2)
boxplot(f18$REWARDS, main="Boxplot of REWARDS", col="coral")


dim(f18)
#We observe a better normal distribution with Rewards
#Hence we will now proceed with the remaining 7178 rows


#4) ANALYSIS FOR Variable WORKUNIT
#STEP 1: Plotting the distribution of the variable REWARDS

#WORKUNIT

lower_outliers <- fivenum(f18$WORKUNIT)[2]- IQR(f18$WORKUNIT)*1.5
lower_outliers
which(f18$WORKUNIT<=lower_outliers)
table(f18$WORKUNIT<= lower_outliers)


f18 <-  f18 %>% filter(WORKUNIT>lower_outliers)
#Removing 55 Outliers , so that we get a normal distribution for WORKUNIT 
dim(f18)


hist(f18$WORKUNIT, main="Distr of WORKUNIT 2018", freq=F, col="steelblue") 
lines(density(f18$WORKUNIT), main="Distr of WORKUNIT 2018", col="red", lwd=2)
boxplot(f18$WORKUNIT, main="Boxplot of WORKUNIT", col="coral")


#5) ANALYSIS FOR Variable AGENCYLEVEL
#AGENCYLEVEL


f18$sqrAGENCYLEVEL <- f18$AGENCYLEVEL * f18$AGENCYLEVEL

hist(f18$sqrAGENCYLEVEL, main="Distr of AGENCYLEVEL '18", freq=F, col="steelblue") # the ylim argument allows the peak of the curve to show. 
lines(density(f18$sqrAGENCYLEVEL), main="Distr of AGENCYLEVEL '18", col="red", lwd=2)
boxplot(f18$sqrAGENCYLEVEL, main="Boxplot of AGENCYLEVEL", col="coral")


#6) ANALYSIS FOR Variable LEADERSHIP

#LEADERSHIP

hist(f18$LEADERSHIP, main="Distr of LEADERSHIP '18", freq=F, col="steelblue") # the ylim argument allows the peak of the curve to show. 
lines(density(f18$LEADERSHIP), main="Distr of LEADERSHIP '18", col="red", lwd=2)
boxplot(f18$LEADERSHIP, main="Boxplot of LEADERSHIP", col="coral")

lower_outliers <- fivenum(f18$LEADERSHIP)[2]- IQR(f18$LEADERSHIP)*1.5
lower_outliers
which(f18$LEADERSHIP<=lower_outliers) #Found no outliers
table(f18$LEADERSHIP<= lower_outliers)

#Removing the Outliers from Leadership
f18 <- f18 %>% filter(LEADERSHIP> lower_outliers)
dim(f18) #The data size reduces to 4519



################### 2015 Dataset
#1)Analysis for Variable TARGET_SATISFACTION
#STEP 1: Univariate Analysis for TARGET_SATISFACTION

par(mfrow=c(2,2))

hist(f15$TARGET_SATISFACTION, 
     freq=F, #Frequency as False
     col="steelblue", 
     breaks=10, 
     xlab="Satisfaction",
     main="Target Satisfaction 2015")

rug(jitter(f15$TARGET_SATISFACTION), col="darkgrey")
lines(density(f15$TARGET_SATISFACTION), col="red", lwd=3) 
box(lwd=2)

boxplot(f15$TARGET_SATISFACTION, col="orange", main="Satisfaction Level in 2015")
#We observe almost normal distribution with the Target Variable, hence we are proceeding with
#this distribution for the target variable TARGET_SATISFACTION



#2)Analysis for Variable PERSONAL

#STEP 1: Plotting the distribution of the variable PERSONAL

hist(f15$PERSONAL, main="Distr of PERSONAL 2015", freq=F, col="grey") 
lines(density(f15$PERSONAL), main="Distribution of PERSONAL", col="yellow", lwd=2)
boxplot(f15$PERSONAL, main="Boxplot of PERSONAL", col="coral")
#We see that the Distribution of variable PERSONAL is having a left tail 
#We considered removing the outliers for making the distribution normal, but it 
#results in elimination of around 100 rows
#By Variable Transformation, we see a normal distribution for this variable
#Therefore, we will be Squaring this variable and use this for our analysis


f15$sqrPERSONAL <- f15$PERSONAL * f15$PERSONAL

#Observing the distribution using Histogram and Barplot
hist(f15$sqrPERSONAL, main="Distr of sqrPERSONAL 15", freq=F, col="grey") 
lines(density(f15$sqrPERSONAL), main="Distr of sqrPERSONAL 15", col="yellow", lwd=2)
boxplot(f15$sqrPERSONAL, main="Boxplot of sqrPERSONAL", col="coral")

#We see a normal distribution for the variable sqrPERSONAL and will be using this for further analysis


#3)Analysis for Variable REWARDS

#STEP 1: Plotting the distribution of the variable REWARDS

#REWARDS
hist(f15$REWARDS, main="Distr of REWARDS 2015", freq=F, col="steelblue") # the ylim argument allows the peak of the curve to show. 
lines(density(f15$REWARDS), main="Distr of REWARDS 2015", col="red", lwd=2)
boxplot(f15$REWARDS, main="Boxplot of REWARDS", col="coral")

#Checking for outliers for Reward
reward_lower_outliers <- fivenum(f15$REWARDS)[2]- IQR(f15$REWARDS)*1.5
reward_lower_outliers
which(f15$REWARDS<=reward_lower_outliers) #We find that there are 6 Lower Outliers
reward_higher_outliers <- fivenum(f15$REWARDS)[4]+IQR(f15$REWARDS)*1.5
reward_higher_outliers
which(f15$REWARDS>=reward_higher_outliers) #No outliers

#Removing the Outliers from Reward
f15 <- f15 %>% filter(REWARDS> reward_lower_outliers)

dim(f15)
#We observe a better normal distribution with Rewards
#Hence we will now proceed with the remaining 4609 rows


#4) ANALYSIS FOR Variable WORKUNIT
#STEP 1: Plotting the distribution of the variable REWARDS

#WORKUNIT

hist(f15$WORKUNIT, main="Distr of WORKUNIT 2015", freq=F, col="steelblue") # the ylim argument allows the peak of the curve to show. 
lines(density(f15$WORKUNIT), main="Distr of WORKUNIT 2015", col="red", lwd=2)
boxplot(f15$WORKUNIT, main="Boxplot of WORKUNIT", col="coral")


#5) ANALYSIS FOR Variable AGENCYLEVEL
#AGENCYLEVEL
lower_outliers <- fivenum(f15$AGENCYLEVEL)[2]- IQR(f15$AGENCYLEVEL)*1.5
lower_outliers
which(f15$AGENCYLEVEL<=lower_outliers) #Found no outliers
table(f15$AGENCYLEVEL<= lower_outliers)

#Removing the Outliers from AgencyLevel
f15 <- f15 %>% filter(AGENCYLEVEL> lower_outliers)
dim(f15) #The data size reduces to 4562

hist(f15$AGENCYLEVEL, main="Distribution of Target", freq=F, col="steelblue") # the ylim argument allows the peak of the curve to show. 
lines(density(f15$AGENCYLEVEL), main="Distribution of price", col="red", lwd=2)
boxplot(f15$AGENCYLEVEL, main="Boxplot of price", col="coral")

#Plotting the squared value instead, we see that 
f15$sqrAGENCYLEVEL <- f15$AGENCYLEVEL * f15$AGENCYLEVEL

hist(f15$sqrAGENCYLEVEL, main="Distr of AGENCYLEVEL '15", freq=F, col="steelblue") # the ylim argument allows the peak of the curve to show. 
lines(density(f15$sqrAGENCYLEVEL), main="Distr of AGENCYLEVEL '15", col="red", lwd=2)
boxplot(f15$sqrAGENCYLEVEL, main="Boxplot of AGENCYLEVEL", col="coral")


#LEADERSHIP
hist(f15$LEADERSHIP, main="Distr of LEADERSHIP '15", freq=F, col="steelblue") # the ylim argument allows the peak of the curve to show. 
lines(density(f15$LEADERSHIP), main="Distr of LEADERSHIP '15", col="red", lwd=2)
boxplot(f15$LEADERSHIP, main="Boxplot of LEADERSHIP", col="coral")


# 2018 Univariate Analysis

table(f18$AGENCY)

#Responses by Agency 
#The Homeland department and Army department has responded the most at the agency level
ggplot(data=f18, aes(AGENCY)) + 
  geom_bar(fill="steelblue") + theme_minimal()+
  labs(x = "", title='Responses by Agency (2018)') +
  scale_x_discrete(labels = c('Interiors','Treasury', 'Health', 'Army', 'Homeland'))


#Responses by Gender
#More number of Male employees who have responded
table(f18$DSEX)
ggplot(data=f18, aes(DSEX)) + 
  geom_bar(fill="steelblue") + theme_minimal()+
  labs(x= "", title='Respondants by Gender (2018)') +
  scale_x_discrete(labels = c('Male','Female'))



#Responses by Education Level
#Almost same proportion of employees with Bachelors degree and Above have responded to survey
table(f18$DEDUC)
ggplot(data=f18, aes(DEDUC)) + 
  geom_bar(fill="steelblue") + theme_minimal()+
  labs(x = "", title='Education level among respondants (2018)') +
  scale_x_discrete(labels = c('Less than Bachelors','Bachelors', 'Above Bachelors'))


#Responses by Tenure in Federal Department
#People with less experience have responded to the survey
table(f18$DFEDTEN)
ggplot(data=f18, aes(DFEDTEN)) + 
  geom_bar(fill="steelblue") + theme_minimal()+
  labs(x = "", title='Tenure in Federal Dept. (2018)') +
  scale_x_discrete(labels = c('Less than 10 Yrs','10 to 20 Yrs', 'More than 20 Yrs'))


#Responses by Supervisors
#Most Employees who have responded to survey are Supervisors
table(f18$DSUPER)
ggplot(data=f18, aes(DSUPER)) + 
  geom_bar(fill="steelblue") + theme_minimal()+
  labs(x = "", title='Supervisory Status (2018)') +
  scale_x_discrete(labels = c('Non-Supervisor','Supervisor'))


#Responses by Employees based on Minority Status
#Most employees belong to Non Minority Group
table(f18$DMINORITY)
ggplot(data=f18, aes(DMINORITY)) + 
  geom_bar(fill="steelblue") + theme_minimal()+
  labs(x = "", title='Minority Designation (2018)') +
  scale_x_discrete(labels = c('Minority','Non-Minority'))


#Responses by Employees based on 
#Whether they are leaving the agency or not

table(f18$DLEAVING)
ggplot(data=f18, aes(DMINORITY)) + 
  geom_bar(fill="steelblue") + theme_minimal()+
  labs(x = "", title='Plans to leave Fed job in the next 1 year (2018)') +
  scale_x_discrete(labels = c('No','Yes'))


##################### 2015 Univariate Analysis ###############################

#Agency 
#The Homeland department and Treasury department has most number of employees responded to survey
table(f15$AGENCY)
ggplot(data=f15, aes(AGENCY)) + 
  geom_bar(fill="steelblue") + theme_minimal()+
  labs(x = "", title='Responses by Agency (2015)') +
  scale_x_discrete(labels = c('Army','Interiors', 'Health', 'Homeland', 'Treasury'))


#Gender
#There is an equal proportion of male and female employees who responded to the survey
table(f15$DSEX)
ggplot(data=f15, aes(DSEX)) + 
  geom_bar(fill="steelblue") + theme_minimal()+
  labs(x= "", title='Respondants by Gender (2015)') +
  scale_x_discrete(labels = c('Male','Female'))

#Eductaion Level
#There is an equal proportion of employees in all education levels
table(f15$DEDUC)
ggplot(data=f15, aes(DEDUC)) + 
  geom_bar(fill="steelblue") + theme_minimal()+
  labs(x = "", title='Education level among respondants (2015)') +
  scale_x_discrete(labels = c('Less than Bachelors','Bachelors', 'Above Bachelors'))


#Tenure in Federal Govt
#There are more senior level employeers in the response dataset of 2015
table(f15$DFEDTEN)
ggplot(data=f15, aes(DFEDTEN)) + 
  geom_bar(fill="steelblue") + theme_minimal()+
  labs(x = "", title='Tenure in Federal Dept. (2015)') +
  scale_x_discrete(labels = c('Less than 10 Yrs','10 to 20 Yrs', 'More than 20 Yrs'))

#SUPERVISOR

table(f15$DSUPER)
ggplot(data=f15, aes(DSUPER)) + 
  geom_bar(fill="steelblue") + theme_minimal()+
  labs(x = "", title='Supervisory Status (2015)') +
  scale_x_discrete(labels = c('Non-Supervisor','Supervisor'))


table(f15$DMINORITY)
ggplot(data=f15, aes(DMINORITY)) + 
  geom_bar(fill="steelblue") + theme_minimal()+
  labs(x = "", title='Minority Designation (2015)') +
  scale_x_discrete(labels = c('Minority','Non-Minority'))


table(f15$DLEAVING)
ggplot(data=f15, aes(DLEAVING)) + 
  geom_bar(fill="steelblue") + theme_minimal()+
  labs(x = "", title='Plans to leave Fed job in the next 1 year (2015)') +
  scale_x_discrete(labels = c('No','Yes'))


##### Correlation Matrix #######

numcols <- c("sqrPERSONAL", "REWARDS", "WORKUNIT", "sqrAGENCYLEVEL", "SUPERVISOR", "LEADERSHIP", "TARGET_SATISFACTION")
cormat <- cor(f18[,numcols])
round(cormat,2)

corrplot(cormat, method="circle", addCoef.col="grey", type="upper") 


################################################# Bivariate Analysis ###################################################

######################################## Numeric Data (2015) #######################################################

#We can make a hypothesis that there exists a linear relationship between 
#PERSONAL and Target_Satisfaction 
#in 2015 dataset
personalscatter15 <- ggplot(data=f15) + aes(x=sqrPERSONAL, y=TARGET_SATISFACTION) +
  geom_point(pch=16, color="coral") +
  labs(title='Relationship between Satisfaction and Personal Experience (2015)',
       x="Personal", y="Satisfaction") # x for xlab, y for ylab
personalscatter15 + geom_smooth(method="lm", color="black", lwd=2) 


#We can make a hypothesis that there exists a linear relationship between 
#REWARDS and Target_Satisfaction 
#in 2015 dataset
rewardscatter15 <- ggplot(data=f15) + aes(x=REWARDS, y=TARGET_SATISFACTION) +
  geom_point(pch=16, color="coral") +
  labs(title='Relationship between Satisfaction and Rewards (2015)',
       x="Rewards", y="Satisfaction") # x for xlab, y for ylab
rewardscatter15 + geom_smooth(method="lm", color="black", lwd=2) 


#We can make a hypothesis that there exists a linear relationship between 
#AGENCYLEVEL and Target_Satisfaction 
#in 2015 dataset
alscatter15 <- ggplot(data=f15) + aes(x=AGENCYLEVEL, y=TARGET_SATISFACTION) +
  geom_point(pch=16, color="coral") +
  labs(title='Relationship between Satisfaction and Agency Level experience (2015)',
       x="Agency_Level", y="Satisfaction") # x for xlab, y for ylab
alscatter15 + geom_smooth(method="lm", color="black", lwd=2) 


#We can make a hypothesis that there exists a linear relationship between 
#WORKUNIT and Target_Satisfaction 
#in 2015 dataset
workscatter15 <- ggplot(data=f15) + aes(x=WORKUNIT, y=TARGET_SATISFACTION) +
  geom_point(pch=16, color="coral") +
  labs(title='Relationship between Satisfaction and Work Unit experience (2015)',
       x="Work Unit", y="Satisfaction") # x for xlab, y for ylab
workscatter15 + geom_smooth(method="lm", color="black", lwd=2) 


#We can make a hypothesis that there exists a linear relationship between 
#SUPERVISOR and Target_Satisfaction 
#in 2015 dataset
supervisor_scatter15 <- ggplot(data=f15) + aes(x=SUPERVISOR, y=TARGET_SATISFACTION) +
  geom_point(pch=16, color="coral") +
  labs(title='Relationship between Satisfaction and Supervisor behaviour (2015)',
       x="Supervisor", y="Satisfaction") # x for xlab, y for ylab
supervisor_scatter15 + geom_smooth(method="lm", color="black", lwd=2)  


#We can make a hypothesis that there exists a linear relationship between 
#LEADERSHIP and Target_Satisfaction 
#in 2015 dataset
leadership_scatter15 <- ggplot(data=f15) + aes(x=LEADERSHIP, y=TARGET_SATISFACTION) +
  geom_point(pch=16, color="coral") +
  labs(title='Relationship between Satisfaction and Leadership behaviour (2015)',
       x="Leadership", y="Satisfaction") # x for xlab, y for ylab
leadership_scatter15 + geom_smooth(method="lm", color="black", lwd=2) 



################################# Satisfaction lavels by Factor Variables ########################
################### 2018 #########################################################################


# comparative boxplot
#AGENCY vs TARGET_SATISFACTION

satisfybyagency <- ggplot(f18, aes(x=AGENCY, y=TARGET_SATISFACTION))
satisfybyagency + geom_boxplot()+
  labs(title='Satisfaction levels by Agencies (2018)', x='') +
  scale_x_discrete(labels = c('Interiors','Treasury', 'Health', 'Army', 'Homeland'))
#Health Agency has the highest satisfaction level out of all 5 agencies


#DSEX vs TARGET_SATISFACTION
satisfactionbygender <- ggplot(f18, aes(x=DSEX, y=TARGET_SATISFACTION))
satisfactionbygender + geom_boxplot() +
  labs(title='Satisfaction levels between genders (2018)', x='') +
  scale_x_discrete(labels = c('Male','Female'))
#We do not see any significance variation when comparing Female and Male for 2018 data



#DFEDTEN vs TARGET_SATISFACTION
satisfactionbytenure <- ggplot(f18, aes(x=DFEDTEN, y=TARGET_SATISFACTION))
satisfactionbytenure + geom_boxplot() +
  labs(title='Satisfaction levels at various tenures (2018)', x='') +
  scale_x_discrete(labels = c('Less than 10 Yrs','10 to 20 Yrs', 'More than 20 Yrs'))
#People with higher experience seems to have more satisfaction in 2018



#DEDUC VS TARGET_SATISFACTION
satisfactionbyeducation <- ggplot(f18, aes(x=DEDUC, y=TARGET_SATISFACTION))
satisfactionbyeducation + geom_boxplot() +
  labs(title='Satisfaction levels at Education Levels (2018)', x='') +
  scale_x_discrete(labels = c('Below Bachelors','Bachelors', 'Above Bachelors'))
#We do not see any significance variation between the levels


#DSUPER VS TARGET_SATISFACTION
satisfactionbysupervisor <- ggplot(f18, aes(x=DSUPER, y=TARGET_SATISFACTION))
satisfactionbysupervisor + geom_boxplot() +
  labs(title='Satisfaction levels at for Supervisors (2018)', x='') +
  scale_x_discrete(labels = c('Non-Supervisor', 'Supervisor'))
#Supervisors are having more satisfaction than non supervisors


#DMINORITY VS TARGET_SATISFACTION
satisfactionby_minority <- ggplot(f18, aes(x=DMINORITY, y=TARGET_SATISFACTION))
satisfactionby_minority + geom_boxplot() +
  labs(title='Satisfaction levels for Minority Groups (2018)', x='') +
  scale_x_discrete(labels = c('Minority', 'Non-Minority'))
#We do not see any significance variation between the levels


#DLEAVING VS TARGET_SATISFACTION 
satisfactionby_leaving <- ggplot(f18, aes(x=DLEAVING, y=TARGET_SATISFACTION))
satisfactionby_leaving + geom_boxplot() +
  labs(title='Satisfaction levels for with people plans to leave (2018)', x='') +
  scale_x_discrete(labels = c('No', 'Yes'))


#ANOVA for 
aov.mod1 <- aov(TARGET_SATISFACTION~DSEX, data=f18)
summary(aov.mod1) 

aov.mod2 <- aov(TARGET_SATISFACTION~DFEDTEN, data=f18)
summary(aov.mod2) 
aov.mod4 <- aov(TARGET_SATISFACTION~DEDUC, data=f18)
summary(aov.mod4)

aov.mod3 <- aov(TARGET_SATISFACTION~DSUPER, data=f18)
summary(aov.mod3)

aov.mod5 <- aov(TARGET_SATISFACTION~DMINORITY, data=f18)
summary(aov.mod5)

aov.mod6 <- aov(TARGET_SATISFACTION~DLEAVING, data=f18)
summary(aov.mod6)



############################################ 2015 ###############################################################


# comparative boxplot
#AGENCY vs TARGET_SATISFACTION

satisfybyagency15 <- ggplot(f15, aes(x=AGENCY, y=TARGET_SATISFACTION))
satisfybyagency15 + geom_boxplot()+
  labs(title='Satisfaction levels by Agencies (2015)', x='') +
  scale_x_discrete(labels = c('Interiors','Treasury', 'Health', 'Army', 'Homeland'))
#Health Agency has the highest satisfaction level out of all 5 agencies


#DSEX vs TARGET_SATISFACTION
satisfactionbygender15 <- ggplot(f15, aes(x=DSEX, y=TARGET_SATISFACTION))
satisfactionbygender15 + geom_boxplot() +
  labs(title='Satisfaction levels between genders (2015)', x='') +
  scale_x_discrete(labels = c('Male','Female'))
#We do not see any significance variation between the levels


#DFEDTEN vs TARGET_SATISFACTION
satisfactionby_tenure15 <- ggplot(f15, aes(x=DFEDTEN, y=TARGET_SATISFACTION))
satisfactionby_tenure15 + geom_boxplot() +
  labs(title='Satisfaction levels at various tenures (2015)', x='') +
  scale_x_discrete(labels = c('Less than 10 Yrs','10 to 20 Yrs', 'More than 20 Yrs'))
#We do not see any significance variation between the levels


#DSUPER vs TARGET_SATISFACTION
satisfactionby_supervisor15 <- ggplot(f15, aes(x=DSUPER, y=TARGET_SATISFACTION))
satisfactionby_supervisor15 + geom_boxplot() +
  labs(title='Satisfaction levels at for Supervisors (2015)', x='') +
  scale_x_discrete(labels = c('Non-Supervisor', 'Supervisor'))
#Supervisors are having more satisfaction than non supervisors


#DEDUC vs TARGET_SATISFACTION
satisfactionby_education15 <- ggplot(f15, aes(x=DEDUC, y=TARGET_SATISFACTION))
satisfactionby_education15 + geom_boxplot() +
  labs(title='Satisfaction levels at Education Levels (2015)', x='') +
  scale_x_discrete(labels = c('Below Bachelors','Bachelors', 'Above Bachelors'))
#We do not see any significance variation between the levels


#DMINORITY vs TARGET_SATISFACTION
satisfactionby_minority15 <- ggplot(f15, aes(x=DMINORITY, y=TARGET_SATISFACTION))
satisfactionby_minority15 + geom_boxplot() +
  labs(title='Satisfaction levels for Minority Groups (2015)', x='') +
  scale_x_discrete(labels = c('Minority', 'Non-Minority'))
#We do not see any significance variation between the levels


#DLEAVING vs TARGET_SATISFACTION
f15$DLEAVING <-  recode_factor(f15$DLEAVING, "C"="B", "D"="B")
satisfactionby_leaving_15 <- ggplot(f15, aes(x=DLEAVING, y=TARGET_SATISFACTION))
satisfactionby_leaving_15 + geom_boxplot() +
  labs(title='Satisfaction levels for with people plans to leave (2015)', x='') +
  scale_x_discrete(labels = c('No', 'Yes'))
#Employees who are Leaving

#ANOVA test for TARGET_SATISFACTION and Gender
aov.mod1_15 <- aov(TARGET_SATISFACTION~DSEX, data=f15)
summary(aov.mod1_15) #Less significance, hence not taking this variable into account

#ANOVA test for TARGET_SATISFACTION and DLEAVING
aov.mod6_15 <- aov(TARGET_SATISFACTION~DLEAVING, data=f15)
summary(aov.mod6_15) #Statistically significant, therefore some relationship exists

#ANOVA test for TARGET_SATISFACTION and DFEDTEN
aov.mod2_15 <- aov(TARGET_SATISFACTION~DFEDTEN, data=f15)
summary(aov.mod2_15)  #Statistically significant, therefore some relationship exists

#ANOVA test for TARGET_SATISFACTION and DEDUC
aov.mod4_15 <- aov(TARGET_SATISFACTION~DEDUC, data=f15)
summary(aov.mod4_15) #Statistically significant, therefore some relationship exists

#ANOVA test for TARGET_SATISFACTION and DSUPER
aov.mod3_15 <- aov(TARGET_SATISFACTION~DSUPER, data=f15)
summary(aov.mod3_15) #Statistically significant, therefore some relationship exists

#ANOVA test for TARGET_SATISFACTION and DMINORITY
aov.mod5_15 <- aov(TARGET_SATISFACTION~DMINORITY, data=f15)
summary(aov.mod5_15) #Statistically significant, therefore some relationship exists







################################################## Linear Modelling #################################################

##############################3###############2018 Data######################################################

#Building the model using all the variables for 2018 data
options(scipen = 99)
m1 <- lm(TARGET_SATISFACTION ~ sqrPERSONAL + REWARDS + WORKUNIT + sqrAGENCYLEVEL + SUPERVISOR + LEADERSHIP + AGENCY + DSEX + DEDUC + DEDUC + DFEDTEN + DSUPER + DMINORITY + DLEAVING, data = f18)
summary(m1)
vif(m1)

par(mfrow=c(2,2))
plot(m1)

# Thus we can conclude that on an average, with every one point change in PERSONAL experience factors, 
# there is a 0.57 point change in the satisfaction experience of the employee.

names(f18)
m2 <- lm(TARGET_SATISFACTION ~ sqrPERSONAL + REWARDS + WORKUNIT + sqrAGENCYLEVEL + SUPERVISOR + LEADERSHIP, data = f18)
summary(m2)


par(mfrow=c(2,2))
plot(m2)

m3 <- lm(TARGET_SATISFACTION ~ AGENCY + DSEX + DEDUC + DFEDTEN + DSUPER + DMINORITY + DLEAVING, data = f18)
summary(m3)

par(mfrow=c(2,2))
plot(m3)


m4 <- lm(TARGET_SATISFACTION ~ REWARDS, data = f18)
summary(m4)
plot(m4)

m5 <- lm(TARGET_SATISFACTION ~ LEADERSHIP, data = f18)
summary(m5)
plot(m5)

WORKUNIT
m6 <- lm(TARGET_SATISFACTION ~ WORKUNIT, data = f18)
summary(m6)
plot(m6)

WORKUNIT
m7 <- lm(TARGET_SATISFACTION ~ sqrPERSONAL, data = f18)
summary(m7)
plot(m7)

m8 <- lm(TARGET_SATISFACTION ~ DSUPER, data = f18)
summary(m8)
plot(m8)

m9 <- lm(TARGET_SATISFACTION ~ sqrPERSONAL + REWARDS + WORKUNIT + SUPERVISOR + LEADERSHIP, data = f18)
summary(m9)

par(mfrow=c(2,2))
plot(m9)





################################################# 2015 Linear Model ################################################
m1_15 <- lm(TARGET_SATISFACTION ~ sqrPERSONAL + REWARDS + WORKUNIT + sqrAGENCYLEVEL + SUPERVISOR + LEADERSHIP + AGENCY + DSEX + DEDUC + DEDUC + DFEDTEN + DSUPER + DMINORITY + DLEAVING, data = f15)
summary(m1_15)

par(mfrow=c(2,2))
plot(m1_15)

# Thus we can conclude that on an average, with every one point change in PERSONAL experience factors, 
# there is a 0.57 point change in the satisfaction experience of the employee.

m2_15 <- lm(TARGET_SATISFACTION ~ sqrPERSONAL + REWARDS + WORKUNIT + sqrAGENCYLEVEL + SUPERVISOR + LEADERSHIP, data = f15)
summary(m2_15)

plot(m2_15)

f15$DFEDTEN <- relevel(f15$DFEDTEN, ref = 3)
levels(f15$DFEDTEN)

f15$DMINORITY <- relevel(f15$DMINORITY, ref = 2)
levels(f15$DMINORITY)

f15$DLEAVING <- relevel(f15$DLEAVING, ref = 2)
levels(f15$DLEAVING)

m3_15 <- lm(TARGET_SATISFACTION ~ AGENCY + DSEX + DEDUC + DFEDTEN + DSUPER + DMINORITY + DLEAVING, data = f15)
summary(m3_15)

par(mfrow=c(2,2))
plot(m3_15)


m4_15 <- lm(TARGET_SATISFACTION ~ REWARDS, data = f15)
summary(m4_15)
plot(m4_15)

m5_15 <- lm(TARGET_SATISFACTION ~ LEADERSHIP, data = f15)
summary(m5_15)
plot(m5)

WORKUNIT
m6 <- lm(TARGET_SATISFACTION ~ WORKUNIT, data = f18)
summary(m6)
plot(m6)

WORKUNIT
m7_15 <- lm(TARGET_SATISFACTION ~ sqrPERSONAL, data = f15)
summary(m7_15)
plot(m7)

m8_15 <- lm(TARGET_SATISFACTION ~ DSUPER, data = f15)
summary(m8_15)
plot(m8)

m9 <- lm(TARGET_SATISFACTION ~ sqrPERSONAL + REWARDS + WORKUNIT + SUPERVISOR + LEADERSHIP, data = f18)
summary(m9)



#####################################################################################################################
############Satisfaction comparison for 2015 model and 2018 model####################################################
#####################################################################################################################


m11 <- lm(TARGET_SATISFACTION~sqrAGENCYLEVEL, data = f18)
coef(m11)

m11_15 <- lm(TARGET_SATISFACTION~sqrAGENCYLEVEL, data = f15)
coef(m11_15)

View(f18)
names(f18)

m12 <- lm(TARGET_SATISFACTION~sqrPERSONAL, data = f18)
coef(m12)

m12_15 <- lm(TARGET_SATISFACTION~sqrPERSONAL, data = f15)
coef(m12_15)

m13 <- lm(TARGET_SATISFACTION~REWARDS, data = f18)
coef(m13)

m13_15 <- lm(TARGET_SATISFACTION~REWARDS, data = f15)
coef(m13_15)

m14 <- lm(TARGET_SATISFACTION~WORKUNIT, data = f18)
coef(m14)

m14_15 <- lm(TARGET_SATISFACTION~WORKUNIT, data = f15)
coef(m14_15)

m15 <- lm(TARGET_SATISFACTION~SUPERVISOR, data = f18)
coef(m15)

m15_15 <- lm(TARGET_SATISFACTION~SUPERVISOR, data = f15)
coef(m15_15)

m16 <- lm(TARGET_SATISFACTION~LEADERSHIP, data = f18)
coef(m16)

m16_15 <- lm(TARGET_SATISFACTION~LEADERSHIP, data = f15)
coef(m16_15)





