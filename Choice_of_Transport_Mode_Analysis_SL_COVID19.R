# Categorical data
# Choice of Tranport Mode After Locdown_Coded suvey data
library(readxl)
library(dplyr)
library(tidyverse)
library(ggplot2)
library(plyr)
# After COVID how they would travel

# Choice_of_Transport_Mode_After_Lockdown_coded.xlsx
#dataCTMSL <- read_excel('Downloads/Choice_of_Transport_Mode_After_Lockdown_coded.xlsx',sheet = "Choice ofTransport Mode After")
dataCTMSL <- read_excel('Downloads/Choice_of_Transport_Mode_After_Lockdown_coded_T.xlsx')
dataCTMSL
View(dataCTMSL)

# take a look at the column data types by using the glimpse() function
glimpse(dataCTMSL)
# ---------------------------------------------------
# Explore the Province 
dataCTMSL_Province <- dataCTMSL$Province
dataCTMSL_Province
# convert the Provice to factors
dataCTMSL_Province_fac <- factor(dataCTMSL_Province)
# Explore the levels of Province
levels(dataCTMSL_Province_fac)

# ---------------------------------------------------
# Explore the Monthly Household Income
dataCTMSL_MHI <- dataCTMSL$`Monthly_Household_Income`
dataCTMSL_MHI
barplot(dataCTMSL_MHI)
boxplot(dataCTMSL_MHI)
hist(dataCTMSL_MHI)

# convert the Monthly Household Income to factors
dataCTMSL_MHI_fac <- factor(dataCTMSL_MHI)
# Explore the levels of Monthly Household Incom
levels(dataCTMSL_MHI_fac)
# Summary of the Monthly Household Income levels
summary(dataCTMSL_MHI_fac)

#-----------------------------------------------
# Explore the Employement Status
dataCTMSL_ES <- dataCTMSL$`Employment Status...26`
dataCTMSL_ES
# Convert the Employeement Status to factors
dataCTMSL_ES_fac <- factor(dataCTMSL_ES)
levels(dataCTMSL_ES_fac)
# TODO : Verify the Employment status colums in the 
# Found another Employement cloumn : let's call it as ES2
dataCTMSL_ES2 <- dataCTMSL$`Employment Status...3`
dataCTMSL_ES2
# Convert the Employeement Status ES2 to factors
dataCTMSL_ES2_fac <- factor(dataCTMSL_ES2)
levels(dataCTMSL_ES2)

# -------------------------------------------
# Explore the Transport Mode BEFORE
dataCTMSL_TMB <- dataCTMSL$`Transport_mode_BEFORE`
dataCTMSL_TMB
# check the missing values (check NAs):
is.na(dataCTMSL_TMB)
# Convert the Transport Mode BEFORE to factors
dataCTMSL_TMB_fac <- factor(dataCTMSL_TMB)
# check the levels of the dataCTMSL_TMB_fac
levels(dataCTMSL_TMB_fac)
summary(dataCTMSL_TMB_fac) # Transport Mode BEFORE

# Creating a table with dataCTMSL_TMB
TMB.table <- table(dataCTMSL_TMB)
barplot(TMB.table)
b <- barplot(TMB.table)

text(b, TMB.table + 5, TMB.table, font=2, col='red')

# put the bars in decreasing order
barplot(TMB.table[order(TMB.table,decreasing = T)])


# Draw the bars horizontally (but turn off the decreasing)
barplot(TMB.table[order(TMB.table)], horiz = T)
barplot(TMB.table[order(TMB.table)], horiz = T, col = "blue", main = "Transport Mode Befor\n COVID19", xlab = "Freequency")

barplot(TMB.table[order(TMB.table)], horiz = T, col = c("beige","blanchedalmond",'bisque1','bisque2','bisque3','bisque4','green','blue'), main = "Transport Mode Befor\n COVID19", xlab = "Freequency")




# ----------- Renaming numerical values to characters ------
str(TMB.table)
# Converting dataCTMSL_TMB to characters
dataCTMSL_TMB_char <- as.character(dataCTMSL_TMB)
str(dataCTMSL_TMB_char)
#TMB_table_with_names <- dataCTMSL_TMB_char %>%
# mutate(
xyz <- recode(dataCTMSL_TMB_char,
              '1' = 'Walking/Cycling',
              '2' = 'Public Transport (Bus)',
              '3' = 'Public Transport (Train)',
              '4' = 'Private Vehicle (Motor Cycle)',
              '5' = 'Private Vehicle (Car/Van/Other)',
              '6' = 'Taxi/Three Wheeler',
              '7' = 'Office Transport Service',
              '8' = 'Official Vehicle')


TMB_char_table <- table(xyz) 
barplot(TMB_char_table)
? boxplot
# ----------------------------------------------------------------------------

#summary(xyz)
# -----------------------------------------
# Explore the Transport Mode AFTER
dataCTMSL_TMA <- dataCTMSL$`Transport_Mode_AFTER`
dataCTMSL_TMA
# check the missing values (check the NAs):
is.na(dataCTMSL_TMA)
# Convert the Transport Mode AFTER to factors
dataCTMSL_TMA_fac <- factor(dataCTMSL_TMA)
# check the levels of the dataCTMSL_TMA_fac
levels(dataCTMSL_TMA_fac)
summary(dataCTMSL_TMA_fac) # Transport Mode AFTER

# converting dataCTMSL_TMA to a table
TMA.table <- table(dataCTMSL_TMA)
mycol <- c("beige","blanchedalmond",'bisque1','bisque2','bisque3','bisque4','green','blue')

b1 <- barplot(TMA.table)
#text(b1, TMA.table + 5, TMA.table, font=2, col=mycol)
text(b1, TMA.table + 5, TMA.table, font=2, col='red')

b2 <- barplot(TMA.table[order(TMA.table)], horiz = T, col = c("beige","blanchedalmond",'bisque1','bisque2','bisque3','bisque4','green','blue'), main = "Transport Mode After\n COVID19", xlab = "Freequency")
b2 <- barplot(TMA.table[order(TMA.table)], horiz = T, col = 'blue', main = "Transport Mode After\n COVID19", xlab = "Freequency")

text(b2, TMA.table + 5, TMA.table, font=2, col=mycol)

barplot(TMA.table[order(TMA.table)], horiz = T)
barplot(TMA.table[order(TMA.table)], horiz = T, col = c("beige","blanchedalmond",'bisque1','bisque2','bisque3','bisque4','green','blue'), main = "Transport Mode After\n COVID19", xlab = "Freequency")
mycol <- c("beige","blanchedalmond",'bisque1','bisque2','bisque3','bisque4','green','blue')
b <- barplot(TMA.table[order(TMA.table)], horiz = T, col = mycol, main = "TTTransport Mode After\n COVID19", xlab = "Freequency")
text(b, TMA.table + 5, TMA.table, font=2, col=mycol)
# ---- Another method to generate bargraph-------------------------

# -----------------------------------------
# Days per Week - Work from Home AFTER
dataCTMSL_DPW_AF <- dataCTMSL$`Days per Week - Work from Home AFTER`
dataCTMSL_DPW_AF
# check the missing values and NAs
is.na(dataCTMSL_DPW_AF)
# Convert the Days per Week - Work from Home AFTER to factors
dataCTMSL_DPW_AF_fac <- factor(dataCTMSL_DPW_AF)
# check levels of the dataCTMSL_DPW_AF_fac
levels(dataCTMSL_DPW_AF_fac)
summary(dataCTMSL_DPW_AF_fac)

# Purpose of travel
# Work trip / School Trip / Home and Other trips 

# School Trip 

# Work from Home ( seperate Analysis)
# Home to other trips

# After COVID how they travel

#------------ Analysis for the Western Province---------
# WP = 1
# creating the dataframe for the WP
WP_population = 5821710  # WP population as of year 2012 

df <- data_frame(dataCTMSL)
# creating a local dataframe
dfl <- tbl_df(df)
head(dfl)
# filter() for filter the rows
# select() for selecting the columns
WP_DF <- filter(df, Province == 1)
nrow(WP_DF) # number of rows in WP_DF = 875
View(WP_DF)

# Wester Province with Employment_Status == 1 (currently employed)
WP_DF_employed <- filter(WP_DF, Employment_Status == 1)
View(WP_DF_employed)
nrow(WP_DF_employed) # number of rows in WP_DF_employed = 774
# number of people in the WP with Employment_Status == 0 is 101 (875 - 774 = 101)

#----- WP_DF_employed Transport Mode Before-----
WP_DF_employed_TMB <- WP_DF_employed$Transport_mode_BEFORE
WP_DF_employed_TMB
# ---- converting WP_DF_employed_TMB to factors ----------
WP_DF_employed_TMB_fac <-factor(WP_DF_employed_TMB)
levels(WP_DF_employed_TMB_fac) # checking the levels
summary(WP_DF_employed_TMB_fac)

# converting WP_DF_employed_TMB into a table
WP_Employed_TMB.table <-table(WP_DF_employed_TMB)
wpETMB <- barplot(WP_Employed_TMB.table)
text(wpETMB, WP_Employed_TMB.table + 5, WP_Employed_TMB.table, font=2, col='red')
# generating the horizontal plot
barplot(WP_Employed_TMB.table[order(WP_Employed_TMB.table)], horiz = T, col = c("beige","blanchedalmond",'bisque1','bisque2','bisque3','bisque4','green','blue'), main = "Wester Province Employed\n Transport Mode Befor\n COVID19", xlab = "Freequency")

#----- WP_DF_employed Transport Mode AFTER-----
WP_DF_employed_TMA <- WP_DF_employed$Transport_Mode_AFTER
WP_DF_employed_TMA
# ---- converting WP_DF_employed_TMA to factors ----------
WP_DF_employed_TMA_fac <- factor(WP_DF_employed_TMA)
WP_DF_employed_TMA_fac
levels(WP_DF_employed_TMA_fac)
summary(WP_DF_employed_TMA_fac)
# converting WP_DF_employed_TMA to a tables
WP_Employed_TMA.table <- table(WP_DF_employed_TMA)
wpETMA <- barplot(WP_Employed_TMA.table)
text(wpETMA, WP_Employed_TMA.table + 5, WP_Employed_TMA.table, font=2, col='red')
# generating the horizontal plot
barplot(WP_Employed_TMA.table[order(WP_Employed_TMA.table)], horiz = T, col = c("beige","blanchedalmond",'bisque1','bisque2','bisque3','bisque4','green','blue'), main = "Wester Province Employed\n Transport Mode After\n COVID19", xlab = "Freequency")


#-------Working from Home AFTER WP employed --------------
# Working_from_Home_AFTER After for the WP Employment Status = 1
WFH_WP_DF_employed <- WP_DF_employed$Working_from_Home_AFTER # 
WFH_WP_DF_employed
# converting WFH_WP_DF_employed to factor
WFH_WP_DF_employed_fac <- factor(WFH_DPW_WP_DF_employed)
WFH_WP_DF_employed_fac
# levels of WFH_DPW_WP_DF_employed_fac
levels(WFH_WP_DF_employed_fac)
summary(WFH_WP_DF_employed_fac) 

# converting WFH_DPW_WP_DF_employed to a table
WFH_WP_DF_employed.table <-table(WFH_WP_DF_employed)
wpEWFH <- barplot(WFH_WP_DF_employed.table)
text(wpEWFH, WFH_WP_DF_employed.table + 5, WFH_WP_DF_employed.table, font=2, col='red')
#--------------------------------------------------------------------


#------ Transport_mode_for_Home_to_Other_Trips_BEFORE------------------------
TMHOT_Before <- WP_DF_employed$Transport_mode_for_Home_to_Other_Trips_BEFORE
TMHOT_Before
# converting TMHOT_Before to a factor
TMHOT_Before_fac <-factor(TMHOT_Before)
levels(TMHOT_Before_fac)
summary(TMHOT_Before_fac)

# converting TMHOT_Before to a table
TMHOT_Before.table <- table(TMHOT_Before)
wpTMHOT <- barplot(TMHOT_Before.table)
text(wpTMHOT, TMHOT_Before.table + 5, TMHOT_Before.table, font=2, col='red')
barplot(TMHOT_Before.table[order(TMHOT_Before.table)], horiz = T, col = c("beige","blanchedalmond",'bisque1','bisque2','bisque3','bisque4'), main = "Wester Province Employed\n Transport Mode for Home to Other Trips BEFORE", xlab = "Freequency")
#----------------------------------------------------------------------------

# Transport Mode for Home to Other Trips AFTER for WP Employment Status = 1
TMHOT_After <- WP_DF_employed$Transport_mode_for_Home_to_Other_Trips_AFTER
TMHOT_After
# converting TMHOT_After to factors
TMHOT_After_fac <- factor(TMHOT_After)
levels(TMHOT_After_fac)
summary(TMHOT_After_fac)

# converting TMHOT_After to a table
TMHOT_After.table <- table(TMHOT_After)
wpTMHOTA <- barplot(TMHOT_After.table)
text(wpTMHOTA, TMHOT_After.table + 5, TMHOT_After.table, font=2, col='red')
barplot(TMHOT_After.table[order(TMHOT_After.table)], horiz = T, col = c("beige","blanchedalmond",'bisque1','bisque2','bisque3','bisque4'), main = "Wester Province Employed\n Transport Mode for Home to Other Trips AFTER", xlab = "Freequency")
#----------------------------------------------------------------------------------------------------

#------- No of Home other trips BEFORE-----------------------------------
# How many Home to Other trips you would do during a WEEK BEFORE Lockdown?
NHOT_Before <- WP_DF_employed$No_of_Home_other_trips_BEFORE
NHOT_Before
# converting NHOT_Before to a factor
NHOT_Before_fac <-factor(NHOT_Before)
levels(NHOT_Before_fac)
summary(NHOT_Before_fac)

# converting NHOT_Before to a table
NHOT_Before.table <- table(NHOT_Before)
wpNHOTB <- barplot(NHOT_Before.table)
text(wpNHOTB, NHOT_Before.table + 5, NHOT_Before.table, font=2, col='red')
#--------------------------------------------------------------------------------------------

#------- No of Home other trips AFTER-----------------------------------
# How many Home to Other trips you  will do during a WEEK AFTER resumption?
NHOT_After <- WP_DF_employed$No_of_Home_other_trips_AFTER
NHOT_After
# converting NHOT_AFTER to a factor
NHOT_After_fac <- factor(NHOT_After)
levels(NHOT_After_fac)
summary(NHOT_After_fac)

# converting NHOT_After to a table
NHOT_After.table <- table(NHOT_After)
wpNHOTA <-barplot(NHOT_After.table)
text(wpNHOTA, NHOT_After.table + 5, NHOT_After.table, font=2, col='red')
barplot(NHOT_After.table[order(NHOT_After.table)], horiz = T, col = c("beige","blanchedalmond",'bisque1','bisque2','bisque3'), main = "Wester Province Employed\n Number of Home to Other Trips\n During a Week AFTER", xlab = "Freequency")
#--------------------------------------------------------------------------------------------------

# -------Access/Egress Method BEFORE  (WP Employ Status = 1) ----------------------------
AEM_Before <- WP_DF_employed$Access_Egress_Method_BEFORE
AEM_Before
# converting AEM_Before to a factor
AEM_Before_fac <- factor(AEM_Before)
levels(AEM_Before_fac)
summary(AEM_Before_fac)

# converting AEM_Before to a table
AEM_Before.table <- table(AEM_Before)
wpAEMB <-barplot(AEM_Before.table)
text(wpAEMB, AEM_Before.table + 5, AEM_Before.table, font=2, col='red')
barplot(AEM_Before.table[order(AEM_Before.table)], horiz = T, col = c("beige","blanchedalmond",'bisque1','bisque2','bisque3','bisque4'), main = "Wester Province Employed\n Access/Egress Method BEFORE", xlab = "Freequency")
#-----------------------------------------------------------------------------------------------------

# -------Access/Egress Method AFTER (WP Employ Status = 1)----------------------------
AEM_After <- WP_DF_employed$Access_Egress_Method_AFTER
AEM_After
# converting AEM_After to a factor
AEM_After_fac <- factor(AEM_After)
levels(AEM_After_fac)
summary(AEM_After_fac)

# converting AEM_After to a table
AEM_After.table <- table(AEM_After)
wpAEMA <- barplot(AEM_After.table)
text(wpAEMA, AEM_After.table + 5, AEM_After.table, font=2, col='red')
barplot(AEM_After.table[order(AEM_After.table)], horiz = T, col = c("beige","blanchedalmond",'bisque1','bisque2','bisque3'), main = "Wester Province Employed\n Access/Egress Method AFTER", xlab = "Freequency")

#--------------------------------------------------------------------------
# Transport Mode Before = 5 for the Western Province
WP_DF_TMB5 <- filter(WP_DF, WP_DF$Transport_mode_BEFORE == 5)
nrow(WP_DF_TMB5) # number of rows in WP_DF_TMB5 = 351
View(WP_DF_TMB5)



# Western Province Mobility Pattern changes

WP_DF_employed %>% select(Transport_mode_BEFORE,Transport_Mode_AFTER) %>% head(15)
tmc5to1 <- WP_DF_employed %>% select(Transport_mode_BEFORE,Transport_Mode_AFTER) %>% filter(Transport_mode_BEFORE == 5 & Transport_Mode_AFTER == 5)
count(tmc5to1)
print(tmc5to1)


tmc5to4 <- WP_DF_employed %>% select(Transport_mode_BEFORE,Transport_Mode_AFTER) %>% filter(Transport_mode_BEFORE == 5 & Transport_Mode_AFTER == 4)
count(tmc5to4)
print(tmc5to4)

tmcdf5to5 <- dataCTMSL %>% select(Transport_mode_BEFORE,Transport_Mode_AFTER) %>% filter(Transport_mode_BEFORE == 5 & Transport_Mode_AFTER == 5)
count(tmcdf5to5)
print(tmcdf5to5)
View(tmcdf5to5)
nrow(tmcdf5to5)

# Transport Mode 1:Walking/Cycling remain same  (in the entire dataset, Before and After)
tmcdf1to1 <- dataCTMSL %>% select(Transport_mode_BEFORE,Transport_Mode_AFTER) %>% filter(Transport_mode_BEFORE == 1 & Transport_Mode_AFTER == 1)
count(tmcdf1to1)
print(tmcdf1to1)
View(tmcdf1to1)
nrow(tmcdf1to1)

# Transport Mode changed from 1:Walking/Cycling to 2:Public Transport (Bus) (in the entire dataset, Before and After)
tmcdf1to2 <- dataCTMSL %>% select(Transport_mode_BEFORE,Transport_Mode_AFTER) %>% filter(Transport_mode_BEFORE == 1 & Transport_Mode_AFTER == 2)
count(tmcdf1to2)
print(tmcdf1to2)
View(tmcdf1to2)
nrow(tmcdf1to2)


# Transport Mode changed from 1 to 3 (in the entire dataset, Before and After)
tmcdf1to3 <- dataCTMSL %>% select(Transport_mode_BEFORE,Transport_Mode_AFTER) %>% filter(Transport_mode_BEFORE == 1 & Transport_Mode_AFTER == 3)
count(tmcdf1to3)
print(tmcdf1to3)
View(tmcdf1to3)
nrow(tmcdf1to3)

# Transport Mode changed from 1:Walking/Cycling to 4:Private Vehicle (Motor Cycle) (in the entire dataset, Before and After)
tmcdf1to4 <- dataCTMSL %>% select(Transport_mode_BEFORE,Transport_Mode_AFTER) %>% filter(Transport_mode_BEFORE == 1 & Transport_Mode_AFTER == 4)
count(tmcdf1to4)
print(tmcdf1to4)
View(tmcdf1to4)
nrow(tmcdf1to4)

# Transport Mode changed from 1:Walking/Cycling to 5:Private Vehicle (Car/Van/Other) (in the entire dataset, Before and After)
tmcdf1to5 <- dataCTMSL %>% select(Transport_mode_BEFORE,Transport_Mode_AFTER) %>% filter(Transport_mode_BEFORE == 1 & Transport_Mode_AFTER == 5)
count(tmcdf1to5)
print(tmcdf1to5)
View(tmcdf1to5)
nrow(tmcdf1to5)

# Transport Mode changed from 1:Walking/Cycling to 6:Taxi/Three Wheeler (in the entire dataset, Before and After)
tmcdf1to6 <- dataCTMSL %>% select(Transport_mode_BEFORE,Transport_Mode_AFTER) %>% filter(Transport_mode_BEFORE == 1 & Transport_Mode_AFTER == 6)
count(tmcdf1to6)
print(tmcdf1to6)
View(tmcdf1to6)
nrow(tmcdf1to6)


# Transport Mode changed from 1:Walking/Cycling to 7:Office Transport Service (in the entire dataset, Before and After)
tmcdf1to7 <- dataCTMSL %>% select(Transport_mode_BEFORE,Transport_Mode_AFTER) %>% filter(Transport_mode_BEFORE == 1 & Transport_Mode_AFTER == 7)
count(tmcdf1to7)
print(tmcdf1to7)
View(tmcdf1to7)
nrow(tmcdf1to7)


# Transport Mode changed from 1:Walking/Cycling to 8:Official Vehicle (in the entire dataset, Before and After)
tmcdf1to8 <- dataCTMSL %>% select(Transport_mode_BEFORE,Transport_Mode_AFTER) %>% filter(Transport_mode_BEFORE == 1 & Transport_Mode_AFTER == 8)
count(tmcdf1to8)
print(tmcdf1to8)
View(tmcdf1to8)
nrow(tmcdf1to8)
#----------------------------
#------ Transport Mode chenge from 2 to other modes------
# Transport Mode 2 = Public Transport (Bus)

# Transport Mode changed from 2:Public Transport (Bus) to 1: Walking/Cycling (in the entire dataset, Before and After)
tmcdf2to1 <- dataCTMSL %>% select(Transport_mode_BEFORE,Transport_Mode_AFTER) %>% filter(Transport_mode_BEFORE == 2 & Transport_Mode_AFTER == 1)
count(tmcdf2to1)
print(tmcdf2to1)
View(tmcdf2to1)
nrow(tmcdf2to1) # 3 people have reported that transport mode of 2:Public Transport (Bus) to 1: Walking/Cycling


# Transport Mode  2:Public Transport (Bus) remain same (in the entire dataset, Before and After)
tmcdf2to2 <- dataCTMSL %>% select(Transport_mode_BEFORE,Transport_Mode_AFTER) %>% filter(Transport_mode_BEFORE == 2 & Transport_Mode_AFTER == 2)
count(tmcdf2to2)
print(tmcdf2to2)
View(tmcdf2to2)
nrow(tmcdf2to2) # 119 people have mentioned that their transport mode of 2 has not changed before and after


# Transport Mode changed from 2:Public Transport (Bus) to 3:Public Transport (Train) (in the entire dataset, Before and After)
tmcdf2to3 <- dataCTMSL %>% select(Transport_mode_BEFORE,Transport_Mode_AFTER) %>% filter(Transport_mode_BEFORE == 2 & Transport_Mode_AFTER == 3)
count(tmcdf2to3)
print(tmcdf2to3)
View(tmcdf2to3)
nrow(tmcdf2to3)# only 1 person has reported that transport mode of 2 has changed to mode 3 

# Transport Mode changed from 2:Public Transport (Bus) to 4:Private Vehicle (Motor Cycle) (in the entire dataset, Before and After)
tmcdf2to4 <- dataCTMSL %>% select(Transport_mode_BEFORE,Transport_Mode_AFTER) %>% filter(Transport_mode_BEFORE == 2 & Transport_Mode_AFTER == 4)
count(tmcdf2to4)
print(tmcdf2to4)
View(tmcdf2to4)
nrow(tmcdf2to4) # 20 people have reported that transport mode of 2 have changed to mode 4

# Transport Mode changed from 2:Public Transport (Bus) to 5:Private Vehicle (Car/Van/Other) (in the entire dataset, Before and After)
tmcdf2to5 <- dataCTMSL %>% select(Transport_mode_BEFORE,Transport_Mode_AFTER) %>% filter(Transport_mode_BEFORE == 2 & Transport_Mode_AFTER == 5)
count(tmcdf2to5)
print(tmcdf2to5)
View(tmcdf2to5)
nrow(tmcdf2to5) # 67 people have reported that transport mode of 2 have changed to mode 5

# Transport Mode changed from 2:Public Transport (Bus) to 6:Taxi/Three Wheeler (in the entire dataset, Before and After)
tmcdf2to6 <- dataCTMSL %>% select(Transport_mode_BEFORE,Transport_Mode_AFTER) %>% filter(Transport_mode_BEFORE == 2 & Transport_Mode_AFTER == 6)
count(tmcdf2to6)
print(tmcdf2to6)
View(tmcdf2to6)
nrow(tmcdf2to6) # 13 people have reported that transport mode of 2 have changed to mode 6

# Transport Mode changed from 2 to 7:Office Transport Service (in the entire dataset, Before and After)
tmcdf2to7 <- dataCTMSL %>% select(Transport_mode_BEFORE,Transport_Mode_AFTER) %>% filter(Transport_mode_BEFORE == 2 & Transport_Mode_AFTER == 7)
count(tmcdf2to7)
print(tmcdf2to7)
View(tmcdf2to7)
nrow(tmcdf2to7) # 13 people have reported that transport mode of 2 have changed to mode 7

# Transport Mode changed from 2 to 8:Official Vehicle (in the entire dataset, Before and After)
tmcdf2to8 <- dataCTMSL %>% select(Transport_mode_BEFORE,Transport_Mode_AFTER) %>% filter(Transport_mode_BEFORE == 2 & Transport_Mode_AFTER == 8)
count(tmcdf2to8)
print(tmcdf2to8)
View(tmcdf2to8)
nrow(tmcdf2to8) # 14 people have reported that transport mode of 2 have changed to mode 8
#------------------------------------------------------------------------------------

#------ Transport Mode chenge from 3 to other modes------
# Transport Mode 3 = Public Transport (Train)

# Transport Mode changed from 3:Public Transport (Train) to 1: Walking/Cycling (in the entire dataset, Before and After)
tmcdf3to1 <- dataCTMSL %>% select(Transport_mode_BEFORE,Transport_Mode_AFTER) %>% filter(Transport_mode_BEFORE == 3 & Transport_Mode_AFTER == 1)
count(tmcdf3to1)
print(tmcdf3to1)
View(tmcdf3to1)
nrow(tmcdf3to1) # People have not reported that they have changed their public transport mode of train changed to walking/cycling

# Transport Mode changed from 3:Public Transport (Train) to 2: Public Transport (Bus) (in the entire dataset, Before and After)
tmcdf3to2 <- dataCTMSL %>% select(Transport_mode_BEFORE,Transport_Mode_AFTER) %>% filter(Transport_mode_BEFORE == 3 & Transport_Mode_AFTER == 2)
count(tmcdf3to2)
print(tmcdf3to2)
View(tmcdf3to2)
nrow(tmcdf3to2) # 5 people have reported that their Mode changed from 3:Public Transport (Train) to 2: Public Transport (Bus)

# Transport Mode 3:Public Transport (Train) remain same (Before and After)
tmcdf3to3 <- dataCTMSL %>% select(Transport_mode_BEFORE,Transport_Mode_AFTER) %>% filter(Transport_mode_BEFORE == 3 & Transport_Mode_AFTER == 3)
count(tmcdf3to3)
print(tmcdf3to3)
View(tmcdf3to3)
nrow(tmcdf3to3) # 48 people reported that their transport mode of train has not changed before and after the COVID19

# Transport Mode changed from 3:Public Transport (Train) to 4: Private Vehicle (Motor Cycle) (in the entire dataset, Before and After)
tmcdf3to4 <- dataCTMSL %>% select(Transport_mode_BEFORE,Transport_Mode_AFTER) %>% filter(Transport_mode_BEFORE == 3 & Transport_Mode_AFTER == 4)
count(tmcdf3to4)
print(tmcdf3to4)
View(tmcdf3to4)
nrow(tmcdf3to4) # 7 people have reported that their Mode changed from 3:Public Transport (Train) to Private Vehicle Motor Cycle

# Transport Mode changed from 3:Public Transport (Train) to 5: Private Vehicle (Car/Van/Other) (in the entire dataset, Before and After)
tmcdf3to5 <- dataCTMSL %>% select(Transport_mode_BEFORE,Transport_Mode_AFTER) %>% filter(Transport_mode_BEFORE == 3 & Transport_Mode_AFTER == 5)
count(tmcdf3to5)
print(tmcdf3to5)
View(tmcdf3to5)
nrow(tmcdf3to5) # 39 people have reported that their Transport Mode changed from 3:Public Transport (Train) to 5: Private Vehicle (Car/Van/Other)

# Transport Mode changed from 3:Public Transport (Train) to 6: Taxi/Three Wheeler (in the entire dataset, Before and After)
tmcdf3to6 <- dataCTMSL %>% select(Transport_mode_BEFORE,Transport_Mode_AFTER) %>% filter(Transport_mode_BEFORE == 3 & Transport_Mode_AFTER == 6)
count(tmcdf3to6)
print(tmcdf3to6)
View(tmcdf3to6)
nrow(tmcdf3to6) # 1 person reported that Transport Mode changed from 3:Public Transport (Train) to 6: Taxi/Three Wheeler

# Transport Mode changed from 3:Public Transport (Train) to 7: Office Transport Service (in the entire dataset, Before and After)
tmcdf3to7 <- dataCTMSL %>% select(Transport_mode_BEFORE,Transport_Mode_AFTER) %>% filter(Transport_mode_BEFORE == 3 & Transport_Mode_AFTER == 7)
count(tmcdf3to7)
print(tmcdf3to7)
View(tmcdf3to7)
nrow(tmcdf3to7) # 8 people have reported that their Transport Mode changed from 3:Public Transport (Train) to  7: Office Transport Service

# Transport Mode changed from 3:Public Transport (Train) to 8: Official Vehicle (in the entire dataset, Before and After)
tmcdf3to8 <- dataCTMSL %>% select(Transport_mode_BEFORE,Transport_Mode_AFTER) %>% filter(Transport_mode_BEFORE == 3 & Transport_Mode_AFTER == 8)
count(tmcdf3to8)
print(tmcdf3to8)
View(tmcdf3to8)
nrow(tmcdf3to8) # 6 people have reported that their Transport Mode changed from 3:Public Transport (Train) to 8: Official Vehicle
#-----------------------------------------------------------------

#------ Transport Mode chenge from 4 to other modes------
# Transport Mode 4 = Private Vehicle (Motor Cycle)

# Transport Mode changed from 4 = Private Vehicle (Motor Cycle) to 1: Walking/Cycling (in the entire dataset, Before and After)
tmcdf4to1 <- dataCTMSL %>% select(Transport_mode_BEFORE,Transport_Mode_AFTER) %>% filter(Transport_mode_BEFORE == 4 & Transport_Mode_AFTER == 1)
count(tmcdf4to1)
print(tmcdf4to1)
View(tmcdf4to1)
nrow(tmcdf4to1) # No one reported their transport mode of Mortor Cycle changed to Walking/Cycling

# Transport Mode changed from 4 = Private Vehicle (Motor Cycle) to 2: Public Transport (Bus) (in the entire dataset, Before and After)
tmcdf4to2 <- dataCTMSL %>% select(Transport_mode_BEFORE,Transport_Mode_AFTER) %>% filter(Transport_mode_BEFORE == 4 & Transport_Mode_AFTER == 2)
count(tmcdf4to2)
print(tmcdf4to2)
View(tmcdf4to2)
nrow(tmcdf4to2)  # No one reported their transport mode of Mortor Cycle changed to Public Transport (bus)

# Transport Mode changed from 4 = Private Vehicle (Motor Cycle) to 3: Public Transport (Train) (in the entire dataset, Before and After)
tmcdf4to3 <- dataCTMSL %>% select(Transport_mode_BEFORE,Transport_Mode_AFTER) %>% filter(Transport_mode_BEFORE == 4 & Transport_Mode_AFTER == 3)
count(tmcdf4to3)
print(tmcdf4to3)
View(tmcdf4to3)
nrow(tmcdf4to3) # No one reported their transport mode of Mortor Cycle changed to Public Transport (Train)

# Transport Mode 4 = Private Vehicle (Motor Cycle) remain same Before and After (in the entire dataset, Before and After)
tmcdf4to4 <- dataCTMSL %>% select(Transport_mode_BEFORE,Transport_Mode_AFTER) %>% filter(Transport_mode_BEFORE == 4 & Transport_Mode_AFTER == 4)
count(tmcdf4to4)
print(tmcdf4to4)
View(tmcdf4to4)
nrow(tmcdf4to4) # 61 people reported that their transport mode of Motor Cycle has not changed before and after the COVID19

# Transport Mode changed from 4 = Private Vehicle (Motor Cycle) to 5: Private Vehicle (Car/Van/Other) (in the entire dataset, Before and After)
tmcdf4to5 <- dataCTMSL %>% select(Transport_mode_BEFORE,Transport_Mode_AFTER) %>% filter(Transport_mode_BEFORE == 4 & Transport_Mode_AFTER == 5)
count(tmcdf4to5)
print(tmcdf4to5)
View(tmcdf4to5)
nrow(tmcdf4to5) # 5 people have reported that their Transport Mode changed from 4 = Private Vehicle (Motor Cycle) to 5: Private Vehicle (Car/Van/Other)

# Transport Mode changed from 4 = Private Vehicle (Motor Cycle) to 6: Taxi Three Wheeler (in the entire dataset, Before and After)
tmcdf4to6 <- dataCTMSL %>% select(Transport_mode_BEFORE,Transport_Mode_AFTER) %>% filter(Transport_mode_BEFORE == 4 & Transport_Mode_AFTER == 6)
count(tmcdf4to6)
print(tmcdf4to6)
View(tmcdf4to6)
nrow(tmcdf4to6) # No one reported their transport mode of Mortor Cycle changed to Taxi/ Three Wheeler

# Transport Mode changed from 4 = Private Vehicle (Motor Cycle) to 7: Office Transport Service (in the entire dataset, Before and After)
tmcdf4to7 <- dataCTMSL %>% select(Transport_mode_BEFORE,Transport_Mode_AFTER) %>% filter(Transport_mode_BEFORE == 4 & Transport_Mode_AFTER == 7)
count(tmcdf4to7)
print(tmcdf4to7)
View(tmcdf4to7)
nrow(tmcdf4to7) # 1 person reported that Transport Mode changed from 4 = Private Vehicle (Motor Cycle) to 7: Office Transport Service

# Transport Mode changed from 4 = Private Vehicle (Motor Cycle) to 8: Official Vehicle (in the entire dataset, Before and After)
tmcdf4to8 <- dataCTMSL %>% select(Transport_mode_BEFORE,Transport_Mode_AFTER) %>% filter(Transport_mode_BEFORE == 4 & Transport_Mode_AFTER == 8)
count(tmcdf4to8)
print(tmcdf4to8)
View(tmcdf4to8)
nrow(tmcdf4to8) # No one reported their transport mode of Mortor Cycle changed to Official Vehicle
#---------------------------------------------------------------------------------

#------ Transport Mode change from 5: Private Vehicle (Car/Van/Other) to other modes------
# Transport Mode 5 = Private Vehicle (Car/Van/Other)

# Transport Mode changed from 5: Private Vehicle (Car/Van/Other) to 1:Walking/Cycling  (in the entire dataset)
tmcdf5to1 <- dataCTMSL %>% select(Transport_mode_BEFORE,Transport_Mode_AFTER) %>% filter(Transport_mode_BEFORE == 5 & Transport_Mode_AFTER == 1)
count(tmcdf5to1)
print(tmcdf5to1)
View(tmcdf5to1)
nrow(tmcdf5to1) # No one reported their transport mode of changed from 5: Private Vehicle (Car/Van/Other) to 1:Walking/Cycling

# Transport Mode changed from 5: Private Vehicle (Car/Van/Other) to 2:Public Transport (Bus) (in the entire dataset)
tmcdf5to2 <- dataCTMSL %>% select(Transport_mode_BEFORE,Transport_Mode_AFTER) %>% filter(Transport_mode_BEFORE == 5 & Transport_Mode_AFTER == 2)
count(tmcdf5to2)
print(tmcdf5to2)
View(tmcdf5to2)
nrow(tmcdf5to2) # 3 people have reported that their Transport Mode changed from 5: Private Vehicle (Car/Van/Other) to 2:Public Transport (Bus)

# Transport Mode changed from 5:Private Vehicle (Car/Van/Other) to 3:Public Transport (Train) (in the entire dataset)
tmcdf5to3 <- dataCTMSL %>% select(Transport_mode_BEFORE,Transport_Mode_AFTER) %>% filter(Transport_mode_BEFORE == 5 & Transport_Mode_AFTER == 3)
count(tmcdf5to3)
print(tmcdf5to3)
View(tmcdf5to3)
nrow(tmcdf5to3)# No one reported their transport mode of changed from 5: Private Vehicle (Car/Van/Other) to 3:Public Transport (Train)

# Transport Mode changed from 5:Private Vehicle (Car/Van/Other) to 4:Private Vehicle (Motor Cycle) (in the entire dataset)
tmcdf5to4 <- dataCTMSL %>% select(Transport_mode_BEFORE,Transport_Mode_AFTER) %>% filter(Transport_mode_BEFORE == 5 & Transport_Mode_AFTER == 4)
count(tmcdf5to4)
print(tmcdf5to4)
View(tmcdf5to4)
nrow(tmcdf5to4) # only 1 person reported Transport Mode changed from 5:Private Vehicle (Car/Van/Other) to 4:Private Vehicle (Motor Cycle)

# Transport Mode remain 5:Private Vehicle (Car/Van/Other)  (in the entire dataset)

tmcdf5to5 <- dataCTMSL %>% select(Transport_mode_BEFORE,Transport_Mode_AFTER) %>% filter(Transport_mode_BEFORE == 5 & Transport_Mode_AFTER == 5)
count(tmcdf5to5)
print(tmcdf5to5)
View(tmcdf5to5)
nrow(tmcdf5to5)

# Transport Mode changed from 5:Private Vehicle (Car/Van/Other) to 6:Taxi/Three Wheeler (in the entire dataset)
tmcdf5to6 <- dataCTMSL %>% select(Transport_mode_BEFORE,Transport_Mode_AFTER) %>% filter(Transport_mode_BEFORE == 5 & Transport_Mode_AFTER == 6)
count(tmcdf5to6)
print(tmcdf5to6)
View(tmcdf5to6)
nrow(tmcdf5to6) # No one reported their transport mode of changed from 5: Private Vehicle (Car/Van/Other) to 6: Taxi/Three Wheeler

# Transport Mode changed from 5:Private Vehicle (Car/Van/Other) to 7:Office Transport Service (in the entire dataset)
tmcdf5to7 <- dataCTMSL %>% select(Transport_mode_BEFORE,Transport_Mode_AFTER) %>% filter(Transport_mode_BEFORE == 5 & Transport_Mode_AFTER == 7)
count(tmcdf5to7)
print(tmcdf5to7)
View(tmcdf5to7)
nrow(tmcdf5to7)  # No one reported their transport mode of changed from 5: Private Vehicle (Car/Van/Other) to 7:Office Transport Service

# Transport Mode changed from 5:Private Vehicle (Car/Van/Other) to 8:Official Vehicle (in the entire dataset)
tmcdf5to8 <- dataCTMSL %>% select(Transport_mode_BEFORE,Transport_Mode_AFTER) %>% filter(Transport_mode_BEFORE == 5 & Transport_Mode_AFTER == 8)
count(tmcdf5to8)
print(tmcdf5to8)
View(tmcdf5to8)
nrow(tmcdf5to8) #3  3 people have reported that their Transport Mode changed from 5: Private Vehicle (Car/Van/Other) to 8:Official Vehicle
#-----------------------------------------------------------------------------------------------------

#------ Transport Mode change from 6: Taxi/Three Wheeler to other modes------
# Transport Mode 6 =Taxi/Three Wheeler

# Transport Mode changed from 6: Taxi/Three Wheeler to 1:Walking/Cycling  (in the entire dataset)
tmcdf6to1 <- dataCTMSL %>% select(Transport_mode_BEFORE,Transport_Mode_AFTER) %>% filter(Transport_mode_BEFORE == 6 & Transport_Mode_AFTER == 1)
count(tmcdf6to1)
print(tmcdf6to1)
View(tmcdf6to1)
nrow(tmcdf6to1) # 3 people have reported that their Transport Mode changed from 6: Taxi/Three Wheeler to 1:Walking/Cycling

# Transport Mode changed from 6: Taxi/Three Wheeler to 2: Public Transport (Bus)  (in the entire dataset)
tmcdf6to2 <- dataCTMSL %>% select(Transport_mode_BEFORE,Transport_Mode_AFTER) %>% filter(Transport_mode_BEFORE == 6 & Transport_Mode_AFTER == 2)
count(tmcdf6to2)
print(tmcdf6to2)
View(tmcdf6to2)
nrow(tmcdf6to2) # 2 people have reported that their Transport Mode changed from 6: Taxi/Three Wheeler to 2: Public Transport (Bus)

# Transport Mode changed from 6: Taxi/Three Wheeler to 3: Public Transport (Train)  (in the entire dataset)
tmcdf6to3 <- dataCTMSL %>% select(Transport_mode_BEFORE,Transport_Mode_AFTER) %>% filter(Transport_mode_BEFORE == 6 & Transport_Mode_AFTER == 3)
count(tmcdf6to3)
print(tmcdf6to3)
View(tmcdf6to3)
nrow(tmcdf6to3) # 1 person has reported reported that their Transport Mode changed from 6: Taxi/Three Wheeler to 3: Public Transport (Train)

# Transport Mode changed from 6: Taxi/Three Wheeler to 4: Private Vehicle (Motor Cycvle)  (in the entire dataset)
tmcdf6to4 <- dataCTMSL %>% select(Transport_mode_BEFORE,Transport_Mode_AFTER) %>% filter(Transport_mode_BEFORE == 6 & Transport_Mode_AFTER == 4)
count(tmcdf6to4)
print(tmcdf6to4)
View(tmcdf6to4)
nrow(tmcdf6to4) # No one reported Transport Mode changed from 6: Taxi/Three Wheeler to 4: Private Vehicle (Motor Cycvle)

# Transport Mode changed from 6: Taxi/Three Wheeler to 5: Private Vehicle (Car/Van/Other)  (in the entire dataset)
tmcdf6to5 <- dataCTMSL %>% select(Transport_mode_BEFORE,Transport_Mode_AFTER) %>% filter(Transport_mode_BEFORE == 6 & Transport_Mode_AFTER == 5)
count(tmcdf6to5)
print(tmcdf6to5)
View(tmcdf6to5)
nrow(tmcdf6to5) # 9 people have reported that their Transport Mode changed from 6: Taxi/Three Wheeler to 5: Private Vehicle (Car/Van/Other) 

# Transport Mode remain same for 6: Taxi/Three Wheeler before and after  (in the entire dataset)
tmcdf6to6 <- dataCTMSL %>% select(Transport_mode_BEFORE,Transport_Mode_AFTER) %>% filter(Transport_mode_BEFORE == 6 & Transport_Mode_AFTER == 6)
count(tmcdf6to6)
print(tmcdf6to6)
View(tmcdf6to6)
nrow(tmcdf6to6) # 22 people have reported that their Transport Mode of 6: Taxi/Three Wheeler remain same


# Transport Mode changed from 6: Taxi/Three Wheeler to 7: Office Transport Service  (in the entire dataset)
tmcdf6to7 <- dataCTMSL %>% select(Transport_mode_BEFORE,Transport_Mode_AFTER) %>% filter(Transport_mode_BEFORE == 6 & Transport_Mode_AFTER == 7)
count(tmcdf6to7)
print(tmcdf6to7)
View(tmcdf6to7)
nrow(tmcdf6to7) # No one reported Transport Mode changed from 6: Taxi/Three Wheeler to 7: Office Transport Service

# Transport Mode changed from 6: Taxi/Three Wheeler to 8: Official Vehicle (in the entire dataset)
tmcdf6to8 <- dataCTMSL %>% select(Transport_mode_BEFORE,Transport_Mode_AFTER) %>% filter(Transport_mode_BEFORE == 6 & Transport_Mode_AFTER == 8)
count(tmcdf6to8)
print(tmcdf6to8)
View(tmcdf6to8)
nrow(tmcdf6to8) # 3 people have reported that their Transport Mode changed from 6: Taxi/Three Wheeler to 7: Office Transport Service
#-----------------------------------------------------------
#------ Transport Mode change from 7: Office Transport Service to other modes------
# Transport Mode 7 =Taxi/Three Wheeler

# Transport Mode changed from 7: Office Transport Service 1:Walking/Cycling  (in the entire dataset)
tmcdf7to1 <- dataCTMSL %>% select(Transport_mode_BEFORE,Transport_Mode_AFTER) %>% filter(Transport_mode_BEFORE == 7 & Transport_Mode_AFTER == 1)
count(tmcdf7to1)
print(tmcdf7to1)
View(tmcdf7to1)
nrow(tmcdf7to1)# No one reported Transport Mode changed from 7: Office Transport Service 1:Walking/Cycling

# Transport Mode changed from 7: Office Transport Service 2:Publiv Transport (Bus)  (in the entire dataset)
tmcdf7to2 <- dataCTMSL %>% select(Transport_mode_BEFORE,Transport_Mode_AFTER) %>% filter(Transport_mode_BEFORE == 7 & Transport_Mode_AFTER == 2)
count(tmcdf7to2)
print(tmcdf7to2)
View(tmcdf7to2)
nrow(tmcdf7to2) # 3 people have reported Transport Mode changed from 7: Office Transport Service 2:Publiv Transport (Bus)

# Transport Mode changed from 7: Office Transport Service 3:Publiv Transport (Train)  (in the entire dataset)
tmcdf7to3 <- dataCTMSL %>% select(Transport_mode_BEFORE,Transport_Mode_AFTER) %>% filter(Transport_mode_BEFORE == 7 & Transport_Mode_AFTER == 3)
count(tmcdf7to3)
print(tmcdf7to3)
View(tmcdf7to3)
nrow(tmcdf7to3) # only 1 person reported that Transport Mode changed from 7: Office Transport Service 3:Publiv Transport (Train) 

# Transport Mode changed from 7: Office Transport Service 4:Private Vehicle (Motor Cycle)  (in the entire dataset)
tmcdf7to4 <- dataCTMSL %>% select(Transport_mode_BEFORE,Transport_Mode_AFTER) %>% filter(Transport_mode_BEFORE == 7 & Transport_Mode_AFTER == 4)
count(tmcdf7to4)
print(tmcdf7to4)
View(tmcdf7to4)
nrow(tmcdf7to4)  # only 1 person reported that Transport Mode changed from 7: Office Transport Service to 4:Private Vehicle (Motor Cycle) 

# Transport Mode changed from 7: Office Transport Service 5:Private Vehicle (Car/Van/Other)  (in the entire dataset)
tmcdf7to5 <- dataCTMSL %>% select(Transport_mode_BEFORE,Transport_Mode_AFTER) %>% filter(Transport_mode_BEFORE == 7 & Transport_Mode_AFTER == 5)
count(tmcdf7to5)
print(tmcdf7to5)
View(tmcdf7to5)
nrow(tmcdf7to5) # 21 people reported that Transport Mode changed from 7: Office Transport Service 5:Private Vehicle (Car/Van/Other)

# Transport Mode changed from 7: Office Transport Service 6:Taxi/Three Wheeler  (in the entire dataset)
tmcdf7to6 <- dataCTMSL %>% select(Transport_mode_BEFORE,Transport_Mode_AFTER) %>% filter(Transport_mode_BEFORE == 7 & Transport_Mode_AFTER == 6)
count(tmcdf7to6)
print(tmcdf7to6)
View(tmcdf7to6)
nrow(tmcdf7to6) # Only 1 person reported  Transport Mode changed from 7: Office Transport Service 6:Taxi/Three Wheeler

# Transport Mode 7: Office Transport Service remain same before and after (in the entire dataset)
tmcdf7to7 <- dataCTMSL %>% select(Transport_mode_BEFORE,Transport_Mode_AFTER) %>% filter(Transport_mode_BEFORE == 7 & Transport_Mode_AFTER == 7)
count(tmcdf7to7)
print(tmcdf7to7)
View(tmcdf7to7)
nrow(tmcdf7to7) # 44 poeple reported that Transport Mode 7: Office Transport Service remain same before and after

# Transport Mode changed from 7: Office Transport Service 8:Official Vehicle  (in the entire dataset)
tmcdf7to8 <- dataCTMSL %>% select(Transport_mode_BEFORE,Transport_Mode_AFTER) %>% filter(Transport_mode_BEFORE == 7 & Transport_Mode_AFTER == 8)
count(tmcdf7to8)
print(tmcdf7to8)
View(tmcdf7to8)
nrow(tmcdf7to8)# Only 1 person reported that Transport Mode changed from 7: Office Transport Service 8:Official Vehicle 
#------------------------------------------------------------

#------ Transport Mode change from 8: Official Vehicle to other modes------
# Transport Mode 8: Official Vehicle

# Transport Mode changed from 8: Official Vehicle to 1:Walking/Cycling  (in the entire dataset)
tmcdf8to1 <- dataCTMSL %>% select(Transport_mode_BEFORE,Transport_Mode_AFTER) %>% filter(Transport_mode_BEFORE == 8 & Transport_Mode_AFTER == 1)
count(tmcdf8to1)
print(tmcdf8to1)
View(tmcdf8to1)
nrow(tmcdf8to1) # No one reported Transport Mode changed from 8: Official Vehicle to 1:Walking/Cycling

# Transport Mode changed from 8: Official Vehicle to 2: Public Transport (Bus)  (in the entire dataset)
tmcdf8to2 <- dataCTMSL %>% select(Transport_mode_BEFORE,Transport_Mode_AFTER) %>% filter(Transport_mode_BEFORE == 8 & Transport_Mode_AFTER == 2)
count(tmcdf8to2)
print(tmcdf8to2)
View(tmcdf8to2)
nrow(tmcdf8to2) # No one reported Transport Mode changed from 8: Official Vehicle to 2: Public Transport (Bus)

# Transport Mode changed from 8: Official Vehicle to 3: Public Transport (Train)  (in the entire dataset)
tmcdf8to3 <- dataCTMSL %>% select(Transport_mode_BEFORE,Transport_Mode_AFTER) %>% filter(Transport_mode_BEFORE == 8 & Transport_Mode_AFTER == 3)
count(tmcdf8to3)
print(tmcdf8to3)
View(tmcdf8to3)
nrow(tmcdf8to3)# No one reported Transport Mode changed from 8: Official Vehicle to 3: Public Transport (Train)

# Transport Mode changed from 8: Official Vehicle to 4: Private Vehicle (Motor Cycle)  (in the entire dataset)
tmcdf8to4 <- dataCTMSL %>% select(Transport_mode_BEFORE,Transport_Mode_AFTER) %>% filter(Transport_mode_BEFORE == 8 & Transport_Mode_AFTER == 4)
count(tmcdf8to4)
print(tmcdf8to4)
View(tmcdf8to4)
nrow(tmcdf8to4)# No one reported Transport Mode changed from 8: Official Vehicle to 4: Private Vehicle (Motor Cycle)

# Transport Mode changed from 8: Official Vehicle to 5: Private Vehicle (Car/Van/Other)  (in the entire dataset)
tmcdf8to5 <- dataCTMSL %>% select(Transport_mode_BEFORE,Transport_Mode_AFTER) %>% filter(Transport_mode_BEFORE == 8 & Transport_Mode_AFTER == 5)
count(tmcdf8to5)
print(tmcdf8to5)
View(tmcdf8to5)
nrow(tmcdf8to5) # 6 People have reported  Transport Mode changed from 8: Official Vehicle to 5: Private Vehicle (Car/Van/Other)

# Transport Mode changed from 8: Official Vehicle to 6: Taxi/ThreeWheeler  (in the entire dataset)
tmcdf8to6 <- dataCTMSL %>% select(Transport_mode_BEFORE,Transport_Mode_AFTER) %>% filter(Transport_mode_BEFORE == 8 & Transport_Mode_AFTER == 6)
count(tmcdf8to6)
print(tmcdf8to6)
View(tmcdf8to6)
nrow(tmcdf8to6)# No one reported Transport Mode changed from 8: Official Vehicle to  6: Taxi/ThreeWheeler

# Transport Mode changed from 8: Official Vehicle to 7: Office Transport Service  (in the entire dataset)
tmcdf8to7 <- dataCTMSL %>% select(Transport_mode_BEFORE,Transport_Mode_AFTER) %>% filter(Transport_mode_BEFORE == 8 & Transport_Mode_AFTER == 7)
count(tmcdf8to7)
print(tmcdf8to7)
View(tmcdf8to7)
nrow(tmcdf8to7) # 2 People have reported  Transport Mode changed from 8: Official Vehicle to 7: Office Transport Service

# Transport Mode 8: Official Vehicle remain same before and after (in the entire dataset)
tmcdf8to8 <- dataCTMSL %>% select(Transport_mode_BEFORE,Transport_Mode_AFTER) %>% filter(Transport_mode_BEFORE == 8 & Transport_Mode_AFTER == 8)
count(tmcdf8to8)
print(tmcdf8to8)
View(tmcdf8to8)
nrow(tmcdf8to8) # 52 poeple reported that Transport Mode 8: Official Vehicle remain same before and after
#-------------------------------------------------------------------------


dataCTMSL %>% summarise(n_distinct(Transport_mode_BEFORE))

WP_DF_employed %>% summarise(n_distinct(Transport_mode_BEFORE))

