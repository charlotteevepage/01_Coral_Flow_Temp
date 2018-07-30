# --------------------
# PAM Raw Formatting #
# --------------------
# ---------------------------------------------
# R script for making a Rmd files for PAM data
# ---------------------------------------------

# Read in RAW data for dark-adapted and noon PAM 
# Reshape the data by finding means of 5 replicates on each coral frag.
# Calculate Qm (`pressure over PSII`) = 1 - [(F/Fm')/(Fv/Fm)]
# Save the resulting files 

# 1. Read and reshape dark-adapted PAM data files. 
#      Create rda. files of data frames
#   a. Montipora ()
#   b. Acropora ()

# 2. Read in noon PAM data files 
#   a. Montipora 
#   b. Acropora 

# 3. Calculate Qm (`pressure over PSII`) = 1 - [(F/Fm')/(Fv/Fm)]
#    Create master rda. files  
#   a. Montipora # save(MontiDigChar, file = "Monti.Rda")
#   b. Acropora   



# Load libraries 

library(tidyverse)
library(reshape2)
library(dplyr)
library(tidyr) 
library(ggplot2)

# ---------------------------------------
# 1. Read in dark-adapted PAM data files 
#   a. Montipora
#   b. Acropora
# ---------------------------------------

# --------------
#   a. Montipora
# --------------

# Read in monti data and do all the corrections 

MontiPAM <- read.csv("Raw_data/PAMMonti.csv")

# Set Treatment and Tank as factors
MontiPAM[,c(2,3)] <- lapply (MontiPAM[,c(2,3)], factor)

# Melt the data frame 

f.zero <- melt(MontiPAM, id.vars = c(1:3), measure.vars = c(5:7),
               variable.name = "F0")

f.max <- melt(MontiPAM, id.vars = c(1:3), measure.vars = c(8:10),
              variable.name = "Fm")

yield <- melt(MontiPAM, id.vars = c(1:3), measure.vars = c(11:13),
              variable.name = "Yield")


# Bind data frames 

MP1 <- cbind(f.zero,f.max[,c(4,5)]) %>%
  cbind(yield[,c(4,5)])

# Rename columns 

colnames(MP1)
colnames(MP1)<- c("Day","Treatment","Tank..", "Coral", "F0", "variable1","Fm","variable","YII")

# Remove other 'variables'

MP1 <- select(MP1, Day:F0, Fm, YII)
head(MP1)

# Check the levels and structure of each data frame

MP1 <- MP1 [order (MP1[,1], MP1[,2], MP1[,3], MP1[,4]),]
head(MP1)

MP1$Day <- MP1$Day - 3

str(MP1)

# Remove Endolith data

MDChar <- MP1 [-which(MP1$Treatment == 'Endolith'),]
MDChar$Treatment <- droplevels(MDChar$Treatment)
MDChar

# Save as an Rda file

save(MDChar, file = "DarkPAMMontiChar.Rda")
load("DarkPAMMontiChar.Rda")

# --------------
#   b. Acropora
# --------------

# Read in acro data and do all the corrections 

AcroPAM <- read.csv("Raw_data/PAMAcro.csv")

# Remove days 1-3

AcroPAM <- AcroPAM[-which(AcroPAM$Day <= 3),]

# Set Treatment and Tank as factors
AcroPAM[,c(2,3)] <- lapply (AcroPAM[,c(2,3)], factor)
str(AcroPAM)
# Melt the data frame 

a_f.zero <- melt(AcroPAM, id.vars = c(1:3), measure.vars = c(5:7),
               variable.name = "F0")

a_f.max <- melt(AcroPAM, id.vars = c(1:3), measure.vars = c(8:10),
              variable.name = "Fm")

a_yield <- melt(AcroPAM, id.vars = c(1:3), measure.vars = c(11:13),
              variable.name = "Yield")

# Bind data frames 

AA1 <- cbind(a_f.zero,a_f.max[,c(4,5)]) %>%
  cbind(a_yield[,c(4,5)])

# Rename columns 

colnames(AA1)
colnames(AA1)<- c("Day","Treatment","Tank..", "Coral", "F0", "variable1","Fm","variable","YII")

# Remove other 'variables'

AA1 <- select(AA1, Day:F0, Fm, YII)
head(AA1)

# Check the levels and structure of each data frame

AA1 <- AA1 [order (AA1[,1], AA1[,2], AA1[,3], AA1[,4]),]
head(AA1)


str(AA1)

# Remove Endolith data

AAChar <- AA1 [-which(AA1$Treatment == 'Endolith'),]
AAChar$Treatment <- droplevels(AAChar$Treatment)
AAChar

# Renumber days 

AAChar$Day <- AAChar$Day - 3
# Save as an Rda file

save(AAChar, file = "DarkPAMAcroChar.Rda")
load("DarkPAMAcroChar.Rda")

# --------------------------------
# 1. Read in noon PAM data files 
#   a. Montipora
#   b. Acropora
# --------------------------------

# --------------
#   a. Montipora
# --------------

# Read in monti data and do all the corrections 

NoonMontiPAM <- read.csv("Raw_Data/Noon_Montipora.csv", stringsAsFactors = FALSE)
str(NoonMontiPAM)

# Temporarily remove columns Treatment, Flow

colnames(NoonMontiPAM)
MAP<- select(NoonMontiPAM, Day, Treatment_match, Tank.., F..1:Y.II...3)

# Remove days 1-3 as per the dark-adapted data 

MAP2 <- MAP[-which(MAP$Day <= 3),]
head(MAP2)

str(MAP2)

# Set Tank as a factor and Day as numeric

MAP2$Tank.. <- as.factor(MAP2$Tank..)
str(MAP2)

MAP2$Day <- as.numeric(MAP2$Day)
str(MAP2)


# Melt the dateframe

m_n_f.zero <- melt(MAP2, id.vars = c(1:3), measure.vars = c(4:6),
               variable.name = "F0")

m_n_f.max <- melt(MAP2, id.vars = c(1:3), measure.vars = c(7:9),
              variable.name = "Fm")

m_n_yield <- melt(MAP2, id.vars = c(1:3), measure.vars = c(10:12),
              variable.name = "Yield")


# Bind these melted data frames into one

MAP3 <- cbind(m_n_f.zero,m_n_f.max[,c(4,5)]) %>%
  cbind(m_n_yield[,c(4,5)])

# Change the column names 

colnames(MAP3)
colnames(MAP3)<- c("Day","Treatment","Tank..", "Coral", "F0", "variable1","Fm","variable","YII")
head(MAP3)

# Remove other 'variables'

MAP4 <- select(MAP3, Day:F0, Fm, YII)
head(MAP4)

# Check the levels and structure of each data frame

MAP4 <- MAP4 [order (MAP4[,1], MAP4[,2], MAP4[,3], MAP4[,4]),]
head(MAP4)

MAP4$Day <- MAP4$Day - 3

str(MAP4)

# --------------
#   a. Acropora
# --------------

# Read in the csv file
NoonAcroPAM <- read.csv("Raw_data/Noon_Acropora.csv")
str(NoonAcroPAM)

# Temporarily remove columns Treatment, Flow

colnames(NoonAcroPAM)
NAP <- select(NoonAcroPAM, Day, Treatment_match, Tank.., F..1:Y.II...3)
head(NAP)

# Remove days 1-3 as per the dark-adapted data 

NAP2<- NAP[-which(NAP$Day <= 3),]
head(NAP2)

# Set Tank as a factor and Day as numeric

NAP2$Tank.. <- as.factor(NAP2$Tank..)
str(NAP2)

NAP2$Day <- as.numeric(NAP2$Day)
str(NAP2)

# Melt the dateframe

a_n_f.zero <- melt(NAP2, id.vars = c(1:3), measure.vars = c(4:6),
               variable.name = "F0")

a_n_f.max <- melt(NAP2, id.vars = c(1:3), measure.vars = c(7:9),
              variable.name = "Fm")

a_n_yield <- melt(NAP2, id.vars = c(1:3), measure.vars = c(10:12),
              variable.name = "Yield")

# Bind these melted data frames into one

colnames(f.max)
AP3 <- cbind(a_n_f.zero,a_n_f.max[,c(4,5)]) %>%
  cbind(a_n_yield[,c(4,5)])

# Change the column names 

colnames(AP3)
colnames(AP3)<- c("Day","Treatment","Tank..", "Coral", "F0", "variable1","Fm","variable","YII")
head(AP3)

# Remove other 'variables'
AP4 <- select(AP3, Day:F0, Fm, YII)
head(AP4)

AP4 <- AP4 [order (AP4[,1], AP4[,2], AP4[,3], AP4[,4]),]
head(AP4)

AP4$Day <- AP4$Day - 3

str(AP4)


# ---------------------------------------------------------------
# 2. Calculate Qm (`pressure over PSII`) = 1 - [(F/Fm')/(Fv/Fm)]
#   a. Montipora
#   b. Acropora
# ---------------------------------------------------------------

# --------------
#   a. Montipora
# --------------

# Make a new data.frame and merge MAP4 and MDChar

MAP5 <- cbind(MAP4, MDChar[,c(5:7)]) 
str(MAP5)

# Change the column names 

colnames(MAP5)<- c("Day","Treatment","Tank..", "Coral", "NoonF0", "NoonFm","NoonYield","F0","Fm","Yield")

# Make a new column with another variable Qm

MAP6 <- mutate(MAP5, Qm = 1 - (NoonYield/Yield))

str(MAP6)

MontiDigChar <- MAP6
str(MontiDigChar)

save(MontiDigChar, file = "Monti.Rda")
load("Monti.Rda")

# -------------
#  b. Acropora
# -------------

# Make a new data.frame and merge AP4 and APChar
str(AAChar)
str(AP4)

AP5 <- cbind(AP4, AAChar[,c(5:7)]) 

str(AP5)
levels(AP5$Treatment)
# Change the column names 

colnames(AP5)<- c("Day","Treatment","Tank..", "Coral", "NoonF0", "NoonFm","NoonYield","F0","Fm","Yield")

# Make a new column with another variable 

AP6 <- mutate(AP5, Qm = 1 - (NoonYield/Yield))

AcroAspChar <- AP6

# Write file

save(AcroAspChar , file = "Acro.Rda")
load("Acro.Rda")
