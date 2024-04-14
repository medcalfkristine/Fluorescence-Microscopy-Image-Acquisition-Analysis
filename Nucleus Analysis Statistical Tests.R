# Author: Kristine Angela Medcalf
# Date: 20/03/2024
#
# Script Name: Statistical Tests.R
# Script Details: run t-test between power intensities and between power and DN equivalent.
# Used for both manual and automated segmentation which requires a change in the files
#
# SET WORKING DIRECTORY --------------------------------------------------------
setwd("~/Documents/research_project")

# importing needed libraries
library(pastecs)
library(car)
library(tidyverse)

# importing data from image 0
file <- read.csv("/Volumes/Untitled/240116/AV1200/Automated/0/Jaccard_Indexes for Ground Truth 0.csv")

# removing N/A from data
clean_data <- na.omit(file)

shapiro_test1 <- shapiro.test(clean_data$X6pc)
shapiro_test2 <- shapiro.test(clean_data$X6pcDN)
shapiro_test3 <- shapiro.test(clean_data$X8pc)
shapiro_test4 <- shapiro.test(clean_data$X8pcDN)

# running t-tests
test01 <- t.test(clean_data$X6pc, clean_data$X6pcDN, paired=T, alternative = "greater") 
test02 <- t.test(clean_data$X8pc, clean_data$X8pcDN, paired=T, alternative = "greater") 
test03 <- t.test(clean_data$X6pc, clean_data$X8pc, paired=T, alternative = "greater") 

# importing data from image 1
file <- read.csv("/Volumes/Untitled/240116/AV1200/Automated/1/Jaccard_Indexes for Ground Truth 1.csv")

# removing N/A from data
clean_data <- na.omit(file)

shapiro_test11 <- shapiro.test(clean_data$X6pc)
shapiro_test12 <- shapiro.test(clean_data$X6pcDN)
shapiro_test13 <- shapiro.test(clean_data$X8pc)
shapiro_test14 <- shapiro.test(clean_data$X8pcDN)

# running t-tests
test11 <- t.test(clean_data$X6pc, clean_data$X6pcDN, paired=T, alternative = "greater") 
test12 <- t.test(clean_data$X8pc, clean_data$X8pcDN, paired=T, alternative = "greater") 
test13 <- t.test(clean_data$X6pc, clean_data$X8pc, paired=T, alternative = "greater") 

# importing data from image 2
file <- read.csv("/Volumes/Untitled/240116/AV1200/Automated/2/Jaccard_Indexes for Ground Truth 2.csv")

# removing N/A from data
clean_data <- na.omit(file)

shapiro_test21 <- shapiro.test(clean_data$X6pc)
shapiro_test22 <- shapiro.test(clean_data$X6pcDN)
shapiro_test23 <- shapiro.test(clean_data$X8pc)
shapiro_test24 <- shapiro.test(clean_data$X8pcDN)

# running t-tests
test21 <- t.test(clean_data$X6pc, clean_data$X6pcDN, paired=T, alternative = "greater") 
test22 <- t.test(clean_data$X8pc, clean_data$X8pcDN, paired=T, alternative = "greater") 
test23 <- t.test(clean_data$X6pc, clean_data$X8pc, paired=T, alternative = "greater")

# importing data from image 3
file <- read.csv("/Volumes/Untitled/240116/AV1200/Automated/3/Jaccard_Indexes for Ground Truth 3.csv")

# removing N/A from data
clean_data <- na.omit(file)

shapiro_test31 <- shapiro.test(clean_data$X6pc)
shapiro_test32 <- shapiro.test(clean_data$X6pcDN)
shapiro_test33 <- shapiro.test(clean_data$X8pc)
shapiro_test34 <- shapiro.test(clean_data$X8pcDN)

# running t-tests
test31 <- t.test(clean_data$X6pc, clean_data$X6pcDN, paired=T, alternative = "greater") 
test32 <- t.test(clean_data$X8pc, clean_data$X8pcDN, paired=T, alternative = "greater") 
test33 <- t.test(clean_data$X6pc, clean_data$X8pc, paired=T, alternative = "greater") 

# importing data from image 3
file <- read.csv("/Volumes/Untitled/240116/AV1200/Automated/4/Jaccard_Indexes for Ground Truth 4.csv")

# removing N/A from data
clean_data <- na.omit(file)

shapiro_test41 <- shapiro.test(clean_data$X6pc)
shapiro_test42 <- shapiro.test(clean_data$X6pcDN)
shapiro_test43 <- shapiro.test(clean_data$X8pc)
shapiro_test44 <- shapiro.test(clean_data$X8pcDN)

# running t-tests
test41 <- t.test(clean_data$X6pc, clean_data$X6pcDN, paired=T, alternative = "greater")
test42 <- t.test(clean_data$X8pc, clean_data$X8pcDN, paired=T, alternative = "greater") 
test43 <- t.test(clean_data$X6pc, clean_data$X8pc, paired=T, alternative = "greater") 

# saving all p-values to a data-frame according to their corresponding images
data <- data.frame(
  Image = c("0","1", "2", "3", "4"), 
  `6pc/6pcDN` = c(test01$p.value, test11$p.value, test21$p.value, test31$p.value, test41$p.value),  
  `8pc/8pcDN` = c(test02$p.value, test12$p.value, test22$p.value, test32$p.value, test42$p.value),  
  `6pc/8pc` = c(test03$p.value, test13$p.value, test23$p.value, test33$p.value, test43$p.value)      
)

# saving the data-frame as a table in a csv file
write_csv(data, file = "/Volumes/Untitled/240116/AV1200/Automated/Statistical Test Results.csv")

