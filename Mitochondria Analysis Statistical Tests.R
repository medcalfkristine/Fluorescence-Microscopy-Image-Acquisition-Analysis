# Author: Kristine Angela Medcalf
# Date: 20/03/2024
#
# Script Name: Statistical Tests.R
# Script Details: run t-test between power intensities and between power and DN equivalent (used for both manual and annotated)
#
#
# SET WORKING DIRECTORY --------------------------------------------------------
setwd("~/Documents/research_project")

# importing needed libraries
library(pastecs)
library(car)
library(tidyverse)

# importing data 
file1 <- read.csv("/Volumes/Untitled 1/Annotated/Jaccard Index Sdh2 001.csv")
file2 <- read.csv("/Volumes/Untitled 1/Annotated/Jaccard Index Sdh2 002.csv")
file3 <- read.csv("/Volumes/Untitled 1/Annotated/Jaccard Index Sdh2 003.csv")
file4 <- read.csv("/Volumes/Untitled 1/Annotated/Jaccard Index Sdh2 004.csv")
file5 <- read.csv("/Volumes/Untitled 1/Annotated/Jaccard Index Sdh2 005.csv")
file6 <- read.csv("/Volumes/Untitled 1/Annotated/Jaccard Index Sdh2 006.csv")
file7 <- read.csv("/Volumes/Untitled 1/Annotated/Jaccard Index Sdh2 007.csv")
file8 <- read.csv("/Volumes/Untitled 1/Annotated/Jaccard Index Sdh2 008.csv")
file9 <- read.csv("/Volumes/Untitled 1/Annotated/Jaccard Index Sdh2 009.csv")
file10 <- read.csv("/Volumes/Untitled 1/Annotated/Jaccard Index Sdh2 010.csv")

# omit NA values
clean_data1 <- na.omit(file1)
clean_data2 <- na.omit(file2)
clean_data3 <- na.omit(file3)
clean_data4 <- na.omit(file4)
clean_data5 <- na.omit(file5)
clean_data6 <- na.omit(file6)
clean_data7 <- na.omit(file7)
clean_data8 <- na.omit(file8)
clean_data9 <- na.omit(file9)
clean_data1 <- na.omit(file10)

# test if parameteric
shapiro_test1 <- shapiro.test(clean_data1$'X50ms_8pc')
shapiro_test2 <- shapiro.test(clean_data1$'X50ms_8pcDN')
shapiro_test3 <- shapiro.test(clean_data1$'X50ms_12pc')
shapiro_test4 <- shapiro.test(clean_data1$'X50ms_12pcDN')
shapiro_test5 <- shapiro.test(clean_data1$'X50ms_20pc')
shapiro_test6 <- shapiro.test(clean_data1$'X50ms_20pcDN')
shapiro_test7 <- shapiro.test(clean_data1$'X100ms_4pc')
shapiro_test8 <- shapiro.test(clean_data1$'X100ms_4pcDN')
shapiro_test9 <- shapiro.test(clean_data1$'X100ms_6pc')
shapiro_test10 <- shapiro.test(clean_data1$'X100ms_6pcDN')
shapiro_test11 <- shapiro.test(clean_data1$'X100ms_10pc')
shapiro_test12 <- shapiro.test(clean_data1$'X100ms_10pcDN')

# all corresponding t-tests:

# tests for denoising
test01 <- t.test(clean_data1$'X50ms_8pcDN', clean_data1$'X50ms_8pc', paired=T, alternative = "greater") 
test02 <- t.test(clean_data1$'X50ms_12pcDN', clean_data1$'X50ms_12pc', paired=T, alternative = "greater") 
test03 <- t.test(clean_data1$'X50ms_20pcDN', clean_data1$'X50ms_20pc', paired=T, alternative = "greater") 
test04 <- t.test(clean_data1$'X100ms_4pcDN', clean_data1$'X100ms_4pc', paired=T, alternative = "greater") 
test05 <- t.test(clean_data1$'X100ms_6pcDN', clean_data1$'X100ms_6pc', paired=T, alternative = "greater") 
test06 <- t.test(clean_data1$'X100ms_10pcDN', clean_data1$'X100ms_10pc', paired=T, alternative = "greater") 

# tests for exposure time
test07 <- t.test(clean_data1$'X50ms_8pc', clean_data1$'X100ms_4pcDN', paired=T, alternative = "two.sided") 
test08 <- t.test(clean_data1$'X50ms_12pc', clean_data1$'X100ms_6pcDN', paired=T, alternative = "two.sided") 
test09 <- t.test(clean_data1$'X50ms_20pc', clean_data1$'X100ms_10pcDN', paired=T, alternative = "two.sided") 
test10 <- t.test(clean_data1$'X50ms_8pcDN', clean_data1$'X100ms_4pcDN', paired=T, alternative = "two.sided") 
test11 <- t.test(clean_data1$'X50ms_12pcDN', clean_data1$'X100ms_6pcDN', paired=T, alternative = "two.sided") 
test12 <- t.test(clean_data1$'X50ms_20pcDN', clean_data1$'X100ms_10pcDN', paired=T, alternative = "two.sided") 

# tests for illumination intensity
test13 <- t.test(clean_data1$'X50ms_12pc', clean_data1$'X50ms_8pc', paired=T, alternative = "greater") 
test14 <- t.test(clean_data1$'X50ms_20pc', clean_data1$'X50ms_12pc', paired=T, alternative = "greater") 
test15 <- t.test(clean_data1$'X50ms_20pc', clean_data1$'X50ms_8pc', paired=T, alternative = "greater") 
test16 <- t.test(clean_data1$'X100ms_6pc', clean_data1$'X100ms_4pc', paired=T, alternative = "greater") 
test17 <- t.test(clean_data1$'X100ms_10pc', clean_data1$'X100ms_6pc', paired=T, alternative = "greater") 
test18 <- t.test(clean_data1$'X100ms_10pc', clean_data1$'X100ms_4pc', paired=T, alternative = "greater") 


# saving all p-values to a data-frame according to their corresponding images
data1 <- data.frame(
  Image = c("9"), 
  "50ms_8pc/50ms_8pcDN" = c(test01$p.value),  
  "50ms_12pc/50ms_12pcDN" = c(test02$p.value),  
  "50ms_20pc/50ms_20pcDN" = c(test03$p.value),
  "100ms_4pc/100ms_4pcDN" = c(test04$p.value),  
  "100ms_6pc/100ms_6pcDN" = c(test05$p.value),  
  "100ms_10pc/100ms_10pcDN" = c(test06$p.value)   
)


data2 <- data.frame(
  Image = c("9"), 
  "50ms_8pc/50ms_12pc" = c(test13$p.value),   
  "50ms_12pc/50ms_20pc" = c(test14$p.value), 
  "50ms_8pc/50ms_20pc" = c(test15$p.value), 
  "100ms_4pc/100ms_6pc" = c(test16$p.value), 
  "100ms_6pc/100ms_10pc" = c(test17$p.value), 
  "100ms_4pc/100ms_10pc" = c(test18$p.value)
)

data3 <- data.frame(
  Image = c("9"), 
  "50ms_8pc/100ms_4pc" = c(test07$p.value),   
  "50ms_12pc/100ms_6pc" = c(test08$p.value), 
  "50ms_20pc/100ms_10pc" = c(test09$p.value), 
  "50ms_8pcDN/100ms_4pcDN" = c(test10$p.value),   
  "50ms_12pcDN/100ms_6pcDN" = c(test11$p.value), 
  "50ms_20pcDN/100ms_10pcDN" = c(test12$p.value)
)

# adds next p-values onto plot
write.table(data1, file = "/Volumes/Untitled 1/Annotated/Statistical Test Mitochondria DN Results.csv", sep = ",", col.names = FALSE, row.names = FALSE, append = TRUE)
write.table(data2, file = "/Volumes/Untitled 1/Annotated/Statistical Test Mitochondria Power Results.csv", sep = ",", col.names = FALSE, row.names = FALSE, append = TRUE)
write.table(data3, file = "/Volumes/Untitled 1/Annotated/Statistical Test 50 vs 100 Results.csv", sep = ",", col.names = FALSE, row.names = FALSE, append = TRUE)


# saving the data-frame as a table in a csv file - only run when first creating plot but not adding on to plot
write_csv(data1, file = "/Volumes/Untitled 1/Annotated/Statistical Test Mitochondria DN Results.csv")
write_csv(data2, file = "/Volumes/Untitled 1/Annotated/Statistical Test Mitochondria Power Results.csv")
write_csv(data3, file = "/Volumes/Untitled 1/Annotated/Statistical Test 50 vs 100 Results.csv")

