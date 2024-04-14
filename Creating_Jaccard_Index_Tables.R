# Author: Kristine Angela Medcalf
# Date: 20/03/2024
#
# Script Name: Creating Jaccard_Index_Tables.R
# Script Details: merging together Jaccard Index tables created in the Python 
# 'Nucleus Analysis' program to create one big table of Jaccard Indexes for each image
#
#
# SET WORKING DIRECTORY --------------------------------------------------------
setwd("~/Documents/research_project")

library(tidyverse)

# create a CSV table with the Jaccard Index for each power and their corresponding nucleus for Image 0
file <- read.csv('/Volumes/Untitled/240116/AV1200/Automated/0/Automated Jaccard Index 6pc.csv') 
sixpc <- select(file, Nucleus, Jaccard_Index)
file <- read.csv('/Volumes/Untitled/240116/AV1200/Automated/0/Automated Jaccard Index 6pcDN.csv') 
sixpcDN <-  select(file, Jaccard_Index)
file <- read.csv('/Volumes/Untitled/240116/AV1200/Automated/0/Automated Jaccard Index 8pc.csv') 
eightpc <- select(file, Jaccard_Index)
file <- read.csv('/Volumes/Untitled/240116/AV1200/Automated/0/Automated Jaccard Index 8pcDN.csv') 
eightpcDN <- select(file, Jaccard_Index)

merge <- data.frame(sixpc, sixpcDN, eightpc, eightpcDN)

new <- merge %>% 
  rename (
    '6pc' = Jaccard_Index,
    '6pcDN' = Jaccard_Index.1,
    '8pc' = Jaccard_Index.2,
    '8pcDN' = Jaccard_Index.3,
  )

write_csv(new, file = "/Volumes/Untitled/240116/AV1200/Automated/0/Jaccard_Indexes for Ground Truth 0.csv")

# create a CSV table with the Jaccard Index for each power and their corresponding nucleus for Image 1
file <- read.csv('/Volumes/Untitled/240116/AV1200/Automated/1/Automated Jaccard Index 6pc.csv') 
sixpc <- select(file, Nucleus, Jaccard_Index)
file <- read.csv('/Volumes/Untitled/240116/AV1200/Automated/1/Automated Jaccard Index 6pcDN.csv') 
sixpcDN <-  select(file, Jaccard_Index)
file <- read.csv('/Volumes/Untitled/240116/AV1200/Automated/1/Automated Jaccard Index 8pc.csv') 
eightpc <- select(file, Jaccard_Index)
file <- read.csv('/Volumes/Untitled/240116/AV1200/Automated/1/Automated Jaccard Index 8pcDN.csv') 
eightpcDN <- select(file, Jaccard_Index)

merge <- data.frame(sixpc, sixpcDN, eightpc, eightpcDN)

new <- merge %>% 
  rename (
    '6pc' = Jaccard_Index,
    '6pcDN' = Jaccard_Index.1,
    '8pc' = Jaccard_Index.2,
    '8pcDN' = Jaccard_Index.3,
  )

write_csv(new, file = "/Volumes/Untitled/240116/AV1200/Automated/1/Jaccard_Indexes for Ground Truth 000.csv")

# create a CSV table with the Jaccard Index for each power and their corresponding nucleus for Image 2
file <- read.csv('/Volumes/Untitled/240116/AV1200/Annotated_Files/002/Jaccard Index 002_6pc.csv') 
sixpc <- select(file, Nucleus, Jaccard_Index)
file <- read.csv('/Volumes/Untitled/240116/AV1200/Annotated_Files/002/Jaccard Index 002_6pcDN.csv') 
sixpcDN <-  select(file, Jaccard_Index)
file <- read.csv('/Volumes/Untitled/240116/AV1200/Annotated_Files/002/Jaccard Index 002_8pc.csv') 
eightpc <- select(file, Jaccard_Index)
file <- read.csv('/Volumes/Untitled/240116/AV1200/Annotated_Files/002/Jaccard Index 002_8pcDN.csv') 
eightpcDN <- select(file, Jaccard_Index)

merge <- data.frame(sixpc, sixpcDN, eightpc, eightpcDN)

new <- merge %>% 
  rename (
    '6pc' = Jaccard_Index,
    '6pcDN' = Jaccard_Index.1,
    '8pc' = Jaccard_Index.2,
    '8pcDN' = Jaccard_Index.3,
  )

write_csv(new, file = "/Volumes/Untitled/240116/AV1200/Annotated_Files/002/Jaccard_Indexes for Ground Truth 002.csv")

# create a CSV table with the Jaccard Index for each power and their corresponding nucleus for Image 3
file <- read.csv('/Volumes/Untitled/240116/AV1200/Annotated_Files/003/Jaccard Index 003_6pc.csv') 
sixpc <- select(file, Nucleus, Jaccard_Index)
file <- read.csv('/Volumes/Untitled/240116/AV1200/Annotated_Files/003/Jaccard Index 003_6pcDN.csv') 
sixpcDN <-  select(file, Jaccard_Index)
file <- read.csv('/Volumes/Untitled/240116/AV1200/Annotated_Files/003/Jaccard Index 003_8pc.csv') 
eightpc <- select(file, Jaccard_Index)
file <- read.csv('/Volumes/Untitled/240116/AV1200/Annotated_Files/003/Jaccard Index 003_8pcDN.csv') 
eightpcDN <- select(file, Jaccard_Index)

merge <- data.frame(sixpc, sixpcDN, eightpc, eightpcDN)

new <- merge %>% 
  rename (
    '6pc' = Jaccard_Index,
    '6pcDN' = Jaccard_Index.1,
    '8pc' = Jaccard_Index.2,
    '8pcDN' = Jaccard_Index.3,
  )

write_csv(new, file = "/Volumes/Untitled/240116/AV1200/Annotated_Files/003/Jaccard_Indexes for Ground Truth 003.csv")

# create a CSV table with the Jaccard Index for each power and their corresponding nucleus for Image 4
file <- read.csv('/Volumes/Untitled/240116/AV1200/Annotated_Files/004/Jaccard Index 004_6pc.csv') 
sixpc <- select(file, Nucleus, Jaccard_Index)
file <- read.csv('/Volumes/Untitled/240116/AV1200/Annotated_Files/004/Jaccard Index 004_6pcDN.csv') 
sixpcDN <-  select(file, Jaccard_Index)
file <- read.csv('/Volumes/Untitled/240116/AV1200/Annotated_Files/004/Jaccard Index 004_8pc.csv') 
eightpc <- select(file, Jaccard_Index)
file <- read.csv('/Volumes/Untitled/240116/AV1200/Annotated_Files/004/Jaccard Index 004_8pcDN.csv') 
eightpcDN <- select(file, Jaccard_Index)

merge <- data.frame(sixpc, sixpcDN, eightpc, eightpcDN)

new <- merge %>% 
  rename (
    '6pc' = Jaccard_Index,
    '6pcDN' = Jaccard_Index.1,
    '8pc' = Jaccard_Index.2,
    '8pcDN' = Jaccard_Index.3,
  )

write_csv(new, file = "/Volumes/Untitled/240116/AV1200/Annotated_Files/004/Jaccard_Indexes for Ground Truth 004.csv")

