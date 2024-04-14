# Author: Kristine Angela Medcalf
# Date: 20/03/2024
#
# Script Name: Nucleus_Histogram.R
# Script Details: Creating histograms for Jaccard indexes of AV1200 Images 000 to 004
#
#
# SET WORKING DIRECTORY --------------------------------------------------------
setwd("~/Documents/research_project")

# import library to create histogram
library(tidyr)

# creating histogram for image 0 
png("/Volumes/Untitled/240116/AV1200/Annotated_Files/Jaccard_Index_Image0.png", width = 2000, height = 1500, res = 300)

# retrieving image 0 data and filtering it for the Jaccard Index
data <- read.csv("/Volumes/Untitled/240116/AV1200/Annotated_Files/000/Jaccard_Indexes for Ground Truth 000.csv")
data_fixed <- gather(data, key = "Power", value = "Jaccard", -Nucleus)
Jaccard_Index0 <- data_fixed$Jaccard

# creating the histogram
image0 <- hist(Jaccard_Index0,
     main="Jaccard Indexes for Image 0",
     xlab="Jaccard Index",
     col="#4682B4",
)

dev.off()

# creating histogram for image 1
png("/Volumes/Untitled/240116/AV1200/Annotated_Files/Jaccard_Index_Image1.png", width = 2000, height = 1500, res = 300)

# retrieving image 1 data and filtering it for the Jaccard Index
data1 <- read.csv("/Volumes/Untitled/240116/AV1200/Annotated_Files/001/Jaccard_Indexes for Ground Truth 001.csv")
data_fixed1 <- gather(data1, key = "Power", value = "Jaccard", -Nucleus)
Jaccard_Index1 <- data_fixed1$Jaccard

# creating the histogram
image1 <- hist(Jaccard_Index1,
     main="Jaccard Indexes for Image 1",
     xlab="Jaccard Index",
     col="#4682B4",
     )

dev.off()

# creating histogram for image 2
png("/Volumes/Untitled/240116/AV1200/Annotated_Files/Jaccard_Index_Image2.png", width = 2000, height = 1500, res = 300)

# retrieving image 2 data and filtering it for the Jaccard Index
data2 <- read.csv("/Volumes/Untitled/240116/AV1200/Annotated_Files/002/Jaccard_Indexes for Ground Truth 002.csv")
data_fixed2 <- gather(data2, key = "Power", value = "Jaccard", -Nucleus)
Jaccard_Index2 <- data_fixed2$Jaccard

# creating the histogram
image2 <- hist(Jaccard_Index2,
     main="Jaccard Indexes for Image 2",
     xlab="Jaccard Index",
     col="#4682B4",
)

dev.off()


# creating histogram for image 3
png("/Volumes/Untitled/240116/AV1200/Annotated_Files/Jaccard_Index_Image3.png", width = 2000, height = 1500, res = 300)

# retrieving image 3 data and filtering it for the Jaccard Index
data3 <- read.csv("/Volumes/Untitled/240116/AV1200/Annotated_Files/003/Jaccard_Indexes for Ground Truth 003.csv")
data_fixed3 <- gather(data3, key = "Power", value = "Jaccard", -Nucleus)
Jaccard_Index3 <- data_fixed3$Jaccard

# creating the histogram
image3 <- hist(Jaccard_Index3,
     main="Jaccard Indexes for Image 3",
     xlab="Jaccard Index",
     col="#4682B4",
)

dev.off()


# creating histogram for image 4
png("/Volumes/Untitled/240116/AV1200/Annotated_Files/Jaccard_Index_Image4.png", width = 2000, height = 1500, res = 300)

# retrieving image 4 data and filtering it for the Jaccard Index
data4 <- read.csv("/Volumes/Untitled/240116/AV1200/Annotated_Files/004/Jaccard_Indexes for Ground Truth 004.csv")
data_fixed4 <- gather(data4, key = "Power", value = "Jaccard", -Nucleus)
Jaccard_Index4 <- data_fixed4$Jaccard

image4 <- hist(Jaccard_Index4,
     main="Jaccard Indexes for Image 4",
     xlab="Jaccard Index",
     col="#4682B4",
)

dev.off()

