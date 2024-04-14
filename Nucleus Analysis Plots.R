# Author: Kristine Angela Medcalf
# Date: 20/03/2024
#
# Script Name: Nucleus Analysis Plots.R
# Script Details: creating a boxplot image showing the correlation 
# between Jaccard index and parameters for both manual and automated segmentation
#
#
# SET WORKING DIRECTORY --------------------------------------------------------
setwd("~/Documents/research_project")

library(ggplot2)
library(tidyr)
library(reshape2)

# creating plot 1

# loading the data for each image
image0 <- read.csv("/Volumes/Untitled/240116/AV1200/Automated/0/Jaccard_Indexes for Ground Truth 0.csv")
image1 <- read.csv("/Volumes/Untitled/240116/AV1200/Automated/1/Jaccard_Indexes for Ground Truth 1.csv")
image2 <- read.csv("/Volumes/Untitled/240116/AV1200/Automated/2/Jaccard_Indexes for Ground Truth 2.csv")
image3 <- read.csv("/Volumes/Untitled/240116/AV1200/Automated/3/Jaccard_Indexes for Ground Truth 3.csv")
image4 <- read.csv("/Volumes/Untitled/240116/AV1200/Automated/4/Jaccard_Indexes for Ground Truth 4.csv")

# creating a function to retrieve the mean, min, and max for each image
calculate_summary <- function(data) {
  data_summary <- c(
    mean = mean(data, na.rm = TRUE),
    min = min(data, na.rm = TRUE),
    max = max(data, na.rm = TRUE)
  )
  return(data_summary)
}

# creating a dataframe for image 0 for the corressponding power data
image0data <- data.frame(
  'Image' =  c(0),
  '6pc' = calculate_summary(image0$X6pc),
  '6pcDN' = calculate_summary(image0$X6pcDN),
  '8pc' = calculate_summary(image0$X8pc),
  '8pcDN' = calculate_summary(image0$X8pcDN)
)

# creating a dataframe for image 1 for the corressponding power data
image1data <- data.frame(
  'Image' =  c(1),
  '6pc' = calculate_summary(image1$X6pc),
  '6pcDN' = calculate_summary(image1$X6pcDN),
  '8pc' = calculate_summary(image1$X8pc),
  '8pcDN' = calculate_summary(image1$X8pcDN)
)
# creating a dataframe for image 2 for the corressponding power data
image2data <- data.frame(
  'Image' =  c(2),
  '6pc' = calculate_summary(image2$X6pc),
  '6pcDN' = calculate_summary(image2$X6pcDN),
  '8pc' = calculate_summary(image2$X8pc),
  '8pcDN' = calculate_summary(image2$X8pcDN)
)

# creating a dataframe for image 3 for the corressponding power data
image3data <- data.frame(
  'Image' =  c(3),
  '6pc' = calculate_summary(image3$X6pc),
  '6pcDN' = calculate_summary(image3$X6pcDN),
  '8pc' = calculate_summary(image3$X8pc),
  '8pcDN' = calculate_summary(image3$X8pcDN)
)

# creating a dataframe for image 4 for the corressponding power data
image4data <- data.frame(
  'Image' =  c(4),
  '6pc' = calculate_summary(image4$X6pc),
  '6pcDN' = calculate_summary(image4$X6pcDN),
  '8pc' = calculate_summary(image4$X8pc),
  '8pcDN' = calculate_summary(image4$X8pcDN)
)

# merging the multiple dataframes together
all_data <- list(image0data, image1data, image2data, image3data, image4data)
merged_data <- do.call(rbind, all_data)

colnames(merged_data) <- c('Image', "6pc", "6pc+DN", "8pc", "8pc+DN")

meansix = 0.8394322
meansixDN = 0.8640026
meaneight = 0.863192
meaneightDN = 0.8819052
  
# reorganising the data frame for plot creation
data_long <- gather(merged_data, key = "Power", value = "Value", -Image)

# creating a boxplot
my_plot <- ggplot(data_long, aes(x = Power, y = Value, fill = as.factor(Image))) +
  geom_boxplot(width = 0.7, outlier.shape = NA) +
  geom_point(data = data_long[data_long$Power == "mean", ], aes(color = "Mean"), size = 3, shape = 3) +
  annotate("text", x = 1, y = meansix, label = paste("Mean =", round(meansix, 3)), 
           vjust = -7, hjust = 0.5, size = 4, color = "black") +
  annotate("text", x = 2, y = meansixDN, label = paste("Mean =", round(meansixDN, 3)), 
           vjust = -6, hjust = 0.55, size = 4, color = "black") +
  annotate("text", x = 3, y = meaneight, label = paste("Mean =", round(meaneight, 3)), 
           vjust = -6, hjust = 0.60, size = 4, color = "black") +
  annotate("text", x = 4, y = meaneightDN, label = paste("Mean =", round(meaneightDN, 3)), 
           vjust = -5.5, hjust = 0.6, size = 4, color = "black") +
  labs(x = "Parameter", y = "Jaccard Index", title = "Automated Mean Jaccard Indexes Across Image Acquisition Parameters", fill = "Image") +
  theme(plot.title = element_text(hjust = 0.5, face = "bold", size = 15),
        axis.title = element_text(size = 14, face= "bold")) +
  scale_fill_manual(values = c("0" = "#009392", "1" = "#39B1B5", "2" = "#9CCB86", 
                               "3" = "#E9E29C", "4" = "#EEB479")) +  
  scale_color_manual(values = c("Mean" = "black"))

# saving the boxplot as a png
ggsave("/Volumes/Untitled/240116/AV1200/Automated/Comparison of Mean Jaccard Indexes by Image Acquisition Parameter Automateds.jpeg", plot = my_plot, width = 8, height = 6, dpi = 300)


# ----------------------------------------------------------------------------
# creating plot 2

# merging files together and indicating order of columns
merged_data_frames <- rbind(image0, image1, image2, image3, image4)
colnames(merged_data_frames) <- c('Nucleus', "6pc", "6pcDN", "8pc", "8pcDN")

# reorders dataset 
data_long <- melt(merged_data_frames)
data_long <- data_long[-(1:390), ]

# Create violin plot and saves as png
plot <- ggplot(data_long, aes(x = variable, y = value, fill = variable)) +
  geom_violin(width = 0.7) +  # Adjust the width as needed
  labs(x = "\nPower Intensity", y = "Jaccard Index\n") +
  ggtitle("Distribution of Jaccard Index Across Power Intensities for AV1200 Images") +
  scale_fill_manual(values = c("6pc" = "#009392", "6pcDN" = "#39B1B5", 
                               "8pc" = "#9CCB86", "8pcDN" = "#E9E29C")) +
  theme_gray() +
  theme(legend.position = "none",
        plot.title = element_text(hjust = 1, face = "bold", size = 15),
        axis.title = element_text(size = 14, face= "bold"),  # Adjust the size as needed
        axis.text.x = element_text(size = 11)) # Adjust the margin as needed

ggsave("/Volumes/Untitled/240116/AV1200/Distribution of Jaccard Index Across Power Intensities for AV1200 Images.png", plot = plot, width = 8, height = 6, dpi = 300)

# create datasets for specific illumination intensities
selected_columns_6 <- merged_data_frames[, c("6pc", "6pcDN")]
selected_columns_8 <- merged_data_frames[, c("8pc", "8pcDN")]
selected_columns_6and8 <- merged_data_frames[, c("6pc", "8pc")]

# restructures datasets
data_long6 <- melt(selected_columns_6)
data_long8 <- melt(selected_columns_8)
data_long6minus8 <- melt(selected_columns_6and8)

# runs t-tests
test01 <- t.test(merged_data_frames$'6pc', merged_data_frames$'6pcDN', paired=T, alternative = "greater") 
test02 <- t.test(merged_data_frames$'8pc', merged_data_frames$'8pcDN', paired=T, alternative = "greater") 
test03 <- t.test(merged_data_frames$'6pc', merged_data_frames$'8pc', paired=T, alternative = "greater") 

# get p-value
valuesix =test01$p.value
valueeight = test02$p.value
valuepower =test03$p.value
  
# create plot for 6pc intensity and saves as png
plot6 <- ggplot(data_long6, aes(x = variable, y = value, fill = variable)) +
  geom_violin(width = 0.7) +  # Adjust the width as needed
  labs(x = "\nParameter", y = "Jaccard Index\n") +
  ggtitle("Jaccard Index Distributions Between 6% Illumination and Denoised Equivalent") +
  annotate("text", x = 1, y = mean_value, label = paste("p-value =", round(valuesix, 3)), 
           fontface = 'bold', vjust = 20, hjust = -1.3, size = 4, color = "red") +
  scale_fill_manual(values = c("6pc" = "#009392", "6pcDN" = "#EEB479")) +  # Corrected line
  theme_gray() +
  theme(legend.position = "none",
        plot.title = element_text(hjust = 0.5, face = "bold", size = 15),
        axis.title = element_text(size = 14, face= "bold"),  # Adjust the size as needed
        axis.text.x = element_text(size = 11))

ggsave("/Volumes/Untitled/240116/AV1200/Distribution of Jaccard Index for 6pc vs. 6pc+DN for AV1200 Images.png", plot = plot6, width = 8, height = 6, dpi = 300)


# Create the plot for the second set of data but for 8pc intensity
plot8 <- ggplot(data_long8, aes(x = variable, y = value, fill = variable)) +
  geom_violin(width = 0.7) +
  labs(x = "\nParameter", y = "Jaccard Index\n") +
  ggtitle("Jaccard Index Distributions Between 8% Illumination and Denoised Equivalent") +
  annotate("text", x = 1, y = mean_value, label = paste("p-value =", round(valueeight, 3)), 
           fontface = "bold", vjust = 20, hjust = -1.3, size = 4, color = "red") +
  scale_fill_manual(values = c("8pc" = "#E88471", "8pcDN" = "#E9E29C")) +
  theme_gray() +
  theme(legend.position = "none",
        plot.title = element_text(hjust = 0.5, face = "bold", size = 15),
        axis.title = element_text(size = 14, face= "bold"),
        axis.text.x = element_text(size = 11))
plot8
ggsave("/Volumes/Untitled/240116/AV1200/Distribution of Jaccard Index for 8pc vs. 8pc+DN for AV1200 Images.png", plot = plot8, width = 8, height = 6, dpi = 300)

# Create the plot for the third set of data but between 6pc and 8pc (without denoising)
plot6minus8 <- ggplot(data_long6minus8, aes(x = variable, y = value, fill = variable)) +
  geom_violin(width = 0.7) +
  labs(x = "\nParameter", y = "Jaccard Index\n") +
  ggtitle("Jaccard Index Distributions Between Power Intensities") +
  annotate("text", x = 1, y = mean_value, label = paste("p-value =", round(valuepower, 3)), 
           fontface = "bold", vjust = 20, hjust = -1.3, size = 4, color = "red") +
  scale_fill_manual(values = c("6pc" = "#009392", "8pc" = "#E88471")) +
  theme_gray() +
  theme(legend.position = "none",
        plot.title = element_text(hjust = 0.5, face = "bold", size = 15),
        axis.title = element_text(size = 14, face= "bold"),
        axis.text.x = element_text(size = 11))

ggsave("/Volumes/Untitled/240116/AV1200/istribution of Jaccard Index for 6pc vs. 8pc for AV1200 Images.png", plot = plot6minus8, width = 8, height = 6, dpi = 300)


#-----------
# loading the data for each annotated image
image0 <- read.csv("/Volumes/Untitled/240116/AV1200/Annotated_Files/000/Processed Data/Jaccard_Indexes for Ground Truth 000.csv")
image1 <- read.csv("/Volumes/Untitled/240116/AV1200/Annotated_Files/001/Jaccard_Indexes for Ground Truth 001.csv")
image2 <- read.csv("/Volumes/Untitled/240116/AV1200/Annotated_Files/002/Jaccard_Indexes for Ground Truth 002.csv")
image3 <- read.csv("/Volumes/Untitled/240116/AV1200/Annotated_Files/003/Jaccard_Indexes for Ground Truth 003.csv")
image4 <- read.csv("/Volumes/Untitled/240116/AV1200/Annotated_Files/004/Jaccard_Indexes for Ground Truth 004.csv")

# merges data together, adds column saying indicating it manual data, and removes nucleus column
all_data <- list(image0, image1, image2, image3, image4)
merged_data <- do.call(rbind, all_data)
merged_data$Type <- "Manual"
merged_data <- subset(merged_data, select = -Nucleus)

# indicates order of parameters and restructures dataset
power_order <- c("X6pc", "X6pcDN", "X8pc", "X8pcDN", "Type") 
new_power_names <- c("6pc", "6pcDN", "8pc", "8pcDN", "Type") 
data_long <- gather(merged_data, key = "Power", value = "Value", -Type)
data_long$Power <- factor(data_long$Power, levels = power_order)

# loading the data for each automated image
image0 <- read.csv("/Volumes/Untitled/240116/AV1200/Automated/0/Jaccard_Indexes for Ground Truth 0.csv")
image1 <- read.csv("/Volumes/Untitled/240116/AV1200/Automated/1/Jaccard_Indexes for Ground Truth 1.csv")
image2 <- read.csv("/Volumes/Untitled/240116/AV1200/Automated/2/Jaccard_Indexes for Ground Truth 2.csv")
image3 <- read.csv("/Volumes/Untitled/240116/AV1200/Automated/3/Jaccard_Indexes for Ground Truth 3.csv")
image4 <- read.csv("/Volumes/Untitled/240116/AV1200/Automated/4/Jaccard_Indexes for Ground Truth 4.csv")

# merges data together, adds column saying indicating it automated data, and removes nucleus column
aautomated_all_data <- list(image0, image1, image2, image3, image4)
merged_data_automated <- do.call(rbind, aautomated_all_data)
merged_data_automated$Type <- "Automated"
merged_data_automated <- subset(merged_data_automated, select = -Nucleus)

# indicates order of parameters and restructures dataset
automated_data_long <- gather(merged_data_automated, key = "Power", value = "Value", -Type)
automated_data_long$Power <- factor(automated_data_long$Power, levels = power_order)

# combines both datasets together
combined_df <- rbind(automated_data_long, data_long)

# creating a boxplot and saves as png
my_plot <- ggplot(combined_df, aes(x = Power, y = Value, fill = as.factor(Type))) +
  geom_boxplot(width = 0.7, outlier.shape = NA) +
  labs(x = "Power", y = "Jaccard Index", title = "AV1200 Comparison Between Automated and Manual Segmentation", fill = "Image") +
  theme(plot.title = element_text(hjust = 0.5, face = "bold")) + 
  scale_fill_manual(values = c("Manual" = "#045275", "Automated" = "#E88471") )+
  scale_x_discrete(labels = new_power_names)

ggsave("/Volumes/Untitled 1/AV1200 Comparison Between Automated and Manual Segmentation.png", plot = my_plot, width = 8, height = 6, dpi = 300)


