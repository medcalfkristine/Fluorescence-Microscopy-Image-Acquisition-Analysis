# Date: 01/04/2024
#
# Script Name: Mitochondrial Analysis Plot.R
# Script Details: creates multiples plots used for the mitochondrial analysis (both automated and manual)
#
#
# SET WORKING DIRECTORY --------------------------------------------------------
setwd("~/Documents/research_project")

# importing needed libraries
library(pastecs)
library(car)
library(tidyverse)
library(ggplot2)
library(dplyr)
library(reshape2)

# importing files
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

# creating a column to denote which images they are from 
# removing the 50ms exposure time parameters which I do not want included in the annotated 100ms plot 

file1$Image <- 1
file1 <- file1[, !colnames(file1) %in% c("Cell", 'X50ms_8pc', 'X50ms_8pcDN', 'X50ms_12pc', 'X50ms_12pcDN', 'X50ms_20pc', 'X50ms_20pcDN')]

file2$Image <- 2
file2 <- file2[, !colnames(file2) %in% c("Cell", 'X50ms_8pc', 'X50ms_8pcDN', 'X50ms_12pc', 'X50ms_12pcDN', 'X50ms_20pc', 'X50ms_20pcDN')]

file3$Image <- 3
file3 <- file3[, !colnames(file3) %in% c("Cell", 'X50ms_8pc', 'X50ms_8pcDN', 'X50ms_12pc', 'X50ms_12pcDN', 'X50ms_20pc', 'X50ms_20pcDN')]

file4$Image <- 4
file4 <- file4[, !colnames(file4) %in% c("Cell", 'X50ms_8pc', 'X50ms_8pcDN', 'X50ms_12pc', 'X50ms_12pcDN', 'X50ms_20pc', 'X50ms_20pcDN')]

file5$Image <- 5
file5 <- file5[, !colnames(file5) %in% c("Cell", 'X50ms_8pc', 'X50ms_8pcDN', 'X50ms_12pc', 'X50ms_12pcDN', 'X50ms_20pc', 'X50ms_20pcDN')]

file6$Image <- 6
file6 <- file6[, !colnames(file6) %in% c("Cell", 'X50ms_8pc', 'X50ms_8pcDN', 'X50ms_12pc', 'X50ms_12pcDN', 'X50ms_20pc', 'X50ms_20pcDN')]

file7$Image <- 7
file7 <- file7[, !colnames(file7) %in% c("Cell", 'X50ms_8pc', 'X50ms_8pcDN', 'X50ms_12pc', 'X50ms_12pcDN', 'X50ms_20pc', 'X50ms_20pcDN')]

file8$Image <- 8
file8 <- file8[, !colnames(file8) %in% c("Cell", 'X50ms_8pc', 'X50ms_8pcDN', 'X50ms_12pc', 'X50ms_12pcDN', 'X50ms_20pc', 'X50ms_20pcDN')]

file9$Image <- 9
file9 <- file9[, !colnames(file9) %in% c("Cell", 'X50ms_8pc', 'X50ms_8pcDN', 'X50ms_12pc', 'X50ms_12pcDN', 'X50ms_20pc', 'X50ms_20pcDN')]

file10$Image <- 10
file10 <- file10[, !colnames(file10) %in% c("Cell", 'X50ms_8pc', 'X50ms_8pcDN', 'X50ms_12pc', 'X50ms_12pcDN', 'X50ms_20pc', 'X50ms_20pcDN')]

# merging all datasets together
all_data <- list(file1, file2, file3, file4, file5, file6, file7, file8, file9, file10)
merged_data <- do.call(rbind, all_data)

# changes the structure of the dataset to be three columns
data_long <- gather(merged_data, key = "Power", value = "Value", -Type)

# ensures that the parameters are order when shown in graph and the names are suitable
power_order <- c("X100ms_4pc", "X100ms_4pcDN", "X100ms_6pc", "X100ms_6pcDN", "X100ms_10pc", "X100ms_10pcDN") 
new_power_names <- c("4pc", "4pcDN", "6pc", "6pcDN", "10pc", "10pcDN") 
data_long$Power <- factor(data_long$Power, levels = power_order)

# creating a boxplot
my_plot <- ggplot(data_long, aes(x = Power, y = Value, fill = as.factor(Image)))+
  geom_boxplot(width = 0.7, outlier.shape = NA) +
  labs(x = "Power", y = "Jaccard Index", title = "Comparison of Jaccard Index Across Power Levels for 100ms Exposure Time Images", fill = "Image") +
  theme(plot.title = element_text(hjust = 0.5, face = "bold")) + 
  scale_fill_manual(values = c("1" = "#045275", "2" = "#009392", "3" = "#39B185", 
                               "4" = "#9CCB86", "5" = "#E9E29C", "6" = "#EEB479",
                               "7" = "#E88471", "8" = "#CF597E", "9" = "#7C1D6F", "10" = "#8A4D9E")) +
  scale_x_discrete(labels = new_power_names)

# saves boxplot as png
ggsave("/Volumes/Untitled 1/Annotated/Comparison of Jaccard Index Across Power Levels for 100ms Exposure Time Images.png", plot = my_plot, width = 8, height = 6, dpi = 300)

# ---------------------

# repeats above process but now creates a plot for 50ms
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

file1$Image <- 1
file1 <- file1[, !colnames(file1) %in% c("Cell", "X100ms_4pc", "X100ms_4pcDN", "X100ms_6pc", "X100ms_6pcDN", "X100ms_10pc", "X100ms_10pcDN")]

file2$Image <- 2
file2 <- file2[, !colnames(file2) %in% c("Cell", "X100ms_4pc", "X100ms_4pcDN", "X100ms_6pc", "X100ms_6pcDN", "X100ms_10pc", "X100ms_10pcDN")]

file3$Image <- 3
file3 <- file3[, !colnames(file3) %in% c("Cell", "X100ms_4pc", "X100ms_4pcDN", "X100ms_6pc", "X100ms_6pcDN", "X100ms_10pc", "X100ms_10pcDN")]

file4$Image <- 4
file4 <- file4[, !colnames(file4) %in% c("Cell", "X100ms_4pc", "X100ms_4pcDN", "X100ms_6pc", "X100ms_6pcDN", "X100ms_10pc", "X100ms_10pcDN")]

file5$Image <- 5
file5 <- file5[, !colnames(file5) %in% c("Cell", "X100ms_4pc", "X100ms_4pcDN", "X100ms_6pc", "X100ms_6pcDN", "X100ms_10pc", "X100ms_10pcDN")]

file6$Image <- 6
file6 <- file6[, !colnames(file6) %in% c("Cell", "X100ms_4pc", "X100ms_4pcDN", "X100ms_6pc", "X100ms_6pcDN", "X100ms_10pc", "X100ms_10pcDN")]

file7$Image <- 7
file7 <- file7[, !colnames(file7) %in% c("Cell", "X100ms_4pc", "X100ms_4pcDN", "X100ms_6pc", "X100ms_6pcDN", "X100ms_10pc", "X100ms_10pcDN")]

file8$Image <- 8
file8 <- file8[, !colnames(file8) %in% c("Cell", "X100ms_4pc", "X100ms_4pcDN", "X100ms_6pc", "X100ms_6pcDN", "X100ms_10pc", "X100ms_10pcDN")]

file9$Image <- 9
file9 <- file9[, !colnames(file9) %in% c("Cell", "X100ms_4pc", "X100ms_4pcDN", "X100ms_6pc", "X100ms_6pcDN", "X100ms_10pc", "X100ms_10pcDN")]

file10$Image <- 10
file10 <- file10[, !colnames(file10) %in% c("Cell", "X100ms_4pc", "X100ms_4pcDN", "X100ms_6pc", "X100ms_6pcDN", "X100ms_10pc", "X100ms_10pcDN")]

all_data <- list(file1, file2, file3, file4, file5, file6, file7, file8, file9, file10)
merged_data <- do.call(rbind, all_data)

data_long <- gather(merged_data, key = "Power", value = "Value", -Image)

power_order <- c('X50ms_8pc', 'X50ms_8pcDN', 'X50ms_12pc', 'X50ms_12pcDN', 'X50ms_20pc', 'X50ms_20pcDN') 
new_power_names <- c('8pc', '8pcDN', '12pc', '12pcDN', '20pc', '20pcDN') 
data_long$Power <- factor(data_long$Power, levels = power_order)

# creating boxplot
my_plot <- ggplot(data_long, aes(x = Power, y = Value, fill = as.factor(Image)))+
  geom_boxplot(width = 0.7, outlier.shape = NA) +
  labs(x = "Power", y = "Jaccard Index", title = "Comparison of Jaccard Index Across Power Levels for 50ms Exposure Time Images", fill = "Image") +
  theme(plot.title = element_text(hjust = 0.5, face = "bold")) + 
  scale_fill_manual(values = c("1" = "#045275", "2" = "#009392", "3" = "#39B185", 
                               "4" = "#9CCB86", "5" = "#E9E29C", "6" = "#EEB479",
                               "7" = "#E88471", "8" = "#CF597E", "9" = "#7C1D6F", "10" = "#8A4D9E")) +
  scale_x_discrete(labels = new_power_names)

ggsave("/Volumes/Untitled 1/Annotated/Comparison of Jaccard Index Across Power Levels for 50ms Images.png", plot = my_plot, width = 8, height = 6, dpi = 300)

# ---------------------

# reading files
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

# omitting NA values
file1 <- na.omit(file1)
file2 <- na.omit(file2)
file3 <- na.omit(file3)
file4 <- na.omit(file4)
file5 <- na.omit(file5)
file6 <- na.omit(file6)
file7 <- na.omit(file7)
file8 <- na.omit(file8)
file9 <- na.omit(file9)
file10 <- na.omit(file10)

# removing the Cell column
file1 <- file1[, !colnames(file1) %in% c("Cell")]
file2 <- file2[, !colnames(file2) %in% c("Cell")]
file3 <- file3[, !colnames(file3) %in% c("Cell")]
file4 <- file4[, !colnames(file4) %in% c("Cell")]
file5 <- file5[, !colnames(file5) %in% c("Cell")]
file6 <- file6[, !colnames(file6) %in% c("Cell")]
file7 <- file7[, !colnames(file7) %in% c("Cell")]
file8 <- file8[, !colnames(file8) %in% c("Cell")]
file9 <- file9[, !colnames(file9) %in% c("Cell")]
file10 <- file10[, !colnames(file10) %in% c("Cell")]

# merging all files into a dataset
all_data <- list(file1, file2, file3, file4, file5, file6, file7, file8, file9, file10)
merged_data <- do.call(rbind, all_data)

# creating new dataframes for each illumination intensity
combined_data_8 <- bind_rows(
  data.frame(Value = merged_data$X50ms_8pc),
  data.frame(Value = merged_data$X50ms_8pcDN)
)


combined_data_12 <- bind_rows(
  data.frame(Value = merged_data$X50ms_12pc),
  data.frame(Value = merged_data$X50ms_12pcDN)
)

combined_data_20 <- bind_rows(
  data.frame(Value = merged_data$X50ms_20pc),
  data.frame(Value = merged_data$X50ms_20pcDN)
)


combined_data_4 <- bind_rows(
  data.frame(Value = merged_data$X100ms_4pc),
  data.frame(Value = merged_data$X100ms_4pcDN)
)

combined_data_6 <- bind_rows(
  data.frame(Value = merged_data$X100ms_6pc),
  data.frame(Value = merged_data$X100ms_6pcDN)
)


combined_data_10 <- bind_rows(
  data.frame(Value = merged_data$X100ms_10pc),
  data.frame(Value = merged_data$X100ms_10pcDN)
)

# combinding the previously created dataframes into one that corresponds to energy dose
combined_data <- data.frame(
  `6.367` = combined_data_4$Value,
  `10.53` = combined_data_6$Value,
  `7.53` = combined_data_8$Value,
  `19.03` = combined_data_10$Value,
  `11.8` = combined_data_12$Value,
  `20.335` = combined_data_20$Value)

# changing the dataset structure and names of values in column
data_long <- gather(combined_data, key = "Power", value = "Value")
data_long$Power <- substr(data_long$Power, 2, nchar(data_long$Power))

# seperating values by exposure time
data_long$Exposure_Type <- ifelse(data_long$Power %in% c(6.367, 10.53, 19.03), "100ms", "50ms")

# designating the order in which the energy dose is to be shown in plot
desired <- c("6.367", "7.53", "10.53", 
            "11.8", "19.03", "20.335" )
data_long$Power <- factor(data_long$Power, levels =  desired)

# creates plot
plot <- ggplot(data_long, aes(x = Power, y = Value, fill = Exposure_Type)) +
  geom_boxplot(width = 0.7, outlier.shape = NA) +
  labs(x = "\nEnergy dose (µj)", y = "Mean Jaccard Index\n", title = "Mean Jaccard Index vs Energy Dose per Image") +
  theme(plot.title = element_text(hjust = 0.5, face = "bold")) +
  scale_fill_manual(values = c("#009392", "#CF597E"), 
                    labels = c("50ms", "100ms")) +
  labs(fill = "Exposure Time")

# save plot as png
ggsave("/Volumes/Untitled 1/Annotated/Mean Jaccard Index vs Energy Dose per Image.png", plot = plot, width = 8, height = 6, dpi = 300)

# ---------------------------

# repeats above process but only for non=denoised parameters
combined_data_8 <- data.frame(Value = merged_data$X50ms_8pc)
combined_data_12 <-  data.frame(Value = merged_data$X50ms_12pc)
combined_data_20 <-  data.frame(Value = merged_data$X50ms_20pc)
combined_data_4 <- data.frame(Value = merged_data$X100ms_4pc)
combined_data_6 <- data.frame(Value = merged_data$X100ms_6pc)
combined_data_10 <- data.frame(Value = merged_data$X100ms_10pc)

combined_data <- data.frame(
  `6.367` = combined_data_4$Value,
  `10.53` = combined_data_6$Value,
  `7.53` = combined_data_8$Value,
  `19.03` = combined_data_10$Value,
  `11.8` = combined_data_12$Value,
  `20.335` = combined_data_20$Value)

library(reshape2)
library(ggplot2)
library(tidyr)

data_long <- gather(combined_data, key = "Power", value = "Value")
data_long$Power <- substr(data_long$Power, 2, nchar(data_long$Power))

data_long$Exposure_Type <- ifelse(data_long$Power %in% c(6.367, 10.53, 19.03), "100ms", "50ms")

desired <- c("6.367", "7.53", "10.53", "11.8", "19.03", "20.335" )
data_long$Power <- factor(data_long$Power, levels =  desired)

plot <- ggplot(data_long, aes(x = Power, y = Value, fill = Exposure_Type)) +
  geom_boxplot(width = 0.7, outlier.shape = NA) +
  labs(x = "\nEnergy dose (µj)", y = "Mean Jaccard Index\n", title = "Mean Jaccard Index Across Non-Denoised Images vs Energy Dose per Image") +
  theme(plot.title = element_text(hjust = 0.5, face = "bold")) +
  scale_fill_manual(values = c("#009392", "#CF597E"), 
                    labels = c("50ms", "100ms")) +
  labs(fill = "Exposure Time")

ggsave("/Volumes/Untitled 1/Annotated/Mean Jaccard Index Across Non-Denoised Images vs Energy Dose per Image.png", plot = plot, width = 8, height = 6, dpi = 300)

# ---------------------------

# rcombines all 50ms parameters into one frame
combined_data_50ms <- data.frame(
  `8pc` = merged_data$X50ms_8pc,
  `8pcDN` = merged_data$X50ms_8pcDN,
  `12pc` = merged_data$X50ms_12pc,
  `12pcDN` = merged_data$X50ms_12pcDN,
  `20pc` = merged_data$X50ms_20pc,
  `20pcDN` = merged_data$X50ms_20pcDN)

# rcombines all 100ms parameters into one frame
combined_data_100ms <- data.frame(
  `4pc` = merged_data$X100ms_4pc,
  `4pcDN` = merged_data$X100ms_4pcDN,
  `6pc` = merged_data$X100ms_6pc,
  `6pcDN` = merged_data$X100ms_6pcDN,
  `10pc` = merged_data$X100ms_10pc,
  `10pcDN` = merged_data$X100ms_10pcDN)

# designates order of parameters for both exposure times
colnames(combined_data_50ms) <- c('8pc', "8pcDN", "12pc", "12pcDN", "20pc", "20pcDN")
colnames(combined_data_100ms) <- c('4pc', "4pcDN", "6pc", "6pcDN", "10pc", "10pcDN")

# restructures the datasets
data_long_50ms <- melt(combined_data_50ms)
data_long_100ms <- melt(combined_data_100ms)

# plots the 50ms dataset and saves it as a png
plot <- ggplot(data_long_50ms, aes(x = variable, y = value, fill = variable)) +
  geom_violin(width = 0.7) +  # Adjust the width as needed
  labs(x = "\nPower Intensity", y = "Jaccard Index\n") +
  ggtitle("Distribution of Jaccard Index for 50ms Sdh2 Images") +
  scale_fill_manual(values = c("8pc" = "#009392", "8pcDN" = "#E88471", 
                               "12pc" = "#009392", "12pcDN" = "#E88471", 
                               "20pc"= "#009392", "20pcDN" = "#E88471")) +
  theme_minimal() +
  theme(legend.position = "none",
        plot.title = element_text(hjust = 0.5, face = "bold", size = 15),
        axis.title = element_text(size = 14, face= "bold"),  # Adjust the size as needed
        axis.text.x = element_text(size = 11)) # Adjust the margin as needed

ggsave("/Volumes/Untitled 1/Annotated/Distribution of Jaccard Index for Sdh2 Images with Exposure Time of 50ms.png", plot = plot, width = 8, height = 6, dpi = 300)

# plots the 100ms dataset and saves it as a png
plot <- ggplot(data_long_100ms, aes(x = variable, y = value, fill = variable)) +
  geom_violin(width = 0.7) +
  labs(x = "\nPower Intensity", y = "Jaccard Index\n") +
  ggtitle("Distribution of Jaccard Index for 100ms Sdh2 Images") +
  scale_fill_manual(values = c("4pc" = "#009392", "4pcDN" = "#E88471", 
                               "6pc" = "#009392", "6pcDN" = "#E88471", 
                               "10pc"= "#009392", "10pcDN" = "#E88471")) +
  theme_minimal() +
  theme(
    legend.position = "none",
    plot.title = element_text(hjust = 0.5, face = "bold", size = 12),
    axis.title = element_text(size = 14, face= "bold"),
    axis.text.x = element_text(size = 11),
    plot.margin = margin(5, 20, 20, 20)  
  ) 

ggsave("/Volumes/Untitled 1/Annotated/Distribution of Jaccard Index for Sdh2 Images with Exposure Time of 100ms.png", plot = plot, width = 8, height = 6, dpi = 300)

# statistical test to be added on top
eight = t.test(combined_data_50ms$'8pcDN', combined_data_50ms$'8pc', paired=T, alternative = "greater") 
twelve = t.test(combined_data_50ms$'12pcDN', combined_data_50ms$'12pc', paired=T, alternative = "greater") 
twenty = t.test(combined_data_50ms$'20pcDN', combined_data_50ms$'20pc', paired=T, alternative = "greater") 

four = t.test(combined_data_100ms$'4pcDN', combined_data_100ms$'4pc', paired=T, alternative = "greater") 
six = t.test(combined_data_100ms$'6pcDN', combined_data_100ms$'6pc', paired=T, alternative = "greater") 
ten = t.test(combined_data_100ms$'10pcDN', combined_data_100ms$'10pc', paired=T, alternative = "greater") 

# --------------------

# reads annotated files
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

# excludes 50ms parameters from dataset
file1 <- file1[, !colnames(file1) %in% c("Cell", 'X50ms_8pc', 'X50ms_8pcDN', 'X50ms_12pc', 'X50ms_12pcDN', 'X50ms_20pc', 'X50ms_20pcDN')]
file2 <- file2[, !colnames(file2) %in% c("Cell", 'X50ms_8pc', 'X50ms_8pcDN', 'X50ms_12pc', 'X50ms_12pcDN', 'X50ms_20pc', 'X50ms_20pcDN')]
file3 <- file3[, !colnames(file3) %in% c("Cell", 'X50ms_8pc', 'X50ms_8pcDN', 'X50ms_12pc', 'X50ms_12pcDN', 'X50ms_20pc', 'X50ms_20pcDN')]
file4 <- file4[, !colnames(file4) %in% c("Cell", 'X50ms_8pc', 'X50ms_8pcDN', 'X50ms_12pc', 'X50ms_12pcDN', 'X50ms_20pc', 'X50ms_20pcDN')]
file5 <- file5[, !colnames(file5) %in% c("Cell", 'X50ms_8pc', 'X50ms_8pcDN', 'X50ms_12pc', 'X50ms_12pcDN', 'X50ms_20pc', 'X50ms_20pcDN')]
file6 <- file6[, !colnames(file6) %in% c("Cell", 'X50ms_8pc', 'X50ms_8pcDN', 'X50ms_12pc', 'X50ms_12pcDN', 'X50ms_20pc', 'X50ms_20pcDN')]
file7 <- file7[, !colnames(file7) %in% c("Cell", 'X50ms_8pc', 'X50ms_8pcDN', 'X50ms_12pc', 'X50ms_12pcDN', 'X50ms_20pc', 'X50ms_20pcDN')]
file8 <- file8[, !colnames(file8) %in% c("Cell", 'X50ms_8pc', 'X50ms_8pcDN', 'X50ms_12pc', 'X50ms_12pcDN', 'X50ms_20pc', 'X50ms_20pcDN')]
file9 <- file9[, !colnames(file9) %in% c("Cell", 'X50ms_8pc', 'X50ms_8pcDN', 'X50ms_12pc', 'X50ms_12pcDN', 'X50ms_20pc', 'X50ms_20pcDN')]
file10 <- file10[, !colnames(file10) %in% c("Cell", 'X50ms_8pc', 'X50ms_8pcDN', 'X50ms_12pc', 'X50ms_12pcDN', 'X50ms_20pc', 'X50ms_20pcDN')]

# merges them together and adds a column to indicate they are manual
all_data <- list(file1, file2, file3, file4, file5, file6, file7, file8, file9, file10)
merged_data <- do.call(rbind, all_data)
merged_data$Type <- "Manual"

# restructures dataset
data_long <- gather(merged_data, key = "Power", value = "Value", -Type)

# reads automated files
file1 <- read.csv("/Volumes/Untitled 1/Automated/Jaccard Index Sdh2 001.csv")
file2 <- read.csv("/Volumes/Untitled 1/Automated/Jaccard Index Sdh2 002.csv")
file3 <- read.csv("/Volumes/Untitled 1/Automated/Jaccard Index Sdh2 003.csv")
file4 <- read.csv("/Volumes/Untitled 1/Automated/Jaccard Index Sdh2 004.csv")
file5 <- read.csv("/Volumes/Untitled 1/Automated/Jaccard Index Sdh2 005.csv")
file6 <- read.csv("/Volumes/Untitled 1/Automated/Jaccard Index Sdh2 006.csv")
file7 <- read.csv("/Volumes/Untitled 1/Automated/Jaccard Index Sdh2 007.csv")
file8 <- read.csv("/Volumes/Untitled 1/Automated/Jaccard Index Sdh2 008.csv")
file9 <- read.csv("/Volumes/Untitled 1/Automated/Jaccard Index Sdh2 009.csv")
file10 <- read.csv("/Volumes/Untitled 1/Automated/Jaccard Index Sdh2 010.csv")

# excludes 50ms parameters from dataset
file1 <- file1[, !colnames(file1) %in% c("Cell", 'X50ms_8pc', 'X50ms_8pcDN', 'X50ms_12pc', 'X50ms_12pcDN', 'X50ms_20pc', 'X50ms_20pcDN')]
file2 <- file2[, !colnames(file2) %in% c("Cell", 'X50ms_8pc', 'X50ms_8pcDN', 'X50ms_12pc', 'X50ms_12pcDN', 'X50ms_20pc', 'X50ms_20pcDN')]
file3 <- file3[, !colnames(file3) %in% c("Cell", 'X50ms_8pc', 'X50ms_8pcDN', 'X50ms_12pc', 'X50ms_12pcDN', 'X50ms_20pc', 'X50ms_20pcDN')]
file4 <- file4[, !colnames(file4) %in% c("Cell", 'X50ms_8pc', 'X50ms_8pcDN', 'X50ms_12pc', 'X50ms_12pcDN', 'X50ms_20pc', 'X50ms_20pcDN')]
file5 <- file5[, !colnames(file5) %in% c("Cell", 'X50ms_8pc', 'X50ms_8pcDN', 'X50ms_12pc', 'X50ms_12pcDN', 'X50ms_20pc', 'X50ms_20pcDN')]
file6 <- file6[, !colnames(file6) %in% c("Cell", 'X50ms_8pc', 'X50ms_8pcDN', 'X50ms_12pc', 'X50ms_12pcDN', 'X50ms_20pc', 'X50ms_20pcDN')]
file7 <- file7[, !colnames(file7) %in% c("Cell", 'X50ms_8pc', 'X50ms_8pcDN', 'X50ms_12pc', 'X50ms_12pcDN', 'X50ms_20pc', 'X50ms_20pcDN')]
file8 <- file8[, !colnames(file8) %in% c("Cell", 'X50ms_8pc', 'X50ms_8pcDN', 'X50ms_12pc', 'X50ms_12pcDN', 'X50ms_20pc', 'X50ms_20pcDN')]
file9 <- file9[, !colnames(file9) %in% c("Cell", 'X50ms_8pc', 'X50ms_8pcDN', 'X50ms_12pc', 'X50ms_12pcDN', 'X50ms_20pc', 'X50ms_20pcDN')]
file10 <- file10[, !colnames(file10) %in% c("Cell", 'X50ms_8pc', 'X50ms_8pcDN', 'X50ms_12pc', 'X50ms_12pcDN', 'X50ms_20pc', 'X50ms_20pcDN')]

# merges them together and adds a column to indicate they are automated
aautomated_all_data <- list(file1, file2, file3, file4, file5, file6, file7, file8, file9, file10)
merged_data_automated <- do.call(rbind, aautomated_all_data)
merged_data_automated$Type <- "Automated"

# restructures dataset
automated_data_long <- gather(merged_data_automated, key = "Power", value = "Value", -Type)

# indicates order for parameters to be shown
power_order <- c("X100ms_4pc", "X100ms_4pcDN", "X100ms_6pc", "X100ms_6pcDN", "X100ms_10pc", "X100ms_10pcDN") 
new_power_names <- c("4pc", "4pcDN", "6pc", "6pcDN", "10pc", "10pcDN") 
automated_data_long$Power <- factor(automated_data_long$Power, levels = power_order)

# binds automated and annotated data sets together
combined_df <- rbind(automated_data_long, data_long)

# creating a boxplot and saves it as a png
my_plot <- ggplot(combined_df, aes(x = Power, y = Value, fill = as.factor(Type))) +
  geom_boxplot(width = 0.7, outlier.shape = NA) +
  labs(x = "Power", y = "Jaccard Index", title = "Comparison Between Automated and Manual Segmentation For 100ms Images", fill = "Image") +
  theme(plot.title = element_text(hjust = 0.5, face = "bold")) + 
  scale_fill_manual(values = c("Manual" = "#045275", "Automated" = "#E88471") )+
  scale_x_discrete(labels = new_power_names)


ggsave("/Volumes/Untitled 1/Annotated/omparison Between Automated and Manual Segmentation For 100ms Images.png", plot = my_plot, width = 8, height = 6, dpi = 300)


#---------

# repeats above process but for the 50ms parameters
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

file1 <- file1[, !colnames(file1) %in% c("Cell", "X100ms_4pc", "X100ms_4pcDN", "X100ms_6pc", "X100ms_6pcDN", "X100ms_10pc", "X100ms_10pcDN")]
file2 <- file2[, !colnames(file2) %in% c("Cell", "X100ms_4pc", "X100ms_4pcDN", "X100ms_6pc", "X100ms_6pcDN", "X100ms_10pc", "X100ms_10pcDN")]
file3 <- file3[, !colnames(file3) %in% c("Cell", "X100ms_4pc", "X100ms_4pcDN", "X100ms_6pc", "X100ms_6pcDN", "X100ms_10pc", "X100ms_10pcDN")]
file4 <- file4[, !colnames(file4) %in% c("Cell", "X100ms_4pc", "X100ms_4pcDN", "X100ms_6pc", "X100ms_6pcDN", "X100ms_10pc", "X100ms_10pcDN")]
file5 <- file5[, !colnames(file5) %in% c("Cell", "X100ms_4pc", "X100ms_4pcDN", "X100ms_6pc", "X100ms_6pcDN", "X100ms_10pc", "X100ms_10pcDN")]
file6 <- file6[, !colnames(file6) %in% c("Cell", "X100ms_4pc", "X100ms_4pcDN", "X100ms_6pc", "X100ms_6pcDN", "X100ms_10pc", "X100ms_10pcDN")]
file7 <- file7[, !colnames(file7) %in% c("Cell", "X100ms_4pc", "X100ms_4pcDN", "X100ms_6pc", "X100ms_6pcDN", "X100ms_10pc", "X100ms_10pcDN")]
file8 <- file8[, !colnames(file8) %in% c("Cell", "X100ms_4pc", "X100ms_4pcDN", "X100ms_6pc", "X100ms_6pcDN", "X100ms_10pc", "X100ms_10pcDN")]
file9 <- file9[, !colnames(file9) %in% c("Cell", "X100ms_4pc", "X100ms_4pcDN", "X100ms_6pc", "X100ms_6pcDN", "X100ms_10pc", "X100ms_10pcDN")]
file10 <- file10[, !colnames(file10) %in% c("Cell", "X100ms_4pc", "X100ms_4pcDN", "X100ms_6pc", "X100ms_6pcDN", "X100ms_10pc", "X100ms_10pcDN")]
all_data <- list(file1, file2, file3, file4, file5, file6, file7, file8, file9, file10)
merged_data <- do.call(rbind, all_data)
merged_data$Type <- "Manual"

data_long <- gather(merged_data, key = "Power", value = "Value", -Type)

file1 <- read.csv("/Volumes/Untitled 1/Automated/Jaccard Index Sdh2 001.csv")
file2 <- read.csv("/Volumes/Untitled 1/Automated/Jaccard Index Sdh2 002.csv")
file3 <- read.csv("/Volumes/Untitled 1/Automated/Jaccard Index Sdh2 003.csv")
file4 <- read.csv("/Volumes/Untitled 1/Automated/Jaccard Index Sdh2 004.csv")
file5 <- read.csv("/Volumes/Untitled 1/Automated/Jaccard Index Sdh2 005.csv")
file6 <- read.csv("/Volumes/Untitled 1/Automated/Jaccard Index Sdh2 006.csv")
file7 <- read.csv("/Volumes/Untitled 1/Automated/Jaccard Index Sdh2 007.csv")
file8 <- read.csv("/Volumes/Untitled 1/Automated/Jaccard Index Sdh2 008.csv")
file9 <- read.csv("/Volumes/Untitled 1/Automated/Jaccard Index Sdh2 009.csv")
file10 <- read.csv("/Volumes/Untitled 1/Automated/Jaccard Index Sdh2 010.csv")

file1 <- file1[, !colnames(file1) %in% c("Cell", "X100ms_4pc", "X100ms_4pcDN", "X100ms_6pc", "X100ms_6pcDN", "X100ms_10pc", "X100ms_10pcDN")]
file2 <- file2[, !colnames(file2) %in% c("Cell", "X100ms_4pc", "X100ms_4pcDN", "X100ms_6pc", "X100ms_6pcDN", "X100ms_10pc", "X100ms_10pcDN")]
file3 <- file3[, !colnames(file3) %in% c("Cell", "X100ms_4pc", "X100ms_4pcDN", "X100ms_6pc", "X100ms_6pcDN", "X100ms_10pc", "X100ms_10pcDN")]
file4 <- file4[, !colnames(file4) %in% c("Cell", "X100ms_4pc", "X100ms_4pcDN", "X100ms_6pc", "X100ms_6pcDN", "X100ms_10pc", "X100ms_10pcDN")]
file5 <- file5[, !colnames(file5) %in% c("Cell", "X100ms_4pc", "X100ms_4pcDN", "X100ms_6pc", "X100ms_6pcDN", "X100ms_10pc", "X100ms_10pcDN")]
file6 <- file6[, !colnames(file6) %in% c("Cell", "X100ms_4pc", "X100ms_4pcDN", "X100ms_6pc", "X100ms_6pcDN", "X100ms_10pc", "X100ms_10pcDN")]
file7 <- file7[, !colnames(file7) %in% c("Cell", "X100ms_4pc", "X100ms_4pcDN", "X100ms_6pc", "X100ms_6pcDN", "X100ms_10pc", "X100ms_10pcDN")]
file8 <- file8[, !colnames(file8) %in% c("Cell", "X100ms_4pc", "X100ms_4pcDN", "X100ms_6pc", "X100ms_6pcDN", "X100ms_10pc", "X100ms_10pcDN")]
file9 <- file9[, !colnames(file9) %in% c("Cell", "X100ms_4pc", "X100ms_4pcDN", "X100ms_6pc", "X100ms_6pcDN", "X100ms_10pc", "X100ms_10pcDN")]
file10 <- file10[, !colnames(file10) %in% c("Cell", "X100ms_4pc", "X100ms_4pcDN", "X100ms_6pc", "X100ms_6pcDN", "X100ms_10pc", "X100ms_10pcDN")]

aautomated_all_data <- list(file1, file2, file3, file4, file5, file6, file7, file8, file9, file10)
merged_data_automated <- do.call(rbind, aautomated_all_data)
merged_data_automated$Type <- "Automated"

automated_data_long <- gather(merged_data_automated, key = "Power", value = "Value", -Type)


power_order <- c("X50ms_8pc', 'X50ms_8pcDN', 'X50ms_12pc', 'X50ms_12pcDN', 'X50ms_20pc', 'X50ms_20pcDN") 
new_power_names <- c("8pc", "8pcDN", "12pc", "12pcDN", "20pc", "20pcDN") 
automated_data_long$Power <- factor(automated_data_long$Power, levels = power_order)

combined_df <- rbind(automated_data_long, data_long)

# creating a boxplot
my_plot <- ggplot(combined_df, aes(x = Power, y = Value, fill = as.factor(Type))) +
  geom_boxplot(width = 0.7, outlier.shape = NA) +
  labs(x = "Power", y = "Jaccard Index", title = "Comparison Between Automated and Manual Segmentation For 50ms Images", fill = "Image") +
  theme(plot.title = element_text(hjust = 0.5, face = "bold")) + 
  scale_fill_manual(values = c("Manual" = "#045275", "Automated" = "#E88471") )+
  scale_x_discrete(labels = new_power_names)

ggsave("/Volumes/Untitled 1/Annotated/omparison Between Automated and Manual Segmentation For 50ms Images.png", plot = my_plot, width = 8, height = 6, dpi = 300)


  