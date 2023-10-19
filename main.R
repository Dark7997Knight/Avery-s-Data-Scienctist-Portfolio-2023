# Date: 09/25/2023
setwd("C:/Users/aholl/Desktop/Data Science Portfolio/Project #1/project#1data")

# Load the required libraries
# readxl is a package to read excel files
library(readxl)
# dplyr is a grammar of data manipulation, providing a consistent set of verbs that help you solve the most common data
library(dplyr)
#ggplot2 is a system for declaratively creating graphics, based on The Grammar of Graphics
library(ggplot2)

# read_excel reads both xls and xlsx files
Summary_Table_by_Contaminant <- read_excel("C:/Users/aholl/Desktop/Data Science Portfolio/Project #1/project#1newdata/Summary Table by Contaminant.xlsx",
    col_types = c("text", "numeric", "numeric",
        "numeric", "numeric", "numeric",
        "numeric", "text", "text"))

#View(Summary_Table_by_Contaminant)
View(Summary_Table_by_Contaminant)

# Change dataframe name
water_data <- Summary_Table_by_Contaminant

# Remove any duplicate rows
water_data <- distinct(water_data)

# Load the required libraries
library(dplyr)
library(ggplot2)

# Summary statistics of the target variable
summary(water_data)

# Load the required libraries
library(FactoMineR)  # For PCA
library(ggplot2)     # For data visualization

# Perform quality control analysis on the reduced dataset using PCA and MCA to identify outliers and missing values in the dataset
# PCA
# Create a new dataframe with only the numeric variables
water_data_num <- water_data[, c(2:7)]

# Perform PCA
water_data_pca <- PCA(water_data_num, graph = FALSE)

# Plot the PCA results with beautiful colors in ggplot2 using the fviz_pca_ind() function
fviz_pca_ind(water_data_pca, col.ind = "cos2", gradient.cols = c("#00AFBB", "#E7B800", "#FC4E07"),
             repel = TRUE, # Avoid text overlapping
             ggtheme = theme_minimal()) + theme(legend.position = "bottom") + ggtitle("PCA of Water Data") +
    theme(plot.title = element_text(hjust = 0.5)) + theme(plot.title = element_text(size = 20)) +
    theme(axis.title.x = element_text(size = 15)) + theme(axis.title.y = element_text(size = 15)) +
    theme(axis.text.x = element_text(size = 15)) + theme(axis.text.y = element_text(size = 15)) +
    theme(legend.title = element_text(size = 15)) + theme(legend.text = element_text(size = 15))

# MCA
# Create a new dataframe with only the categorical variables
water_data_cat <- water_data[, c(1, 8:9)]

# Perform MCA
water_data_mca <- MCA(water_data_cat, graph = FALSE) # graph = FALSE to avoid the plot

# Plot the MCA results with beautiful colors in ggplot2 using the fviz_mca_ind() function



fviz_mca_ind(water_data_mca, col.ind = "cos2", gradient.cols = c("#00AFBB", "#E7B800", "#FC4E07"),
             repel = TRUE, # Avoid text overlapping
             ggtheme = theme_minimal()) + theme(legend.position = "bottom") + ggtitle("MCA of Water Data") +
    theme(plot.title = element_text(hjust = 0.5)) + theme(plot.title = element_text(size = 20)) +
    theme(axis.title.x = element_text(size = 15)) + theme(axis.title.y = element_text(size = 15)) +
    theme(axis.text.x = element_text(size = 15)) + theme(axis.text.y = element_text(size = 15)) +
    theme(legend.title = element_text(size = 15)) + theme(legend.text = element_text(size = 15))

# Load the required libraries
library(dplyr)
library(ggplot2)

GGally heatmap of the correlation matrix of the numeric variables colorful and easy to read ggplot2 theme and colors

# Create a new dataframe with only the numeric variables
water_data_num <- water_data[, c(2:7)]

# Compute the correlation matrix
correlation_matrix <- cor(water_data_num)

# Plot the correlation matrix with beautiful colors in ggplot2 using the ggcorrplot() function
ggcorrplot(correlation_matrix, hc.order = TRUE, type = "lower", lab = TRUE, lab_size = 3, method = "circle",
           colors = c("#00AFBB", "white", "#FC4E07"), ggtheme = theme_minimal()) +
    ggtitle("Correlation Matrix of Water Data") + theme(plot.title = element_text(hjust = 0.5)) +
    theme(plot.title = element_text(size = 20)) + theme(axis.title.x = element_text(size = 15)) +
    theme(axis.title.y = element_text(size = 15)) + theme(axis.text.x = element_text(size = 15)) +
    theme(axis.text.y = element_text(size = 15)) + theme(legend.title = element_text(size = 15)) +
    theme(legend.text = element_text(size = 15))

# Load the required libraries
library(dplyr)
library(ggplot2)

ggpairs() function from the GGally package to plot the pairwise relationships between the numeric variables
# Create a new dataframe with only the numeric variables
water_data_num <- water_data[, c(2:7)]

# Plot the pairwise relationships between the numeric variables with beautiful colors in ggplot2 using the ggpairs() function

ggpairs(water_data_num, lower = list(continuous = wrap("points", alpha = 0.3, size = 0.5)),
        diag = list(continuous = wrap("barDiag", bins = 10, alpha = 0.3, size = 0.5)),
        upper = list(continuous = wrap("blank", size = 0.5)), axisLabels = "none",
        title = "Pairwise Relationships Between the Numeric Variables") +
    theme(plot.title = element_text(hjust = 0.5)) + theme(plot.title = element_text(size = 20)) +
    theme(axis.title.x = element_text(size = 15)) + theme(axis.title.y = element_text(size = 15)) +
    theme(axis.text.x = element_text(size = 15)) + theme(axis.text.y = element_text(size = 15)) +
    theme(legend.title = element_text(size = 15)) + theme(legend.text = element_text(size = 15))

# Load the required libraries
library(dplyr)

#t.test on water_data to compare the mean of the numeric variables between the two groups
# Create a new dataframe with only the numeric variables
water_data_num <- water_data[, c(2:7)]

# Perform t.test on water_data to compare the mean of the numeric variables between the two groups
t.test(water_data_num$`2018-2019`, water_data_num$`2019-2020`, paired = TRUE)

# Load the required libraries
library(dplyr)
library(ggplot2)

# Create a new dataframe with only the numeric variables
water_data_num <- water_data[, c(2:7)]


