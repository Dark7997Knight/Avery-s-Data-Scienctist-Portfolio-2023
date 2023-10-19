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
library(readxl)
Summary_Table_by_Contaminant <- read_excel("C:/Users/aholl/Desktop/Data Science Portfolio/Project #1/project#1newdata/Summary Table by Contaminant.xlsx", 
                                           col_types = c("text", "numeric", "numeric", 
                                                         "numeric", "numeric", "numeric", 
                                                         "numeric", "skip", "skip"))


#View(Summary_Table_by_Contaminant)
View(Summary_Table_by_Contaminant)

# Change dataframe name
water_data <- Summary_Table_by_Contaminant

# Load the required libraries
library(dplyr)
library(ggplot2)

# Remove Containment temporariry from data set to run the scale function since the variable was a character
water_data <- water_data[, -1]

# Summary statistics of the target variable
summary(water_data)

# Identify numeric columns (assuming your data frame contains both character and numeric columns)
numeric_columns <- sapply(water_data, is.numeric)

# Standardize only the numeric columns
water_data[, numeric_columns] <- scale(water_data[, numeric_columns])

#  Standardize the data by subtracting the mean and dividing by the standard deviation
water_data <- scale(water_data)

# Compute the covariance matrix of the standardized data
cov_matrix <- cov(water_data)

# Compute the eigenvalues and eigenvectors of the covariance matrix
eigen_values <- eigen(cov_matrix)$values
eigen_vectors <- eigen(cov_matrix)$vectors

# Compute the proportion of variance explained by each principal component
variance_explained <- eigen_values / sum(eigen_values)

# Compute the cumulative proportion of variance explained by the first k principal components
cumulative_variance_explained <- cumsum(variance_explained)

# Plot the cumulative proportion of variance explained by the first k principal components
plot(cumulative_variance_explained, xlab = "Number of Principal Components", ylab = "Cumulative Proportion of Variance Explained", type = "b")

# Load the ggplot2 library
library(ggplot2)

# Create a data frame with the cumulative variance explained
cumulative_variance_df <- data.frame(
  Components = 1:length(cumulative_variance_explained),
  Cumulative_Variance = cumulative_variance_explained
)

# Create a ggplot2 plot
ggplot(cumulative_variance_df, aes(x = Components, y = Cumulative_Variance)) +
  geom_line(color = "blue", size = 1) +
  geom_point(color = "blue", size = 3) +
  labs(
    x = "Number of Principal Components",
    y = "Cumulative Proportion of Variance Explained",
    title = "Cumulative Variance Explained by Principal Components"
  ) +
  theme_minimal() +
  theme(
    plot.title = element_text(size = 20, face = "bold"),
    axis.text = element_text(size = 12),
    axis.title = element_text(size = 14, face = "bold")
  ) +
  geom_hline(yintercept = 0.95, linetype = "dashed", color = "red") +
  annotate(
    "text",
    x = max(cumulative_variance_df$Components),
    y = 0.95,
    label = "95% Variance Explained",
    hjust = -0.1,
    vjust = -0.5,
    size = 4,
    color = "red"
  )


# Compute the principal component scores
principal_component_scores <- water_data %*% eigen_vectors

# Plot the first two principal component scores
plot(principal_component_scores[, 1], principal_component_scores[, 2], xlab = "First Principal Component", ylab = "Second Principal Component")


# Load the ggplot2 library
library(ggplot2)

# Create a data frame with the principal component scores
pca_df <- data.frame(
  PC1 = principal_component_scores[, 1],
  PC2 = principal_component_scores[, 2]
)

# Create an exciting ggplot2 plot
ggplot(pca_df, aes(x = PC1, y = PC2)) +
  geom_point(
    aes(color = PC1 + PC2, size = PC1 - PC2),
    alpha = 0.7,
    shape = 19
  ) +
  labs(
    x = "First Principal Component",
    y = "Second Principal Component",
    title = "PCA Plot"
  ) +
  scale_color_gradient(
    low = "blue",
    high = "red",
    guide = guide_colorbar(title = "PC1 + PC2")
  ) +
  scale_size_continuous(
    range = c(2, 10),
    guide = guide_legend(title = "PC1 - PC2")
  ) +
  theme_minimal() +
  theme(
    legend.position = "bottom",
    plot.title = element_text(size = 20, face = "bold"),
    axis.text = element_text(size = 12),
    axis.title = element_text(size = 14, face = "bold")
  )

# Load the required libraries
library(FactoMineR)  # For PCA
library(ggplot2)     # For data visualization

# Perform PCA
pca_result <- PCA(water_data, scale.unit = TRUE, graph = FALSE)

# Scree plot to visualize the variance explained by each principal component

ggplot(scree_data, aes(x = Components, y = cumsum(pca_result$eig) / sum(pca_result$eig))) +
  geom_bar(stat = "identity", fill = "blue") +
  labs(
    x = "Number of Principal Components",
    y = "Variance Explained",
    title = "Scree Plot"
  ) +
  theme_minimal()

# Calculate cumulative variance explained
cumulative_variance_data <- data.frame(
  Components = 1:length(pca_result$eig),
  Cumulative_Variance = cumsum(pca_result$eig) / sum(pca_result$eig)
)

# Plot the cumulative proportion of variance explained
ggplot(cumulative_variance_data, aes(x = Components, y = Cumulative_Variance)) +
  geom_line(color = "blue") +
  geom_point(color = "blue") +
  labs(
    x = "Number of Principal Components",
    y = "Cumulative Proportion of Variance Explained",
    title = "Cumulative Variance Explained"
  ) +
  theme_minimal()

# Choose the number of principal components that explain a sufficient amount of variance
# For example, you can choose a threshold like 95% variance explained
threshold <- 0.95
cumulative_variance_data <- data.frame(
  Components = 1:length(pca_result$eig),
  Cumulative_Variance = cumsum(pca_result$eig) / sum(pca_result$eig)
)
num_components <- which(cumulative_variance_data$Cumulative_Variance >= threshold)[1]


# Load the required libraries
library(FactoMineR)  # For PCA
library(ggplot2)     # For data visualization

# Perform quality control analysis on the reduced dataset using PCA and MCA to identify outliers and missing values in the dataset
# PCA
# Create a new dataframe with only the numeric variables
water_data_num <- water_data[, c(1:6)]

# Perform PCA
water_data_pca <- PCA(water_data_num, graph = FALSE)

#install.packages("factoextra")
library(factoextra)


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
water_data_cat <- Summary_Table_by_Contaminant[, c(1)]


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
