##Load Libraries
library(ggplot2)
library(purrr)
library(lubridate)
library(dplyr)
##read file
df <- read.csv("C:/Users/debja/Downloads/german_credit_data.csv")
head(df)
##Check Null & Duplicate Value
sum(is.na(df))
sum(duplicated(df))
##Preprocessing the Dataset
summary(df)
# Assuming your data frame is named `df`
df <- df %>%
  mutate(
    log_Age = log(Age + 1),
    log_Credit_amount = log(Credit.amount + 1),
    log_Duration = log(Duration + 1)
  )
##Plot Histograms Before and After Transformation
# Function to plot histograms before and after transformation
plot_histograms <- function(original, transformed, variable_name) {
  p1 <- ggplot(df, aes(x = {{original}})) +
    geom_histogram(bins = 30, fill = 'blue', alpha = 0.5) +
    labs(title = paste("Original", variable_name), x = variable_name, y = "Frequency") +
    theme_minimal()
  
  p2 <- ggplot(df, aes(x = {{transformed}})) +
    geom_histogram(bins = 30, fill = 'green', alpha = 0.5) +
    labs(title = paste("Log Transformed", variable_name), x = paste("log(", variable_name, " + 1)"), y = "Frequency") +
    theme_minimal()
  
  return(list(p1, p2))
}

# Plot histograms for 'Age'
plots_Age <- plot_histograms(Age, log_Age, "Age")

# Plot histograms for 'Credit.amount'
plots_Credit_amount <- plot_histograms(Credit.amount, log_Credit_amount, "Credit.amount")

# Plot histograms for 'Duration'
plots_Duration <- plot_histograms(Duration, log_Duration, "Duration")

# Display the plots
gridExtra::grid.arrange(grobs = c(plots_Age, plots_Credit_amount, plots_Duration), ncol = 2)

library(corrplot)
### Select only numeric columns for correlation
numeric_df <- df %>% select(Age, Credit.amount, Duration, log_Age, log_Credit_amount, log_Duration)
# Calculate correlation matrix
cor_matrix <- cor(numeric_df, use = "complete.obs")
# Plot correlation heatmap
corrplot(cor_matrix, method = "color", type = "upper", tl.col = "black", tl.srt = 45,addCoef.col = "black")

### # Plot histogram of loan applicants by gender
ggplot(df, aes(x = Sex)) +
  geom_bar(fill = "steelblue") +
  labs(title = "Distribution of Loan Applicants by Gender",
       x = "Gender",
       y = "Count") +
  theme_minimal()
# Create a data frame for pie chart
gender_counts <- df %>%
  group_by(Sex) %>%
  summarise(count = n())

### Plot pie chart of loan applicants by gender
ggplot(gender_counts, aes(x = "", y = count, fill = Sex)) +
  geom_bar(width = 1, stat = "identity") +
  coord_polar("y") +
  labs(title = "Loan Applicants by Gender") +
  theme_minimal() +
  theme(axis.title.x = element_blank(),
        axis.title.y = element_blank(),
        panel.grid = element_blank(),
        axis.text.x = element_blank(),
        axis.ticks = element_blank())

### Plot histogram of loan applicants by housing status
ggplot(df, aes(x = Housing)) +
  geom_bar(fill = "steelblue") +
  labs(title = "Distribution of Loan Applicants by Housing Status",
       x = "Housing Status",
       y = "Count") +
  theme_minimal()
# Create a data frame for pie chart with percentages
housing_counts <- df %>%
  group_by(Housing) %>%
  summarise(count = n()) %>%
  mutate(percentage = count / sum(count) * 100,
         label = paste0(Housing, " (", round(percentage, 1), "%)"))
### Plot pie chart of loan applicants by housing status with percentages
ggplot(housing_counts, aes(x = "", y = count, fill = Housing)) +
  geom_bar(width = 1, stat = "identity") +
  coord_polar("y") +
  geom_text(aes(label = label), position = position_stack(vjust = 0.5)) +
  labs(title = "Loan Applicants by Housing Status") +
  theme_minimal() +
  theme(axis.title.x = element_blank(),
        axis.title.y = element_blank(),
        panel.grid = element_blank(),
        axis.text.x = element_blank(),
        axis.ticks = element_blank())
### Plot histogram of loan applicants by purpose
ggplot(df, aes(x = Purpose)) +
  geom_bar(fill = "steelblue") +
  labs(title = "Distribution of Loan Applicants by Purpose",
       x = "Purpose",
       y = "Count") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))
### Create a data frame for pie chart with percentages
purpose_counts <- df %>%
  group_by(Purpose) %>%
  summarise(count = n()) %>%
  mutate(percentage = count / sum(count) * 100,
         label = paste0(Purpose, " (", round(percentage, 1), "%)"))
### Plot pie chart of loan applicants by purpose with percentages
ggplot(purpose_counts, aes(x = "", y = count, fill = Purpose)) +
  geom_bar(width = 1, stat = "identity") +
  coord_polar("y") +
 labs(title = "Loan Applicants by Purpose") +
 
  ##
  # Install LabelEncoder package if not already installed
  if (!requireNamespace("LabelEncoder", quietly = TRUE)) {
    install.packages("LabelEncoder")
  }

# Load necessary libraries
library(LabelEncoder)
# Convert categorical features to factors and then apply label encoding
df$Sex <- as.numeric(factor(df$Sex))
df$Housing <- as.numeric(factor(df$Housing))
df$Saving.accounts <- as.numeric(factor(df$Saving.accounts, levels = unique(df$Saving.accounts)))
df$Checking.account <- as.numeric(factor(df$Checking.account, levels = unique(df$Checking.account)))
df$Purpose <- as.numeric(factor(df$Purpose, levels = unique(df$Purpose)))
# Display the modified data frame
print(df)
head(df)
# Assuming your dataset is named 'df'
# Selecting numeric columns to scale
numeric_cols <- c("Age", "Credit.amount", "Duration")


# Standardizing and scaling the numeric columns
df_scaled <- df
df_scaled[numeric_cols] <- scale(df[numeric_cols])

# Checking the scaled dataset
head(df_scaled)

##we are using K-means clustering
# Install and load the zoo package
install.packages("zoo")
library(zoo)

# Impute missing values with the mean
df_imputed <- na.aggregate(df, FUN = mean)

# Apply K-means clustering
kmeans_result <- kmeans(df_imputed, centers = 4)

library(cluster)
# Check for missing values in each element of the list
any_missing <- any(sapply(df, function(x) any(is.na(x))))

# Check for NaNs in each element of the list
any_nan <- any(sapply(df, function(x) any(is.nan(x))))

# Check for infinite values in each element of the list
any_inf <- any(sapply(df, function(x) any(is.infinite(x))))

# Print the results
print(any_missing)
print(any_nan)
print(any_inf)

# Check the number of missing values in each column
missing_values <- colSums(is.na(df))

# Print the results
print(missing_values)
# Remove missing values
cleaned_df <- na.omit(df)

# Perform K-means clustering
library(stats)
set.seed(123) # for reproducibility
kmeans_result <- kmeans(cleaned_df[, c("Age", "Credit.amount", "Duration")], centers = 3)

# Print the cluster centers
print(kmeans_result$centers)

# Print the cluster assignments for each data point
print(kmeans_result$cluster)

# Visualize the clusters
# Add the cluster information back to your cleaned_df dataset
cleaned_df$cluster <- kmeans_result$cluster

# Load the ggplot2 library for visualization
library(ggplot2)

# Visualize the clusters
# Assuming kmeans_result is your kmeans clustering result
# Assuming cleaned_df is your cleaned dataset

# Add the cluster information back to your cleaned_df dataset
cleaned_df$cluster <- kmeans_result$cluster

# Check if the cluster variable is added correctly
print(head(cleaned_df))


# Visualize the clusters
ggplot(cleaned_df, aes(x = Credit.amount, y = Age, color = factor(cluster))) +
  geom_point() +
  labs(title = "K-means Clustering of Customer Data",
       x = "Credit Amount",
       y = "Age",
       color = "Cluster") +
  theme_minimal()
# Compute and store the within-cluster sum of squares (WCSS) for different values of k
wcss <- vector()
for (i in 1:10) {
  kmeans_result <- kmeans(cleaned_df[, c("Credit.amount", "Age")], centers = i)
  wcss[i] <- kmeans_result$tot.withinss
}

# Plot the elbow curve
plot(1:10, wcss, type = "b", pch = 19, frame = FALSE, 
     xlab = "Number of Clusters (k)",
     ylab = "Within-Cluster Sum of Squares (WCSS)",
     main = "Elbow Method for Optimal k")
# Compute silhouette scores for different numbers of clusters (2 to 10, for example)
silhouette_scores <- sapply(2:10, function(k) {
  kmeans_result <- kmeans(cleaned_df[, c("Credit.amount", "Age")], centers = k)
  mean(silhouette(kmeans_result$cluster, dist(cleaned_df[, c("Credit.amount", "Age")])))
})

# Plot the silhouette scores
plot(2:10, silhouette_scores, type = "b", pch = 19, frame = FALSE, 
     xlab = "Number of Clusters",
     ylab = "Average Silhouette Score",
     main = "Silhouette Score for Optimal Cluster Count")

# Add a vertical line at the optimal number of clusters
abline(v = which.max(silhouette_scores), col = "red")



