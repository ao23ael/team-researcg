# Install and load necessary packages
install.packages(c("ggplot2", "dplyr", "corrplot", "GGally", "reshape2"))
library(ggplot2)
library(dplyr)
library(corrplot)
library(GGally)
library(reshape2)

# Set the working directory to the folder where the CSV files are located
setwd("C:/Users/Desktop/Gitworks/team-researcg/World University ranking")

# Read in the datasets
cwur_data <- read.csv("cwurData.csv")
education_expenditure <- read.csv("education_expenditure_supplementary_data.csv")
education_attainment <- read.csv("educational_attainment_supplementary_data.csv")
school_country_table <- read.csv("school_and_country_table.csv")
shanghai_data <- read.csv("shanghaiData.csv")
times_data <- read.csv("timesData.csv")

# Check the data structure
str(cwur_data)
str(times_data)
str(shanghai_data)
str(school_country_table)

# Ensure relevant columns are numeric for analysis (if necessary)
cwur_data$score <- as.numeric(cwur_data$score)
times_data$total_score <- as.numeric(times_data$total_score)
shanghai_data$total_score <- as.numeric(shanghai_data$total_score)

# Step 1: Correlation Heatmaps for Numerical Data

# CWUR Data - Correlation Heatmap with Correlation Values
corr_cwur <- cor(cwur_data %>% select_if(is.numeric), use = "pairwise.complete.obs")
corrplot(corr_cwur, method = "color", 
         addCoef.col = "black", # Display correlation values inside cells
         number.cex = 0.7, # Adjust size of the numbers
         tl.cex = 0.8, main = "Correlation Heatmap - CWUR Data")

# Shanghai Data - Correlation Heatmap with Correlation Values
corr_shanghai <- cor(shanghai_data %>% select_if(is.numeric), use = "pairwise.complete.obs")
corrplot(corr_shanghai, method = "color", 
         addCoef.col = "black", # Display correlation values inside cells
         number.cex = 0.7, # Adjust size of the numbers
         tl.cex = 0.8, main = "Correlation Heatmap - Shanghai Data")

# Times Data - Correlation Heatmap with Correlation Values
corr_times <- cor(times_data %>% select_if(is.numeric), use = "pairwise.complete.obs")
corrplot(corr_times, method = "color", 
         addCoef.col = "black", # Display correlation values inside cells
         number.cex = 0.7, # Adjust size of the numbers
         tl.cex = 0.8, main = "Correlation Heatmap - Times Data")

# Step 2: Histograms for Numerical Data

# CWUR Data (Score Distribution)
ggplot(cwur_data, aes(x = score)) + 
  geom_histogram(binwidth = 5, fill = "skyblue", color = "black") +
  labs(title = "Distribution of Scores - CWUR Data", x = "Score", y = "Frequency")

# Times Data (Total Score Distribution)
ggplot(times_data, aes(x = total_score)) + 
  geom_histogram(binwidth = 5, fill = "lightgreen", color = "black") +
  labs(title = "Distribution of Total Scores - Times Data", x = "Total Score", y = "Frequency")

# Shanghai Data (Total Score Distribution)
ggplot(shanghai_data, aes(x = total_score)) + 
  geom_histogram(binwidth = 5, fill = "lightcoral", color = "black") +
  labs(title = "Distribution of Total Scores - Shanghai Data", x = "Total Score", y = "Frequency")

# Step 3: Bar Charts for Categorical Data

# CWUR Data (Country Distribution)
ggplot(cwur_data, aes(x = country)) + 
  geom_bar(fill = "skyblue", color = "black") +
  labs(title = "Country Distribution - CWUR Data", x = "Country", y = "Count") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

# Top universities by frequency for Shanghai Data
top_universities_shanghai <- shanghai_data %>%
  count(university_name) %>%
  filter(n > 1) %>%  # Filter out universities that appear only once
  arrange(desc(n)) %>%  # Sort by frequency in descending order
  head(10)  # Select top 10

ggplot(top_universities_shanghai, aes(x = university_name, y = n)) + 
  geom_bar(stat = "identity", fill = "lightgreen", color = "black") +
  labs(title = "Top 10 University Distribution - Shanghai Data", x = "University", y = "Count") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
  coord_flip()  # Flip the plot for better readability

# Top universities by frequency for Times Data
top_universities_times <- times_data %>%
  count(university_name) %>%
  filter(n > 1) %>%  # Filter out universities that appear only once
  arrange(desc(n)) %>%  # Sort by frequency in descending order
  head(10)  # Select top 10

ggplot(top_universities_times, aes(x = university_name, y = n)) + 
  geom_bar(stat = "identity", fill = "lightcoral", color = "black") +
  labs(title = "Top 10 University Distribution - Times Data", x = "University", y = "Count") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
  coord_flip()  # Flip the plot for better readability




# Step 4: Pair Plots for Numerical Data

# Pairplot for CWUR Data (Numerical Columns)
ggpairs(cwur_data %>% select_if(is.numeric))

# Pairplot for Times Data (Numerical Columns)
ggpairs(times_data %>% select_if(is.numeric))

# Step 5: Boxplots for Numerical Variables

# Boxplot for CWUR Data (Score)
ggplot(cwur_data, aes(y = score)) + 
  geom_boxplot(fill = "skyblue", color = "black") +
  labs(title = "Boxplot of Scores - CWUR Data", y = "Score")

# Boxplot for Times Data (Total Score)
ggplot(times_data, aes(y = total_score)) + 
  geom_boxplot(fill = "lightgreen", color = "black") +
  labs(title = "Boxplot of Total Scores - Times Data", y = "Total Score")

# Step 6: Categorical Data Visualization (Education Expenditure)

# Bar Chart for Education Expenditure (Country Distribution)
ggplot(education_expenditure, aes(x = country)) + 
  geom_bar(fill = "lightblue", color = "black") +
  labs(title = "Country Distribution - Education Expenditure", x = "Country", y = "Count") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))


# Step 7: Categorical Data Visualization (Educational Attainment)

# Limit the number of countries to top 20 most frequent ones
top_countries_education <- education_attainment %>%
  count(country_name) %>%
  arrange(desc(n)) %>%
  head(20)

ggplot(top_countries_education, aes(x = country_name, y = n)) + 
  geom_bar(stat = "identity", fill = "lightyellow", color = "black") +
  labs(title = "Top 20 Countries - Educational Attainment", x = "Country", y = "Count") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
  coord_flip()  # Flip for better readability

# Step 8: Visualize the Distribution of Rankings (for universities)
# Plotting the distribution of rankings for CWUR data
ggplot(cwur_data, aes(x = world_rank)) + 
  geom_histogram(binwidth = 1, fill = "lightpink", color = "black") +
  labs(title = "Distribution of World Rankings - CWUR Data", x = "World Rank", y = "Frequency")

# For Shanghai Data (World Rank Distribution)
top_shanghai_ranks <- shanghai_data %>%
  count(world_rank) %>%
  arrange(desc(n)) %>%
  head(20)

ggplot(top_shanghai_ranks, aes(x = factor(world_rank), y = n)) + 
  geom_bar(stat = "identity", fill = "lightyellow", color = "black") +
  labs(title = "Top 20 World Rank Distribution - Shanghai Data", x = "World Rank", y = "Frequency") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

# For Times Data (World Rank Distribution)
top_times_ranks <- times_data %>%
  count(world_rank) %>%
  arrange(desc(n)) %>%
  head(20)

ggplot(top_times_ranks, aes(x = factor(world_rank), y = n)) + 
  geom_bar(stat = "identity", fill = "lightblue", color = "black") +
  labs(title = "Top 20 World Rank Distribution - Times Data", x = "World Rank", y = "Frequency") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

