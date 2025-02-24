# Load required libraries
library(readxl)
library(dplyr)
library(ggplot2)
library(tidyr)

# Step 1: Load the Excel file and extract the relevant table
file_path <- "/Users/zuhannoor/Documents/1Studies/fundamentals/cw_r.xlsx"  # Replace with the actual file path if needed
data <- read_excel(file_path, sheet = "Table 3d", skip = 8)

# Rename columns for easier access
colnames(data) <- c(
  "Category", "2012_2013", "2013_2014", "2014_2015",
  "2015_2016", "2016_2017", "2017_2018",
  "2018_2019", "2019_2020", "2020_2021",
  "2021_2022", "Percentage_Change"
)

# Remove rows with no numeric data in year columns
year_columns <- c("2012_2013", "2013_2014", "2014_2015", 
                  "2015_2016", "2016_2017", "2017_2018", 
                  "2018_2019", "2019_2020", "2020_2021", "2021_2022")

# Convert year columns to numeric, coercing invalid entries to NA
data[year_columns] <- lapply(data[year_columns], function(x) as.numeric(as.character(x)))

# Filter out rows with all NA values in the year columns
data <- data %>%
  filter(!is.na(Category)) %>%
  filter_at(vars(all_of(year_columns)), any_vars(!is.na(.)))

# Step 2: Create subsets for the two categories
# Banking and credit industry fraud
df_banking <- data %>%
  filter(Category == "Banking and credit industry fraud")

# Computer fraud (combine all relevant subcategories)
computer_fraud_categories <- c(
  "Computer viruses/malware",
  "Denial of service attack",
  "Denial of service attack (extortion)",
  "Hacking - server",
  "Hacking - personal",
  "Hacking - social media and email",
  "Hacking - PBX/dial through",
  "Hacking (extortion)"
)

df_computer_fraud <- data %>%
  filter(Category %in% computer_fraud_categories)

# Combine computer fraud categories into one
df_computer_fraud_combined <- df_computer_fraud %>%
  summarise(across(all_of(year_columns), sum, na.rm = TRUE))

# Step 3: Visualize dispersion and central tendency
# Convert data from wide to long format for easier plotting
banking_long <- df_banking %>%
  select(-Percentage_Change) %>%
  pivot_longer(cols = all_of(year_columns), names_to = "Year", values_to = "Value")

computer_fraud_long <- df_computer_fraud_combined %>%
  pivot_longer(cols = all_of(year_columns), names_to = "Year", values_to = "Value")

# Dispersion: Boxplot
ggplot(banking_long, aes(x = Year, y = Value)) +
  geom_(fill = "blue", alpha = 0.5) +
  labs(title = "Dispersion of Banking Fraud Over Years",
       x = "Year", y = "Value") +
  theme_minimal()

ggplot(computer_fraud_long, aes(x = Year, y = Value)) +
  geom_boxplot(fill = "red", alpha = 0.5) +
  labs(title = "Dispersion of Computer Fraud Over Years",
       x = "Year", y = "Value") +
  theme_minimal()

# Central Tendency: Mean by Year
banking_mean <- banking_long %>%
  group_by(Year) %>%
  summarise(Mean_Value = mean(Value, na.rm = TRUE))

computer_fraud_mean <- computer_fraud_long %>%
  group_by(Year) %>%
  summarise(Mean_Value = mean(Value, na.rm = TRUE))

ggplot(banking_mean, aes(x = Year, y = Mean_Value)) +
  geom_col(fill = "blue", alpha = 0.7) +
  labs(title = "Central Tendency (Mean) of Banking Fraud",
       x = "Year", y = "Mean Value") +
  theme_minimal()

ggplot(computer_fraud_mean, aes(x = Year, y = Mean_Value)) +
  geom_col(fill = "red", alpha = 0.7) +
  labs(title = "Central Tendency (Mean) of Computer Fraud",
       x = "Year", y = "Mean Value") +
  theme_minimal()




//////////////////
  
  

# Load required libraries
library(readxl)
library(dplyr)
library(ggplot2)
library(tidyr)

# Step 1: Load the Excel file and extract the relevant table
file_path <- "/Users/zuhannoor/Documents/1Studies/fundamentals/cw_r.xlsx"  # Replace with the actual file path if needed
data <- read_excel(file_path, sheet = "Table 3d", skip = 8)

# Rename columns for easier access
colnames(data) <- c(
  "Category", "2012_2013", "2013_2014", "2014_2015",
  "2015_2016", "2016_2017", "2017_2018",
  "2018_2019", "2019_2020", "2020_2021",
  "2021_2022", "Percentage_Change"
)

# Remove rows with no numeric data in year columns
year_columns <- c("2012_2013", "2013_2014", "2014_2015", 
                  "2015_2016", "2016_2017", "2017_2018", 
                  "2018_2019", "2019_2020", "2020_2021", "2021_2022")

# Convert year columns to numeric, coercing invalid entries to NA
data[year_columns] <- lapply(data[year_columns], function(x) as.numeric(as.character(x)))

# Filter out rows with all NA values in the year columns
data <- data %>%
  filter(!is.na(Category)) %>%
  filter_at(vars(all_of(year_columns)), any_vars(!is.na(.)))

# Step 2: Create subsets for the two categories
# Banking and credit industry fraud subcategories
banking_fraud_categories <- c(
  "Cheque, plastic card and online bank accounts (not PSP) [note 5, 6]",
  "Application fraud (excluding mortgages)",
  "Mortgage related fraud",
  "Mandate fraud",
  "Dishonestly retaining a wrongful credit"
)

df_banking <- data %>%
  filter(Category %in% banking_fraud_categories)

# Computer fraud subcategories
computer_fraud_categories <- c(
  "Computer viruses/malware",
  "Denial of service attack",
  "Denial of service attack (extortion)",
  "Hacking - server",
  "Hacking - personal",
  "Hacking - social media and email",
  "Hacking - PBX/dial through",
  "Hacking (extortion)"
)

df_computer_fraud <- data %>%
  filter(Category %in% computer_fraud_categories)

# Convert data to long format for visualization
banking_long <- df_banking %>%
  pivot_longer(cols = all_of(year_columns), names_to = "Year", values_to = "Value") %>%
  mutate(Year = gsub("_", "-", Year))  # Replace underscores for better labels

computer_fraud_long <- df_computer_fraud %>%
  pivot_longer(cols = all_of(year_columns), names_to = "Year", values_to = "Value") %>%
  mutate(Year = gsub("_", "-", Year))  # Replace underscores for better labels

# Step 3: Enhanced Visualizations

## Banking Fraud Boxplot
ggplot(banking_long, aes(x = Year, y = Value, fill = Category)) +
  geom_boxplot(alpha = 0.7, outlier.color = "red", outlier.size = 1) +
  labs(title = "Dispersion of Banking Fraud Subcategories Over Years",
       x = "Year", y = "Value", fill = "Subcategory") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

## Computer Fraud Boxplot
ggplot(computer_fraud_long, aes(x = Year, y = Value, fill = Category)) +
  geom_boxplot(alpha = 0.7, outlier.color = "red", outlier.size = 1) +
  labs(title = "Dispersion of Computer Fraud Subcategories Over Years",
       x = "Year", y = "Value", fill = "Subcategory") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

# Central Tendency: Mean by Year
banking_mean <- banking_long %>%
  group_by(Year) %>%
  summarise(Mean_Value = mean(Value, na.rm = TRUE))

computer_fraud_mean <- computer_fraud_long %>%
  group_by(Year) %>%
  summarise(Mean_Value = mean(Value, na.rm = TRUE))

ggplot(banking_mean, aes(x = Year, y = Mean_Value)) +
  geom_col(fill = "blue", alpha = 0.7) +
  labs(title = "Central Tendency (Mean) of Banking Fraud",
       x = "Year", y = "Mean Value") +
  theme_minimal()

ggplot(computer_fraud_mean, aes(x = Year, y = Mean_Value)) +
  geom_col(fill = "red", alpha = 0.7) +
  labs(title = "Central Tendency (Mean) of Computer Fraud",
       x = "Year", y = "Mean Value") +
  theme_minimal()
