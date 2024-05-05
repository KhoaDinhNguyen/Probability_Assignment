library(magrittr)
library(readr)
library(dplyr)
library(ggplot2)
library(tidyr)
library(zoo)
library(corrplot)
library(readxl)
library(DescTools)
library(caret)
###################################### READ FILE STEP ######################################
# @function setw: set working directory (must change per computer)
setwd("C:/Users/dinhk/desktop/Probability and Statistic/Rcode")

#read file
CPU <- read.csv("Intel_CPUs.csv", na.string = c("","N/A"))

#find the number of missing value for each attribute
missing_values <- colSums(is.na(CPU))

missing_df <- data.frame(Column = names(missing_values), Missing_Values = missing_values)

missing_df$Column <- factor(missing_df$Column, levels = missing_df$Column[order(missing_df$Missing_Values, decreasing = TRUE)])

ggplot(missing_df, aes(x = Missing_Values, y = Column)) +
  geom_bar(stat = "identity", fill = "skyblue") +
  labs(title = "Number of Missing Values by Column", x = "Number of Missing Values",  y = "Column") +
  theme_minimal()

print(colnames(CPU))

CPUs_data = CPU[,c("Product_Collection","Vertical_Segment","Lithography","Launch_Date","Recommended_Customer_Price", "nb_of_Cores", "Processor_Base_Frequency", "Cache","TDP","Bus_Speed","T")]

names(CPUs_data) <- c("PName", "Type", "Litho", "lDate","Price","nCore","baseFreq","Cache","TDP","Bus","Temp")

#select column
CPUs_data = CPU[,c("Lithography","Launch_Date","Recommended_Customer_Price", "nb_of_Cores", "Processor_Base_Frequency", "Cache","TDP","T")]

# @function names: set the name for data frame
# 1st parameter: the object / data frame want to rename
names(CPUs_data) <- c("Litho", "lDate","Price","nCore","baseFreq","Cache","TDP","Temp")

colSums(is.na(CPUs_data))

print(str(CPUs_data))
###################################### CLEANING STEP ######################################
# LITHOGRAPHY
CPUs_data <- CPUs_data[!(is.na(CPUs_data$Litho)), ]
CPUs_data$Litho <- as.integer(gsub(pattern = " nm",replacement = "", x = CPUs_data$Litho))

#CLEAN LAUNCH_DATE
CPUs_data$lDate <- as.yearqtr(x = CPUs_data$lDate, format = "Q%q'%y")
CPUs_data <- CPUs_data[(is.finite(CPUs_data$lDate)), ]
CPUs_data$lDate <- as.Date(CPUs_data$lDate)

#CLEAN TDP
CPUs_data <- CPUs_data[!(is.na(CPUs_data$TDP)), ]
CPUs_data$TDP <- gsub(pattern = "\\ W", replacement = "", x = CPUs_data$TDP)
CPUs_data$TDP <- as.double(CPUs_data$TDP)


# CLEAN CACHE
# Regular expression pattern to extract numeric value and unit
cache_pattern <- "([0-9.]+) (MB|KB)"

# Function to convert bus speed to Gbps
convert_to_kb <- function(amount) {
  match <- regmatches(amount, regexec(cache_pattern, amount))
  if (length(match) == 0) {
    return(NA)  # Return NA for invalid values
  }
  numeric_value <- as.numeric(match[[1]][2])
  unit <- match[[1]][3]
  
  conversion_factor <- ifelse(unit == "KB", 1024 ,1)
  converted_amount <- numeric_value / conversion_factor
  
  return(converted_amount)
}

# Filter out rows with invalid bus_speed values
CPUs_data <- CPUs_data[!is.na(sapply(CPUs_data$Cache, convert_to_kb)), ]

CPUs_data$Cache <- sapply(CPUs_data$Cache, convert_to_kb)

#CLEAN FREQUENCY 
process_fre_pattern <- "([0-9.]+) (MHz|GHz)"

# Function to convert bus speed to Gbps
convert_to_ghz <- function(fre) {
  match <- regmatches(fre, regexec(process_fre_pattern, fre))
  if (length(match) == 0) {
    return(NA)  # Return NA for invalid values
  }
  numeric_value <- as.numeric(match[[1]][2])
  unit <- match[[1]][3]
  
  conversion_factor <- ifelse(unit == "MHz", 0.001, 1)
  conversion_fre <- numeric_value * conversion_factor
  
  return(conversion_fre)
}

# Filter out rows with invalid bus_speed values
CPUs_data <- CPUs_data[!is.na(sapply(CPUs_data$baseFreq, convert_to_ghz)), ]

# Apply conversion function to bus speed column
CPUs_data$baseFreq <- sapply(CPUs_data$baseFreq, convert_to_ghz)

#CLEAN TEMPERATURE

parseTemperature <- function(string) {
  # Adjusted pattern to match temperatures in Celsius, including optional decimal part
  pattern <- "[+-]?[0-9]+\\.?[0-9]*(?=\\s|\\d|\\.|°C)"
  
  # Use gregexpr to find all matches of the pattern in the string
  matches <- gregexpr(pattern, string,  perl = TRUE)
  # Extract the matched temperatures
  temperatures <- regmatches(string, matches)
  # Check if there are any valid matches
  if (length(temperatures[[1]]) == 0) {
    return(NA)  # Return NA for strings with no valid numbers
  }
  
  # Flatten the list to a numeric vector (if necessary) and convert to numeric
  temperatures <- as.numeric(unlist(temperatures))
  
  # Calculate the average temperature
  average <- mean(temperatures)
  
  return(average)
}

CPUs_data[,"Temp"] <- sapply(X = CPUs_data[,"Temp"], FUN = parseTemperature)
CPUs_data <- CPUs_data[!is.na(CPUs_data$Temp), ]

# CLEAN PRICE
CPUs_data <- CPUs_data[!(is.na(CPUs_data$Price)), ]
recommend_price <- function(price_range) {
  if(grepl('-', price_range)) {
    range <- strsplit(price_range, "-")[[1]]
    return((as.double(range[1]) + as.double(range[2])) / 2)
  }
  return (price_range)
}

CPUs_data$Price <- gsub("\\$", "", CPUs_data$Price) 
CPUs_data$Price <- gsub(",", "", CPUs_data$Price)
CPUs_data$Price <- sapply(CPUs_data$Price, recommend_price)
CPUs_data$Price <- as.double(CPUs_data$Price)


CPUs_data$Litho <- as.numeric(CPUs_data$Litho)
CPUs_data$nCore <- as.numeric(CPUs_data$nCore)

###################################### SUMMARIZE STEP ######################################
colSums(is.na(CPUs_data))
print(str(CPUs_data))

summary(CPUs_data)

#Plot data function
plot_data <-function(data, feature_name, binVal){
  ggplot(data = CPUs_data, aes(x = data)) +
    geom_histogram(color = "black", fill = "grey", binwidth = binVal) +
    labs(title = paste("Histogram for" , feature_name), x = feature_name, y = "Frequency") +
    geom_vline(aes(xintercept = mean(data)),color = "red",lwd = 0.75, lty = "dashed") +
    geom_vline(aes(xintercept = median(data)),color = "green",lwd = 1, linetype = "dashed") +
    geom_vline(aes(xintercept = Mode(data)),color = "blue",lwd = 0.5, linetype = "dashed")
}

plot_data(CPUs_data$baseFreq, "baseFreq", 0.1)
plot_data(CPUs_data$Temp, "Temp", 5)
plot_data(CPUs_data$Litho, "Litho",3)
plot_data(CPUs_data$TDP, "TDP", 5)
plot_data(CPUs_data$Cache, "Cache", 1)
plot_data(CPUs_data$nCore, "nCore", 1)
plot_data(CPUs_data$Price, "Price", 500)

### Proving litho is categorical variables
ggplot(CPUs_data, aes(x = lDate, y = Litho)) +
  geom_point()
CPUs_data$Litho <- as.factor(CPUs_data$Litho)

CPUs_core <- CPUs_data[,c("Price","nCore","baseFreq","Cache","TDP","Temp")]
cor_matrix <- cor(CPUs_core)

col <- colorRampPalette(c("#BB4444", "#EE9988", "#FFFFFF", "#77AADD", "#4477AA"))

corrplot(cor_matrix, 
         method="color", type = "full", col = col(200),
         addCoef.col = "black", # Add coefficient of correlation
         tl.col="black", tl.srt=45, #Text label color and rotation
)
###################################### LINEAR MODEL ######################################
model <- lm(TDP ~ Litho + nCore + baseFreq + Price + Cache + Temp, data = CPUs_data)
summary(model)

model <- lm(TDP ~ Litho + nCore + baseFreq + Cache + Temp, data = CPUs_data)
summary(model)

par(mfrow = c(1, 2))
plot(model, which = 2)
plot(model, which = 3)
par(mfrow = c(1, 1))

# @function qqnorm: draw the Q-Q test
# 1st parameter: the plot data
# main : the title of plot
# xlab: lable for x - axis
# ylab: lable for y - axis
# col: color of dot
qqnorm(model$residuals, main = '', 
       xlab = 'Theoretical Quantiles', ylab = 'Data Quantiles', col = 'black')

# @function qqline: draw the Q-Q line in Q-Q test
# col: color of line
# lwd: line width
# lty: type of line (dashed, ...)
qqline(model$residuals, col = '#FF3333', lwd = 2, lty = "dashed")

summary(model)

# Create data frame for real tdp value and predicted tdp value (for Testing the test set)
predicion <- CPUs_data['TDP']
predicion['TDP_Predcit'] <- as.data.frame(predict(model, newdata = CPUs_data))

# Plotting
# The majority of points lie near the line, so its ok.
ggplot(predicion, aes(x = TDP, y = TDP_Predcit)) +
  geom_point(shape=1, color="blue") +
  geom_abline(mapping=aes(intercept= 0, slope = 1), color="darkblue") + 
  labs(x = "TDP", y = "TDP Predicted")
################################################################################
anova <- aov(TDP ~ Litho, data = CPUs_data)
plot(anova, which = 2)

summary(two.way)


plot(model, which = 1)
plot(model, which = 3)

#################################################################################

###################################################################################
tukey_res <- TukeyHSD(anova)
tukey_df <- as.data.frame(tukey_res$Litho)
tukey_df$Comparison <- rownames(tukey_df)

ggplot(tukey_df, aes(xmin = lwr, xmax = upr, y = Comparison)) +
  geom_vline(xintercept = 0, linetype = "dashed", color = "red") +
  geom_errorbarh(aes(height = 0.2), color = "blue") +
  geom_point(aes(x = diff), color = "black") +
  labs(title = "Tukey HSD Test Results", x = "Difference in Means", y = "Comparisons") +
  theme_minimal()

############################# CROSS-VALIDATION #################################
### 5 FOLD
# Set the number of folds for cross-validation
num_folds <- 5

# Set the control parameters for cross-validation
control <- trainControl(method = "cv", number = num_folds)

# Train the linear regression model using cross-validation
model <- train(TDP ~ .-TDP, data = CPUs_data, method = "lm", trControl = control)

# Print the cross-validation results
print(model)
# Get the coefficients of the linear regression model
coefficients <- model$finalModel$coefficients

# Print the coefficients
print(coefficients)

### 10 FOLD
num_folds <- 10

control <- trainControl(method = "cv", number = num_folds)

model <- train(TDP ~ .-TDP, data = CPUs_data, method = "lm", trControl = control)

print(model)

coefficients <- model$finalModel$coefficients

print(coefficients)

### 100 FOLD
num_folds <- 100

control <- trainControl(method = "cv", number = num_folds)

model <- train(TDP ~ .-TDP, data = CPUs_data, method = "lm", trControl = control)

print(model)

coefficients <- model$finalModel$coefficients

print(coefficients)

