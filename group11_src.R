library(magrittr)
library(readr)
library(dplyr)
library(ggplot2)
library(tidyr)
library(zoo)
library(corrplot)
library(readxl)
library(DescTools)
#set directory  (must change per computer)
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
