install.packages(c("readxl", "dplyr"))

library(readxl)
library(dplyr)

 

data <- read_excel("E:\\Data Science\\Dataset\\Depression Student Dataset.xlsx")

str (data)
summary(data)
colSums(is.na(data))


show(data$Age_norm)
class(data$Gender)
data$Gender
data$Age
unique(data)
mean(data$Age)
mode(data$Age)


#Missing value replacement (starts here)
#checking missing values
missing_summary <- colSums(is.na(data))
missing_summary
colSums(is.na(data))


#Age missing value
data$Age[is.na(data$Age)] <- mean(data$Age, na.rm = TRUE)
data$Age
data$Age <- as.integer(round(data$Age))
head(data)

#Gender column
table(data$Gender)
mode_gender <- names(sort(table(data$Gender), decreasing = TRUE))[1]
data$Gender[is.na(data$Gender)] <- mode_gender

# Sleep Duration
unique(data$`Sleep Duration`)
mode_sleep <- names(sort(table(data$`Sleep Duration`), decreasing = TRUE))[1]
data$`Sleep Duration`[is.na(data$`Sleep Duration`)] <- mode_sleep

#study hours
hist(data$`Study Hours`)
data$`Study Hours`[is.na(data$`Study Hours`)] <- mean(data$`Study Hours`, na.rm = TRUE)
data$`Study Hours` <- as.integer(round(data$`Study Hours`))

#Depression
mode_depression <- names(sort(table(data$Depression), decreasing = TRUE))[1]
data$Depression[is.na(data$Depression)] <- mode_depression


