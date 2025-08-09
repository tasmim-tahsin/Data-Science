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

# Outliers Detection

#Age column
boxplot(data$Age)
mean(data$Age)
quantile(data$Age)

x <- data$Age
x
iqr <- IQR(x)
iqr

lower_bound <-  22 -  1.5 * iqr
upper_bound <-  30 + 1.5 * iqr

x[x < lower_bound | x > upper_bound] <- median(x)
x <- as.integer(round(x))

boxplot(x)
data$Age <- x


#Academic Pressure
boxplot(data$`Academic Pressure`)
quantile(data$`Academic Pressure`)

y <- data$`Academic Pressure`
y

iqr <- IQR(y)
iqr

lower_bound_y <-  2 -  1.5 * iqr
upper_bound_y <-  4 + 1.5 * iqr

y[y < lower_bound_y | y > upper_bound_y] <- median(y)
y <- as.integer(round(y))
boxplot(y)
data$`Academic Pressure` <- y


#Noisy Value
#Suicidal thought column
unique(data$`Have you ever had suicidal thoughts ?`)
data <- data %>%
  mutate(suicidal_thoughts_clean = case_when(
    tolower(`Have you ever had suicidal thoughts ?`) %in% c("yes", "yess") ~ "Yes",
    tolower(`Have you ever had suicidal thoughts ?`) %in% c("no", "noo") ~ "No"
  ))
data$`Have you ever had suicidal thoughts ?` <- data$suicidal_thoughts_clean
data$suicidal_thoughts_clean <- NULL

#Categorical to Numerical
#Gender
data <- data %>%
  mutate(gender_numeric = case_when(
    tolower(Gender) == "male" ~ 1,
    tolower(Gender) == "female" ~ 0
  ))
data$Gender <- data$gender_numeric
data$gender_numeric <- NULL

#Family History
data <- data %>%
  mutate(history_numeric = case_when(
    tolower(`Family History of Mental Illness`) == "yes" ~ 1,
    tolower(`Family History of Mental Illness`) == "no" ~ 0
  ))
data$`Family History of Mental Illness` <- data$history_numeric
data$history_numeric <- NULL

#Normalization
#Study Hours
data <- data %>%
  mutate(StudyHours_normalized = (`Study Hours` - min(`Study Hours`)) / 
           (max(`Study Hours`, na.rm = TRUE) - min(`Study Hours`)))
data$`Study Hours`<- data$StudyHours_normalized
data$StudyHours_normalized <- NULL

#Filtering
filtered_data1 <- data %>%
  filter(`Study Hours` > 10 | Age < 20)

filtered_data2 <- data %>%
  filter(`Financial Stress` > 3 & Age < 20)

filtered_data3 <- data %>%
  filter(`Have you ever had suicidal thoughts ?` == 'Yes' & `Financial Stress` > 3)

#Data Balancing
class_distribution <- data %>%
  count(Gender) %>%
  mutate(percentage = n / sum(n) * 100)

balanced_data <- data %>%
  group_by(Gender) %>%
  slice_sample(n = min(table(data$Gender)), replace = FALSE) %>%
  ungroup()

class_distribution_balanced <- balanced_data %>%
  count(Gender) %>%
  mutate(percentage = n / sum(n) * 100)


#Split Train and Test data

set.seed(123)
train_data <- data %>% 
  sample_frac(0.8)

test_data <- data %>% 
  anti_join(train_data)
