install.packages(c("readxl", "dplyr"))

library(readxl)
library(dplyr)

 

data <- read_excel("E:\\Data Science\\Dataset\\Depression Student Dataset.xlsx")

str (data)
summary(data)
colSums(is.na(data))
