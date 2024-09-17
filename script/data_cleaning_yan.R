rm(list = ls())
library(readxl)
library(dplyr)
library(data.table)

data <- data.table(read_excel("EbolaLineList-Freetown_Practical.xlsx"))
head(data)
data[`date of outcome` < `date of infection`, `date of outcome` := NA]
data[`date of outcome` < `date of onset`, `date of outcome` := NA]
data[`date of outcome` < `date of hospitalisation`, `date of outcome` := NA]
