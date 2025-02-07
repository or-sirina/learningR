library(dplyr)
library(readr)

#прочтение файла полность
data <- read.csv("C:\\Users\\Vinogradov_V_V\\Downloads\\us_foreign_aid_complete.csv")

#чтение файла чанками
fileIn <- file("in.csv","r")
chunkSize <- 100000 # choose the best size for you
data <- readLines(fileIn, n = chunkSize)
unique(data$Funding.Agency.Name)

dataNew <-  data %>% filter(Funding.Agency.Name == "U.S. Agency for International Development")

