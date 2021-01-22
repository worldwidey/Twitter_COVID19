#######################################
#COVID tweet analysis
#JOBLOSS prediction#
#20/1/11 by Inyoung Jun
#######################################
rm(list=ls())


setwd("C:/Users/inyoungjun/Dropbox (Personal)/[research]year2/[research]TweetCOVID/data")

#####Function#####
library(dplyr)
library(magrittr)
library(lubridate)
library(tidyr)
library(ggplot2)

#install.packages("pROC")
library(pROC)

normalize <- function(x) {
  return((x-min(x))/(max(x)-min(x)))
}
#########################################
###Step 1: Prepare the data set

rawdata <- read.csv("echen_data_with_jobloss_prediction_IY.csv")
data_us <- rawdata %>% 
           filter(us_state!="")
write.csv(data_us, "data_us.csv", row.names = FALSE)  
data_us_labeled<-read.csv("data_us.csv")

data_us_labeled_only<-data_us_labeled %>% filter(is.na(IY_label)==FALSE)

write.csv(data_us_labeled_only,"data_us_labeled_only.csv",row.names = FALSE)
#table(rawdata_us$us_state, useNA = "ifany")
rocdata <- data_us_labeled %>% filter(is.na(IY_label)==FALSE)
table(rocdata$IY_label)#100 RELEVANT, 400 IRRELEVANT

roc_result <- roc(rocdata$IY_label, rocdata$jobloss_probabilities)
roc_result$thresholds
roc_result$auc
roc_result
tiff("ROC_Curve3.tiff")   


plot.roc(roc_result, 
         col="red", 
         print.auc=TRUE,   
         max.auc.polygon=TRUE,   
         print.thres=TRUE, print.thres.pch=19, print.thres.col = "red",   
         auc.polygon=TRUE, auc.polygon.col="#D1F2EB") 
dev.off()   # 저장해줍니다. 

data <- data_us %>% filter(jobloss_probabilities>=0.5)
head(data)

hist(data_us$jobloss_probabilities)
hist(rocdata$jobloss_probabilities)

#Date Transformation
format.str <- "%a %b %d %H:%M:%S %z %Y"
data$date<-as.POSIXct(strptime(data[,"created_at"], format.str, tz = "GMT"), tz = "GMT")
table(month(data$date))

table()
