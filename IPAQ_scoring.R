#########################################################################################################################################################
# International Physical Activity Questionnaire (IPAQ) scoring
# Created by Aayush Patel
#########################################################################################################################################################
#this package must be installed
install.packages(matrixStats)
library(matrixStats)

dataframe <- read.csv("/Users/aayushpatel/Desktop/SB_wave2_All_ages_Block2_UpdatedCLEANFAKEDATA.csv", header = TRUE)
IPAQ <- function(dataframe)
#calculates the score for vigorous activity for each individual by multiplying days per week, minutes per day, and MET constant of 8; omitted if all of the data is not given
dataframe$IPAQ_score_vigorous <- ifelse(dataframe$IPAQ_1==0, dataframe$IPAQ_score_vigorous <- 0, dataframe$IPAQ_score_vigorous <- rowProds(as.matrix(cbind(dataframe[,paste("IPAQ", c(1,2), "1_TEXT", sep = "_")],8)),na.rm =F))
#calculates the score for moderate activity for each individual by multiplying days per week, minutes per day, and MET constant of 4; omitted if all of the data is not given
dataframe$IPAQ_score_moderate <- ifelse(dataframe$IPAQ_3==0, dataframe$IPAQ_score_moderate <- 0, dataframe$IPAQ_score_moderate <- rowProds(as.matrix(cbind(dataframe[,paste("IPAQ", c(3,4), "1_TEXT", sep = "_")],4)),na.rm =F))
#calculates the score for walking activity for each individual by multiplying days per week, minutes per day, and MET constant of 3.3; omitted if all of the data is not given
dataframe$IPAQ_score_walking <- ifelse(dataframe$IPAQ_5==0, dataframe$IPAQ_score_walking  <- 0, dataframe$IPAQ_score_walking <- rowProds(as.matrix(cbind(dataframe[,paste("IPAQ", c(5,6), "1_TEXT", sep = "_")],3.3)), na.rm =F))
#calculates the final score by summing the vigorous, moderate, and walking activity scores together.
dataframe$final_score <- rowSums(dataframe[,paste("IPAQ","score",c("vigorous","moderate","walking"),sep='_')],na.rm = FALSE)
dataframe$final_score

