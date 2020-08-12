#########################################################################################################################################################
# International Physical Activity Questionnaire (IPAQ) scoring
# Created by Aayush Patel
#########################################################################################################################################################

dataframe <- read.csv("/Users/aayushpatel/Desktop/DATAIneedtouse.csv", header = TRUE)
IPAQ <- function(dataframe)
finalscore<- c()
  for(i in 1:length(dataframe$IPAQ_1)) {
    if(dataframe$IPAQ_1[i] ==1 & dataframe$IPAQ_2[i] ==1){
      vigorous_activity <- dataframe$IPAQ_1_1_TEXT[i] * dataframe$IPAQ_2_1_TEXT[i] * 8
    } else {
      vigorous_activity <- 0
    }
    
    if(dataframe$IPAQ_3[i] ==1 & dataframe$IPAQ_4[i] ==1){
      moderate_activity <- dataframe$IPAQ_3_1_TEXT[i] * dataframe$IPAQ_4_1_TEXT[i] * 4
    } else {
      moderate_activity <- 0
    }
    
    if(dataframe$IPAQ_5[i] ==1 & dataframe$IPAQ_6[i] ==1){
      walking_activity <- dataframe$IPAQ_5_1_TEXT[i] * dataframe$IPAQ_6_1_TEXT[i] * 3.3
    } else {
      walking_activity <-0
    }
    totalscore = vigorous_activity + moderate_activity + walking_activity
    finalscore = append(finalscore, totalscore)
  }

finalscore




