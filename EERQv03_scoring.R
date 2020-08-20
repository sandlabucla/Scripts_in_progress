#########################################################################################################################################################
# E-Emotion Regulation Questionnaire (EERQ) version 03 scoring 
# Created by (insert RA name(s) here)
#########################################################################################################################################################

EERQ <- function(dataframe){
  #calculate Cognitive Reappraisal Mean
  dataframe$ERQ_Reapp_MEAN <- rowMeans(dataframe[,paste("EERQ",c(1,5,10,17,19,22),sep="")], na.rm = TRUE)
  #calculate Cognitive Reappraisal Sum Total
  dataframe$ERQ_Reapp_TOTAL <- rowSums(dataframe[,paste("EERQ",c(1,5,10,17,19,22),sep="")], na.rm = FALSE)
  
  #calculate Expressive Suppression Mean
  dataframe$ERQ_Supp_MEAN <- rowMeans(dataframe[,paste("EERQ",c(4,9,13,20),sep="")], na.rm = TRUE)
  #calculate Expressive Suppression Sum Total
  dataframe$ERQ_Supp_TOTAL <- rowSums(dataframe[,paste("EERQ",c(4,9,13,20),sep="")], na.rm = FALSE) 

  return(dataframe)
}