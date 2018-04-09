#Christopher Vollmer -1128484
#Professor Azencott - Machine Learning - Project #1-BANKNOTES

##plots 2d with colors for class
ggplot(data = training, aes(x = training$`Variance WTI`, y = training$`Skewness WTI`))+
  geom_point(color = ifelse(training$Class == 0, "blue", "red"))+ 
  xlab("Variance WTI") + ylab("Skewness WTI")+ ggtitle("Variance WTI versus Skewness WTI") + theme_bw()

ggplot(data = training, aes(x = training$`Variance WTI`, y = training$`Kurtosis WTI`))+
  geom_point(color = ifelse(training$Class == 0, "blue", "red"))+
  xlab("Variance WTI") + ylab("Kurtosis WTI")+ ggtitle("Variance WTI versus Kurtosis WTI") + theme_bw()

ggplot(data = training, aes(x = training$`Variance WTI`, y = training$`Entropy WTI`))+
  geom_point(color = ifelse(training$Class == 0, "blue", "red"))+
  xlab("Variance WTI") + ylab("Entropy WTI") + ggtitle("Variance WTI versus Entropy WTI") + theme_bw()

ggplot(data = training, aes(x = training$`Skewness WTI`, y = training$`Kurtosis WTI`))+
  geom_point(color = ifelse(training$Class == 0, "blue", "red"))+
  xlab("Skewness WTI") + ylab("Kurtosis WTI") + ggtitle("Skewness WTI versus Kurtosis WTI") + theme_bw()

ggplot(data = training, aes(x = training$`Skewness WTI`, y = training$`Entropy WTI`))+
  geom_point(color = ifelse(training$Class == 0, "blue", "red"))+
  xlab("Skewness WTI") + ylab("Entropy WTI") + ggtitle("Skewness WTI versus Entropy WTI") + theme_bw()

ggplot(data = training, aes(x = training$`Kurtosis WTI`, y = training$`Entropy WTI`))+
  geom_point(color = ifelse(training$Class == 0, "blue", "red"))+
  xlab("Kurtosis WTI") + ylab("Entropy WTI") + ggtitle("Kurtosis WTI versus Entropy WTI") + theme_bw()

#package for partition plotting LDA- only plots two predictors
# library(klaR)
# drawparti(x=training$`Variance WTI`,y=training$`Skewness WTI`, grouping=as.factor(training[,5]),method="lda", prec = 500,
#           col.mean="magenta", image.colors =c("blue","red"), col.correct = "black", col.wrong = "white",
#           xlab = "Variance WTI", ylab = "Skewness WTI")
# 
# drawparti(x=training$`Variance WTI`,y=training$`Kurtosis WTI`, grouping=as.factor(training[,5]),method="lda", prec = 500,
#           col.mean="magenta", image.colors =c("blue","red"), col.correct = "black", col.wrong = "white",
#           xlab = "Variance WTI", ylab = "Kurtosis WTI")
# 
# drawparti(x=training$`Variance WTI`,y=training$`Entropy WTI`, grouping=as.factor(training[,5]),method="lda", prec = 500,
#           col.mean="magenta", image.colors =c("blue","red"), col.correct = "black", col.wrong = "white",
#           xlab = "Variance WTI", ylab = "Entropy WTI")
# 
# drawparti(x=training$`Skewness WTI`,y=training$`Kurtosis WTI`, grouping=as.factor(training[,5]),method="lda", prec = 500,
#           col.mean="magenta", image.colors =c("blue","red"), col.correct = "black", col.wrong = "white",
#           xlab = "Skewness WTI", ylab = "Kurtosis WTI")
# 
# drawparti(x=training$`Skewness WTI`,y=training$`Entropy WTI`, grouping=as.factor(training[,5]),method="lda", prec = 500,
#           col.mean="magenta", image.colors =c("blue","red"), col.correct = "black", col.wrong = "white",
#           xlab = "Skewness WTI", ylab = "Entropy WTI")
# 
# drawparti(x=training$`Kurtosis WTI`,y=training$`Entropy WTI`, grouping=as.factor(training[,5]),method="lda", prec = 500,
#           col.mean="magenta", image.colors =c("blue","red"), col.correct = "black", col.wrong = "white",
#           xlab = "Kurtosis WTI", ylab = "Entropy WTI")





