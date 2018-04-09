#Christopher Vollmer -1128484
#Professor Azencott - Machine Learning - Project #1-BANKNOTES


library(caret)
library(plyr)
library(MASS)
library(ggthemes)
library(plotly)
library(ggplot2)
set.seed(20)

banknotes <- read.table("banknote data.txt", sep = ",")

names(banknotes) <- c("Variance WTI", "Skewness WTI", "Kurtosis WTI", "Entropy WTI", "Class")

  
#The following produce the global min, max, mean and standard deviation of features
bn_stats <- data.frame(4,4)
for(i in 1:4) #i = 1 to number of attributes
{
  bn_stats[1,i] <- min(banknotes[,i])
  bn_stats[2,i] <- max(banknotes[,i])
  bn_stats[3,i] <- mean(banknotes[,i])
  bn_stats[4,i] <- sd(banknotes[,i])
}



colnames(bn_stats) = c("Variance WTI", "Skewness WTI", "Kurtosis WTI", "Entropy WTI")
rownames(bn_stats) = c("Minimum", "Maximum", "Mean", "Standard Dev.")

#Separate my data into training and testing
bn_training <- createDataPartition(banknotes$Class, p = .8, list = FALSE)
training <- banknotes[bn_training,]
testing <- banknotes[-bn_training,]



num_training <- length(training[,1])
num_testing <- length(testing[,1])

#standardize by centering
for(i in 1:4)
{
  training[,i] <- training[,i]/mean(abs(training[,i]))
  testing[,i] <- testing[,i]/mean(abs(testing[,i]))
}


#Training and Testing Set min, max, mean, std#
training_stats <- data.frame(4,4) # preallocate
testing_stats <- data.frame(4,4) #prealloacte
for(i in 1:4) #i = 1 to number of attributes
{
  training_stats[1,i] <- min(training[,i])
  training_stats[2,i] <- max(training[,i])
  training_stats[3,i] <- mean(training[,i])
  training_stats[4,i] <- sd(training[,i])
  
  testing_stats[1,i] <- min(testing[,i])
  testing_stats[2,i] <- max(testing[,i])
  testing_stats[3,i] <- mean(testing[,i])
  testing_stats[4,i] <- sd(testing[,i])
}



colnames(training_stats) = c("Variance WTI", "Skewness WTI", "Kurtosis WTI", "Entropy WTI")
rownames(training_stats) = c("Minimum", "Maximum", "Mean", "Standard Dev.")
colnames(testing_stats) = c("Variance WTI", "Skewness WTI", "Kurtosis WTI", "Entropy WTI")
rownames(testing_stats) = c("Minimum", "Maximum", "Mean", "Standard Dev.")



#Below, I am creating data.frames for the class-conditional min, max, mean and std in the Training Set
training_cond_min <- aggregate(training[,1:4], list(training$Class), min)
training_cond_max <- aggregate(training[,1:4], list(training$Class), max)
training_cond_mean <- aggregate(training[,1:4], list(training$Class), mean)
training_cond_sd <- aggregate(training[,1:4], list(training$Class), sd)
training_counts <- as.data.frame(table(training[,5]))
training_freq_zero <- training_counts[1,2]/num_training #Frequency of Class 0 in the Training Set
training_freq_one <- training_counts[2,2]/num_training #Frequency of Class 1 in the Training Set

colnames(training_cond_min)[1] = "Class_min"
colnames(training_cond_max)[1] = "Class_max"
colnames(training_cond_mean)[1] = "Class_mean"
colnames(training_cond_sd)[1] = "Class_sd"
colnames(training_counts)[1] = "Class_counts"
training_cond_stats <- cbind(training_cond_min, training_cond_max, training_cond_mean, training_cond_sd)
#----------------------------------------------------------------------------------------------------#


#Below, I am creating data.frames for the class-conditional min, max, mean and std in the Testing Set
testing_cond_min <- aggregate(testing[,1:4], list(testing$Class), min)
testing_cond_max <- aggregate(testing[,1:4], list(testing$Class), max)
testing_cond_mean <- aggregate(testing[,1:4], list(testing$Class), mean)
testing_cond_sd <- aggregate(testing[,1:4], list(testing$Class), sd)
testing_counts <- as.data.frame(table(testing[,5]))
testing_freq_zero <- testing_counts[1,2]/num_testing #Frequency of Class 0 in the Testing Set
testing_freq_one <- testing_counts[2,2]/num_testing #Frequency of Class 1 in the Testing Set

colnames(testing_cond_min)[1] = "Class_min"
colnames(testing_cond_max)[1] = "Class_max"
colnames(testing_cond_mean)[1] = "Class_mean"
colnames(testing_cond_sd)[1] = "Class_sd"
colnames(testing_counts)[1] = "Class_counts"
testing_cond_stats <- data.frame(c(testing_cond_min, testing_cond_max, testing_cond_mean, testing_cond_sd))
#--------------------------------------------------------------------------------------------------#


 





###Linear Discriminant Analysis###
#save lda as currlda
currlda <- lda(training[,-5], training[,5], prior = c(training_freq_zero,training_freq_one))

#save the predicted values of lda into the last column of Training and Testing dframes.
training_predict <- predict(currlda, training[,-5])#$Class
training$Pred <- training_predict$class

testing_predict <- predict(currlda, testing[,-5])#$Class
testing$Pred <- testing_predict$class



### ROC Curves ###
### library(DAAG)
### NO time to study and implement, maybe next time
### Interesting but not sure implications of making class priors so unbalanced.

# truepos <- numeric(19)
# falsepos <- numeric(19)
# p1 <- (1:19)/20
# for (i in 1:19) {
#  p <- p1[i]
#  templda <- lda(training[,-5], training[,5], CV = TRUE, prior = c(p,1 - p))
#  confmat <- confusion(training$Class, templda$class, printit = FALSE)$confusion
#  falsepos[i] <- confmat[1, 2]
#  truepos[i] <- confmat[2, 2]
#   }
# ##Now plot the curve.
# tempdf = data.frame(False = falsepos, True = truepos)
# ggplot(data = tempdf, aes(x=False, y = True)) + geom_point() +
#   xlab("False positive rate(Specificity)") + ylab("True positive rate (Sensitivity)")


###Random Forest Algorithm. I don't know much about this and don't have time to research###
# library(randomForest)
# rfdf <- randomForest(Class ~ ., xtest = testing[, -5], ytest = testing[,5], data = training)
# rfdf
