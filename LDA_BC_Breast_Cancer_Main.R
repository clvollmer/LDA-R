#Christopher Vollmer -1128484
#Professor Azencott - Machine Learning - Project #1- Breast Cancer


library(caret)
library(plyr)
library(MASS)
library(ggthemes)
library(plotly)
library(ggplot2)

set.seed(20)

bcdf <- read.table("Breast Cancer Data.txt", sep = ",", na.strings = "?")
bcdf <- bcdf[, -1]
names(bcdf) <- c("Thickness", "Uni.Size", "Uni.Shape", "Adhesion", "Epithelial",
                 "Nuclei","Chromatin","Nucleoli","Mitosis","Class")

bcdf <- na.omit(bcdf)
#I chose to flat out remove the rows with the NA values. 
#I believe this is the best route since most of the NA valued rows were Class '2'
#of which there are more cases anyway. The other alternative is to replace the missing 
#values with the mean of that attribute. NOTE: all of the missing values were from 
#attribute 6,'Single Epithelial Cell Size'

###Mitosis does not have a value for 9###

#I am amending the data set to inlcude more samples with Class = '4'. There
#were close to double the values for Class '2' compared to Class '4'.
count = 683
for (i in 1:683)
{
  if(bcdf[i,10]=='4')
  {
    count = count + 1
    bcdf[count,] <- bcdf[i,]
  }
} 

#Separate my data into training and testing. I am allocating 70% of the rows to training
#and 30% to testing. createDataPartition considers Class balance when creating training and testing

bcdf_partition <- createDataPartition(bcdf$Class, p = .7, list = FALSE)
training <- bcdf[bcdf_partition,]
testing <- bcdf[-bcdf_partition,]
num_training <- length(training[,1])
num_testing <- length(testing[,1])



#The following produce the global min, max, mean and standard deviation of features
bcdf_stats <- data.frame(4,4)
for(i in 1:9) #i = 1 to number of attributes
{
  bcdf_stats[1,i] <- min(bcdf[,i])
  bcdf_stats[2,i] <- max(bcdf[,i])
  bcdf_stats[3,i] <- mean(bcdf[,i])
  bcdf_stats[4,i] <- sd(bcdf[,i])
}
colnames(bcdf_stats) = c("Thickness", "Uni.Size", "Uni.Shape", "Adhesion", "Epithelial",
                             "Nuclei","Chromatin","Nucleoli","Mitosis")
rownames(bcdf_stats) = c("Minimum", "Maximum", "Mean", "Standard Dev.")

#Training and Testing Set min, max, mean, std#
training_stats <- data.frame(4,4) # preallocate
testing_stats <- data.frame(4,4) #prealloacte


#Normalizing the dataframes training and testing by dividing by the absolute mean
for(i in 1:9)
{
  training[,i] <- training[,i]/mean(abs(training[,i]))
  testing[,i] <- testing[,i]/mean(abs(testing[,i]))
}


for(i in 1:9) #i = 1 to number of attributes
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


colnames(training_stats) = c("Thickness", "Uni.Size", "Uni.Shape", "Adhesion", "Epithelial",
                             "Nuclei","Chromatin","Nucleoli","Mitosis")
rownames(training_stats) = c("Minimum", "Maximum", "Mean", "Standard Dev.")

colnames(testing_stats) = c("Thickness", "Uni.Size", "Uni.Shape", "Adhesion", "Epithelial",
                            "Nuclei","Chromatin","Nucleoli","Mitosis")
rownames(testing_stats) = c("Minimum", "Maximum", "Mean", "Standard Dev.")



#Below, I am creating data.frames for the class-conditional min, max, mean and std in the Training Set
training_cond_min <- aggregate(training[,1:9], list(training$Class), min)
training_cond_max <- aggregate(training[,1:9], list(training$Class), max)
training_cond_mean <- aggregate(training[,1:9], list(training$Class), mean)
training_cond_sd <- aggregate(training[,1:9], list(training$Class), sd)
training_counts <- as.data.frame(table(training[,10]))
training_freq_two <- training_counts[1,2]/num_training #Frequency of Class 2 in the Training Set
training_freq_four <- training_counts[2,2]/num_training #Frequency of Class 4 in the Training Set

colnames(training_cond_min)[1] = "Class_min"
colnames(training_cond_max)[1] = "Class_max"
colnames(training_cond_mean)[1] = "Class_mean"
colnames(training_cond_sd)[1] = "Class_sd"
colnames(training_counts)[1] = "Class_counts"
training_cond_stats <- cbind(training_cond_min, training_cond_max, training_cond_mean, training_cond_sd)
#----------------------------------------------------------------------------------------------------#


#Below, I am creating data.frames for the class-conditional min, max, mean and std in the Testing Set
testing_cond_min <- aggregate(testing[,1:9], list(testing$Class), min)
testing_cond_max <- aggregate(testing[,1:9], list(testing$Class), max)
testing_cond_mean <- aggregate(testing[,1:9], list(testing$Class), mean)
testing_cond_sd <- aggregate(testing[,1:9], list(testing$Class), sd)
testing_counts <- as.data.frame(table(testing[,10]))
testing_freq_two <- testing_counts[1,2]/num_testing #Frequency of Class 0 in the Testing Set
testing_freq_four <- testing_counts[2,2]/num_testing #Frequency of Class 1 in the Testing Set

colnames(testing_cond_min)[1] = "Class_min"
colnames(testing_cond_max)[1] = "Class_max"
colnames(testing_cond_mean)[1] = "Class_mean"
colnames(testing_cond_sd)[1] = "Class_sd"
colnames(testing_counts)[1] = "Class_counts"
testing_cond_stats <- data.frame(c(testing_cond_min, testing_cond_max, testing_cond_mean, testing_cond_sd))
#--------------------------------------------------------------------------------------------------#




###Linear Discriminant Analysis###
#creating the lda object as breast.lda
breast.lda <- lda(training[,-10], training[,10], prior = c(training_freq_two,training_freq_four))

#save the predicted values of lda into the last column of Training and Testing dframes.
training_predict <- predict(breast.lda, training[,-10])#$Class
training$Pred <- training_predict$class

testing_predict <- predict(breast.lda, testing[,-10])#$Class
testing$Pred <- testing_predict$class




### ROC Curves --- played around but didn't end up implementing ###
### library(DAAG)
### NO time to study and implement, maybe next time
### Interesting but not sure implications of making class priors so unbalanced.

# truepos <- numeric(19)
# falsepos <- numeric(19)
# p1 <- (1:19)/20
# for (i in 1:19) {
#   p <- p1[i]
#   Pima.CV1p <- lda(training[,-10], training[,10], CV = TRUE, prior = c(p,1 - p))
#   confmat <- confusion(training$Class, Pima.CV1p$class, printit = FALSE)$confusion
#   falsepos[i] <- confmat[1, 2]
#   truepos[i] <- confmat[2, 2]
# }
# ##Now plot the curve.
# tempdf = data.frame(False = falsepos, True = truepos)
# ggplot(data = tempdf, aes(x=False, y = True)) + geom_point() +
#   xlab("False positive rate(Specificity)") + ylab("True positive rate (Sensitivity)")


###Random Forest Algorithm###
# library(randomForest)
# rfdf <- randomForest(Class ~ ., xtest = testing[, -10], ytest = testing[,10], data = training)
# rfdf

