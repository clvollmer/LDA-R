#Christopher Vollmer -1128484
#Professor Azencott - Machine Learning - Project #1 - Banknotes
#Performance 

#generates conditional success rates of lda applied to TRAINING set

train_success_zero = 0
train_success_one = 0
for(i in 1:length(training[,5]))
{
  if (training[i,6] == training[i,5])
  { 
    if(training[i,5] == 0)
    {train_success_zero = train_success_zero + 1}
    else train_success_one = train_success_one + 1
  }
}

#generates conditional success rates of lda applied to TESTING set

test_success_zero = 0
test_success_one = 0
for(i in 1:length(testing[,5]))
{
  if (testing[i,6] == testing[i,5])
  { if(testing[i,5] == 0)
  {test_success_zero = test_success_zero + 1}
    else test_success_one = test_success_one + 1
  }
}

#------------------Building Confusion Matrix for TRAINING Set
success_rate1 <- data.frame(train_success_zero/training_counts[1,2],(training_counts[2,2]-train_success_one)/training_counts[2,2])
success_rate2 <- data.frame((training_counts[1,2]-train_success_zero)/training_counts[1,2], train_success_one/training_counts[2,2])

colnames(success_rate1) = c("Condition Positive", "Condition Negative")
rownames(success_rate1) = c("Outcome Positive")
colnames(success_rate2) = c("Condition Positive", "Condition Negative")
rownames(success_rate2) = c("Outcome Negative")


training_confusion <- rbind(success_rate1, success_rate2)
View(training_confusion)
#---------------------Building Diagnostics for TRAINING Set
training_sens <- training_confusion[1,1]/(training_confusion[1,1] + training_confusion[2,1])
training_spec <- training_confusion[2,2]/(training_confusion[1,2] + training_confusion[2,2])
training_pos_pred_value <- training_confusion[1,1]/(training_confusion[1,1] + training_confusion[2,1])
training_neg_pred_value <- training_confusion[2,2]/(training_confusion[2,2] + training_confusion[1,2])
acc.training <- (training_sens*614+training_spec*484)/1098
training_diagnostics <- rbind(training_sens, training_spec, training_pos_pred_value, training_neg_pred_value,acc.training)
rownames(training_diagnostics) = c("Sensitivity", "Specificity", "Positive Predictive Value",
                                   "Negative Predictive Value", "Accuracy")
colnames(training_diagnostics) =c("Training")
View(training_diagnostics)

#------------------Building Confusion Matrix for TESTING Set
success_rate3 <- data.frame(test_success_zero/testing_counts[1,2],(testing_counts[2,2]-test_success_one)/testing_counts[2,2])
success_rate4 <- data.frame((testing_counts[1,2]-test_success_zero)/testing_counts[1,2], test_success_one/testing_counts[2,2])

colnames(success_rate3) = c("Condition Positive", "Condition Negative")
rownames(success_rate3) = c("Outcome Positive")
colnames(success_rate4) = c("Condition Positive", "Condition Negative")
rownames(success_rate4) = c("Outcome Negative")


testing_confusion <- rbind(success_rate3, success_rate4)
View(testing_confusion)
#---------------------Building Diagnostics for TESTING Set
testing_sens <- testing_confusion[1,1]/(testing_confusion[1,1] + testing_confusion[2,1])
testing_spec <- testing_confusion[2,2]/(testing_confusion[1,2] + testing_confusion[2,2])
testing_pos_pred_value <- testing_confusion[1,1]/(testing_confusion[1,1] + testing_confusion[2,1])
testing_neg_pred_value <- testing_confusion[2,2]/(testing_confusion[2,2] + testing_confusion[1,2])
acc.testing <- (testing_sens*148+testing_spec*126)/274
testing_diagnostics <- rbind(testing_sens, testing_spec, testing_pos_pred_value, testing_neg_pred_value, acc.testing)
rownames(testing_diagnostics) = c("Sensitivity", "Specificity", "Positive Predictive Value",
                                  "Negative Predictive Value", "Accuracy")
colnames(testing_diagnostics) =c("Testing")
View(testing_diagnostics)



#Building Confidence Intervals in TRAINING SET

#------------CONFIDENCE INTERVAL OF 90%
x <- train_success_zero/num_training #proportion predicted as class 0

training_CI_zero_Lower <- data.frame(x-1.645*sqrt(x*(1-x)/num_training))
training_CI_zero_Upper <- data.frame(x+1.645*sqrt(x*(1-x)/num_training))
training_CI90_zero <- data.frame(training_CI_zero_Lower, training_CI_zero_Upper)
colnames(training_CI90_zero) = c("Lower", "Upper")

x <- train_success_one/num_training #proportion predicted as class 1
training_CI_one_Lower <- data.frame(x-1.645*sqrt(x*(1-x)/num_training))
training_CI_one_Upper <- data.frame(x+1.645*sqrt(x*(1-x)/num_training))
training_CI90_one <- data.frame(training_CI_one_Lower, training_CI_one_Upper)
colnames(training_CI90_one) = c("Lower", "Upper")

#------------CONFIDENCE INTERVAL OF 95%
x <- train_success_zero/num_training #proportion predicted as class 0

training_CI_zero_Lower <- data.frame(x-1.96*sqrt(x*(1-x)/num_training))
training_CI_zero_Upper <- data.frame(x+1.96*sqrt(x*(1-x)/num_training))
training_CI95_zero <- data.frame(training_CI_zero_Lower, training_CI_zero_Upper)
colnames(training_CI95_zero) = c("Lower", "Upper")

x <- train_success_one/num_training #proportion predicted as class 1
training_CI_one_Lower <- data.frame(x-1.96*sqrt(x*(1-x)/num_training))
training_CI_one_Upper <- data.frame(x+1.96*sqrt(x*(1-x)/num_training))
training_CI95_one <- data.frame(training_CI_one_Lower, training_CI_one_Upper)
colnames(training_CI95_one) = c("Lower", "Upper")

#------------CONFIDENCE INTERVAL OF 99%

x <- train_success_zero/num_training #proportion predicted as class 0

training_CI_zero_Lower <- data.frame(x-2.53*sqrt(x*(1-x)/num_training))
training_CI_zero_Upper <- data.frame(x+2.53*sqrt(x*(1-x)/num_training))
training_CI99_zero <- data.frame(training_CI_zero_Lower, training_CI_zero_Upper)
colnames(training_CI99_zero) = c("Lower", "Upper")

x <- train_success_one/num_training #proportion predicted as class 1
training_CI_one_Lower <- data.frame(x-2.53*sqrt(x*(1-x)/num_training))
training_CI_one_Upper <- data.frame(x+2.53*sqrt(x*(1-x)/num_training))
training_CI99_one <- data.frame(training_CI_one_Lower, training_CI_one_Upper)
colnames(training_CI99_one) = c("Lower", "Upper")

#--------------------------------------------------------

training_CI_zero <- rbind(training_CI90_zero,training_CI95_zero,training_CI99_zero)
training_CI_zero$Actual <- training_freq_zero

training_CI_one <- rbind(training_CI90_one,training_CI95_one,training_CI99_one)
training_CI_one$Actual <- training_freq_one



training_CI <- rbind(training_CI_zero, training_CI_one)

rownames(training_CI) <- c("90% | CLASS 0","95% | CLASS 0","99% | CLASS 0","90% | CLASS 1","95% | CLASS 1","99% | CLASS 1")






#Building Confidence Intervals in TESTING SET

#------------CONFIDENCE INTERVAL OF 90%
x <- test_success_zero/num_testing #proportion predicted as class 0

testing_CI_zero_Lower <- data.frame(x-1.645*sqrt(x*(1-x)/num_testing))
testing_CI_zero_Upper <- data.frame(x+1.645*sqrt(x*(1-x)/num_testing))
testing_CI90_zero <- data.frame(testing_CI_zero_Lower, testing_CI_zero_Upper)
colnames(testing_CI90_zero) = c("Lower", "Upper")

x <- test_success_one/num_testing #proportion predicted as class 1
testing_CI_one_Lower <- data.frame(x-1.645*sqrt(x*(1-x)/num_testing))
testing_CI_one_Upper <- data.frame(x+1.645*sqrt(x*(1-x)/num_testing))
testing_CI90_one <- data.frame(testing_CI_one_Lower, testing_CI_one_Upper)
colnames(testing_CI90_one) = c("Lower", "Upper")

#------------CONFIDENCE INTERVAL OF 95%
x <- test_success_zero/num_testing #proportion predicted as class 0

testing_CI_zero_Lower <- data.frame(x-1.96*sqrt(x*(1-x)/num_testing))
testing_CI_zero_Upper <- data.frame(x+1.96*sqrt(x*(1-x)/num_testing))
testing_CI95_zero <- data.frame(testing_CI_zero_Lower, testing_CI_zero_Upper)
colnames(testing_CI95_zero) = c("Lower", "Upper")

x <- test_success_one/num_testing #proportion predicted as class 1
testing_CI_one_Lower <- data.frame(x-1.96*sqrt(x*(1-x)/num_testing))
testing_CI_one_Upper <- data.frame(x+1.96*sqrt(x*(1-x)/num_testing))
testing_CI95_one <- data.frame(testing_CI_one_Lower, testing_CI_one_Upper)
colnames(testing_CI95_one) = c("Lower", "Upper")

#------------CONFIDENCE INTERVAL OF 99%

x <- test_success_zero/num_testing #proportion predicted as class 0

testing_CI_zero_Lower <- data.frame(x-2.53*sqrt(x*(1-x)/num_testing))
testing_CI_zero_Upper <- data.frame(x+2.53*sqrt(x*(1-x)/num_testing))
testing_CI99_zero <- data.frame(testing_CI_zero_Lower, testing_CI_zero_Upper)
colnames(testing_CI99_zero) = c("Lower", "Upper")

x <- test_success_one/num_testing #proportion predicted as class 1
testing_CI_one_Lower <- data.frame(x-2.53*sqrt(x*(1-x)/num_testing))
testing_CI_one_Upper <- data.frame(x+2.53*sqrt(x*(1-x)/num_testing))
testing_CI99_one <- data.frame(testing_CI_one_Lower, testing_CI_one_Upper)
colnames(testing_CI99_one) = c("Lower", "Upper")

#--------------------------------------------------------

testing_CI_zero <- rbind(testing_CI90_zero,testing_CI95_zero,testing_CI99_zero)
testing_CI_zero$Actual <- testing_freq_zero

testing_CI_one <- rbind(testing_CI90_one,testing_CI95_one,testing_CI99_one)
testing_CI_one$Actual <- testing_freq_one



testing_CI <- rbind(testing_CI_zero, testing_CI_one)

rownames(testing_CI) <- c("90% | CLASS 0","95% | CLASS 0","99% | CLASS 0","90% | CLASS 1","95% | CLASS 1","99% | CLASS 1")



###wrongs1df looks at the vectors which generated wrong predictions.
###the plots show misclassifications based on being above or below log(prior class 0/ prior class1)

wrongsdf1 = training[training$Class!=training$Pred,]
wrongsdf2 = testing[testing$Class!=testing$Pred,]

ggplot() + geom_point(aes(x=1:length(training_predict$x), y = training_predict$x),
                      color = ifelse(training$Class!=training$Pred, "black", ifelse(training$Class == "0", "blue", "red")),
                      shape = ifelse(training$Class == "0", "0", "1"), size = 3)+
  geom_hline(yintercept = log(currlda$prior[1]/currlda$prior[2]))+
  xlab("Index")+ylab("Decision Value")

ggplot() + geom_point(aes(x=1:length(testing_predict$x), y = testing_predict$x),
                      color = ifelse(testing$Class!=testing$Pred, "black", ifelse(testing$Class == "0", "blue", "red")),
                      shape = ifelse(testing$Class == "0", "0", "1"), size = 3)+
  geom_hline(yintercept = log(currlda$prior[1]/currlda$prior[2]))+
  xlab("Index")+ylab("Decision Value")




