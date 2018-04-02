#Christopher Vollmer -1128484
#Professor Azencott - Machine Learning - Project #1-breast cancer
#Performance 


#generates conditional success rates of lda applied to TRAINING set

train_success_two = 0
train_success_four = 0
for(i in 1:length(training[,10]))
{
  if (training[i,10] == training[i,11])
  { 
    if(training[i,10] == 2)
    {train_success_two = train_success_two + 1}
    else train_success_four = train_success_four + 1
  }
}

#generates conditional success rates of lda applied to TESTING set

test_success_two = 0
test_success_four = 0
for(i in 1:length(testing[,10]))
{
  if (testing[i,10] == testing[i,11])
  { if(testing[i,10] == 2)
  {test_success_two = test_success_two + 1}
    else test_success_four = test_success_four + 1
  }
}

#------------------Building Confusion Matrix for TRAINING Set
success_rate1 <- data.frame(train_success_two/training_counts[1,2],(training_counts[2,2]-train_success_four)/training_counts[2,2])
success_rate2 <- data.frame((training_counts[1,2]-train_success_two)/training_counts[1,2], train_success_four/training_counts[2,2])

colnames(success_rate1) = c("Condition Positive", "Condition Negative")
rownames(success_rate1) = c("Outcome Positive")
colnames(success_rate2) = c("Condition Positive", "Condition Negative")
rownames(success_rate2) = c("Outcome Negative")


training_confusion <- rbind(success_rate1, success_rate2)

#---------------------Building Diagnostics for TRAINING Set
training_sens <- training_confusion[1,1]/(training_confusion[1,1] + training_confusion[2,1])
training_spec <- training_confusion[2,2]/(training_confusion[1,2] + training_confusion[2,2])
training_pos_pred_value <- training_confusion[1,1]/(training_confusion[1,1] + training_confusion[2,1])
training_neg_pred_value <- training_confusion[2,2]/(training_confusion[2,2] + training_confusion[1,2])
acc.training <- (training_sens*310+training_spec*336)/646
training_diagnostics <- rbind(training_sens, training_spec, training_pos_pred_value, training_neg_pred_value,acc.training)
rownames(training_diagnostics) = c("Sensitivity", "Specificity", "Positive Predictive Value",
                                   "Negative Predictive Value", "Accuracy")
colnames(training_diagnostics) = c("Training")
View(training_diagnostics)

#------------------Building Confusion Matrix for TESTING Set
success_rate3 <- data.frame(test_success_two/testing_counts[1,2],(testing_counts[2,2]-test_success_four)/testing_counts[2,2])
success_rate4 <- data.frame((testing_counts[1,2]-test_success_two)/testing_counts[1,2], test_success_four/testing_counts[2,2])

colnames(success_rate3) = c("Condition Positive", "Condition Negative")
rownames(success_rate3) = c("Outcome Positive")
colnames(success_rate4) = c("Condition Positive", "Condition Negative")
rownames(success_rate4) = c("Outcome Negative")


testing_confusion <- rbind(success_rate3, success_rate4)

#---------------------Building Diagnostics for TESTING Set
testing_sens <- testing_confusion[1,1]/(testing_confusion[1,1] + testing_confusion[2,1])
testing_spec <- testing_confusion[2,2]/(testing_confusion[1,2] + testing_confusion[2,2])
testing_pos_pred_value <- testing_confusion[1,1]/(testing_confusion[1,1] + testing_confusion[2,1])
testing_neg_pred_value <- testing_confusion[2,2]/(testing_confusion[2,2] + testing_confusion[1,2])
acc.testing <- (testing_sens*134+testing_spec*142)/276
testing_diagnostics <- rbind(testing_sens, testing_spec, testing_pos_pred_value, testing_neg_pred_value,acc.testing)
rownames(testing_diagnostics) = c("Sensitivity", "Specificity", "Positive Predictive Value",
                                  "Negative Predictive Value", "Accuracy")
colnames(testing_diagnostics) = c("Testing")
View(testing_diagnostics)

#Building Confidence Intervals in TRAINING SET

#------------CONFIDENCE INTERVAL OF 90%
x <- train_success_two/num_training #proportion predicted as class 2

training_CI_two_Lower <- data.frame(x-1.645*sqrt(x*(1-x)/num_training))
training_CI_two_Upper <- data.frame(x+1.645*sqrt(x*(1-x)/num_training))
training_CI90_two <- data.frame(training_CI_two_Lower, training_CI_two_Upper)
colnames(training_CI90_two) = c("Lower", "Upper")

x <- train_success_four/num_training #proportion predicted as class 4
training_CI_four_Lower <- data.frame(x-1.645*sqrt(x*(1-x)/num_training))
training_CI_four_Upper <- data.frame(x+1.645*sqrt(x*(1-x)/num_training))
training_CI90_four <- data.frame(training_CI_four_Lower, training_CI_four_Upper)
colnames(training_CI90_four) = c("Lower", "Upper")

#------------CONFIDENCE INTERVAL OF 95%
x <- train_success_two/num_training #proportion predicted as class 2

training_CI_two_Lower <- data.frame(x-1.96*sqrt(x*(1-x)/num_training))
training_CI_two_Upper <- data.frame(x+1.96*sqrt(x*(1-x)/num_training))
training_CI95_two <- data.frame(training_CI_two_Lower, training_CI_two_Upper)
colnames(training_CI95_two) = c("Lower", "Upper")

x <- train_success_four/num_training #proportion predicted as class 4
training_CI_four_Lower <- data.frame(x-1.96*sqrt(x*(1-x)/num_training))
training_CI_four_Upper <- data.frame(x+1.96*sqrt(x*(1-x)/num_training))
training_CI95_four <- data.frame(training_CI_four_Lower, training_CI_four_Upper)
colnames(training_CI95_four) = c("Lower", "Upper")

#------------CONFIDENCE INTERVAL OF 99%

x <- train_success_two/num_training #proportion predicted as class 2

training_CI_two_Lower <- data.frame(x-2.53*sqrt(x*(1-x)/num_training))
training_CI_two_Upper <- data.frame(x+2.53*sqrt(x*(1-x)/num_training))
training_CI99_two <- data.frame(training_CI_two_Lower, training_CI_two_Upper)
colnames(training_CI99_two) = c("Lower", "Upper")

x <- train_success_four/num_training #proportion predicted as class 4
training_CI_four_Lower <- data.frame(x-2.53*sqrt(x*(1-x)/num_training))
training_CI_four_Upper <- data.frame(x+2.53*sqrt(x*(1-x)/num_training))
training_CI99_four <- data.frame(training_CI_four_Lower, training_CI_four_Upper)
colnames(training_CI99_four) = c("Lower", "Upper")

#--------------------------------------------------------

training_CI_two <- rbind(training_CI90_two,training_CI95_two,training_CI99_two)
training_CI_two$Actual <- training_freq_two

training_CI_four <- rbind(training_CI90_four,training_CI95_four,training_CI99_four)
training_CI_four$Actual <- training_freq_four



training_CI <- rbind(training_CI_two, training_CI_four)

rownames(training_CI) <- c("90% | CLASS 2","95% | CLASS 2","99% | CLASS 2","90% | CLASS 4","95% | CLASS 4","99% | CLASS 4")






#Building Confidence Intervals in TESTING SET

#------------CONFIDENCE INTERVAL OF 90%
x <- test_success_two/num_testing #proportion predicted as class 2

testing_CI_two_Lower <- data.frame(x-1.645*sqrt(x*(1-x)/num_testing))
testing_CI_two_Upper <- data.frame(x+1.645*sqrt(x*(1-x)/num_testing))
testing_CI90_two <- data.frame(testing_CI_two_Lower, testing_CI_two_Upper)
colnames(testing_CI90_two) = c("Lower", "Upper")

x <- test_success_four/num_testing #proportion predicted as class 4
testing_CI_four_Lower <- data.frame(x-1.645*sqrt(x*(1-x)/num_testing))
testing_CI_four_Upper <- data.frame(x+1.645*sqrt(x*(1-x)/num_testing))
testing_CI90_four <- data.frame(testing_CI_four_Lower, testing_CI_four_Upper)
colnames(testing_CI90_four) = c("Lower", "Upper")

#------------CONFIDENCE INTERVAL OF 95%
x <- test_success_two/num_testing #proportion predicted as class 2

testing_CI_two_Lower <- data.frame(x-1.96*sqrt(x*(1-x)/num_testing))
testing_CI_two_Upper <- data.frame(x+1.96*sqrt(x*(1-x)/num_testing))
testing_CI95_two <- data.frame(testing_CI_two_Lower, testing_CI_two_Upper)
colnames(testing_CI95_two) = c("Lower", "Upper")

x <- test_success_four/num_testing #proportion predicted as class 4
testing_CI_four_Lower <- data.frame(x-1.96*sqrt(x*(1-x)/num_testing))
testing_CI_four_Upper <- data.frame(x+1.96*sqrt(x*(1-x)/num_testing))
testing_CI95_four <- data.frame(testing_CI_four_Lower, testing_CI_four_Upper)
colnames(testing_CI95_four) = c("Lower", "Upper")

#------------CONFIDENCE INTERVAL OF 99%

x <- test_success_two/num_testing #proportion predicted as class 2

testing_CI_two_Lower <- data.frame(x-2.53*sqrt(x*(1-x)/num_testing))
testing_CI_two_Upper <- data.frame(x+2.53*sqrt(x*(1-x)/num_testing))
testing_CI99_two <- data.frame(testing_CI_two_Lower, testing_CI_two_Upper)
colnames(testing_CI99_two) = c("Lower", "Upper")

x <- test_success_four/num_testing #proportion predicted as class 4
testing_CI_four_Lower <- data.frame(x-2.53*sqrt(x*(1-x)/num_testing))
testing_CI_four_Upper <- data.frame(x+2.53*sqrt(x*(1-x)/num_testing))
testing_CI99_four <- data.frame(testing_CI_four_Lower, testing_CI_four_Upper)
colnames(testing_CI99_four) = c("Lower", "Upper")

#--------------------------------------------------------

testing_CI_two <- rbind(testing_CI90_two,testing_CI95_two,testing_CI99_two)
testing_CI_two$Actual <- testing_freq_two

testing_CI_four <- rbind(testing_CI90_four,testing_CI95_four,testing_CI99_four)
testing_CI_four$Actual <- testing_freq_four



testing_CI <- rbind(testing_CI_two, testing_CI_four)

rownames(testing_CI) <- c("90% | CLASS 2","95% | CLASS 2","99% | CLASS 2","90% | CLASS 4","95% | CLASS 4","99% | CLASS 4")



###wrongs1df looks at the vectors which generated wrong predictions.
###the plots show misclassifications based on being above or below log(prior class 2/ prior class4).

wrongsdf1 = training[training$Class!=training$Pred,]
wrongsdf2 = testing[testing$Class!=testing$Pred,]

ggplot() + geom_point(aes(x=1:length(training_predict$x), y = training_predict$x),
                      color = ifelse(training$Class!=training$Pred, "black", ifelse(training$Class == "2", "blue", "red")),
                      shape = ifelse(training$Class == "2", "2", "4"), size = 3)+
  geom_hline(yintercept = log(breast.lda$prior[1]/breast.lda$prior[2]))+
  xlab("Index")+ylab("Decision Value")

ggplot() + geom_point(aes(x=1:length(testing_predict$x), y = testing_predict$x),
                      color = ifelse(testing$Class!=testing$Pred, "black", ifelse(testing$Class == "2", "blue", "red")),
                      shape = ifelse(testing$Class == "2", "2", "4"), size = 3)+
  geom_hline(yintercept = log(breast.lda$prior[1]/breast.lda$prior[2]))+
  xlab("Index")+ylab("Decision Value")





