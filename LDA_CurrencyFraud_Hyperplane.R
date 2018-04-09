#Christopher Vollmer -1128484
#Professor Azencott - Machine Learning - Project #1-BANKNOTES
#This script builds teh hyperplane and projects it down onto the chosen attributes called
#in ggplot


library(ggplot2)
###formula for building the hyperplane###
ldahyperplane <- function(sigmainv,mu1,mu2,prior1,prior2) {
  J <- 2 # number of classes                                                                                 
  b <- sigmainv%*%(mu1 - mu2)
  c <- -(1/2)*t(mu1 + mu2)%*%sigmainv%*%(mu1 - mu2) + log(prior1/prior2) 
  return(list(b=b,c=c))
}


A <- currlda$scaling #extract A matrix
sigmahatinv <- A%*%t(A) #calculate Sigma inverse
priorhat <- currlda$prior #prior probabilities             
muhat <- currlda$means #mu hat


###Calculate decision boundary between Variance and Skewness###                                  
sep.hyp <- ldahyperplane(sigmahatinv,muhat[1,],muhat[2,],
                             priorhat[1],priorhat[2])
###Line slope and intersect for easier graphing###

wrongsdf = training[training$Class!=training$Pred,] #locations of mis-classified objects


###Plot Variance vs. Skewness with the decision boundary.###
linearize <- function(sep.hyp) {
  return(list(beta0=-sep.hyp$c/sep.hyp$b[2],                                   
              beta1=-sep.hyp$b[1]/sep.hyp$b[2]))
}

line <- linearize(sep.hyp)
ggplot(data = training, aes(x = training$`Variance WTI`, y = training$`Skewness WTI`))+
  geom_point(data = wrongsdf, aes(x=`Variance WTI`, y =`Skewness WTI`), shape = 17, size = 5)+
  geom_point(color = ifelse(training$Class == 0, "blue", "red"))+
  geom_abline(intercept = line$beta0, slope = line$beta1)+
  xlab("Variance WTI") + ylab("Skewness WTI")+ ggtitle("Variance WTI versus Skewness WTI") + theme_bw()


###Plot Variance vs. Kurtosis with the decision boundary.###
linearize <- function(sep.hyp) {
  return(list(beta0=-sep.hyp$c/sep.hyp$b[3],                                   
              beta1=-sep.hyp$b[1]/sep.hyp$b[3]))
}

line <- linearize(sep.hyp)

ggplot(data = training, aes(x = training$`Variance WTI`, y = training$`Kurtosis WTI`))+
  geom_point(data = wrongsdf, aes(x=`Variance WTI`, y =`Kurtosis WTI`), shape = 17, size = 5)+
  geom_point(color = ifelse(training$Class == 0, "blue", "red"))+
  geom_abline(intercept = line$beta0, slope = line$beta1)+
  xlab("Variance WTI") + ylab("Kurtosis WTI")+ ggtitle("Variance WTI versus Kurtosis WTI") + theme_bw()


###Plot Variance vs. Entropy with the decision boundary.###
linearize <- function(sep.hyp) {
  return(list(beta0=-sep.hyp$c/sep.hyp$b[4],                                   
              beta1=-sep.hyp$b[1]/sep.hyp$b[4]))
}

line <- linearize(sep.hyp)

ggplot(data = training, aes(x = training$`Variance WTI`, y = training$`Entropy WTI`))+
  geom_point(data = wrongsdf, aes(x=`Variance WTI`, y =`Entropy WTI`), shape = 17, size = 5)+
  geom_point(color = ifelse(training$Class == 0, "blue", "red"))+
  geom_abline(intercept = line$beta0, slope = line$beta1)+
  xlab("Variance WTI") + ylab("Entropy WTI")+ ggtitle("Variance WTI versus Entropy WTI") + theme_bw()


###Plot Skewness vs. Kurtosis with the decision boundary.###
linearize <- function(sep.hyp) {
  return(list(beta0=-sep.hyp$c/sep.hyp$b[3],                                   
              beta1=-sep.hyp$b[2]/sep.hyp$b[3]))
}

line <- linearize(sep.hyp)

ggplot(data = training, aes(x = training$`Skewness WTI`, y = training$`Kurtosis WTI`))+
  geom_point(data = wrongsdf, aes(x=`Skewness WTI`, y =`Kurtosis WTI`), shape = 17, size = 5)+
  geom_point(color = ifelse(training$Class == 0, "blue", "red"))+
  geom_abline(intercept = line$beta0, slope = line$beta1)+
  xlab("Skewness WTI") + ylab("Kurtosis WTI")+ ggtitle("Skewness WTI versus Kurtosis WTI") + theme_bw()


###Plot Skewness vs. Entropy with the decision boundary.###
linearize <- function(sep.hyp) {
  return(list(beta0=-sep.hyp$c/sep.hyp$b[4],                                   
              beta1=-sep.hyp$b[2]/sep.hyp$b[4]))
}

line <- linearize(sep.hyp)
ggplot(data = training, aes(x = training$`Skewness WTI`, y = training$`Entropy WTI`))+
  geom_point(data = wrongsdf, aes(x=`Skewness WTI`, y =`Entropy WTI`), shape = 17, size = 5)+
  geom_point(color = ifelse(training$Class == 0, "blue", "red"))+
  geom_abline(intercept = line$beta0, slope = line$beta1)+
  xlab("Skewness WTI") + ylab("Entropy WTI")+ ggtitle("Skewness WTI versus Entropy WTI") + theme_bw()


###Plot Kurtosis vs. Entropy with the decision boundary.###
linearize <- function(sep.hyp) {
  return(list(beta0=-sep.hyp$c/sep.hyp$b[4],                                   
              beta1=-sep.hyp$b[3]/sep.hyp$b[4]))
}

line <- linearize(sep.hyp)
ggplot(data = training, aes(x = training$`Kurtosis WTI`, y = training$`Entropy WTI`))+
  geom_point(data = wrongsdf, aes(x=`Kurtosis WTI`, y =`Entropy WTI`), shape = 17, size = 5)+
  geom_point(color = ifelse(training$Class == 0, "blue", "red"))+
  geom_abline(intercept = line$beta0, slope = line$beta1)+
  xlab("Kurtosis WTI") + ylab("Entropy WTI")+ ggtitle("Kurtosis WTI versus Entropy WTI") + theme_bw()
