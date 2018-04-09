library(plot3D)
library(rgl)

###output and lists the values and their frequency in the vector inputed###
a <- rle(sort(wrongsdf$`Variance WTI`))
length(a$lengths)
#-----We see that there are 13 values since some objects are duplicates. 

##line used for hyperplane. functions written out in hyperplane.r
linearize <- function(sep.hyp) {
  return(list(beta0=-sep.hyp$c/sep.hyp$b[2],
              beta1=-sep.hyp$b[1]/sep.hyp$b[2]))
}
line <- linearize(sep.hyp)


###below is 3d graphing with hyperplane, remove planes3d for just attributes in 3d.

###Variance vs Skewness vs. Kurtosis###
rgl.bbox(color = "#333377",specular="#3333FF")
plot3d(training$`Variance WTI`, training$`Skewness WTI`, training$`Kurtosis WTI`,
       col = ifelse(training$Class!=training$Pred, "white", ifelse(training$Class == "0", "blue", "red")),
       type = "s", size = 1, xlab="Variance", ylab="Skewness",zlab="Kurtosis")
planes3d(a= sep.hyp$b[1], b=sep.hyp$b[2], c= sep.hyp$b[3], d = sep.hyp$c, add = TRUE, alpha = .75, color = "grey")
shapelist3d(icosahedron3d(), x=training_cond_mean[,2],y=training_cond_mean[,3], z=training_cond_mean[,4],
         size = .25, add = TRUE, col = "orange")


###Variance vs Skewness vs. Entropy###
rgl.bbox(color = "#333377",specular="#3333FF")
plot3d(training$`Variance WTI`, training$`Skewness WTI`, training$`Entropy WTI`,
       col = ifelse(training$Class!=training$Pred, "white", ifelse(training$Class == "0", "blue", "red")),
       type = "s", size = 1, xlab="Variance", ylab="Skewness",zlab="Entropy")
planes3d(a= sep.hyp$b[1], b=sep.hyp$b[2], c= sep.hyp$b[4], d = sep.hyp$c, add = TRUE, alpha = .75, color = "grey")
shapelist3d(icosahedron3d(), x=training_cond_mean[,2],y=training_cond_mean[,3], z=training_cond_mean[,5],
            size = .25, add = TRUE, col = "orange")



###Variance vs Kurtosis vs. Entropy###
rgl.bbox(color = "#333377",specular="#3333FF")
plot3d(training$`Variance WTI`, training$`Kurtosis WTI`, training$`Entropy WTI`,
       col = ifelse(training$Class!=training$Pred, "white", ifelse(training$Class == "0", "blue", "red")),
       type = "s", size = 1, xlab="Variance", ylab="Kurtosis",zlab="Entropy")
planes3d(a= sep.hyp$b[1], b=sep.hyp$b[3], c= sep.hyp$b[4], d = sep.hyp$c, add = TRUE, alpha = .75, color = "grey")
shapelist3d(icosahedron3d(), x=training_cond_mean[,2],y=training_cond_mean[,4], z=training_cond_mean[,5],
            size = .25, add = TRUE, col = "orange")


###Skewness vs Kurtosis vs. Entropy###
rgl.bbox(color = "#333377",specular="#3333FF")
plot3d(training$`Skewness WTI`, training$`Kurtosis WTI`, training$`Entropy WTI`,
       col = ifelse(training$Class!=training$Pred, "white", ifelse(training$Class == "0", "blue", "red")),
       type = "s", size = 1, xlab="Skewness", ylab="Kurtosis",zlab="Entropy")
planes3d(a= sep.hyp$b[2], b=sep.hyp$b[3], c= sep.hyp$b[4], d = sep.hyp$c, add = TRUE, alpha = .75, color = "grey")
shapelist3d(icosahedron3d(), x=training_cond_mean[,3],y=training_cond_mean[,4], z=training_cond_mean[,5],
            size = .25, add = TRUE, col = "orange")




