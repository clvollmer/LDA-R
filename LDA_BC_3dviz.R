#Christopher Vollmer -1128484
#Professor Azencott - Machine Learning - Project #1-breast cancer


library(rgl)


###the below plots were changed according to what i wanted to plot
###didn't need to copy paste

rgl.bbox(color = "#333377",specular="#3333FF")
plot3d(training$Nucleoli, training$Uni.Size, training$Epithelial, type = "s", 
       xlab="Nucleoli", ylab="Uniformity Cell Size",zlab="Epithelial", col = ifelse(training$Class == "2", "blue", "red"), size = 1)



### 3d hyperplane -- it's broken here. worked before, double check documentation and fix!###
rgl.bbox(color = "#333377",specular="#3333FF")
plot3d(training$Nuclei, training$Nucleoli, training$Epithelial,
       col = ifelse(training$Class!=training$Pred, "white", ifelse(training$Class == "2", "blue", "red")),
       type = "s", size = 1, xlab="Bare Nuclei", ylab="Nucleoli",zlab="Epithelial")


planes3d(a= sep.hyp$b[6], b=sep.hyp$b[8], c= sep.hyp$b[5], d = sep.hyp$c, add = TRUE, alpha = .75, color = "red")
shapelist3d(icosahedron3d(), x=training_cond_mean[,2],y=training_cond_mean[,3], z=training_cond_mean[,4],
            size = .25, add = TRUE, col = "orange")

