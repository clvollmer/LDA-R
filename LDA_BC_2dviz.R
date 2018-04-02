#Christopher Vollmer -1128484
#Professor Azencott - Machine Learning - Project #1-Breast Cancer
library(ggplot2)
library(gridExtra)


#drawparti graphs from klar. they only find the discriminating partition from two variables
#so I will see how well LDA splits the classes using just two variables.
library(klaR)
#for(){
drawparti(x=training$Thickness,y=training$Uni.Size, grouping=as.factor(training[,10]),method="lda", prec = 500,
          col.mean="magenta", image.colors =c("blue","red"), col.correct = "black", col.wrong = "white",
          xlab = "Clump Thickness", ylab = "Uniformity of Cell Size")

drawparti(x=training$Thickness,y=training$Uni.Shape, grouping=as.factor(training[,10]),method="lda", prec = 500,
          col.mean="magenta", image.colors =c("blue","red"), col.correct = "black", col.wrong = "white",
          xlab = "Clump Thickness", ylab = "Uniformity of Cell Shape")

drawparti(x=training$Thickness,y=training$Adhesion, grouping=as.factor(training[,10]),method="lda", prec = 500,
          col.mean="magenta", image.colors =c("blue","red"), col.correct = "black", col.wrong = "white",
          xlab = "Clump Thickness", ylab = "Marginal Adhesion")

drawparti(x=training$Uni.Size,y=training$Epithelial, grouping=as.factor(training[,10]),method="lda", prec = 500,
          col.mean="magenta", image.colors =c("blue","red"), col.correct = "black", col.wrong = "white",
          xlab = "Clump Thickness", ylab = "Single Epithelial Cell Size")

drawparti(x=training$Thickness,y=training$Nuclei, grouping=as.factor(training[,10]),method="lda", prec = 500,
          col.mean="magenta", image.colors =c("blue","red"), col.correct = "black", col.wrong = "white",
          xlab = "Clump Thickness", ylab = "Bare Nuclei")

drawparti(x=training$Thickness,y=training$Chromatin, grouping=as.factor(training[,10]),method="lda", prec = 500,
          col.mean="magenta", image.colors =c("blue","red"), col.correct = "black", col.wrong = "white",
          xlab = "Clump Thickness", ylab = "Bland Chromatin")

drawparti(x=training$Thickness,y=training$Nucleoli, grouping=as.factor(training[,10]),method="lda", prec = 500,
          col.mean="magenta", image.colors =c("blue","red"), col.correct = "black", col.wrong = "white",
          xlab = "Clump Thickness", ylab = "Normal Nucleoli")

drawparti(x=training$Thickness,y=training$Mitosis, grouping=as.factor(training[,10]),method="lda", prec = 500,
          col.mean="magenta", image.colors =c("blue","red"), col.correct = "black", col.wrong = "white",
          xlab = "Clump Thickness", ylab = "Mitosis")


###CELL SIZE VS.###
drawparti(x=training$Uni.Size,y=training$Uni.Shape, grouping=as.factor(training[,10]),method="lda", prec = 500,
          col.mean="magenta", image.colors =c("blue","red"), col.correct = "black", col.wrong = "white",
          xlab = "Uniformity of Cell Size", ylab = "Uniformity of Cell Shape")

drawparti(x=training$Uni.Size,y=training$Adhesion, grouping=as.factor(training[,10]),method="lda", prec = 500,
          col.mean="magenta", image.colors =c("blue","red"), col.correct = "black", col.wrong = "white",
          xlab = "Uniformity of Cell Size", ylab = "Marginal Adhesion")

drawparti(x=training$Uni.Size,y=training$Epithelial, grouping=as.factor(training[,10]),method="lda", prec = 500,
          col.mean="magenta", image.colors =c("blue","red"), col.correct = "black", col.wrong = "white",
          xlab = "Uniformity of Cell Size", ylab = "Single Epithelial Cell Size")

drawparti(x=training$Uni.Size,y=training$Nuclei, grouping=as.factor(training[,10]),method="lda", prec = 500,
          col.mean="magenta", image.colors =c("blue","red"), col.correct = "black", col.wrong = "white",
          xlab = "Uniformity of Cell Size", ylab = "Bare Nuclei")

drawparti(x=training$Uni.Size,y=training$Chromatin, grouping=as.factor(training[,10]),method="lda", prec = 500,
          col.mean="magenta", image.colors =c("blue","red"), col.correct = "black", col.wrong = "white",
          xlab = "Uniformity of Cell Size", ylab = "Bland Chromatin")

drawparti(x=training$Uni.Size,y=training$Nucleoli, grouping=as.factor(training[,10]),method="lda", prec = 500,
          col.mean="magenta", image.colors =c("blue","red"), col.correct = "black", col.wrong = "white",
          xlab = "Uniformity of Cell Size", ylab = "Normal Nucleoli")

drawparti(x=training$Uni.Size,y=training$Mitosis, grouping=as.factor(training[,10]),method="lda", prec = 500,
          col.mean="magenta", image.colors =c("blue","red"), col.correct = "black", col.wrong = "white",
          xlab = "Uniformity of Cell Size", ylab = "Mitosis")

###CELL SHAPE VS.###
drawparti(x=training$Uni.Shape,y=training$Adhesion, grouping=as.factor(training[,10]),method="lda", prec = 500,
          col.mean="magenta", image.colors =c("blue","red"), col.correct = "black", col.wrong = "white",
          xlab = "Uniformity of Cell Shape", ylab = "Marginal Adhesion")

drawparti(x=training$Uni.Shape,y=training$Epithelial, grouping=as.factor(training[,10]),method="lda", prec = 500,
          col.mean="magenta", image.colors =c("blue","red"), col.correct = "black", col.wrong = "white",
          xlab = "Uniformity of Cell Shape", ylab = "Single Epithelial Cell Size")

drawparti(x=training$Uni.Shape,y=training$Nuclei, grouping=as.factor(training[,10]),method="lda", prec = 500,
          col.mean="magenta", image.colors =c("blue","red"), col.correct = "black", col.wrong = "white",
          xlab = "Uniformity of Cell Shape", ylab = "Bare Nuclei")

drawparti(x=training$Uni.Shape,y=training$Chromatin, grouping=as.factor(training[,10]),method="lda", prec = 500,
          col.mean="magenta", image.colors =c("blue","red"), col.correct = "black", col.wrong = "white",
          xlab = "Uniformity of Cell Shape", ylab = "Bland Chromatin")

drawparti(x=training$Uni.Shape,y=training$Nucleoli, grouping=as.factor(training[,10]),method="lda", prec = 500,
          col.mean="magenta", image.colors =c("blue","red"), col.correct = "black", col.wrong = "white",
          xlab = "Uniformity of Cell Shape", ylab = "Normal Nucleoli")

drawparti(x=training$Uni.Shape,y=training$Mitosis, grouping=as.factor(training[,10]),method="lda", prec = 500,
          col.mean="magenta", image.colors =c("blue","red"), col.correct = "black", col.wrong = "white",
          xlab = "Uniformity of Cell Shape", ylab = "Mitosis")

###NUCLEI VS.###
drawparti(x=training$Nuclei,y=training$Adhesion, grouping=as.factor(training[,10]),method="lda", prec = 500,
          col.mean="magenta", image.colors =c("blue","red"), col.correct = "black", col.wrong = "white",
          xlab = "Bare Nuclei", ylab = "Marginal Adhesion")

drawparti(x=training$Nuclei,y=training$Epithelial, grouping=as.factor(training[,10]),method="lda", prec = 500,
          col.mean="magenta", image.colors =c("blue","red"), col.correct = "black", col.wrong = "white",
          xlab = "Bare Nuclei", ylab = "Single Epithelial Cell Size")

drawparti(x=training$Nuclei,y=training$Chromatin, grouping=as.factor(training[,10]),method="lda", prec = 500,
          col.mean="magenta", image.colors =c("blue","red"), col.correct = "black", col.wrong = "white",
          xlab = "Bare Nuclei", ylab = "Bland Chromatin")

drawparti(x=training$Nuclei,y=training$Nucleoli, grouping=as.factor(training[,10]),method="lda", prec = 500,
          col.mean="magenta", image.colors =c("blue","red"), col.correct = "black", col.wrong = "white",
          xlab = "Bare Nuclei", ylab = "Normal Nucleoli")

drawparti(x=training$Nuclei,y=training$Mitosis, grouping=as.factor(training[,10]),method="lda", prec = 500,
          col.mean="magenta", image.colors =c("blue","red"), col.correct = "black", col.wrong = "white",
          xlab = "Bare Nuclei", ylab = "Mitosis")
#}




###I had to make a special labeller values so that my facted graphs were more readable. 
###standarding my data in the first place is what generated senseless bins for original values
###The original values were all 1-10, now they are as below, so I reassigned them to 1-10

class_names <- as_labeller(c('2' = "Benign", '4' = "Malignant"))
# xx = as.character(levels(factor(training$Mitosis)))     ##used to more easily allow me to copy/paste long strings
# yy = as.character(c(1:10))                              ##of decimals, as below.
# xx = cbind(xx,yy)
# xx
thickness_names <- as_labeller(c("0.194023569023569" = "1", "0.388047138047138" = "2", "0.58207" = "3", 
                                 "0.77609" = "4", "0.973477998794455" = "5", "1.16817359855335" = "6",
                                 "1.36286919831224" = "7", "1.55756479807113" = "8", "1.75226039783002" = "9", 
                                 "1.94695599758891" = "10"))
size_names <- as_labeller(c("0.247583243823845" = "1", "0.495166487647691" = "2", "0.742749731471536" = "3", "0.990332975295381" = "4", 
                            "1.23791621911923" = "5", "1.48549946294307" = "6", "1.73308270676692" = "7", 
                            "1.98066595059076" = "8", "2.22824919441461" = "9", "2.47583243823845" = "10"))
shape_names <- as_labeller(c("0.244952178533475" = "1", "0.48990435706695" = "2", "0.734856535600425"= "3", "0.9798087141339"  = "4", 
                             "1.22476089266737" = "5", "1.46971307120085" = "6", "1.71466524973433" = "7", "1.9596174282678"  = "8", 
                             "2.20456960680128" = "9", "2.44952178533475" = "10"))
adhesion_names <- as_labeller(c("0.282129742962056" = "1", "0.564259485924113" = "2", "0.846389228886169" = "3", "1.12851897184823" =  "4", 
                                "1.41064871481028" =  "5", "1.69277845777234" = "6", "1.97490820073439" = "7", "2.25703794369645" = "8", 
                                "2.53916768665851" = "9", "2.82129742962056" = "10"))  
epithelial_names <- as_labeller(c("0.264790350373349" = "1", "0.529580700746697" = "2", "0.794371051120046" = "3", "1.05916140149339" = "4", 
                                  "1.32395175186674" = "5", "1.58874210224009" = "6", "1.85353245261344" = "7", "2.11832280298679" = "8", 
                                  "2.38311315336014" = "9", "2.64790350373349" = "10"))  
nuclei_names <- as_labeller(c("0.217247879359095" = "1", "0.43449575871819" = "2", "0.651743638077286" = "3", "0.868991517436381" = "4", 
                              "1.08623939679548" = "5", "1.30348727615457"  = "6", "1.52073515551367" = "7", "1.73798303487276" = "8", 
                              "1.95523091423186" = "9", "2.17247879359095" = "10"))  
chromatin_names <- as_labeller(c("0.243850833112933"= "1", "0.487701666225866" ="2", "0.731552499338799"= "3", "0.975403332451732" ="4", 
                                 "1.21925416556467" = "5", "1.4631049986776" = "6", "1.70695583179053" = "7", "1.95080666490346" = "8", 
                                 "2.1946574980164" = "9", "2.43850833112933" = "10"))  
nucleoli_names <- as_labeller(c("0.274404761904762"= "1", "0.548809523809524"= "2", "0.823214285714286"= "3", "1.09761904761905" = "4", 
                                "1.37202380952381" = "5", "1.64642857142857" = "6", "1.92083333333333" = "7", "2.19523809523809" = "8", 
                                "2.46964285714286" = "9", "2.74404761904762" = "10"))
mitosis_names <- as_labeller(c("0.536983110075713"= "1", "1.07396622015143" = "2", "1.61094933022714" = "3", "2.14793244030285" = "4", 
                               "2.68491555037857" = "5", "3.22189866045428" = "6", "3.75888177052999" = "7", "4.29586488060571" = "8", 
                               "5.36983110075713" = "10"))







##CELL SIZE###

##NO FACET##
ggplot(data = training, aes(x = Uni.Size, y = Nucleoli))+
  geom_jitter(color = ifelse(training$Class == 2, "blue", "red"),
              alpha = .3, size = 3, width = .05, height = .05, aes(x = Uni.Size, y = Nucleoli))+
  xlab("Uniformity of Cell Size") + ylab("Nucleoli") + ggtitle("Uniformity in Cell Size Vs. Nucleoli")+
  theme(axis.title = element_text(size = 12),panel.grid.major = element_line(color = "grey", linetype = 2),
        axis.ticks.x = element_blank(), axis.text.x = element_blank())


##FACETED##  
ggplot(data = training, aes(x = Uni.Size, y = Nucleoli))+
  geom_point(color = ifelse(training$Class == 2, "blue", "red"), alpha = .3, size = 3,
              aes(x = Uni.Size, y = Nucleoli))+
  ggtitle("Uniformity of Cell Size Vs. Nucleoli") +  facet_grid(Class~Uni.Size, labeller = labeller(Class = class_names, Uni.Size = size_names))+
  theme(axis.text.x = element_blank(), axis.ticks.x = element_blank(),
        panel.grid.minor.x = element_blank(), axis.title = element_text(size = 12), plot.title = element_text(size = 18))


###SINGLE EPITHELIAL CELL SIZE###

##NO FACET##
ggplot(data = training, aes(y = Nucleoli, x = Epithelial))+
  geom_jitter(color = ifelse(training$Class == 2, "blue", "red"),
              alpha = .3, size = 3, width = .1, height = .1, aes(y = Nucleoli, x = Epithelial))+
  ylab("Nucleoli") + xlab("Epithelial Cell Size") + ggtitle("Epithelial Cell Size Vs. Nucleoli")+
  theme(axis.title = element_text(size = 12),panel.grid.major = element_line(color = "grey", linetype = 2),
        axis.ticks.x = element_blank(), axis.text.x = element_blank())


##FACETED#  
ggplot(data = training, aes(y = Nucleoli, x = Epithelial))+
  geom_point(color = ifelse(training$Class == 2, "blue", "red"),alpha = .3, size = 3,
              aes(y = Nucleoli, x = Epithelial))+
  ggtitle("Epithelial Cell Size Vs. Nucleoli") + 
  facet_grid(Class~Epithelial, labeller = labeller(Class = class_names, Epithelial = epithelial_names))+
  theme(axis.text.x = element_blank(), axis.ticks.x = element_blank(),
        panel.grid.minor.x = element_blank(), axis.title = element_text(size = 12), plot.title = element_text(size = 18))

###BARE NUCLEI###

##NO FACET##
ggplot(data = training, aes(y = Nucleoli, x = Nuclei))+
  geom_jitter(color = ifelse(training$Class == 2, "blue", "red"),
              alpha = .3, size = 3, width = .1, height = .1, aes(y = Nucleoli, x = Nuclei))+
  ylab("Nucleoli") + xlab("Bare Nuclei") + ggtitle("Bare Nuclei Vs. Nucleoli")+
  theme(axis.title = element_text(size = 12),panel.grid.major = element_line(color = "grey", linetype = 2),
        axis.ticks.x = element_blank(), axis.text.x = element_blank())


##FACETED##  
ggplot(data = training, aes(y = Nucleoli, x = Nuclei))+
  geom_point(color = ifelse(training$Class == 2, "blue", "red"),alpha = .3, size = 3,
              aes(y = Nucleoli, x = Nuclei))+
  ggtitle("Bare Nuclei Vs. Nucleoli") + 
  facet_grid(Class~Nuclei, labeller = labeller(Class = class_names, Nuclei = nuclei_names))+
  theme(axis.text.x = element_blank(), axis.ticks.x = element_blank(),
        panel.grid.minor.x = element_blank(), axis.title = element_text(size = 12), plot.title = element_text(size = 18))

###NORMAL NUCLEOLI###

##NO FACET##
ggplot(data = training, aes(y = Nuclei, x = Nucleoli))+
  geom_jitter(color = ifelse(training$Class == 2, "blue", "red"),
              alpha = .3, size = 3, width = .1, height = .1, aes(y = Nuclei, x = Nucleoli))+
  ylab("Bare Nuclei") + xlab("Normal Nucleoli") + ggtitle("Normal Nucleoli Vs. Bare Nuclei")+
  theme(axis.title = element_text(size = 12),panel.grid.major = element_line(color = "grey", linetype = 2),
        axis.ticks.x = element_blank(), axis.text.x = element_blank())


##FACETED##  
ggplot(data = training, aes(y = Nuclei, x = Nucleoli))+
  geom_point(color = ifelse(training$Class == 2, "blue", "red"),alpha = .3, size = 3,
              aes(y = Nuclei, x = Nucleoli))+
  ggtitle("Bare Nuclei Vs. Normal Nucleoli") + 
  facet_grid(Class~Nucleoli, labeller = labeller(Class = class_names, Nucleoli = nucleoli_names))+
  theme(axis.text.x = element_blank(), axis.ticks.x = element_blank(),
        panel.grid.minor.x = element_blank(), axis.title = element_text(size = 12), plot.title = element_text(size = 18))






##DONT DELETE TIP: as.numeric(levels(x)) -> xx STORES THE LEVELS OF x INTO xx
#lockBinding('xx', .GlobalEnv)
#unlockBinding('xx', .GlobalEnv)