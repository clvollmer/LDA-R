#Christopher Vollmer -1128484
#Professor Azencott - Machine Learning - Project #1- Breast Cancer
#This SCRIPT generates histograms of each attribute given the Class in the Training Set 

#-------------------------------------------------------------------------------#   
library(ggplot2)

out_two <- training[training$Class == 2,]
out_four <- training[training$Class == 4,]

#out_temp <- training[(training$Class == 4 & training$V11 == 2),]


###THICKNESS###
DF <- rbind(data.frame(fill="Given Class 2", obs=out_two$Thickness),
            data.frame(fill="Given Class 4", obs=out_four$Thickness))

tempdf <- data.frame(fill=levels(DF$fill), mean=training_cond_mean$Thickness)

ggplot(DF, aes(x = obs, fill = fill )) + geom_histogram(aes(y=..density..),bins =10, color = "black", position = "stack")+
  geom_vline(aes(xintercept=mean, color = "Mean"),data = tempdf, size = 1, linetype = 2, show.legend = TRUE)+
  geom_density(aes(fill = factor(DF$fill)), color ="green", alpha = .3, size = 2)+
  xlab('Thickness given Class') +  ylab("Density")+ ggtitle("Class Conditional Histogram of Thickness for Training Set")+
  geom_text(data = tempdf, aes(x = mean, y =Inf, label = round(tempdf$mean, digits = 4)),
            vjust = 1, hjust = -.5,
            color = 'orange', size = 5, fontface = "bold")+
  scale_fill_manual(name="Thickness given Class",values = c('blue', 'red'))+
  scale_color_manual(name="Mean of Thickness",values=c(Mean = 'orange'))


###UNIFORMITY OF CELL SIZE###
DF <- rbind(data.frame(fill="Given Class 2", obs=out_two$Uni.Size),
            data.frame(fill="Given Class 4", obs=out_four$Uni.Size))

tempdf <- data.frame(fill=levels(DF$fill), mean=training_cond_mean$Uni.Size)

ggplot(DF, aes(x = obs, fill = fill )) + geom_histogram(aes(y=..density..),bins = 10, color = "black")+
  geom_vline(aes(xintercept=mean, color = "Mean"),data = tempdf, size = 1, linetype = 2, show.legend = TRUE)+
  geom_density(aes(fill = factor(DF$fill)),color = "green", alpha = .3, size = 2)+
  xlab('Uniformity of Cell Size given Class') +  ylab("Density")+ ggtitle("Class Conditional Histogram of Uniformity of Cell Size for Training Set")+
  geom_text(data = tempdf, aes(x = mean, y =Inf, label = round(tempdf$mean, digits = 4)),
            vjust = 1, hjust = -.5,
            color = 'orange', size = 5, fontface = "bold")+
  scale_fill_manual(name="Uniformity of Cell Size given Class",values = c('blue', 'red'))+
  scale_color_manual(name="Mean of Uniformity of Cell Size",values=c(Mean = 'orange'))+
  facet_grid(.~fill)

###UNIFORMITY OF CELL SHAPE###

DF <- rbind(data.frame(fill="Given Class 2", obs=out_two$Uni.Shape),
            data.frame(fill="Given Class 4", obs=out_four$Uni.Shape))

tempdf <- data.frame(fill=levels(DF$fill), mean=training_cond_mean$Uni.Shape)

ggplot(DF, aes(x = obs, fill = fill )) + geom_histogram(aes(y=..density..),bins = 10, color = "black", position = "stack")+
  geom_vline(aes(xintercept=mean, color = "Mean"),data = tempdf, size = 1, linetype = 2, show.legend = TRUE)+
  geom_density(aes(fill = factor(DF$fill)),color = "green", alpha = .3, size = 2)+ 
  xlab('Uniformity of Cell Shape given Class') +  ylab("Density")+ ggtitle("Class Cond. Histogram of Uniformity of Cell Shape for Training Set")+
  geom_text(data = tempdf, aes(x = mean, y =Inf, label = round(tempdf$mean, digits = 4)),
            vjust = 1, hjust = -.5,
            color = 'orange', size = 5, fontface = "bold")+
  scale_fill_manual(name="Uniformity of Cell Shape given Class",values = c('blue', 'red'))+
  scale_color_manual(name="Mean of Uniformity of Cell Shape",values=c(Mean = 'orange'))

###MARGINAL ADHESION###

DF <- rbind(data.frame(fill="Given Class 2", obs=out_two$Adhesion),
            data.frame(fill="Given Class 4", obs=out_four$Adhesion))

tempdf <- data.frame(fill=levels(DF$fill), mean=training_cond_mean$Adhesion)

ggplot(DF, aes(x = obs, fill = fill )) + geom_histogram(aes(y=..density..),bins = 10, color = "black", position = "stack")+
  geom_vline(aes(xintercept=mean, color = "Mean"),data = tempdf, size = 1, linetype = 2, show.legend = TRUE)+
  geom_density(aes(fill = factor(DF$fill)),color = "green", alpha = .3, size = 2)+ 
  xlab('Adhesion given Class') +  ylab("Density")+ ggtitle("Class Conditional Histogram of Adhesion for Training Set")+
  geom_text(data = tempdf, aes(x = mean, y =Inf, label = round(tempdf$mean, digits = 4)),
            vjust = 1, hjust = -.5,
            color = 'orange', size = 5, fontface = "bold")+
  scale_fill_manual(name="Adhesion given Class",values = c('blue', 'red'))+
  scale_color_manual(name="Mean of Adhesion",values=c(Mean = 'orange'))

###SINGLE EPITHELIAL CELL SIZE###

DF <- rbind(data.frame(fill="Given Class 2", obs=out_two$Epithelial),
            data.frame(fill="Given Class 4", obs=out_four$Epithelial))

tempdf <- data.frame(fill=levels(DF$fill), mean=training_cond_mean$Epithelial)

ggplot(DF, aes(x = obs, fill = fill )) + geom_histogram(aes(y=..density..),bins = 10, color = "black", position = "stack")+
  geom_vline(aes(xintercept=mean, color = "Mean"),data = tempdf, size = 1, linetype = 2, show.legend = TRUE)+
  geom_density(aes(fill = factor(DF$fill)),color = "green", alpha = .3, size = 2)+ 
  xlab('Epithelial given Class') +  ylab("Density")+ ggtitle("Class Conditional Histogram of Epithelial for Training Set")+
  geom_text(data = tempdf, aes(x = mean, y =Inf, label = round(tempdf$mean, digits = 4)),
            vjust = 1, hjust = -.5,
            color = 'orange', size = 5, fontface = "bold")+
  scale_fill_manual(name="Epithelial given Class",values = c('blue', 'red'))+
  scale_color_manual(name="Mean of Epithelial",values=c(Mean = 'orange'))

###BARE NUCLEI###

DF <- rbind(data.frame(fill="Given Class 2", obs=out_two$Nuclei),
            data.frame(fill="Given Class 4", obs=out_four$Nuclei))

tempdf <- data.frame(fill=levels(DF$fill), mean=training_cond_mean$Nuclei)

ggplot(DF, aes(x = obs, fill = fill )) + geom_histogram(aes(y=..density..),bins = 10, color = "black", position = "stack")+
  geom_vline(aes(xintercept=mean, color = "Mean"),data = tempdf, size = 1, linetype = 2, show.legend = TRUE)+
  geom_density(aes(fill = factor(DF$fill)),color = "green", alpha = .3, size = 2)+ 
  xlab('Nuclei given Class') +  ylab("Density")+ ggtitle("Class Conditional Histogram of Nuclei for Training Set")+
  geom_text(data = tempdf, aes(x = mean, y =Inf, label = round(tempdf$mean, digits = 4)),
            vjust = 1, hjust = -.5,
            color = 'orange', size = 5, fontface = "bold")+
  scale_fill_manual(name="Nuclei given Class",values = c('blue', 'red'))+
  scale_color_manual(name="Mean of Nuclei",values=c(Mean = 'orange'))

###BLAND CHROMATIN###

DF <- rbind(data.frame(fill="Given Class 2", obs=out_two$Chromatin),
            data.frame(fill="Given Class 4", obs=out_four$Chromatin))

tempdf <- data.frame(fill=levels(DF$fill), mean=training_cond_mean$Chromatin)

ggplot(DF, aes(x = obs, fill = fill )) + geom_histogram(aes(y=..density..),bins = 10, color = "black", position = "stack")+
  geom_vline(aes(xintercept=mean, color = "Mean"),data = tempdf, size = 1, linetype = 2, show.legend = TRUE)+
  geom_density(aes(fill = factor(DF$fill)),color = "green", alpha = .3, size = 2)+ 
  xlab('Chromatin given Class') +  ylab("Density")+ ggtitle("Class Conditional Histogram of Chromatin for Training Set")+
  geom_text(data = tempdf, aes(x = mean, y =Inf, label = round(tempdf$mean, digits = 4)),
            vjust = 1, hjust = -.5,
            color = 'orange', size = 5, fontface = "bold")+
  scale_fill_manual(name="Chromatin given Class",values = c('blue', 'red'))+
  scale_color_manual(name="Mean of Chromatin",values=c(Mean = 'orange'))

###NORMAL NUCLEOLI###

DF <- rbind(data.frame(fill="Given Class 2", obs=out_two$Nucleoli),
            data.frame(fill="Given Class 4", obs=out_four$Nucleoli))

tempdf <- data.frame(fill=levels(DF$fill), mean=training_cond_mean$Nucleoli)

ggplot(DF, aes(x = obs, fill = fill )) + geom_histogram(aes(y=..density..),bins = 10, color = "black", position = "stack")+
  geom_vline(aes(xintercept=mean, color = "Mean"),data = tempdf, size = 1, linetype = 2, show.legend = TRUE)+
  geom_density(aes(fill = factor(DF$fill)),color = "green", alpha = .3, size = 2)+ 
  xlab('Nucleoli given Class') +  ylab("Density")+ ggtitle("Class Conditional Histogram of Nucleoli for Training Set")+
  geom_text(data = tempdf, aes(x = mean, y =Inf, label = round(tempdf$mean, digits = 4)),
            vjust = 1, hjust = -.5,
            color = 'orange', size = 5, fontface = "bold")+
  scale_fill_manual(name="Nucleoli given Class",values = c('blue', 'red'))+
  scale_color_manual(name="Mean of Nucleoli",values=c(Mean = 'orange'))

###MITOSES###

DF <- rbind(data.frame(fill="Given Class 2", obs=out_two$Mitosis),
            data.frame(fill="Given Class 4", obs=out_four$Mitosis))

tempdf <- data.frame(fill=levels(DF$fill), mean=training_cond_mean$Mitosis)

ggplot(DF, aes(x = obs, fill = fill )) + geom_histogram(aes(y=..density..),bins = 10, color = "black", position = "stack")+
  geom_vline(aes(xintercept=mean, color = "Mean"),data = tempdf, size = 1, linetype = 2, show.legend = TRUE)+
  geom_density(aes(fill = factor(DF$fill)),color = "green", alpha = .3, size = 2)+ 
  xlab('Mitosis given Class') +  ylab("Density")+ ggtitle("Class Conditional Histogram of Mitosis for Training Set")+
  geom_text(data = tempdf, aes(x = mean, y =Inf, label = round(tempdf$mean, digits = 4)),
            vjust = 1, hjust = -.5,
            color = 'orange', size = 5, fontface = "bold")+
  scale_fill_manual(name="Mitosis given Class",values = c('blue', 'red'))+
  scale_color_manual(name="Mean of Mitosis",values=c(Mean = 'orange'))