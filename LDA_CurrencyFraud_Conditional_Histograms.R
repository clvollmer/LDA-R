#Christopher Vollmer -1128484
#Professor Azencott - Machine Learning - Project #1-Banknotes
#This SCRIPT generates histograms of each attribute given the Class in the Training Set 

#-------------------------------------------------------------------------------#   
library(ggplot2)

out_zero <- training[training$Class == 0,] #matrix with only class 0
out_one <- training[training$Class == 1,] #matrix with only class 1


DF <- rbind(data.frame(fill="Given Class 0", obs=out_zero[,1]),
            data.frame(fill="Given Class 1", obs=out_one[,1]))

tempdf <- data.frame(fill=levels(DF$fill), mean=training_cond_mean$`Variance WTI`)

ggplot(DF, aes(x = obs, fill = fill )) + geom_histogram(aes(y=..density..),binwidth = .5, color = "black")+
  geom_vline(aes(xintercept=mean, color = "Mean"),data = tempdf, size = 1, linetype = 2, show.legend = TRUE)+
  geom_density(color = "green", alpha = .5, size = 2)+
  xlab('Variance WTI given Class') +  ylab("Density")+ ggtitle("Class Conditional Histogram of Variance WTI for Training Set")+
  geom_text(data = tempdf, aes(x = mean, y =Inf, label = round(tempdf$mean, digits = 4)),
            vjust = 1, hjust = -.5, color = 'orange', size = 5, fontface = "bold")+
  scale_fill_manual(name="Variance WTI given Class",values = c('blue', 'red'))+
  scale_color_manual(name="Mean of Variance WTI",values=c(Mean = 'orange'))+
  facet_grid(.~fill)

##NOTE, TO GET THE VERTICAL LINES TO APPEAR ON BOTH GRAPHS, I HAD TO CREATE
##ANOTHER DATA FRAME WITH THE VALUES FOR THE LINES. BUT!! I HAD TO INCLUDE A COLUMN
##THAT HAD THE LEVELS AT WHICH I AM FACETING. SO, IN THIS CASE, I AM FACETING BY 'fill'
##SO I HAD TO MAKE A DATA FRAME WITH THE TYPES OF 'fill' I WAS FACETING AND THEIR
##CORRESPONDING MEAN VALUES.


#-------------------------------------------------------------------------------#     
    
DF <- rbind(data.frame(fill="Given Class 0", obs=out_zero[,2]),
                data.frame(fill="Given Class 1", obs=out_one[,2]))

tempdf <- data.frame(fill=levels(DF$fill), mean=training_cond_mean$`Skewness WTI`)
    
    
ggplot(DF, aes(x = obs, fill = fill )) +geom_histogram(aes(y=..density..),binwidth = .5, color = "black")+
  geom_vline(aes(xintercept=mean, color = "Mean"),data = tempdf, size = 1, linetype = 2, show.legend = TRUE)+
  geom_density(color = "green", alpha = .5, size = 2)+
  xlab('Skewness WTI given Class') +  ylab("Density") + ggtitle("Class Conditional Histogram of Skewness WTI for Training Set")+
  geom_text(data = tempdf, aes(x = mean, y =Inf, label = round(tempdf$mean, digits = 4)),
            vjust = 1, hjust = -.5, color = 'orange', size = 5, fontface = "bold")+
  scale_fill_manual(name="Skewness WTI given Class",values = c('blue', 'red'))+
  scale_color_manual(name="Mean of Skewness WTI",values=c(Mean = 'orange'))+
  facet_grid(.~fill)
#-------------------------------------------------------------------------------#    

DF <- rbind(data.frame(fill="Given Class 0", obs=out_zero[,3]),
            data.frame(fill="Given Class 1", obs=out_one[,3]))
tempdf <- data.frame(fill=levels(DF$fill), mean=training_cond_mean$`Kurtosis WTI`)
    
    
ggplot(DF, aes(x = obs, fill = fill )) +geom_histogram(aes(y=..density..),binwidth = .5, color = "black")+
  geom_density(color = "green", alpha = .5, size = 2)+
  geom_vline(aes(xintercept=mean, color = "Mean"),data = tempdf, size = 1, linetype = 2, show.legend = TRUE)+
  xlab('Kurtosis WTI given Class') +  ylab("Density") + ggtitle("Class Conditional Histogram of Kurtosis WTI for Training Set")+
  geom_text(data = tempdf, aes(x = mean, y =Inf, label = round(tempdf$mean, digits = 2)),
            vjust = 1, hjust = -.5*tempdf$mean, color = 'orange', size = 4, fontface = "bold")+  
  scale_fill_manual(name="Kurtosis WTI given Class",values = c('blue', 'red'))+
  scale_color_manual(name="Mean of Kurtosis WTI",values=c(Mean = 'orange'))+
  facet_grid(.~fill)
#-------------------------------------------------------------------------------#    
   
DF <- rbind(data.frame(fill="Given Class 0", obs=out_zero[,4]),
            data.frame(fill="Given Class 1", obs=out_one[,4]))
tempdf <- data.frame(fill=levels(DF$fill), mean=training_cond_mean$`Entropy WTI`)
    
ggplot(DF, aes(x = obs, fill = fill )) +geom_histogram(aes(y=..density..),binwidth = .5, color = "black", position = "stack")+
  geom_density(color = "green", alpha = .5, size = 2)+
  geom_vline(aes(xintercept=mean, color = "Mean"),data = tempdf, size = 1, linetype = 2, show.legend = TRUE)+
  xlab('Entropy WTI given Class') +  ylab("Density") + ggtitle("Class Conditional Histogram of Entropy WTI for Training Set")+
  geom_text(data = tempdf, aes(x = mean[1], y =Inf, label = paste('-0.6787 and -0.7088')),
            vjust = 1, hjust = 1.1, color = 'orange', size = 5, fontface = "bold")+
  scale_fill_manual(name="Entropy WTI given Class",values = c('blue', 'red'))+
  scale_color_manual(name="Mean of Entropy WTI",values=c(Mean = 'orange'))
#-------------------------------------------------------------------------------#
    

#to stack them for a different comparison, add ',position = "stack"' into geom_histogram AND remove facet_grid.
#out_zero <- training[(training$Class == 0 & training$class == 1),] SWEET, THAT'S HOW YOU DO IT.
#^^ example of how to state mutliple conditions when defining new array or data.frame, etc.