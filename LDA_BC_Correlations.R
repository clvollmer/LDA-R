#Christopher Vollmer -1128484
#Professor Azencott - Machine Learning - Project #1- Breast Cancer

###I wanted to see correlations between variables. I made  nicer way to output it.

mosthighlycorrelated <- function(mydataframe,numtoreport)
{
  # find the correlations
  cormatrix <- cor(mydataframe)
  # set the correlations on the diagonal and lower triangle to zero,
  # so they will not be reported as the highest ones and won't be double counted:
  diag(cormatrix) <- 0
  cormatrix[lower.tri(cormatrix)] <- 0
  fm <- as.data.frame(as.table(cormatrix))  # flatten the matrix into a dataframe for easy sorting
  names(fm) <- c("First.Variable", "Second.Variable","Correlation")
  head(fm[order(abs(fm$Correlation),decreasing=T),],n=numtoreport)# sort and print the top n correlations
}

# I have removed Uni.Shape Attribute since it's so closely corellated to Uni.Size
breast.corr1 <- mosthighlycorrelated(training[training$Class == 2,c(-10,-11,-3)], 10)
breast.corr2 <- mosthighlycorrelated(training[training$Class == 4,c(-10,-11,-3)], 10)