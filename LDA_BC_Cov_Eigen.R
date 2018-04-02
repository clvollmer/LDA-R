#Christopher Vollmer -1128484
#Professor Azencott - Machine Learning - Project #2
#computes the covariance Matrix for entire set of data, Breast Cancer

library(ggplot2)
library(kernlab)
library(gridExtra) # for grid.arrange

###Performs  t-test on the attributes###
ttest = data.frame()
for (a in 1:9)
{
  ttest[1,a]=t.test(training[,a]~training[,10], conf.level = .99)
}
colnames(ttest) <- c("Thickness", "Uni.Size", "Uni.Shape", "Adhesion", "Epithelial",
                     "Nuclei","Chromatin","Nucleoli","Mitosis")
rownames(ttest) <- ("T-Test")
View(ttest)


###makes covariance matrix for attributes
cov_matrix <- data.frame()
cov_matrix <- cov(training[,1:9])

names(cov_matrix) <- c("Thickness", "Uni.Size", "Uni.Shape", "Adhesion", "Epithelial",
                       "Nuclei","Chromatin","Nucleoli","Mitosis")
row.names(cov_matrix) <- t(c("Thickness", "Uni.Size", "Uni.Shape", "Adhesion", "Epithelial",
                             "Nuclei","Chromatin","Nucleoli","Mitosis")) #remember this easy way to name symmetric matrices
training_eigen <- eigen(cov_matrix, symmetric = TRUE, only.values = TRUE)
training_eigen <- data.frame(training_eigen[1])
colnames(training_eigen) <- c('Eigenvalues')


#-------find the eigenvalue number that adds the last bit of information from the covariance matrix
#-------needed to explain 95% of the behavior
run_sum = data.frame(`Eigenvalue` = training_eigen[,1],`Running Sum` = 0, Threshold = character(9),stringsAsFactors=FALSE)
count = 0

for(i in 1:9)
{
  run_sum[i,2] = training_eigen[i,1] + count
  count = run_sum[i,2]
  run_sum[i,3] = paste(round(100*count/sum(training_eigen), 4),'%')
}


y=data.frame(1:9)
train_gauss_eigen = cbind(y,training_eigen,run_sum[1:9,2])

q=ggplot(data = train_gauss_eigen,aes(x=train_gauss_eigen[,1], y=train_gauss_eigen[,2])) + geom_point(size = 3)+ 
  xlab('Eigenvalue: From Largest to Smallest') +  ylab("Eigenvalue Magnitude") + ggtitle("Eigenvalue magnitude from largest to smallest")+
  geom_vline(aes(xintercept = train_gauss_eigen[,1], linetype = 2), color = 'red', size = .5, linetype = 2)+
  geom_text(x=train_gauss_eigen[1,1], y=train_gauss_eigen[1,2], label = run_sum[1,3],
            hjust = -.1, vjust = .5, size = 4, color = 'red')+
  geom_text(x=train_gauss_eigen[2,1], y=train_gauss_eigen[1,2]-train_gauss_eigen[2,2], label = run_sum[2,3],
            hjust = -.1, vjust = .5, size = 4, color = 'red')+
  geom_text(x=train_gauss_eigen[3,1], y=train_gauss_eigen[1,2]-train_gauss_eigen[3,2], label = run_sum[3,3],
            hjust = -.1, vjust = .5, size = 4, color = 'red')+
  geom_text(x=train_gauss_eigen[4,1], y=train_gauss_eigen[1,2]-train_gauss_eigen[4,2], label = run_sum[4,3],
            hjust = -.1, vjust = .5, size = 4, color = 'red')+
  geom_text(x=train_gauss_eigen[5,1], y=train_gauss_eigen[1,2]-train_gauss_eigen[5,2], label = run_sum[5,3],
            hjust = -.1, vjust = .5, size = 4, color = 'red')+
  geom_text(x=train_gauss_eigen[6,1], y=train_gauss_eigen[1,2]-train_gauss_eigen[6,2], label = run_sum[6,3],#6
            hjust = -.1, vjust = 2, size = 4, color = 'red')+
  geom_text(x=train_gauss_eigen[7,1], y=train_gauss_eigen[1,2]-train_gauss_eigen[7,2], label = run_sum[7,3],
            hjust = -.1, vjust = 3, size = 4, color = 'red')+
  geom_text(x=train_gauss_eigen[8,1], y=train_gauss_eigen[1,2]-train_gauss_eigen[8,2], label = run_sum[8,3],
            hjust = -.1, vjust = 4, size = 4, color = 'red')+
  geom_text(x=train_gauss_eigen[9,1], y=train_gauss_eigen[1,2]-train_gauss_eigen[9,2], label = run_sum[9,3],
            hjust = -.05, vjust = 6, size = 4, color = 'red')
  

qq=ggplot(train_gauss_eigen) + geom_line(aes(train_gauss_eigen[,1], train_gauss_eigen[,3]), size = 1) + 
  xlab('Running Count: Largest to Smallest') +  ylab("Running Sum") + ggtitle("Explanation Threshold")+
  geom_vline(aes(xintercept = 7, linetype = 2), color = 'red', size = .5, linetype = 2)+
  geom_text(x=3, y=train_gauss_eigen[7,3], label = paste(run_sum[7,3], " Threshold Met"),
            hjust =-2.5, vjust = 2, size = 4, color = 'red')

grid.arrange(q, qq, ncol = 1)###can use library(gridExtra) to call grid.arrange. for when don't want faceted from ggplot
###but do want stacked or side-by-side grids














###!!!In Azencott's recommendations for analsysi, he asked to look at the eigenvalues 
###of the gramian. I have since found that the covariance matrix can be synomonous
### with gramian in LDA context. The below code was not used but is kept for possible 
### future need.




#-------Create Gauss Kernal, find eigenvalues, compute a running sum, 
#-------find the eigenvalue number that adds the last bit of information 
#-------needed to explain 95% of the behavior given pre-specified, allowable deviation.

# eigen_threshold = matrix(nrow=5, ncol=1)
# 
# train_eigen_sum = num_training#THIS CANNOT BE CORRECT. FIX!! But seems to be!
# 
# #SIGMA = 0.1
# 
# rbf <- rbfdot(sigma = .1)
# train_gauss = kernelMatrix(rbf, as.matrix(training[,1:9]))
# 
# train_gauss_eigen = eigen(train_gauss, symmetric = TRUE, only.values = TRUE)
# train_gauss_eigen = data.frame(train_gauss_eigen[1])
# 
# run_sum = matrix(nrow = num_training, ncol = 1)
# count = 0
# run_sum[,1] = 0
# 
# for(i in 1:num_training)
# {
#   run_sum[i,1] = train_gauss_eigen[i,1] + count
#   count = run_sum[i,1]
#   if(count/train_eigen_sum <= .95)
#   {
#     eigen_threshold[1,1] = i
#   }
# }
# 
# y=data.frame(1:num_training)
# train_gauss_eigen = cbind(y,train_gauss_eigen,run_sum)
# 
# ggplot(train_gauss_eigen) + geom_point(aes(train_gauss_eigen$X1.num_training, train_gauss_eigen$values), size = 1) + 
#   xlab('Eigenvalue: From Largest to Smallest') +  ylab("Eigenvalue Magnitude") + ggtitle("Eigenvalue magnitude from largest to smallest with SIGMA = 0.1")+
#   geom_vline(xintercept = eigen_threshold[1,1], size = 1, linetype = 2, color = 'red', alpha = .5)+
#   geom_text(x=eigen_threshold[1,1], y=train_gauss_eigen[eigen_threshold[1],2], label = eigen_threshold[1,1], hjust = -.5,vjust=-1.5, size = 7, color = 'red')
# 
# ggplot(train_gauss_eigen) + geom_line(aes(train_gauss_eigen$X1.num_training, train_gauss_eigen$run_sum), size = 1) + 
#   xlab('Running Count: Largest to Smallest') +  ylab("Running Sum") + ggtitle("Explanation Threshold with SIGMA = 0.1")+
#   geom_vline(xintercept = eigen_threshold[1,1], size = 1, linetype = 2, color = 'red', alpha = .5)+
#   geom_text(x=eigen_threshold[1,1], y=count, label = eigen_threshold[1,1], hjust = -.5, vjust = 2.5, size = 7, color = 'red')
# 
# #SIGMA = 0.5
# rbf <- rbfdot(sigma = .5)
# train_gauss = kernelMatrix(rbf, as.matrix(training[,1:9]))
# 
# train_gauss_eigen = eigen(train_gauss, symmetric = TRUE, only.values = TRUE)
# train_gauss_eigen = data.frame(train_gauss_eigen[1])
# 
# run_sum = matrix(nrow = num_training, ncol = 1)
# count = 0
# run_sum[,1] = 0
# 
# for(i in 1:num_training)
# {
#   run_sum[i,1] = train_gauss_eigen[i,1] + count
#   count = run_sum[i,1]
#   if(count/train_eigen_sum <= .95)
#   {eigen_threshold[2,1] = i}
# }
# y=data.frame(1:num_training)
# train_gauss_eigen = cbind(y,train_gauss_eigen, run_sum)
# 
# 
# ggplot(train_gauss_eigen) + geom_point(aes(train_gauss_eigen$X1.num_training, train_gauss_eigen$values), size = 1) + 
#   xlab('Eigenvalue: From Largest to Smallest') +  ylab("Eigenvalue Magnitude") + ggtitle("Eigenvalue magnitude from largest to smallest with SIGMA = 0.5")+
#   geom_vline(xintercept = eigen_threshold[2,1], size = 1, linetype = 2, color = 'red')+
#   geom_text(x=eigen_threshold[2,1], y=train_gauss_eigen[eigen_threshold[2],2],label = eigen_threshold[2,1], hjust = 1.5, size = 7, color = 'red')
# 
# ggplot(train_gauss_eigen) + geom_line(aes(train_gauss_eigen$X1.num_training, train_gauss_eigen$run_sum), size = 1) + 
#   xlab('Running Count: Largest to Smallest') +  ylab("Running Sum") + ggtitle("Explanation Threshold with SIGMA = 0.5")+
#   geom_vline(xintercept = eigen_threshold[2,1], size = 1, linetype = 2, color = 'red')+
#   geom_text(x=eigen_threshold[2,1], y=count,label = eigen_threshold[2,1], hjust = 1.5, size = 7, color = 'red')
# 
# #SIGMA = 1.0
# rbf <- rbfdot(sigma = 1)
# train_gauss = kernelMatrix(rbf, as.matrix(training[,1:9]))
# 
# train_gauss_eigen = eigen(train_gauss, symmetric = TRUE, only.values = TRUE)
# train_gauss_eigen = data.frame(train_gauss_eigen[1])
# 
# 
# run_sum = matrix(nrow = num_training, ncol = 1)
# count = 0
# run_sum[,1] = 0
# 
# for(i in 1:num_training)
# {
#   run_sum[i,1] = train_gauss_eigen[i,1] + count
#   count = run_sum[i,1]
#   if(count/train_eigen_sum <= .95)
#   {eigen_threshold[3,1] = i}
# }
# y=data.frame(1:num_training)
# train_gauss_eigen = cbind(y,train_gauss_eigen, run_sum)
# 
# ggplot(train_gauss_eigen) + geom_point(aes(train_gauss_eigen$X1.num_training, train_gauss_eigen$values), size = 1) + 
#   xlab('Eigenvalue: From Largest to Smallest') +  ylab("Eigenvalue Magnitude") + ggtitle("Eigenvalue magnitude from largest to smallest with SIGMA = 1")+
#   geom_vline(xintercept = eigen_threshold[3,1], size = 1, linetype = 2, color = 'red', alpha = .5)+
#   geom_text(x=eigen_threshold[3,1], y=train_gauss_eigen[eigen_threshold[3],2],label = eigen_threshold[3,1], hjust = -.5, vjust = -2, size = 7, color = 'red')
# 
# ggplot(train_gauss_eigen) + geom_line(aes(train_gauss_eigen$X1.num_training, train_gauss_eigen$run_sum), size = 1) + 
#   xlab('Running Count: Largest to Smallest') +  ylab("Running Sum") + ggtitle("Explanation Threshold with SIGMA = 1.0")+
#   geom_vline(xintercept = eigen_threshold[3,1], size = 1, linetype = 2, color = 'red', alpha = .5)+
#   geom_text(x=eigen_threshold[3,1], y=count,label = eigen_threshold[3,1], hjust = 1.5, size = 7, color = 'red')
# 
# 
# #SIGMA = 2.0
# rbf <- rbfdot(sigma = 2)
# train_gauss = kernelMatrix(rbf, as.matrix(training[,1:9]))
# 
# train_gauss_eigen = eigen(train_gauss, symmetric = TRUE, only.values = TRUE)
# train_gauss_eigen = data.frame(train_gauss_eigen[1])
# 
# run_sum = matrix(nrow = num_training, ncol = 1)
# count = 0
# run_sum[,1] = 0
# 
# for(i in 1:num_training)
# {
#   run_sum[i,1] = train_gauss_eigen[i,1] + count
#   count = run_sum[i,1]
#   if(count/train_eigen_sum <= .95)
#   {eigen_threshold[4,1] = i}
# }
# y=data.frame(1:num_training)
# train_gauss_eigen = cbind(y,train_gauss_eigen, run_sum)
# 
# ggplot(train_gauss_eigen) + geom_point(aes(train_gauss_eigen$X1.num_training, train_gauss_eigen$values), size = 1) + 
#   xlab('Eigenvalue: From Largest to Smallest') +  ylab("Eigenvalue Magnitude") + ggtitle("Eigenvalue magnitude from largest to smallest with SIGMA = 2")+
#   geom_vline(xintercept = eigen_threshold[4,1], size = 1, linetype = 2, color = 'red', alpha = .5)+
#   geom_text(x=eigen_threshold[4,1], y=train_gauss_eigen[eigen_threshold[4],2],label = eigen_threshold[4,1], hjust = -.5, vjust = -2, size = 7, color = 'red')
# 
# 
# ggplot(train_gauss_eigen) + geom_line(aes(train_gauss_eigen$X1.num_training, train_gauss_eigen$run_sum), size = 1) + 
#   xlab('Running Count: Largest to Smallest') +  ylab("Running Sum") + ggtitle("Explanation Threshold with SIGMA = 2")+
#   geom_vline(xintercept = eigen_threshold[4,1], size = 1, linetype = 2, color = 'red', alpha = .5)+
#   geom_text(x=eigen_threshold[4,1], y=count,label = eigen_threshold[4,1], hjust = 1.5, size = 7, color = 'red')
# 
# 
# #SIGMA = 5.0
# rbf <- rbfdot(sigma = 5)
# train_gauss = kernelMatrix(rbf, as.matrix(training[,1:9]))
# 
# train_gauss_eigen = eigen(train_gauss, symmetric = TRUE, only.values = TRUE)
# train_gauss_eigen = data.frame(train_gauss_eigen[1])
# 
# run_sum = matrix(nrow = num_training, ncol = 1)
# count = 0
# run_sum[,1] = 0
# 
# for(i in 1:num_training)
# {
#   run_sum[i,1] = train_gauss_eigen[i,1] + count
#   count = run_sum[i,1]
#   if(count/train_eigen_sum <= .95)
#   {eigen_threshold[5,1] = i}
# }
# y=data.frame(1:num_training)
# train_gauss_eigen = cbind(y,train_gauss_eigen, run_sum)
# 
# ggplot(train_gauss_eigen) + geom_point(aes(train_gauss_eigen$X1.num_training, train_gauss_eigen$values), size = 1) + 
#   xlab('Eigenvalue: From Largest to Smallest') +  ylab("Eigenvalue Magnitude") + ggtitle("Eigenvalue magnitude from largest to smallest with SIGMA = 5")+
#   geom_vline(xintercept = eigen_threshold[5,1], size = 1, linetype = 2, color = 'red', alpha = .5)+
#   geom_text(x=eigen_threshold[5,1], y=train_gauss_eigen[eigen_threshold[5],2],label = eigen_threshold[5,1], hjust = -.5, vjust = -2, size = 7, color = 'red')
# 
# ggplot(train_gauss_eigen) + geom_line(aes(train_gauss_eigen$X1.num_training, train_gauss_eigen$run_sum), size = 1) + 
#   xlab('Running Count: Largest to Smallest') +  ylab("Running Sum") + ggtitle("Explanation Threshold with SIGMA = 5")+
#   geom_vline(xintercept = eigen_threshold[5,1], size = 1, linetype = 2, color = 'red', alpha = .5)+
#   geom_text(x=eigen_threshold[5,1], y=count,label = eigen_threshold[5,1], hjust = 1.5, size = 7, color = 'red')






