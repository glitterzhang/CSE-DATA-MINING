twodimeasy<- read.csv('C:/Users/ab/Desktop/A.easy.csv')
twodimhard<- read.csv('C:/Users/ab/Desktop/A.hard.csv')
twodimeasy1<-twodimeasy[,2:3]
twodimhard2<-twodimhard[,1:2]
# K-Means Cluster Analysis
fit.easy<- kmeans(twodimeasy1, 2) # 5 cluster solution
fit.easy
fit.hard<- kmeans(twodimeasy1, 2) # 5 cluster solution
# get cluster means 
aggregate(twodimeasy1,by=list(fit$cluster),FUN=mean)
# append cluster assignment
mydata <- data.frame(twodimeasy1, fit$cluster)
