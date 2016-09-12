Iris<-read.csv('./Iris.csv')
# remove the class attribute 
iris=Iris[,-5]
#remove duplicated rows
which(duplicated(iris)==TRUE)
iris=iris[!duplicated(iris),]

# attribute transformation
#min-max normaization 
min_max <- function(x) {
  return ((x - min(x)) / (max(x) - min(x)))
}
iris1=iris
for(i in 1:4)
{
  iris1[,i]<-min_max(iris1[,i])

}

#standardization;z-score nomalization
stand<-function(x)
{
  return(scale(x,center=TRUE,scale=TRUE))
}
iris2=iris
for(i in 1:4)
  {
  iris2[,i]<-stand(iris2[,i])
}

##eulidean distance under min_max normalization
dist_eulidean<-matrix(0,nrow=nrow(iris),ncol=nrow(iris))
for(i in 1:nrow(iris1))
{
  for(j in 1:nrow(iris1))
  {  
    dist_eulidean[i,j]<-sqrt((iris1[i,1]-iris1[j,1])^2+(iris1[i,2]-iris1[j,2])^2+
                               (iris1[i,3]-iris1[j,3])^2+(iris1[i,4]-iris1[j,4])^2)
  }
}

#top 5 closest distance
top<-matrix(0,nrow = nrow(dist_eulidean),ncol = 6)
for(i in 1:nrow(dist_eulidean))
{
  top[i,]<-head(sort(dist_eulidean[i,]),6)
}
top5<-top[,-1]

#index
index<-matrix(0,nrow = nrow(dist_eulidean),ncol = 5)
for(i in 1:nrow((dist_eulidean)))
{
  for(j in 1:5)
  {
  index[i,j]<-head(which(dist_eulidean[i,]==top5[i,j]),1)
  }
}
#The dataframe of 5 closest distance.
distance_eu<-matrix(0,nrow=nrow(top5),ncol = 11)
distance_eu[,1]<-c(1:nrow(top5))
for(i in 1:5)
{
distance_eu[,2*i]=index[,i]
distance_eu[,2*i+1]=top5[,i]
}
distance_eu<-data.frame(distance_eu)
colnames(distance_eu)<-c("Transiation_ID","1st","1st_dist","2nd","2nd_dist","3rd","3rd_dist","4th","4th_dist","5th","5th_dist")
write.csv(distance_eu,"euclideandistance.csv")
##Manhattan distance under min_max normalization
dist_manh<-matrix(0,nrow=nrow(iris),ncol=nrow(iris))
for(i in 1:nrow(iris1))
{
  for(j in 1:nrow(iris1))
  {
    for(k in 1:ncol(iris1))
    {
      d<-abs(iris1[j,k]-iris1[i,k])
      dist_manh[i,j]<-dist_manh[i,j]+d
    }
  }
}
#top 5 closest distance
top<-matrix(0,nrow = nrow(dist_manh),ncol = 6)
for(i in 1:nrow(dist_manh))
{
  top[i,]<-head(sort(dist_manh[,i]),6)
}
top5=top[,-1]

#index
index<-matrix(0,nrow = nrow(dist_manh),ncol = 5)
for(i in 1:nrow((dist_manh)))
{
  for(j in 1:5)
  {
    index[i,j]<-head(which(dist_manh[i,]==top5[i,j]),1)
  }
}

distance_manh<-matrix(0,nrow=nrow(top5),ncol = 11)
distance_manh[,1]<-c(1:nrow(top5))
for(i in 1:5)
{
  distance_manh[,2*i]=index[,i]
  distance_manh[,2*i+1]=top5[,i]
}
distance_manh<-data.frame(distance_manh)

colnames(distance_manh)<-c("Transiation_ID","1st","1st_dist","2nd","2nd_dist","3rd","3rd_dist","4th","4th_dist","5th","5th_dist")
write.csv(distance_manh,"manhattandistance.csv")
# compare two different distance histogram
library(ggplot2)
hist(dist_eulidean,xlab="Euclidean distance",ylab="Number of observation",main="Euclidean distance distribution")
hist(dist_manh,xlab = "Manhattan distance",ylab="number of oberseravtion",main="Manhattan distance distribution")

#outlier anlysis
x<-matrix(0,nrow=147,ncol=1)
x<-apply(dist_eulidean,1,sum)
sort_eu<-sort(x,decreasing = TRUE)
hist(x,xlab = "sumdistance",ylab="Number of observations",main="Euclidean distance")
     

y<-matrix(0,nrow=147,ncol=1)
y<-apply(dist_manh,2, sum)
sort_man<-sort(y,decreasing = TRUE)
hist(y,xlab = "sumdistance",ylab="Number of observations",main="Manhattan distance")

#outlier analysis
library(mvoutlier)
aq.plot(iris, delta=qchisq(0.975, df=ncol(iris)), quan=1/2, alpha=0.001)
as.numeric(Iris[,5])

#visualization
library(ggplot2)
plot(iris[c("sepal_length","sepal_width")],col=Iris$class)
plot(iris[c("petal_length","petal_width")],col=Iris$class)
pairs(Iris)
#boxplot
ggplot(Iris,aes(Iris[,5],sepal_length))+geom_boxplot(outlier.colour = "red")
ggplot(Iris,aes(Iris[,5],sepal_width))+geom_boxplot(outlier.colour = "red")
ggplot(Iris,aes(Iris[,5],petal_length))+geom_boxplot(outlier.colour = "red")
ggplot(Iris,aes(Iris[,5],petal_width))+geom_boxplot(outlier.colour = "red")
