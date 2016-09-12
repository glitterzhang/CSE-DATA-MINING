Iris<-read.csv('./Iris.csv')
Iris_test<-read.csv('./Iris_Test.csv')
# remove the class attribute 
iris=Iris[,-5]
iris_test=Iris_test[,-5]

# attribute transformation
#min-max normalization 
min_max <- function(x) {
  return ((x - min(x)) / (max(x) - min(x)))
}
iris_norm=iris
i<-1
while(i<=4){
  iris_norm[,i]<-min_max(iris_norm[,i])
  
  i<-i+1
}
iris_test_norm=iris_test
i<-1
while(i<=4){
  iris_test_norm[,i]<-min_max(iris_test_norm[,i])
  i<-i+1
}


##Manhattan distance under min_max normalization
dist_manh<-matrix(0,nrow=nrow(iris_test),ncol=nrow(iris))
for(i in 1:nrow(iris_test_norm))
{
  for(j in 1:nrow(iris))
  {
    for(k in 1:ncol(iris_test))
    {
      d<-abs(iris_test_norm[i,k]-iris_norm[j,k])
      dist_manh[i,j]<-dist_manh[i,j]+d
    }
  }
}

#top 5 closest distance
top<-matrix(0,nrow = nrow(dist_manh),ncol =10)
index<-matrix(0,nrow = nrow(dist_manh),ncol = 10)
for(i in 1:nrow(dist_manh))
{
  top[i,]<-head(sort(dist_manh[i,]),10)
  index[i,]<-order(dist_manh[i,])[1:10]
}

for(i in 1:nrow(iris_test))
{
  
  if(sum(Iris[index[i,],5]=="Iris-setosa")>=max(sum(Iris[index[i,],5]=="Iris-versicolor"),sum(Iris[index[i,],5]=="Iris-virginica")))
  { 
    iris_test[i,5]="Iris-setosa"
    iris_test[i,6]=sum(Iris[index[i,],5]=="Iris-setosa")/5
  }
  else if(sum(Iris[index[i,],5]=="Iris-versicolor")>=max(sum(Iris[index[i,],5]=="Iris-setosa"),sum(Iris[index[i,],5]=="Iris-virginica")))
  {   
    iris_test[i,5] ="Iris-versicolor" 
    iris_test[i,6]=sum(Iris[index[i,],5]=="Iris-versicolor")/5
  }
  else if(sum(Iris[index[i,],5]=="Iris-virginica")>=max(sum(Iris[index[i,],5]=="Iris-setosa"),sum(Iris[index[i,],5]=="Iris-versicolor")))
  {
    iris_test[i,5] ="Iris-virginica" 
    iris_test[i,6]=sum(Iris[index[i,],5]=="Iris-virginica")/5   
  }
}
##knn classification
knn<-data.frame(TransactionID=1:nrow(iris_test),Actual_class=Iris_test[,5],Predicted_class=iris_test[,5],Posterior_Probability=iris_test[,6])
write.csv(knn,file="iris-manhattan-knn.csv")
# confusion metrix
confusion_matrix<-table(knn[,2],knn[,3])
confusion_matrix
