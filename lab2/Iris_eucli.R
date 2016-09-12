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



##eulidean distance under min_max normalization
dist_eulidean<-matrix(0,nrow=nrow(iris_test),ncol=nrow(iris))
for(i in 1:nrow(iris_test))
{
  for(j in 1:nrow(iris))
  {  
    dist_eulidean[i,j]<-sqrt((iris_test_norm[i,1]-iris_norm[j,1])^2+(iris_test_norm[i,2]-iris_norm[j,2])^2+(iris_test_norm[i,3]-iris_norm[j,3])^2+(iris_test_norm[i,4]-iris_norm[j,4])^2)
  }
}

#top 5 closest distance
#knn classification k=5
top<-matrix(0,nrow = nrow(dist_eulidean),ncol = 5)
index<-matrix(0,nrow = nrow(dist_eulidean),ncol = 5)
for(i in 1:nrow(dist_eulidean))
{
  top[i,]<-head(sort(dist_eulidean[i,]),5)
  index[i,]<-order(dist_eulidean[i,])[1:5]
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
  print(knn)
write.csv(knn,file="iris-euclidean-knn.csv")
# confusion metrix
confusion_matrix<-table(knn[,2],knn[,3])
print("confusion matrix")
print(confusion_matrix)




