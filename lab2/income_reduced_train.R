income_train<-read.csv('C:/Users/ab/Desktop/RStudio/income_TRAIN_FINAL.CSV')
income_test<-read.csv('C:/Users/ab/Desktop/RStudio/income_TEST_FINAL.CSV')
##delete the object that contains missing data

income_train<-income_train[income_train[,2]!=" ?",]
income_train<-income_train[income_train[,7]!=" ?",]
income_train<-income_train[income_train[,14]!=" ?",]
income_test<-income_test[income_test[,2]!=" ?",]
income_test<-income_test[income_test[,7]!=" ?",]
income_test<-income_test[income_test[,14]!=" ?",]



#min-max normalization 
income_train1<-income_train[1:244,]
income_test1<-income_test
min_max <- function(x) {
  return ((x - min(x)) / (max(x) - min(x)))
}
income_train1[,1]<-min_max(income_train1[,1])
income_train1[,3]<-min_max(income_train1[,3])
income_train1[,5]<-min_max(income_train1[,5])
income_train1[,11]<-min_max(income_train1[,11])
income_train1[,12]<-min_max(income_train1[,12])
income_train1[,13]<-min_max(income_train1[,13])
income_test1[,1]<-min_max(income_test1[,1])
income_test1[,3]<-min_max(income_test1[,3])
income_test1[,5]<-min_max(income_test1[,5])
income_test1[,11]<-min_max(income_test1[,11])
income_test1[,12]<-min_max(income_test1[,12])
income_test1[,13]<-min_max(income_test1[,13])
income_train1<-income_train1[,-4]
income_test1<-income_test1[,-4]

#Distance_euclidean distance for numeric
income_euclidean<-matrix(0,nrow=nrow(income_train1),ncol=nrow(income_test1))
for(i in 1:nrow(income_train1))
{
  for(j in 1:nrow(income_test1))
  {  
    income_euclidean[i,j]<-sqrt((income_train1[i,1]-income_test1[j,1])^2+(income_train1[i,2]!=income_test1[j,2])^2+
                                  (income_train1[i,3]-income_test1[j,3])^2+(income_train1[i,4]-income_test1[j,4])^2+                                
                                  (as.character(income_train1[i,5])!=as.character(income_test1[j,5]))^2+(as.character(income_train1[i,6])!=as.character(income_test1[j,6]))^2+
                                  (as.character(income_train1[i,7])!=as.character(income_test1[j,7]))^2+ (as.character(income_train1[i,8])!=as.character(income_test1[j,8]))^2+
                                  (as.character(income_train1[i,9])!=as.character(income_test1[j,9]))^2+ (income_train1[i,10]-income_test1[j,10])^2+
                                  (income_train1[i,11]-income_test1[j,11])^2+(income_train1[i,12]-income_test1[j,12])^2+
                                  (as.character(income_train1[i,13])!=as.character(income_test1[j,13]))^2)
    
  }
}


##knn classification
top_in<-matrix(0,nrow = ncol(income_euclidean),ncol = 5)
index_in<-matrix(0,nrow = ncol(income_euclidean),ncol =5 )
for(i in 1:ncol(income_euclidean))
{
  top_in[i,]<-head(sort(income_euclidean[,i]),5)
  index_in[i,]<-order(income_euclidean[,i])[1:5]
}


for(i in 1:nrow(income_test))
{
  max<-max(sum(income_train1[index_in[i,],14]==" <=50K"),sum(income_train1[index_in[i,],14]==" >50K"))
  if(max==sum(income_train1[index_in[i,],14]==" <=50K"))
    income_test1[i,14]=" <=50K"
  else if(max==sum(income_train1[index_in[i,],14]==" >50K"))
    income_test1[i,14]=" >50K"
}

for(i in 1:nrow(income_test))
{
  income_test1[i,15]=sum(income_train1[index_in[i,],14]==" <=50K")/5
  
}
income_knn<-data.frame(TransactionID=1:nrow(income_test),Actual_class=income_test[,15],Predicted_class=income_test1[,14],Posterior_Probability=income_test1[,15])
income_confusion_metrix<-as.matrix(table(income_knn[,2],income_knn[,3]))
income_confusion_metrix