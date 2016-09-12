setwd("./lab2")
income_train<-read.csv('./income_NEW.csv')
income_test<-read.csv('./income_TEST_FINAL.csv')

##delete the object that contains missing data
income_train<-income_train[income_train[,2]!=" ?",]
income_train<-income_train[income_train[,7]!=" ?",]
income_train<-income_train[income_train[,14]!=" ?",]
income_test<-income_test[income_test[,2]!=" ?",]
income_test<-income_test[income_test[,7]!=" ?",]
income_test<-income_test[income_test[,14]!=" ?",]

#min-max normalization 
income_train1<-income_train
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
## education and education_cat display duplicate information. So,we can remove education column
  income_train1<-income_train1[,-4]
  income_test1<-income_test1[,-4]

#Distance_euclidean distance for numeric
income_euclidean<-matrix(0,nrow=nrow(income_test1),ncol=nrow(income_train1))
for(j in 1:nrow(income_test1))
{
  for(i in 1:nrow(income_train1))
      {  
  income_euclidean[j,i]<-sqrt((income_test1[j,1]-income_train1[i,1])^2+(as.character(income_train1[i,2])!=as.character(income_test1[j,2]))^2+
                                (income_test1[j,3]-income_train1[i,3])^2+(income_test1[j,4]-income_train1[i,4])^2+                                
                                (as.character(income_train1[i,5])!=as.character(income_test1[j,5]))^2+(as.character(income_train1[i,6])!=as.character(income_test1[j,6]))^2+
                                (as.character(income_train1[i,7])!=as.character(income_test1[j,7]))^2+ (as.character(income_train1[i,8])!=as.character(income_test1[j,8]))^2+
                                (as.character(income_train1[i,9])!=as.character(income_test1[j,9]))^2+ (income_test1[j,10]-income_train1[i,10])^2+
                                (income_test1[j,11]-income_train1[i,11])^2+(income_test1[j,12]-income_train1[i,12])^2+
                                (as.character(income_train1[i,13])!=as.character(income_test1[j,13]))^2)
                               
    }
}

##knn classification
k<-5
top_in<-matrix(0,nrow = nrow(income_euclidean),ncol = k)
index_in<-matrix(0,nrow = nrow(income_euclidean),ncol =k)
for(i in 1:nrow(income_euclidean))
{
  top_in[i,]<-head(sort(income_euclidean[i,]),k)
  index_in[i,]<-order(income_euclidean[i,])[1:k]
}

for(i in 1:nrow(income_test))
{
  max<-max(sum(income_train1[index_in[i,],14]==" <=50K"),sum(income_train1[index_in[i,],14]==" >50K"))
  if(max==sum(income_train1[index_in[i,],14]==" <=50K"))
    {
    income_test1[i,14]=" <=50K"
   income_test1[i,15]=sum(income_train1[index_in[i,],14]==" <=50K")/5
    income_test1[i,16]=sum(income_train1[index_in[i,],14]==" <=50K")/5
  }
    else if(max==sum(income_train1[index_in[i,],14]==" >50K"))
    {
      income_test1[i,14]=" >50K"
   income_test1[i,15]=sum(income_train1[index_in[i,],14]==" >50K")/5
   income_test1[i,16]=sum(income_train1[index_in[i,],14]==" <=50K")/5
}
   }

income_knn<-data.frame(TransactionID=1:nrow(income_test),Actual_class=income_test[,15],Predicted_class=income_test1[,14],Posterior_Probability=income_test1[,15],Poster_probility2=income_test1[,16])
write.csv(income_knn,file="income_knn.csv")
income_confusion_metrix<-as.matrix(table(income_knn[,2],income_knn[,3]))
income_confusion_metrix
(income_confusion_metrix[1,2]+income_confusion_metrix[2,1])/267


Measures<-data.frame(TPR=income_confusion_metrix[1,1]/sum(income_confusion_metrix[1,]),TNR=income_confusion_metrix[2,2]/sum(income_confusion_metrix[2,]),
                    FPR=income_confusion_metrix[2,1]/sum(income_confusion_metrix[2,]),FNR=income_confusion_metrix[1,2]/sum(income_confusion_metrix[1,]),
                    Recall_=income_confusion_metrix[1,1]/sum(income_confusion_metrix[1,]),Precision=income_confusion_metrix[1,1]/sum(income_confusion_metrix[,1]),
                    F_measure=(2*income_confusion_metrix[1,1])/((2*income_confusion_metrix[1,1])+income_confusion_metrix[1,2]+income_confusion_metrix[2,1]))
print(Measures)

###### ROC curv
income_knn1<-income_knn
roc<-matrix(0,nrow=6,ncol = 2)
new.function<-function(a,b){
for(i in 1:nrow(income_knn1))
{
  if (income_knn1[i,5]>=a)
    income_knn1[i,6]=" <=50K"
  else income_knn1[i,6]=" >50K"
}  
 ## return(income_knn1[,6])
  frequen<-as.matrix(table(income_knn1[,2],income_knn1[,6]))
  roc[b,1]=frequen[1,1]/sum(frequen[1,])
  roc[b,2]=frequen[2,1]/sum(frequen[2,])
  return(roc[b,])
}
 
roc[1,]<-new.function(1,1)
roc[2,]<-new.function(0.8,2)
roc[3,]<-new.function(0.6,3)
roc[4,]<-new.function(0.4,4)
roc[5,]<-new.function(0.2,5)
roc[6,]<-new.function(0,6)
print(roc[])
roc<-data.frame(TPR=roc[,1],FPR=roc[,2])
write.csv(roc,file="roc_curve.csv")
install.packages(ggplot2)
library(ggplot2)
ggplot(data=roc,aes(x=roc[,2],y=roc[,1]))+xlab("False Positive rate")+ylab("True positive rate")+geom_line()+ggtitle("ROC curve")                                                                                           
