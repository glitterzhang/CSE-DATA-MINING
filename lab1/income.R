income<-read.csv('./income_NEW.CSV')
income1<-income[,-15]

##delete the object that contains missing data
#inc[inc[]==" ?"]<-NA
income1<-income1[income1[,2]!=" ?",]
income1<-income1[income1[,7]!=" ?",]
income1<-income1[income1[,14]!=" ?",]

##sperate into 2 data frame
#numberic
income2<-data.frame(income1[,1],income1[,3],income1[,5],income1[,11],income1[,12],income1[,13])

#min-max normalization 
min_max <- function(x) {
    return ((x - min(x)) / (max(x) - min(x)))
}

i<-1
while(i<=ncol(income2)){
  income2[,i]<-min_max(income2[,i])
  i<-i+1
}
#euclidean distance
income_euclidean<-matrix(0,nrow=nrow(income2),ncol=nrow(income2))
for(i in 1:nrow(income2))
{
  for(j in 1:nrow(income2))
  {  
    D<-0
      for(k in 1:ncol(income2))
    {
      
        d<-(income2[j,k]-income2[i,k])^2
        D<-D+d
    }
    income_euclidean[i,j]<-sqrt(D)
  }
}
#use functiont to calculate the distance
dist_e<-function(x)
{
  dist(x,method="euclidean",diag=TRUE,upper=TRUE)
}
inc2_d1<-as.matrix(dist_e(income2))

##manhattan distance
income_manhattan<-matrix(0,nrow=nrow(income2),ncol=nrow(income2))
for(i in 1:nrow(income2))
{
  for(j in 1:nrow(income2))
  {
      for(k in 1:ncol(income2))
    {
        d<-abs(income2[j,k]-income2[i,k])
      income_manhattan[i,j]<-income_manhattan[i,j]+d
    }
  }
}

#distance function
dist_m<-function(x){
  dist(x,method="manhattan",diag=TRUE,upper=TRUE)
}
inc2_d2<-as.matrix(dist_m(income2))

##nominal
#because the information of educaiton attribute and education_cat are same ,
#so i dont cover the education column in income3 dataframe
income3<-data.frame(income1[,2],income1[,6],income1[,7],income1[,8],income1[,9],income1[,10],income1[14])

#convert nominal into numeric
for(i in 1:7)
{
 income3[,i]<-as.numeric(income3[,i]) 
}
#distance of dataframe income3
distance_char<-matrix(0,nrow=nrow(income3),ncol=nrow(income3))
for(i in 1:nrow(income3))
{
  for(j in 1:nrow(income3))
  {
      for(k in 1:ncol(income3))
    {
        if(abs(income3[j,k]-income3[i,k])>0)
          {
          d<-1}
        else
        {
          d<-0
        }
       distance_char[i,j]<-distance_char[i,j]+d
      }
  }
}

#euclidean distance for total rows
distance_euc<-inc2_d1+distance_char

#top 5 closest distance
income_top_m<-matrix(0,nrow = nrow(distance_euc),ncol = 6)
for(i in 1:nrow(distance_euc))
{
income_top_e[i,]<-head(sort(distance_euc[,i]),6)
}
income_top_e=income_top_e[,-1]

#index
index_inc<-matrix(0,nrow = nrow(distance_euc),ncol = 5)
for(i in 1:nrow((distance_euc)))
{
  for(j in 1:5)
  {
    index_inc[i,j]<-head(which(distance_euc[i,]==income_top_e[i,j]),1)
  }
}

distance_income<-matrix(0,nrow=nrow(income_top_e),ncol = 11)
distance_income[,1]<-c(1:nrow(income_top_e))
for(i in 1:5)
{
  distance_income[,2*i]=index_inc[,i]
  distance_income[,2*i+1]=income_top_e[,i]
}
distance_income<-data.frame(distance_income)
colnames(distance_income)<-c("Transiation_ID","1st","1st_dist","2nd","2nd_dist","3rd","3rd_dist","4th","4th_dist","5th","5th_dist")

##manhattan distance for total rows
distance_m<-inc2_d2+distance_char

income_top_m<-matrix(0,nrow = nrow(distance_m),ncol = 6)
for(i in 1:nrow(distance_m))
{
  income_top_m[i,]<-head(sort(distance_m[,i]),6)
}
income_top_m=income_top_m[,-1]

#index
index_inc2<-matrix(0,nrow = nrow(distance_m),ncol = 5)
for(i in 1:nrow((distance_m)))
{
  for(j in 1:5)
  {
    index_inc2[i,j]<-head(which(distance_m[i,]==income_top_m[i,j]),1)
  }
}

distance_income2<-matrix(0,nrow=nrow(income_top_m),ncol = 11)
distance_income2[,1]<-c(1:nrow(income_top_m))
for(i in 1:5)
{
  distance_income2[,2*i]=index_inc2[,i]
  distance_income2[,2*i]=income_top_m[,i]
}
distance_income2<-data.frame(distance_income2)
colnames(distance_income2)<-c("Transiation_ID","1st","1st_dist","2nd","2nd_dist","3rd","3rd_dist","4th","4th_dist","5th","5th_dist")


# outlier analysis
#outlier anlysis
x<-matrix(0,nrow=nrow(distance_euc),ncol=1)
x<-apply(distance_euc,1,sum)
sort(x,decreasing = TRUE)
hist(x,xlab = "sumdistance",ylab="Number of observations",main="Euclidean distance")

y<-matrix(0,nrow=distance_m,ncol=1)
y<-apply(distance_m,2, sum)
sort(y,decreasing = TRUE)
hist(y,xlab = "sumdistance",ylab="Number of observations",main="Manhattan distance")

#output analysis

ggplot(data=income,aes(x=capital_gain,y=education_cat))+geom_point()+ggtitle("The relationship bewteen education_cat and capital_gain")



