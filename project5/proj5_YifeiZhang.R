#set work directory
setwd("C:/Users/ab/Desktop/project5")

twodimeasy<- read.csv('A.easy.csv')
twodimhard<- read.csv('A.hard.csv')
wine<-read.csv("wine.csv")

install.packages("ggplot2")
library(ggplot2)
install.packages("SpatialTools")
library(SpatialTools)
install.packages("gridExtra")
library("gridExtra")
install.packages("cowplot")
library("cowplot")
p<-ggplot(data=twodimeasy,aes(x=X.1,y=X.2,group=cluster,color=cluster))+geom_point()+ggtitle("true cluster")
p
q<-ggplot(data=twodimhard,aes(x=X.1,y=X.2,color=id))+geom_point()+ggtitle("true cluster")
q
#Twodimeasy 
#centroid for cluster1
centroid_easy<-matrix(0,nrow = 2,ncol=3)
centroid_easy[1,1]<-mean(subset(twodimeasy,cluster==1)$X.1)
centroid_easy[1,2]<-mean(subset(twodimeasy,cluster==1)$X.2)
#centriod for cluster2
centroid_easy[2,1]<-mean(subset(twodimeasy,cluster==2)$X.1)
centroid_easy[2,2]<-mean(subset(twodimeasy,cluster==2)$X.2)

##SSE funtion
SSE<-function(df,cluster1,i,j,k,centroid){
  SSE=sum((subset(df,cluster==cluster1)$X.1-centroid[i,j])^2+(subset(df,cluster==cluster1)$X.2-centroid[i,k])^2)
}

##SSE for cluster1  in Twodimeasy
easy.SSE_cluster1<-SSE(twodimeasy,1,1,1,2,centroid_easy)
##SSE for cluster2  in Twodimeasy
easy.SSE_cluster2<-SSE(twodimeasy,2,2,1,2,centroid_easy)
## total SSE
easy.SSE_overall=easy.SSE_cluster1+easy.SSE_cluster2

## SSB
N_cluster1.easy<-nrow(subset(twodimeasy,cluster==1))
N_cluster2.easy<-nrow(subset(twodimeasy,cluster==2))
SSB_easy=(N_cluster1.easy*((centroid_easy[1,1]-mean(twodimeasy$X.1))^2+(centroid_easy[1,2]-mean(twodimeasy$X.2))^2)+
  N_cluster2.easy*((centroid_easy[2,1]-mean(twodimeasy$X.1))^2+(centroid_easy[2,2]-mean(twodimeasy$X.2))^2))

SSE_SSB.easy.true<-data.frame(SSE.cluster1=easy.SSE_cluster1,SSE.cluster2=easy.SSE_cluster2,
                         SSE.overall=easy.SSE_overall,SSB=SSB_easy)
print(SSE_SSB.easy.true)


###k-mean clustering (randomly choose initial point k=2) in twodimwasy dataset
# stop condition: set change=false if less than 1% the ponits change clusters.
initial_easy1<-data.frame(twodimeasy[sample(1:300,size = 2,replace=FALSE),])
centroid.easy1<-initial_easy1[,2:3]
kis2.easy1<-data.frame(ID=1:nrow(twodimeasy),X.1=twodimeasy$X.1,X.2=twodimeasy$X.2,cluster=0)
change<-TRUE
oldcluster<-matrix(0,nrow=nrow(twodimeasy),ncol = 2)
oldcluster[,1]=twodimeasy[,1]
while(change==TRUE)
{
  dist<-matrix(0,nrow=nrow(twodimeasy),ncol = 2)
  oldcluster[,2]=kis2.easy1[,4]
  for (i in 1:300)
  {
    dist[i,1]<-sqrt((kis2.easy1[i,2]-centroid.easy1[1,1])^2+(kis2.easy1[i,3]-centroid.easy1[1,2])^2)
    dist[i,2]<-sqrt((kis2.easy1[i,2]-centroid.easy1[2,1])^2+(kis2.easy1[i,3]-centroid.easy1[2,2])^2)
    if(dist[i,1]==min(dist[i,]))
    {
      kis2.easy1[i,4]=1
    }
    else{
      kis2.easy1[i,4]=2
    } 
  } 
  x1<-mean(subset(kis2.easy1,cluster==1)$X.1)
  x2<-mean(subset(kis2.easy1,cluster==1)$X.2) 
  y1<-mean(subset(kis2.easy1,cluster==2)$X.1)
  y2<-mean(subset(kis2.easy1,cluster==2)$X.2)
if(sum(oldcluster[,2]!=kis2.easy1[,4])/nrow(kis2.easy1)<=0.01)
{
  change=FALSE
}
  centroid.easy1[1,1]=x1
  centroid.easy1[1,2]=x2
  centroid.easy1[2,1]=y1
  centroid.easy1[2,2]=y2
}
kis2.easy.output<-kis2.easy1[,-2:-3]
print("k-means clustering for twodimeasy data whenk=2")
print(kis2.easy.output)
write.csv(kis2.easy.output, "kis2.output.easy.csv", row.names=TRUE)

p2<-ggplot(data=kis2.easy1,aes(x=X.1,y=X.2,group=cluster,color=cluster))+geom_point()+ggtitle("k=2 cluster")
plot_grid(p,p2)

##caculate SSE for each cluster and SSB
##SSE for cluster1
easy.SSE.cluster1<-SSE(kis2.easy1,1,1,1,2, centroid.easy1)
##SSE for cluster2
easy.SSE.cluster2<-SSE(kis2.easy1,2,2,1,2, centroid.easy1)

##total SSE
kis2.SSE.easy_overall<-easy.SSE.cluster1+easy.SSE.cluster2
kis2.SSE.easy_overall
##SSB
N_cluster1.easy<-nrow(subset(kis2.easy1,cluster==1))
N_cluster2.easy<-nrow(subset(kis2.easy1,cluster==2))
kis2.SSB.easy=(N_cluster1.easy*((centroid.easy1[1,1]-mean(kis2.easy1$X.1))^2+(centroid.easy1[1,2]-mean(kis2.easy1$X.2))^2)+
            N_cluster2.easy*((centroid.easy1[2,1]-mean(kis2.easy1$X.1))^2+(centroid.easy1[2,2]-mean(kis2.easy1$X.2))^2))

SSE_SSB.easy.kis2<-data.frame(SSE.cluster1=easy.SSE.cluster1,SSE.cluster2=easy.SSE.cluster2,
                              SSE.overall=kis2.SSE.easy_overall,SSB=kis2.SSB.easy)
print(SSE_SSB.easy.kis2)

##silhouette width  

cluster1<-as.matrix(subset(kis2.easy1,cluster==1)[,2:3])
cluster2<-as.matrix(subset(kis2.easy1,cluster==2)[,2:3])

##sw for points in cluster1
cluster1.dist<-as.matrix(dist(cluster1, method = "euclidean",p=2))
Dist.1to2<-dist2(cluster1,cluster2)
ai_bi.cluster1<-data.frame(ai=apply(cluster1.dist,1,mean),bi=apply(Dist.1to2, 1, mean))
max.1<-apply(ai_bi.cluster1,1,max)
sw1<-(ai_bi.cluster1$bi-ai_bi.cluster1$ai)/max.1
mean(sw1)
asw1.easy.kis2<-mean(sw1)

##sw for points in cluster2
cluster2.dist<-as.matrix(dist(cluster2, method = "euclidean",diag = FALSE, upper = FALSE, p = 2))
Dist.2to1<-dist2(cluster2,cluster1)
ai_bi.cluster2<-data.frame(ai=apply(cluster2.dist,1,mean),bi=apply(Dist.2to1, 1, mean))
max.2<-apply(ai_bi.cluster2,1,max)
sw2<-(ai_bi.cluster2$bi-ai_bi.cluster2$ai)/max.2
asw2.easy.kis2<-mean(sw2)

ASW.easy<-(sum(sw2)+sum(sw1))/nrow(kis2.easy1)

print("ASW for k=2 in twodimeasy is" )
sw<-data.frame(asw.cluster1=asw1.easy.kis2,asw.cluster2=asw2.easy.kis2,asw.entire=ASW.easy)
print(sw)

##confusion matrix
confusion.easy2<-table(twodimeasy[,4],kis2.easy1[,4])
confusion.matrix.e2<-data.frame(kmean.cluster1=confusion.easy2[,1],kmean.cluster2=confusion.easy2[,2])
rownames(confusion.matrix.e2)<-c("true cluster1","true cluster2")
print("confusion matrix for twoeasy when k=2")
print(confusion.matrix.e2)

##kmean for twodimeasy dataset k=3
# stop condition: set change=false if less than 1% the ponts change clusters.
initial_easy2<-data.frame(twodimeasy[sample(1:300,size = 3,replace=FALSE),])
centroid.easy2<-initial_easy2[,2:3]
kis3.easy1<-data.frame(ID=1:nrow(twodimeasy),X.1=twodimeasy$X.1,X.2=twodimeasy$X.2,cluster=0)
change<-TRUE
oldcluster<-matrix(0,nrow=nrow(twodimeasy),ncol = 2)
oldcluster[,1]=twodimeasy[,1]
while(change==TRUE)
{
  dist<-matrix(0,nrow=nrow(twodimeasy),ncol = 3)
  oldcluster[,2]=kis3.easy1[,4]
  for (i in 1:300)
  {
    dist[i,1]<-sqrt((kis3.easy1[i,2]-centroid.easy2[1,1])^2+(kis3.easy1[i,3]-centroid.easy2[1,2])^2)
    dist[i,2]<-sqrt((kis3.easy1[i,2]-centroid.easy2[2,1])^2+(kis3.easy1[i,3]-centroid.easy2[2,2])^2)
    dist[i,3]<-sqrt((kis3.easy1[i,2]-centroid.easy2[3,1])^2+(kis3.easy1[i,3]-centroid.easy2[3,2])^2)
    if(dist[i,1]==min(dist[i,]))
    {
      kis3.easy1[i,4]=1
    }
    else if(dist[i,2]==min(dist[i,]))
      {
      kis3.easy1[i,4]=2
    } 
    else if(dist[i,3]==min(dist[i,])){
      kis3.easy1[i,4]=3
    }
  } 
  x1<-mean(subset(kis3.easy1,cluster==1)$X.1)
  x2<-mean(subset(kis3.easy1,cluster==1)$X.2) 
  y1<-mean(subset(kis3.easy1,cluster==2)$X.1)
  y2<-mean(subset(kis3.easy1,cluster==2)$X.2)
  z1<-mean(subset(kis3.easy1,cluster==3)$X.1)
  z2<-mean(subset(kis3.easy1,cluster==3)$X.2)
  if(sum(oldcluster[,2]!=kis3.easy1[,4])/nrow(kis3.easy1)<=0.01)
  {
    change=FALSE
  }
  centroid.easy2[1,1]=x1
  centroid.easy2[1,2]=x2
  centroid.easy2[2,1]=y1
  centroid.easy2[2,2]=y2
  centroid.easy2[3,1]=z1
  centroid.easy2[3,2]=z2
}
kis3.easy.output<-kis3.easy1[,-2:-3]
print("Kmean clustering for twodimeasy when k=3")
print(kis3.easy.output)
write.csv(kis3.easy.output, "kis3.output.easy.csv", row.names=TRUE)
p3<-ggplot(data=kis3.easy1,aes(x=X.1,y=X.2,group=cluster,color=cluster))+geom_point()+ggtitle("k=3 cluster")
plot_grid(p,p3)

##caculate SSE for each cluster and SSB
##SSE for cluster1
easy.SSE.cluster1<-SSE(kis3.easy1,1,1,1,2, centroid.easy2)
##SSE for cluster2
easy.SSE.cluster2<-SSE(kis3.easy1,2,2,1,2, centroid.easy2)
##SSE for cluster3
easy.SSE.cluster3<-SSE(kis3.easy1,3,3,1,2, centroid.easy2)
##total SSE
kis3.SSE.easy_overall<-easy.SSE.cluster1+easy.SSE.cluster2+easy.SSE.cluster3
kis3.SSE.easy_overall
##SSB
N_cluster1.easy<-nrow(subset(kis3.easy1,cluster==1))
N_cluster2.easy<-nrow(subset(kis3.easy1,cluster==2))
N_cluster3.easy<-nrow(subset(kis3.easy1,cluster==3))
kis3.SSB.easy=(N_cluster1.easy*((centroid.easy2[1,1]-mean(kis3.easy1$X.1))^2+(centroid.easy2[1,2]-mean(kis3.easy1$X.2))^2)+
                 N_cluster2.easy*((centroid.easy2[2,1]-mean(kis3.easy1$X.1))^2+(centroid.easy2[2,2]-mean(kis3.easy1$X.2))^2)+
                 N_cluster3.easy*((centroid.easy2[3,1]-mean(kis3.easy1$X.1))^2+(centroid.easy2[3,2]-mean(kis3.easy1$X.2))^2) )

SSE_SSB.easy.kis3<-data.frame(SSE.cluster1=easy.SSE.cluster1,SSE.cluster2=easy.SSE.cluster2,SSE.cluster3=easy.SSE.cluster3,
                              SSE.overall=kis3.SSE.easy_overall,SSB=kis3.SSB.easy)
print("SSE and SSE When k is3")
print(SSE_SSB.easy.kis3)

##sw
cluster1<-as.matrix(subset(kis3.easy1,cluster==1)[,2:3])
cluster2<-as.matrix(subset(kis3.easy1,cluster==2)[,2:3])
cluster3<-as.matrix(subset(kis3.easy1,cluster==3)[,2:3])
##sw for cluster1
cluster1.dist<-as.matrix(dist(cluster1, method = "euclidean",p=2))
Dist.1to2<-dist2(cluster1,cluster2)
Dist.1to3<-dist2(cluster1,cluster3)
b2<-apply(Dist.1to2, 1, mean)
b3<-apply(Dist.1to3, 1, mean)
b<-cbind(b2,b3)
ai_bi.cluster1.easy<-data.frame(ai=apply(cluster1.dist,1,mean),bi=apply(b,1,max))
max.1<-apply(ai_bi.cluster1.easy,1,max)
sw1.easy<-(ai_bi.cluster1.easy$bi-ai_bi.cluster1.easy$ai)/max.1
asw1.easy.kis3<-mean(sw1.easy)
##sw for points in cluster2
cluster2.dist<-as.matrix(dist(cluster2, method = "euclidean",p=2))
Dist.2to1<-dist2(cluster2,cluster1)
Dist.2to3<-dist2(cluster2,cluster3)
b1<-apply(Dist.2to1, 1, mean)
b3<-apply(Dist.2to3, 1, mean)
b<-cbind(b1,b3)
ai_bi.cluster2.easy<-data.frame(ai=apply(cluster2.dist,1,mean),bi=apply(b,1,max))
max.2<-apply(ai_bi.cluster2.easy,1,max)
sw2.easy<-(ai_bi.cluster2.easy$bi-ai_bi.cluster2.easy$ai)/max.2
asw2.easy.kis3<-mean(sw2.easy)
#sw for points in cluster3
cluster3.dist<-as.matrix(dist(cluster3, method = "euclidean",p=2))
Dist.3to1<-dist2(cluster3,cluster1)
Dist.3to2<-dist2(cluster3,cluster2)
Dist.3to4<-dist2(cluster3,cluster4)
b1<-apply(Dist.3to1, 1, mean)
b2<-apply(Dist.3to2, 1, mean)
b<-cbind(b1,b2)
ai_bi.cluster3.easy<-data.frame(ai=apply(cluster3.dist,1,mean),bi=apply(b,1,max))
max.3<-apply(ai_bi.cluster3.easy,1,max)
sw3.easy<-(ai_bi.cluster3.easy$bi-ai_bi.cluster3.easy$ai)/max.3
asw3.easy.kis3<-mean(sw3.easy)

#average sw for entire dataset
ASW.easy<-(sum(sw1.easy)+sum(sw2.easy)+sum(sw3.easy))/nrow(kis3.easy1)
print("ASW for k=3 in twodimeasy is" )
sw<-data.frame(asw.cluster1=asw1.easy.kis3,asw.cluster2=asw2.easy.kis3,asw.cluster3=asw3.easy.kis3,asw.entire=ASW.easy)
print(sw)

##confusion matrix
confusion.easy3<-table(twodimeasy[,4],kis3.easy1[,4])
confusion.easy3
confusion.matrix.e3<-data.frame(kmean.cluster1=confusion.easy3[,1],kmean.cluster2=confusion.easy3[,2],kmean.cluster3=confusion.easy3[,3])
rownames(confusion.matrix.e3)<-c("true cluster1","true cluster2")
print("confusion matrix for twoeasy when k=3")
print(confusion.matrix.e3)

##Twodimhard dataset
#For true cluster
#centroid 
centroid_hard<-matrix(0,nrow = 4,ncol=3)
colnames(centroid_hard)<-c('cluster','x.1','x.2')
centroid_hard[,1]<-1:4
##centroid for cluster1
centroid_hard[1,2]<-mean(subset(twodimhard,id==1)$X.1)
centroid_hard[1,3]<-mean(subset(twodimhard,id==1)$X.2)
centroid_hard[2,2]<-mean(subset(twodimhard,id==2)$X.1)
centroid_hard[2,3]<-mean(subset(twodimhard,id==2)$X.2)
centroid_hard[3,2]<-mean(subset(twodimhard,id==3)$X.1)
centroid_hard[3,3]<-mean(subset(twodimhard,id==3)$X.2)
centroid_hard[4,2]<-mean(subset(twodimhard,id==4)$X.1)
centroid_hard[4,3]<-mean(subset(twodimhard,id==4)$X.2)
##SSE for cluster 1 in twodimhard
hard.SSE_cluster1=sum((subset(twodimhard,id==1)$X.1-centroid_hard[1,2])^2+(subset(twodimhard,id==1)$X.2-centroid_hard[1,3])^2)
##SSE for cluster2  in Twodimeasy
hard.SSE_cluster2=sum((subset(twodimhard,id==2)$X.1-centroid_hard[2,2])^2+(subset(twodimhard,id==2)$X.2-centroid_hard[2,3])^2)
##SSE for cluster3 in twodimhard
hard.SSE_cluster3=sum((subset(twodimhard,id==3)$X.1-centroid_hard[3,2])^2+(subset(twodimhard,id==3)$X.2-centroid_hard[3,3])^2)
##SSE for cluster4 in twodimhard
hard.SSE_cluster4=sum((subset(twodimhard,id==4)$X.1-centroid_hard[4,2])^2+(subset(twodimhard,id==4)$X.2-centroid_hard[4,3])^2)
## total SSE
easy.SSE_overall= hard.SSE_cluster1+hard.SSE_cluster2+hard.SSE_cluster3+hard.SSE_cluster4
## SSB
N_cluster1.hard<-nrow(subset(twodimhard,id==1))
N_cluster2.hard<-nrow(subset(twodimhard,id==2))
N_cluster3.hard<-nrow(subset(twodimhard,id==3))
N_cluster4.hard<-nrow(subset(twodimhard,id==4))
SSB_hard=(N_cluster1.hard*((centroid_hard[1,2]-mean(twodimhard$X.1))^2+(centroid_hard[1,3]-mean(twodimhard$X.2))^2)+
          N_cluster2.hard*((centroid_hard[2,2]-mean(twodimhard$X.1))^2+(centroid_hard[2,3]-mean(twodimhard$X.2))^2)+
          N_cluster3.hard*((centroid_hard[3,2]-mean(twodimhard$X.1))^2+(centroid_hard[3,3]-mean(twodimhard$X.2))^2)+
          N_cluster2.hard*((centroid_hard[4,2]-mean(twodimhard$X.1))^2+(centroid_hard[4,3]-mean(twodimhard$X.2))^2))
SSE_SSB.hard.kis4.true<-data.frame(SSE.cluster1=hard.SSE_cluster1,SSE.cluster2=hard.SSE_cluster2,
                              SSE.cluster3=hard.SSE_cluster3,SSE.cluster4=hard.SSE_cluster4,
                              SSE.overall=easy.SSE_overall,SSB=SSB_hard)
print(SSE_SSB.hard.kis4.true)

##k-mean clustering,k=4
initial_hard1<-data.frame(twodimhard[sample(1:nrow(twodimhard),size = 4,replace=FALSE),])
centroid.hard1<-initial_hard1[,-3]
kis4.hard1<-data.frame(ID=1:nrow(twodimhard),X.1=twodimhard$X.1,X.2=twodimhard$X.2,cluster=0)
change<-TRUE
oldcluster.hard<-matrix(0,nrow=nrow(twodimhard),ncol = 2)
oldcluster.hard[,1]=1:nrow(twodimhard)
while(change==TRUE)
{
  dist<-matrix(0,nrow=nrow(twodimhard),ncol = 4)
  oldcluster.hard[,2]=kis4.hard1[,4]
  for (i in 1:nrow(twodimhard))
  {
    dist[i,1]<-sqrt((kis4.hard1[i,2]-centroid.hard1[1,1])^2+(kis4.hard1[i,3]-centroid.hard1[1,2])^2)
    dist[i,2]<-sqrt((kis4.hard1[i,2]-centroid.hard1[2,1])^2+(kis4.hard1[i,3]-centroid.hard1[2,2])^2)
    dist[i,3]<-sqrt((kis4.hard1[i,2]-centroid.hard1[3,1])^2+(kis4.hard1[i,3]-centroid.hard1[3,2])^2)
    dist[i,4]<-sqrt((kis4.hard1[i,2]-centroid.hard1[4,1])^2+(kis4.hard1[i,3]-centroid.hard1[4,2])^2)
    if(dist[i,1]==min(dist[i,]))
    {
      kis4.hard1[i,4]=1
    }
    else if(dist[i,2]==min(dist[i,]))
             {
      kis4.hard1[i,4]=2
    } 
    else if(dist[i,3]==min(dist[i,]))
    {
      kis4.hard1[i,4]=3
    } 
    else{kis4.hard1[i,4]=4}
  } 
  x1<-mean(subset(kis4.hard1,cluster==1)$X.1)
  x2<-mean(subset(kis4.hard1,cluster==1)$X.2) 
  y1<-mean(subset(kis4.hard1,cluster==2)$X.1)
  y2<-mean(subset(kis4.hard1,cluster==2)$X.2)
  z1<-mean(subset(kis4.hard1,cluster==3)$X.1)
  z2<-mean(subset(kis4.hard1,cluster==3)$X.2) 
  g1<-mean(subset(kis4.hard1,cluster==4)$X.1)
  g2<-mean(subset(kis4.hard1,cluster==4)$X.2)
  if(sum(oldcluster.hard[,2]!=kis4.hard1[,4])/nrow(kis4.hard1)<=0.01)
  {
    change=FALSE
  }
  centroid.hard1[1,1]=x1
  centroid.hard1[1,2]=x2
  centroid.hard1[2,1]=y1
  centroid.hard1[2,2]=y2
  centroid.hard1[3,1]=z1
  centroid.hard1[3,2]=z2
  centroid.hard1[4,1]=g1
  centroid.hard1[4,2]=g2
}
Kis4.hard1.output<-data.frame(ID=kis4.hard1$ID,cluster=kis4.hard1$cluster)
print("k-means clustering for twodimehard data whenk=4")
print(Kis4.hard1.output)
write.csv(Kis4.hard1.output, "kis4.output.hard.csv", row.names=TRUE)
q4<-ggplot(data=kis4.hard1,aes(x=X.1,y=X.2,group=cluster,color=cluster))+geom_point()+ggtitle("k=4 cluster")
plot_grid(q,q4)
##SSE 
SSE.cluster1.hard<-SSE(kis4.hard1,1,1,1,2,centroid.hard1)
SSE.cluster2.hard<-SSE(kis4.hard1,2,2,1,2,centroid.hard1)
SSE.cluster3.hard<-SSE(kis4.hard1,3,3,1,2,centroid.hard1)
SSE.cluster4.hard<-SSE(kis4.hard1,4,4,1,2,centroid.hard1)
SSE.overall.hard<-SSE.cluster1.hard+SSE.cluster2.hard+SSE.cluster3.hard+SSE.cluster4.hard
SSE.overall.hard
centroid.hard1
## SSB
N_cluster1.hard<-nrow(subset(kis4.hard1,cluster==1))
N_cluster2.hard<-nrow(subset(kis4.hard1,cluster==2))
N_cluster3.hard<-nrow(subset(kis4.hard1,cluster==3))
N_cluster4.hard<-nrow(subset(kis4.hard1,cluster==4))

SSB.hard=(N_cluster1.hard*((centroid.hard1[1,2]-mean(kis4.hard1$X.1))^2+(centroid.hard1[1,2]-mean(kis4.hard1$X.2))^2)+
          N_cluster2.hard*((centroid.hard1[2,2]-mean(kis4.hard1$X.1))^2+(centroid.hard1[2,2]-mean(kis4.hard1$X.2))^2)+
          N_cluster3.hard*((centroid.hard1[3,2]-mean(kis4.hard1$X.1))^2+(centroid.hard1[3,2]-mean(kis4.hard1$X.2))^2)+
          N_cluster4.hard*((centroid.hard1[4,2]-mean(kis4.hard1$X.1))^2+(centroid.hard1[4,2]-mean(kis4.hard1$X.2))^2))
SSB.hard
SSE_SSB.hard.kis4<-data.frame(SSE.cluster1=SSE.cluster1.hard,SSE.cluster2=SSE.cluster2.hard,
                              SSE.cluster3=SSE.cluster3.hard,SSE.cluster4=SSE.cluster4.hard,
                              SSE.overall=SSE.overall.hard,SSB=SSB.hard)
print(SSE_SSB.hard.kis4)

##silhouette width for twodimhard  
cluster1<-as.matrix(subset(kis4.hard1,cluster==1)[,2:3])
cluster2<-as.matrix(subset(kis4.hard1,cluster==2)[,2:3])
cluster3<-as.matrix(subset(kis4.hard1,cluster==3)[,2:3])
cluster4<-as.matrix(subset(kis4.hard1,cluster==4)[,2:3])

##sw for points in cluster1
cluster1.dist<-as.matrix(dist(cluster1, method = "euclidean",p=2))
Dist.1to2<-dist2(cluster1,cluster2)
Dist.1to3<-dist2(cluster1,cluster3)
Dist.1to4<-dist2(cluster1,cluster4)
b2<-apply(Dist.1to2, 1, mean)
b3<-apply(Dist.1to3, 1, mean)
b4<-apply(Dist.1to4, 1, mean)
b<-cbind(b2,b3,b4)
ai_bi.cluster1.hard<-data.frame(ai=apply(cluster1.dist,1,mean),bi=apply(b,1,max))
max.1<-apply(ai_bi.cluster1.hard,1,max)
sw1.hard<-(ai_bi.cluster1.hard$bi-ai_bi.cluster1.hard$ai)/max.1
asw1.hard.kis4<-mean(sw1.hard)
##sw for points in cluster2
cluster2.dist<-as.matrix(dist(cluster2, method = "euclidean",p=2))
Dist.2to1<-dist2(cluster2,cluster1)
Dist.2to3<-dist2(cluster2,cluster3)
Dist.2to4<-dist2(cluster2,cluster4)
b1<-apply(Dist.2to1, 1, mean)
b3<-apply(Dist.2to3, 1, mean)
b4<-apply(Dist.2to4, 1, mean)
b<-cbind(b1,b3,b4)
ai_bi.cluster2.hard<-data.frame(ai=apply(cluster2.dist,1,mean),bi=apply(b,1,max))
max.2<-apply(ai_bi.cluster2.hard,1,max)
sw2.hard<-(ai_bi.cluster2.hard$bi-ai_bi.cluster2.hard$ai)/max.2
asw2.hard.kis4<-mean(sw2.hard)
##sw for points in cluster3
cluster3.dist<-as.matrix(dist(cluster3, method = "euclidean",p=2))
Dist.3to1<-dist2(cluster3,cluster1)
Dist.3to2<-dist2(cluster3,cluster2)
Dist.3to4<-dist2(cluster3,cluster4)
b1<-apply(Dist.3to1, 1, mean)
b2<-apply(Dist.3to2, 1, mean)
b4<-apply(Dist.3to4, 1, mean)
b<-cbind(b1,b2,b4)
ai_bi.cluster3.hard<-data.frame(ai=apply(cluster3.dist,1,mean),bi=apply(b,1,max))
max.3<-apply(ai_bi.cluster3.hard,1,max)
sw3.hard<-(ai_bi.cluster3.hard$bi-ai_bi.cluster3.hard$ai)/max.3
asw3.hard.kis4<-mean(sw3.hard)
##sw for points in cluster4
cluster4.dist<-as.matrix(dist(cluster4, method = "euclidean",p=2))
Dist.4to1<-dist2(cluster4,cluster1)
Dist.4to2<-dist2(cluster4,cluster2)
Dist.4to3<-dist2(cluster4,cluster3)
b1<-apply(Dist.4to1, 1, mean)
b2<-apply(Dist.4to2, 1, mean)
b3<-apply(Dist.4to3, 1, mean)
b<-cbind(b1,b2,b3)
ai_bi.cluster4.hard<-data.frame(ai=apply(cluster4.dist,1,mean),bi=apply(b,1,max))
max.4<-apply(ai_bi.cluster4.hard,1,max)
sw4.hard<-(ai_bi.cluster4.hard$bi-ai_bi.cluster4.hard$ai)/max.4
asw4.hard.kis4<-mean(sw4.hard)

#average sw for entir dataset
ASW.hard<-(sum(sw1.hard)+sum(sw2.hard)+sum(sw3.hard)+sum(sw4.hard))/nrow(kis4.hard1)
print("ASW for k=2 in twodimhard is" )
sw.hard<-data.frame(asw.cluster1=asw1.hard.kis4,asw.cluster2=asw2.hard.kis4,
               asw.cluster3=asw3.hard.kis4,asw.cluster4=asw4.hard.kis4,asw.entire=ASW.hard)
print(sw.hard)


##confusion matrix
confusion.hard4<-table(kis4.hard1$cluster,twodimhard$id)
confusion.matrix.h4<-data.frame(kmean.cluster1=confusion.hard4[,1],kmean.cluster2=confusion.hard4[,2],kmean.cluster3=confusion.hard4[,3],kmean.cluster4=confusion.hard4[,4])
rownames(confusion.matrix.h4)<-c("true cluster1","true cluster2","true cluster3","true cluster4")
print("confusion matrix for twodimhard when k=4")
print(confusion.matrix.h4)


##k-mean clustering,k=3
initial_hard2<-data.frame(twodimhard[sample(1:nrow(twodimhard),size = 3,replace=FALSE),])
centroid.hard2<-initial_hard2[,-3]
kis3.hard1<-data.frame(ID=1:nrow(twodimhard),X.1=twodimhard$X.1,X.2=twodimhard$X.2,cluster=0)
change<-TRUE
oldcluster.hard<-matrix(0,nrow=nrow(twodimhard),ncol = 2)
oldcluster.hard[,1]=1:nrow(twodimhard)
while(change==TRUE)
{
  dist<-matrix(0,nrow=nrow(twodimhard),ncol = 3)
  oldcluster.hard[,2]=kis3.hard1[,4]
  for (i in 1:nrow(twodimhard))
  {
    dist[i,1]<-sqrt((kis3.hard1[i,2]-centroid.hard2[1,1])^2+(kis3.hard1[i,3]-centroid.hard2[1,2])^2)
    dist[i,2]<-sqrt((kis3.hard1[i,2]-centroid.hard2[2,1])^2+(kis3.hard1[i,3]-centroid.hard2[2,2])^2)
    dist[i,3]<-sqrt((kis3.hard1[i,2]-centroid.hard2[3,1])^2+(kis3.hard1[i,3]-centroid.hard2[3,2])^2)
      if(dist[i,1]==min(dist[i,]))
    {
      kis3.hard1[i,4]=1
    }
    else if(dist[i,2]==min(dist[i,]))
    {
      kis3.hard1[i,4]=2
    } 
    else {
      kis3.hard1[i,4]=3
    } 
    } 
  x1<-mean(subset(kis3.hard1,cluster==1)$X.1)
  x2<-mean(subset(kis3.hard1,cluster==1)$X.2) 
  y1<-mean(subset(kis3.hard1,cluster==2)$X.1)
  y2<-mean(subset(kis3.hard1,cluster==2)$X.2)
  z1<-mean(subset(kis3.hard1,cluster==3)$X.1)
  z2<-mean(subset(kis3.hard1,cluster==3)$X.2) 
    if(sum(oldcluster.hard[,2]!=kis3.hard1[,4])/nrow(kis3.hard1)<=0.01)
  {
    change=FALSE
  }
  centroid.hard2[1,1]=x1
  centroid.hard2[1,2]=x2
  centroid.hard2[2,1]=y1
  centroid.hard2[2,2]=y2
  centroid.hard2[3,1]=z1
  centroid.hard2[3,2]=z2
 }
Kis3.hard1.output<-data.frame(ID=kis3.hard1$ID,cluster=kis3.hard1$cluster)
print("k-means clustering for twodimehard data whenk=3")
print(Kis3.hard1.output)
write.csv(Kis3.hard1.output, "Kis3.output.hard.csv", row.names=TRUE)
q3<-ggplot(data=kis3.hard1,aes(x=X.1,y=X.2,group=cluster,color=cluster))+geom_point()+ggtitle("k=3 cluster")
plot_grid(q,q3)
##SSE 
SSE.cluster1.hard<-SSE(kis3.hard1,1,1,1,2,centroid.hard2)
SSE.cluster2.hard<-SSE(kis3.hard1,2,2,1,2,centroid.hard2)
SSE.cluster3.hard<-SSE(kis3.hard1,3,3,1,2,centroid.hard2)
SSE.overall.hard<-SSE.cluster1.hard+SSE.cluster2.hard+SSE.cluster3.hard
SSE.overall.hard
## SSB
N_cluster1.hard<-nrow(subset(kis3.hard1,cluster==1))
N_cluster2.hard<-nrow(subset(kis3.hard1,cluster==2))
N_cluster3.hard<-nrow(subset(kis3.hard1,cluster==3))
centroid.hard2
SSB.hard=N_cluster1.hard*((centroid.hard2[1,2]-mean(kis3.hard1$X.1))^2+(centroid.hard2[1,2]-mean(kis3.hard1$X.2))^2)+
            N_cluster2.hard*((centroid.hard2[2,2]-mean(kis3.hard1$X.1))^2+(centroid.hard2[2,2]-mean(kis3.hard1$X.2))^2)+
            N_cluster3.hard*((centroid.hard2[3,2]-mean(kis3.hard1$X.1))^2+(centroid.hard2[3,2]-mean(kis3.hard1$X.2))^2)
SSB.hard
SSE_SSB.hard.kis3<-data.frame(SSE.cluster1=SSE.cluster1.hard,SSE.cluster2=SSE.cluster2.hard,
                              SSE.cluster3=SSE.cluster3.hard,SSE.overall=SSE.overall.hard,SSB=SSB.hard)                            
print(SSE_SSB.hard.kis3)

##silhouette width for twodimhard  
cluster1<-as.matrix(subset(kis3.hard1,cluster==1)[,2:3])
cluster2<-as.matrix(subset(kis3.hard1,cluster==2)[,2:3])
cluster3<-as.matrix(subset(kis3.hard1,cluster==3)[,2:3])

##sw for points in cluster1
cluster1.dist<-as.matrix(dist(cluster1, method = "euclidean",p=2))
Dist.1to2<-dist2(cluster1,cluster2)
Dist.1to3<-dist2(cluster1,cluster3)
b2<-apply(Dist.1to2, 1, mean)
b3<-apply(Dist.1to3, 1, mean)
b<-cbind(b2,b3)
ai_bi.cluster1.hard<-data.frame(ai=apply(cluster1.dist,1,mean),bi=apply(b,1,max))
max.1<-apply(ai_bi.cluster1.hard,1,max)
sw1.hard<-(ai_bi.cluster1.hard$bi-ai_bi.cluster1.hard$ai)/max.1
asw1.hard.kis3<-mean(sw1.hard)
##sw for points in cluster2
cluster2.dist<-as.matrix(dist(cluster2, method = "euclidean",p=2))
Dist.2to1<-dist2(cluster2,cluster1)
Dist.2to3<-dist2(cluster2,cluster3)
b1<-apply(Dist.2to1, 1, mean)
b3<-apply(Dist.2to3, 1, mean)
b<-cbind(b1,b3)
ai_bi.cluster2.hard<-data.frame(ai=apply(cluster2.dist,1,mean),bi=apply(b,1,max))
max.2<-apply(ai_bi.cluster2.hard,1,max)
sw2.hard<-(ai_bi.cluster2.hard$bi-ai_bi.cluster2.hard$ai)/max.2
asw2.hard.kis3<-mean(sw2.hard)
##sw for points in cluster3
cluster3.dist<-as.matrix(dist(cluster3, method = "euclidean",p=2))
Dist.3to1<-dist2(cluster3,cluster1)
Dist.3to2<-dist2(cluster3,cluster2)
b1<-apply(Dist.3to1, 1, mean)
b2<-apply(Dist.3to2, 1, mean)
b<-cbind(b1,b2)
ai_bi.cluster3.hard<-data.frame(ai=apply(cluster3.dist,1,mean),bi=apply(b,1,max))
max.3<-apply(ai_bi.cluster3.hard,1,max)
sw3.hard<-(ai_bi.cluster3.hard$bi-ai_bi.cluster3.hard$ai)/max.3
asw3.hard.kis3<-mean(sw3.hard)

#average sw for entire dataset
ASW.hard.kis3<-(sum(sw1.hard)+sum(sw2.hard)+sum(sw3.hard))/nrow(kis4.hard1)
print("ASW for k=3 in twodimhard is" )
sw.hard.kis3<-data.frame(asw.cluster1=asw1.hard.kis3,asw.cluster2=asw2.hard.kis3,
                    asw.cluster3=asw3.hard.kis3,asw.entire=ASW.hard.kis3)
print(sw.hard.kis3)


##confusion matrix
confusion.hard3<-table(kis3.hard1$cluster,twodimhard$id)
confusion.matrix.h3<-data.frame(kmean.cluster1=confusion.hard3[,1],kmean.cluster2=confusion.hard3[,2],kmean.cluster3=confusion.hard3[,3])
rownames(confusion.matrix.h3)<-c("true cluster1","true cluster2","true cluster3")
print("confusion matrix for twodimhard when k=3")
print(confusion.matrix.h3)


#wine data

wine1<-wine[,1:12]

##normalize
normalization<-function(df,i){
df[,i]<-(df[,i]-min(df[,i]))/(max(df[,i])-min(df[,i])) 
}
for( j in 2:12)
{
  wine1[,j]<-normalization(wine1,j)
}

##kmean clustering k=2 (wine data)
initial.wine<-data.frame(wine1[sample(1:nrow(wine1),size = 2,replace=FALSE),])
centroid.wine=initial.wine
change<-TRUE
kis2.wine<-wine1
kis2.wine["cluster"]<-0
oldcluster.wine<-matrix(0,nrow=nrow(wine1),ncol = 2)
oldcluster.wine[,1]=1:nrow(wine1)
while(change==TRUE)
{
  dist<-matrix(0,nrow=nrow(wine1),ncol = 2)
  oldcluster.wine[,2]=kis2.wine$cluster
  for (i in 1:nrow(wine1))
  { 
    D1=0
    D2=0
    for(j in 2:12)
    {
      d1=(kis2.wine[i,j]-centroid.wine[1,j])^2
      D1=d1+D1 
      d2=(kis2.wine[i,j]-centroid.wine[2,j])^2
      D2=d2+D2 
      }
    dist[i,1]=sqrt(D1)
    dist[i,2]=sqrt(D2)
    if(dist[i,1]<=dist[i,2])
    {
      kis2.wine[i,13]=1
    }
    else
    {
      kis2.wine[i,13]=2
    } 
  } 
  if(sum(oldcluster.wine[,2]!=kis2.wine$cluster)/nrow(kis2.wine)<=0.01)
  {
    change=FALSE
  }
  for(j in 2:12){
    centroid.wine[1,j]=mean(subset(kis2.wine,cluster==1)[,j])
    centroid.wine[2,j]=mean(subset(kis2.wine,cluster==2)[,j])
  }
}

kis2.wine.output<-data.frame(ID=kis2.wine$ID,cluster=kis2.wine$cluster)
print("k-means clustering for wine data when k=2")
print(kis2.wine.output)
write.csv(kis2.wine.output, "kis2.output.wine.csv", row.names=TRUE)
confusion_matrix.wine1<-table(wine$class,kis2.wine$cluster)
confusion_matrix.wine1

confusion_matrix.wine2<-table(wine$quality,kis2.wine$cluster)
confusion_matrix.wine2

##SSE
SSE<-function(df,n,centr){
sse=0
for(i in 2:12){
   d<-sum((subset(df,cluster==n)[,i]-centr[1,i])^2)
   sse=sse+d
}
return(sse)
}
SSE.2cluster1.wine<-SSE(kis2.wine,1,centroid.wine)
SSE.2cluster2.wine<-SSE(kis2.wine,2,centroid.wine)
SSE.overall.wine2<-SSE.2cluster1.wine+SSE.2cluster2.wine

#SSB
N_cluster1<-nrow(subset(kis2.wine,cluster==1))
N_cluster2<-nrow(subset(kis2.wine,cluster==2))

#j is cluster
dist_between<-function(df,centr,j){
Di=0
for(i in 2:12)
{
  di<-(centr[j,i]-mean(df[,i]))^2
  Di<-di+Di
}
return(Di)
}
dist_between(kis2.wine,centroid.wine,1)
SSB.kis2.wine<-N_cluster1*(dist_between(kis2.wine,centroid.wine,1))+N_cluster2*(dist_between(kis2.wine,centroid.wine,2))

SSE_SSB.wine<-data.frame(SSE.cluster1=SSE.2cluster1.wine,SSE.cluster2=SSE.2cluster2.wine,
                         SSE.overall=SSE.overall.wine2,SSB=SSB.kis2.wine)                            
print(SSE_SSB.wine)

##k=3
initial.wine3<-data.frame(wine1[sample(1:nrow(wine1),size = 3,replace=FALSE),])
centroid.wine3=initial.wine3
change<-TRUE
kis3.wine<-wine1
kis3.wine["cluster"]<-0
oldcluster.wine<-matrix(0,nrow=nrow(wine1),ncol = 2)
oldcluster.wine[,1]=1:nrow(wine1)
while(change==TRUE)
{
  dist<-matrix(0,nrow=nrow(wine1),ncol = 3)
  oldcluster.wine[,2]=kis3.wine$cluster
  for (i in 1:nrow(wine1))
  { 
    D1=0
    D2=0
    D3=0
    for(j in 2:12)
    {
      d1=(kis3.wine[i,j]-centroid.wine3[1,j])^2
      D1=d1+D1 
      d2=(kis3.wine[i,j]-centroid.wine3[2,j])^2
      D2=d2+D2 
      d3=(kis3.wine[i,j]-centroid.wine3[3,j])^2
      D3=d3+D3 
    }
    dist[i,1]=sqrt(D1)
    dist[i,2]=sqrt(D2)
    dist[i,3]=sqrt(D3)
    if(dist[i,1]==min(dist[i,]))
    {
      kis3.wine[i,13]=1
    }
    else if(dist[i,2]==min(dist[i,]))
    {
      kis3.wine[i,13]=2
    } 
    else if(dist[i,3]==min(dist[i,]))
    {
      kis3.wine[i,13]=3
    }  
  }
  if(sum(oldcluster.wine[,2]!=kis3.wine$cluster)/nrow(kis3.wine)<=0.1)
  {
    change=FALSE
  }
  for(j in 2:12){
    centroid.wine3[1,j]=mean(subset(kis3.wine,cluster==1)[,j])
    centroid.wine3[2,j]=mean(subset(kis3.wine,cluster==2)[,j])
    centroid.wine3[3,j]=mean(subset(kis3.wine,cluster==3)[,j])
  }
}

kis3.wine.output<-data.frame(ID=kis3.wine$ID,cluster=kis3.wine$cluster)
print("k-means clustering for wine data when k=3")
print(kis3.wine.output)
write.csv(kis3.wine.output, "kis3.output.wine.csv", row.names=TRUE)
confusion_matrix3<-table(wine$quality,kis3.wine$cluster)
confusion_matrix3
##SSE
##n is cluster number
SSE.3cluster1.wine<-SSE(kis3.wine,1,centroid.wine3)
SSE.3cluster2.wine<-SSE(kis3.wine,2,centroid.wine3)
SSE.3cluster3.wine<-SSE(kis3.wine,3,centroid.wine3)
SSE.overall.wine3<-SSE.3cluster1.wine+SSE.3cluster2.wine+SSE.3cluster3.wine

#SSB
N_cluster1<-nrow(subset(kis3.wine,cluster==1))
N_cluster2<-nrow(subset(kis3.wine,cluster==2))
N_cluster3<-nrow(subset(kis3.wine,cluster==3))
SSB.kis3.wine<-N_cluster1*(dist_between(kis3.wine,centroid.wine3,1))+N_cluster2*(dist_between(kis3.wine,centroid.wine3,2))+
  N_cluster3*(dist_between(kis3.wine,centroid.wine3,3))


SSE_SSB.wine<-data.frame(SSE.cluster1=SSE.3cluster1.wine,SSE.cluster2=SSE.3cluster2.wine,
                         SSE.cluster3=SSE.3cluster3.wine,SSE.overall=SSE.overall.wine3,SSB=SSB.kis3.wine)                            
print(SSE_SSB.wine)

##K=4 Clustering

initial.wine4<-data.frame(wine1[sample(1:nrow(wine1),size = 4,replace=FALSE),])
centroid.wine4=initial.wine4
change<-TRUE
kis4.wine<-wine1
kis4.wine["cluster"]<-0
oldcluster.wine<-matrix(0,nrow=nrow(wine1),ncol = 2)
oldcluster.wine[,1]=1:nrow(wine1)
while(change==TRUE)
{
  dist<-matrix(0,nrow=nrow(wine1),ncol = 4)
  oldcluster.wine[,2]=kis4.wine$cluster
  for (i in 1:nrow(wine1))
  { 
    D1=0
    D2=0
    D3=0
    D4=0
      for(j in 2:12)
    {
      d1=(kis4.wine[i,j]-centroid.wine4[1,j])^2
      D1=d1+D1 
      d2=(kis4.wine[i,j]-centroid.wine4[2,j])^2
      D2=d2+D2 
      d3=(kis4.wine[i,j]-centroid.wine4[3,j])^2
      D3=d3+D3
      d4=(kis4.wine[i,j]-centroid.wine4[4,j])^2
      D4=d4+D4 
        }
    dist[i,1]=sqrt(D1)
    dist[i,2]=sqrt(D2)
    dist[i,3]=sqrt(D3)
    dist[i,4]=sqrt(D4)
   
    if(dist[i,1]==min(dist[i,]))
    {
      kis4.wine[i,13]=1
    }
    else if(dist[i,2]==min(dist[i,]))
    {
      kis4.wine[i,13]=2
    } 
    else if(dist[i,3]==min(dist[i,]))
    {
      kis4.wine[i,13]=3
    }  
    else {
      kis4.wine[i,13]=4
    }  
   }
  if(sum(oldcluster.wine[,2]!=kis4.wine$cluster)/nrow(kis4.wine)<=0.01)
  {
    change=FALSE
  }
  for(j in 2:12){
    centroid.wine4[1,j]=mean(subset(kis4.wine,cluster==1)[,j])
    centroid.wine4[2,j]=mean(subset(kis4.wine,cluster==2)[,j])
    centroid.wine4[3,j]=mean(subset(kis4.wine,cluster==3)[,j])
    centroid.wine4[4,j]=mean(subset(kis4.wine,cluster==4)[,j])
     }
}

kis4.wine.output<-data.frame(ID=kis4.wine$ID,cluster=kis4.wine$cluster)
print("k-means clustering for wine data when k=4")
print(kis4.wine.output)
write.csv(kis4.wine.output, "kis4.output.wine.csv", row.names=TRUE)
confusion_matrix.wine4<-as.matrix(table(wine$quality,kis4.wine$cluster))
print(confusion_matrix.wine4)
#SSE
SSE.4cluster1.wine<-SSE(kis4.wine,1,centroid.wine4)
SSE.4cluster2.wine<-SSE(kis4.wine,2,centroid.wine4)
SSE.4cluster3.wine<-SSE(kis4.wine,3,centroid.wine4)
SSE.4cluster4.wine<-SSE(kis4.wine,4,centroid.wine4)

SSE.overall.wine4<-SSE.4cluster1.wine+SSE.4cluster2.wine+SSE.4cluster3.wine+SSE.4cluster4.wine

#SSB
N_cluster1<-nrow(subset(kis4.wine,cluster==1))
N_cluster2<-nrow(subset(kis4.wine,cluster==2))
N_cluster3<-nrow(subset(kis4.wine,cluster==3))
N_cluster4<-nrow(subset(kis4.wine,cluster==4))

SSB.kis4.wine<-N_cluster1*(dist_between(kis4.wine,centroid.wine4,1))+N_cluster2*(dist_between(kis4.wine,centroid.wine4,2))+
  N_cluster3*(dist_between(kis4.wine,centroid.wine4,3))+N_cluster4*(dist_between(kis4.wine,centroid.wine4,4))
SSE_SSB.wine<-data.frame(SSE.cluster1=SSE.4cluster1.wine,SSE.cluster2=SSE.4cluster2.wine,
                         SSE.cluster3=SSE.4cluster3.wine,SSE.cluster4=SSE.4cluster4.wine,
                         SSE.overall=SSE.overall.wine4,SSB=SSB.kis4.wine)                            
print(SSE_SSB.wine)


##k=6 clustering
initial.wine6<-data.frame(wine1[sample(1:nrow(wine1),size = 6,replace=FALSE),])
centroid.wine6=initial.wine6
change<-TRUE
kis6.wine<-wine1
kis6.wine["cluster"]<-0
oldcluster.wine<-matrix(0,nrow=nrow(wine1),ncol = 2)
oldcluster.wine[,1]=1:nrow(wine1)
while(change==TRUE)
{
  dist<-matrix(0,nrow=nrow(wine1),ncol = 6)
  oldcluster.wine[,2]=kis6.wine$cluster
  for (i in 1:nrow(wine1))
  { 
    D1=0
    D2=0
    D3=0
    D4=0
    D5=0
    D6=0
    for(j in 2:12)
    {
      d1=(kis6.wine[i,j]-centroid.wine6[1,j])^2
      D1=d1+D1 
      d2=(kis6.wine[i,j]-centroid.wine6[2,j])^2
      D2=d2+D2 
      d3=(kis6.wine[i,j]-centroid.wine6[3,j])^2
      D3=d3+D3
      d4=(kis6.wine[i,j]-centroid.wine6[4,j])^2
      D4=d4+D4 
      d5=(kis6.wine[i,j]-centroid.wine6[5,j])^2
      D5=d5+D5 
      d6=(kis6.wine[i,j]-centroid.wine6[6,j])^2
      D6=d6+D6
    }
    dist[i,1]=sqrt(D1)
    dist[i,2]=sqrt(D2)
    dist[i,3]=sqrt(D3)
    dist[i,4]=sqrt(D4)
    dist[i,5]=sqrt(D5)
    dist[i,6]=sqrt(D6)
      if(dist[i,1]==min(dist[i,]))
    {
      kis6.wine[i,13]=1
    }
    else if(dist[i,2]==min(dist[i,]))
    {
      kis6.wine[i,13]=2
    } 
    else if(dist[i,3]==min(dist[i,]))
    {
      kis6.wine[i,13]=3
    }  
    else if(dist[i,4]==min(dist[i,]))
    {
      kis6.wine[i,13]=4
    }  
    else if(dist[i,5]==min(dist[i,]))
    {
      kis6.wine[i,13]=5
    }  
    else if(dist[i,6]==min(dist[i,]))
    {
      kis6.wine[i,13]=6
    }  
  }
  if(sum(oldcluster.wine[,2]!=kis6.wine$cluster)/nrow(kis2.wine)<=0.01)
  {
    change=FALSE
  }
  for(j in 2:12){
    centroid.wine6[1,j]=mean(subset(kis6.wine,cluster==1)[,j])
    centroid.wine6[2,j]=mean(subset(kis6.wine,cluster==2)[,j])
    centroid.wine6[3,j]=mean(subset(kis6.wine,cluster==3)[,j])
    centroid.wine6[4,j]=mean(subset(kis6.wine,cluster==4)[,j])
    centroid.wine6[5,j]=mean(subset(kis6.wine,cluster==5)[,j])
    centroid.wine6[6,j]=mean(subset(kis6.wine,cluster==6)[,j])
  }
}

kis6.wine.output<-data.frame(ID=kis6.wine$ID,cluster=kis6.wine$cluster)
print("k-means clustering for wine data when k=6")
print(kis6.wine.output)
write.csv(kis6.wine.output, "kis6.output.wine.csv", row.names=TRUE)
confusion_matrix.wine6<-as.matrix(table(wine$quality,kis6.wine$cluster))
print(confusion_matrix.wine6)
#SSE
SSE.6cluster1.wine<-SSE(kis6.wine,1,centroid.wine6)
SSE.6cluster2.wine<-SSE(kis6.wine,2,centroid.wine6)
SSE.6cluster3.wine<-SSE(kis6.wine,3,centroid.wine6)
SSE.6cluster4.wine<-SSE(kis6.wine,4,centroid.wine6)
SSE.6cluster5.wine<-SSE(kis6.wine,5,centroid.wine6)
SSE.6cluster6.wine<-SSE(kis6.wine,6,centroid.wine6)
SSE.overall.wine6<-SSE.6cluster1.wine+SSE.6cluster2.wine+SSE.6cluster3.wine+SSE.6cluster4.wine+
  SSE.6cluster5.wine+SSE.6cluster6.wine
#SSB
N_cluster1<-nrow(subset(kis6.wine,cluster==1))
N_cluster2<-nrow(subset(kis6.wine,cluster==2))
N_cluster3<-nrow(subset(kis6.wine,cluster==3))
N_cluster4<-nrow(subset(kis6.wine,cluster==4))
N_cluster5<-nrow(subset(kis6.wine,cluster==5))
N_cluster6<-nrow(subset(kis6.wine,cluster==6))
SSB.kis6.wine<-N_cluster1*(dist_between(kis6.wine,centroid.wine6,1))+N_cluster2*(dist_between(kis6.wine,centroid.wine6,2))+
  N_cluster3*(dist_between(kis6.wine,centroid.wine6,3))+N_cluster4*(dist_between(kis6.wine,centroid.wine6,4))+N_cluster5*(dist_between(kis6.wine,centroid.wine6,5))+
  N_cluster6*(dist_between(kis6.wine,centroid.wine6,6))
SSE_SSB.wine<-data.frame(SSE.cluster1=SSE.6cluster1.wine,SSE.cluster2=SSE.6cluster2.wine,
                              SSE.cluster3=SSE.6cluster3.wine,SSE.cluster4=SSE.6cluster4.wine,
                    SSE.cluster5=SSE.6cluster5.wine,SSE.cluster6=SSE.6cluster6.wine,
                    SSE.overall=SSE.overall.wine6,SSB=SSB.kis6.wine)                            
print(SSE_SSB.wine)
