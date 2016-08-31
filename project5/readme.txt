Read data:
setwd() is to set work directory. In this program, I use ¡°C:/Users/ab/Desktop/project5¡± as working directory. All the dataset which are used in this program are saved in is file path, under the projec5 folder.
User can change the file path to set work directory of according to requirement.
setwd("C:/Users/ab/Desktop/project5")

Then data can be loaded into R by following code directly.
twodimeasy<- read.csv('A.easy.csv')
twodimhard<- read.csv('A.hard.csv')
wine<-read.csv("wine.csv")

Install.package:
Before run program, user need to intall.package( ggplot2, SpatialTools, gridExtra,cowplot)into R, and then call the package by using library(). These package are very helpful for generate plots with following code.
install.packages(ggplot2) 
library(ggplot2) 
install.packages("SpatialTools")
library(SpatialTools) 
install.packages("gridExtra")
library("gridExtra")
install.packages("cowplot")
library("cowplot")


Output data:
For output data, I use print() to print the data on window. And used write.csv() to  write the output into the work directory. User can review the output on working directory after run the program .eg.
write.csv(Kis4.hard1.output, "kis4.output.hard.csv", row.names=TRUE)
