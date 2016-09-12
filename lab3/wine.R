```{R}
  wine<-read.csv("C:/Users/ab/Desktop/wine.csv")
library(ggplot2)
##remove ID and quality 
wine<-wine[,-13]
wine<-wine[,-1]
wine_noclass<-wine[,-12]
summary(wine)
correlation<-cor(wine_noclass, use="all.obs", method="pearson") 
ggplot(data=wine,aes(x=fx_acidity, y=citric_acid))+geom_point()
rnames(wine)
```