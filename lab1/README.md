There are two input datasets that you need to testh your algorithm:
1. Iris data
2. Income_New data

This algorithm is for  data preprocessing of  two datasets and calculating the manhattan Distance and Euclidean distance. This program will outputs the k row ids for those examples which are closest to the given example (as measured by a chosen distance function - see below) with their distances.

This algortihm will handle these data issues:
1) Both categorical and continuous attributes which nominal, ordinal, ratio
and interval.
2) Missing values and outliers
3) Attributes of different scales - please implement appropriate attribute
transformation methods
4) Implement 2 different distance measures
5) Both datasets have a ‘class’ attribute - please do NOT use this attribute
in the distance function
6) The parameter k should be variable, but in this exercise use k = 5.

Run the code:
Rscript  Iris.R


To view the output:
The output of distance will be exported to manhattandistance.csv and euclideandistance.csv. 
These two files will appear on your current work directory.
