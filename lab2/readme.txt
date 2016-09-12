This program is mainly to implement K-Nearest Neighbor(KNN) classification algorithm  and test on two datasets—iris_test.csv and income_test.csv. In these cases, K is set as 5, you can change the k value according to requirements. In this lab,I implement two distance method in lab1.

Files:

iris_eucli.R : In this program,k is set as 5. The distance is calculated with euclidean distance.
iris_manhattan.R : In this program,k is set as 5. The distance is calculated with manhattan distance
income_eucli.R: In this program,k is set as 5. The distance is calculated with euclidean distance.
income_manhattan.R : In this program,k is set as 5. The distance is calculated with manhattan distance.
income_reduced_train.R: In this program,k is set as 5. The distance is calculated with euclidean distance. the training data is reduced into 244 objects.


How to run:

1.command line:
Rscript  Filename
eg: Rscript Iris.R

2. Before you run this program, make sure you put the original datasets( Iris.CSV,Iris_Test.CSV )under your current work directory. 

3. Ggplot2:
For gglot2, you need first use following  commandto install the ggplot2 package 
install.packages(gglot2)
library(ggplot2)
                                                                                        

4. Output
 The table which display the transaction ID,Actual class, Predicated Class, Posterior Probability will be exported into iris-euclidean-knn.csv ,income_knn_eucli.csv,income_knn_manh.csv  which under the current work directory. 

The data for drawing the roc curve is exported into roc_curve.csv

The confusion matrix, TPR,FPR, TNR,FNR ,RECALL, Precision, F-measure will be printed on the screen  after you run.