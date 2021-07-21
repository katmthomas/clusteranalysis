######################################
### Scaled Cluster Kmeans for Mall ###
rm(list=ls())
cat('\f')
library(cluster)
library(factoextra)
library(ggplot2)
library(tidyverse)
library(caret)
library(readr)
library(dplyr)
library(scatterplot3d)
library(hrbrthemes)
library(dendextend)
mall <-  read_csv("MGSC 7330 R/Mall_Customers.csv")

    #Checking the Data#

summary(mall)
str(mall)

newcolnames <- c('id',
                 'Gender',
                 'Age',
                 'Income',
                 'Score')
names(mall) <- newcolnames
colSums(is.na(mall))
summary(mall)
##################################
#####EXPLORATORY ANALYSIS#########
#################################
#Visualization to Check for Correlation#

  #Income and Score correlation to gender#

plot1 <- mall %>% 
  ggplot(aes(x = Income,
             y = Score,
             colour = Gender)) +
  geom_point(size = 2,
             alpha = 0.6,
             show.legend = FALSE) +
  labs(title = 'Scatterplots',
       subtitle = ' Income - Score') 
plot1

  #Income and Age correlation to gender#
plot2 <- mall %>% 
  ggplot(aes(x = Income,
             y = Age,
             colour = Gender)) +
  geom_point(size = 2,
             alpha = 0.6,
             show.legend = FALSE) +
  labs(title = 'Scatterplots',
       subtitle = 'Income - Age')
plot2
  #Age and Score correlation to gender#
plot3<- mall %>% 
  ggplot(aes(x = Score,
             y = Age,
             colour = Gender)) +
  geom_point(size = 2,
             alpha = 0.6,
             show.legend = FALSE) +
  labs(title = 'Scatterplots',
       subtitle = 'Age- Score')
plot3

  #Visualizing Distribution of Variables by Gender#
v1 <- mall %>% ggplot() + geom_histogram(mapping = aes(x = Age, fill = Gender),bins = 10, colour = "black") + 
  ggtitle("Distribution of Age")+ xlab("Age") + ylab("Total Count")
v1

v2 <- mall %>% ggplot() + geom_histogram(mapping = aes(x = Income, fill = Gender),bins = 10, colour = "black") + 
  ggtitle("Distribution of Annual Income (in thousands)")+ xlab("Annual Income (k$)") + ylab("Total Count") 
v2

v3 <- mall %>% ggplot() + geom_histogram(mapping = aes(x = Score, fill = Gender),bins = 10, colour = "black") + 
  ggtitle("Distribution of Score")+ xlab("Spending Score") + ylab("Total Count")
v3

################################
#####Cluster Analysis##########
###############################

  #Histograms for Outliers check#

#AGE#
hist(mall$Age)
summary(mall$Age)
# Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
#18.00   28.75   36.00   38.85   49.00   70.00 

#INCOME#
hist(mall$Income)
summary(mall$Income)
#Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
#15.00   41.50   61.50   60.56   78.00  137.00

#SCORE#
hist(mall$Score)
summary(mall$Score)
# Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
#1.00   34.75   50.00   50.20   73.00   99.00 

  ##Age and Income are positively skewed left for their distributions; thus we must rescale and standardize##
##Score can be considered normal distrib. because mean(50.20) is almost = to median(50.00); thus we only need to standardize##

#---------------------------------------#
#Transformation/Standardization AGE#
mall$Age.l<-sqrt(mall$Age)
summary(mall$Age.l)
  #Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
  #4.243   5.362   6.000   6.133   7.000   8.367 
hist(mall$Age.l)

#Standardizing AGE#
mall$Age.l<-(mall$Age.l-mean(mall$Age.l))/sd(mall$Age.l)
summary(mall$Age.l)
#   Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
#-1.6964 -0.6921 -0.1194  0.0000  0.7780  2.0043

hist(mall$Age.l)
sd(mall$Age.ls) #1
mean(mall$Age.ls) #0

  #TRANSFORMING/STANDARDIZING INCOME#

mall$Income.l <- sqrt(mall$Income)
summary(mall$Income.l)
#Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
#3.873   6.442   7.842   7.581   8.832  11.705 
hist(mall$Income.l)

#Standardization Income#
mall$Income.l<-(mall$Income.l-mean(mall$Income.l))/sd(mall$Income.l)
summary(mall$Income.l)
#Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
#-2.1061 -0.6472  0.1482  0.0000  0.7102  2.3419 
hist(mall$Income.l)
sd(mall$Income.l) #0


  #STANDARDIZING SCORE#

mall$Score.l <- (mall$Score-mean(mall$Score))/sd(mall$Score)
summary(mall$Score.l)
#Min.   1st Qu.    Median      Mean   3rd Qu.      Max. 
#-1.905240 -0.598292 -0.007745  0.000000  0.882916  1.889750 
hist(mall$Score.l)


#creating database with non-scaled/non-standardized variables#
mall2 <- mall[-c(6:8)]
#####################################
  ##Cluster Analysis for all data##
#####################################

  ##Method 1: using non-scaled/non-standardized variables##

df <- mall2[,3:5]
set.seed(123)
fviz_nbclust(df, kmeans, method = "wss")

# Elbow method indicates k = 5##
cldf <- kmeans(df,5, nstart = 10)

#Plot the clusters#
with(df, scatterplot3d(Age, Score, Income, type = "p", color = cldf$cluster))

#Add cluster to the dataset
mall2$Cluster<-cldf$cluster
table(mall2$Cluster)
#segment sizes
table(cldf$size)

#Boxplots of clusters#
boxplot(Income~Cluster,data=mall2)
boxplot(Age~Cluster,data=mall2)
boxplot(Score~Clusterdata=mall2)

#---------------------------------------------------#
  ## Method2: Using Scaled/Standardized Variables ##
dfl <- mall[c(6:8)]
set.seed(123)
fviz_nbclust(dfl, kmeans, method = "wss")

# Elbow method indicates k = 4##
cldfl <- kmeans(dfl,4, nstart = 10)

##Add clusters to the dataset##
mall$Cluster.l<-cldfl$cluster

  #VISUALIZING METHOD 2: CLUSTERS#

#2d Plot#
#Plotting Cluster to Standardized and Scaled Variables#
fviz_cluster(cldfl, dfl, ellipse.type = "norm")+
  theme_minimal()

#Boxplots of Method 2 Clusters, linked to standardized and scaled variables#
boxplot(Income.l~Cluster.l,data=mall)
boxplot(Age.l~Cluster.l,data=mall)
boxplot(Score.l~Cluster.l,data=mall)

#-------------------------------------------------#
  ## IDENTIFYING BEST CLUSTER ##

sil <- silhouette(cldfl$cluster, dist(dfl))
summary(sil)

#Cluster sizes and average silhouette widths:
#56        39        40        65 
#0.2979642 0.3551465 0.6069432 0.3868270 
fviz_silhouette(sil)



#Finding Negative ave.sil.width
negsilindex <- which(sil[, "sil_width"] < 0)
sil[negsilindex, , drop = FALSE]
#cluster neighbor     sil_width
#[1,]       1        3 -0.1130557845
#[2,]       1        2 -0.0008297148
#[3,]       1        3 -0.0866396306
#[4,]       4        2 -0.0047282404

hc.cut <- hcut(dfl, k = 4, hc_method = "complete")
fviz_dend(hc.cut, show_labels = FALSE, rect = TRUE)
fviz_silhouette(hc.cut)

    #Cross Tabulation##
table(mall$Gender, cldfl$cluster)
#       1  2  3  4
#Female 34 19 22 37
#Male   22 20 18 28
table(mall$Age, cldfl$cluster)
#Output Notes:
#cluser 1: no one age>40(young)
#cluster 2: split over range of age (max = 60)
#cluster 3:no one age > 40(young)
#cluster 4: age>34 (mid to old),
summary(mall$Age)
table(mall$Income, cldfl$cluster)
#cluser 1: salary<=67(low income)
#cluster 2: salary >= 64k-137k (high income)
#cluster 3:no one salary>= 69k(high income)
#cluster 4: salary<=67(low income),
    #outlier in cluster 4, record at 79k
summary(mall$Income)
table(mall$Score, cldfl$cluster)
#cluser 1: outlier at 6,87, 92,94 score: 35-82(equal spread range for score)
#cluster 2: score <= 42 (low score)
#cluster 3:outliers: 58, 63? score>= 68 (high score)
#cluster 4: Score <= 60 (range = 35-60) low score?
summary(mall$Score)




