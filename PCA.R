#loading the data
wine <- read.csv(choose.files())
summary(wine)
str(wine)

#building pca summary
w.pca <- princomp(wine[,-1],cor = TRUE,scores = TRUE,covmat = NULL)
summary(w.pca)
#############################################################
#Importance of components:
#  Comp.1    Comp.2    Comp.3    Comp.4     Comp.5     Comp.6     Comp.7
#Standard deviation     2.1692972 1.5801816 1.2025273 0.9586313 0.92370351 0.80103498 0.74231281
#Proportion of Variance 0.3619885 0.1920749 0.1112363 0.0706903 0.06563294 0.04935823 0.04238679
#Cumulative Proportion  0.3619885 0.5540634 0.6652997 0.7359900 0.80162293 0.85098116 0.89336795
#Comp.8     Comp.9    Comp.10    Comp.11    Comp.12     Comp.13
#Standard deviation     0.59033665 0.53747553 0.50090167 0.47517222 0.41081655 0.321524394
#Proportion of Variance 0.02680749 0.02222153 0.01930019 0.01736836 0.01298233 0.007952149
#Cumulative Proportion  0.92017544 0.94239698 0.96169717 0.97906553 0.99204785 1.000000000
####################################################################

# as we can see first 7 variables contribute to 90% of information required for the data.
#hence 13 variables will be reduced to 7 for further analysis.

plot(w.pca)
biplot(w.pca)

wine_pca<- w.pca$scores [,1:7] # as the first 7 components represents the whole data

#cbind is used to bind the data coloumn wise
#considering the top 7 components and combinding them with wines dat


# determinig the no of cluster 
#install.packages('NbClust')
library(NbClust)
no_of_Clusters = NbClust(wine_pca, distance = "euclidean", min.nc = 2, max.nc = 10,
                         method = "complete", index ="all")

#According to the majority rule, the best number of clusters is  7


#hyrarchial clustering

install.packages('factoextra')
library(factoextra)
hclust.complete = eclust(wine_pca, "hclust", k = 7, method = "complete", graph = FALSE) 
fviz_dend(hclust.complete, rect = TRUE, show_labels = FALSE)

# k-means clustering

km.7 = eclust(wine_pca, "kmeans", k =5, nstart = 25, graph = FALSE)
fviz_cluster(km.7, geom = "point", frame.type = "norm")

