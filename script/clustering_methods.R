# site: https://www.kaggle.com/arjunbhasin2013/ccdata

# Format
options("scipen" = 100, "digits" = 2)


# Load the file
cc_general <- read.table("data/CC_General.csv",
                         header = TRUE,
                         sep = ",",
                         dec = ".",
                         row.names = 1)


# Load Functions  

source("functions/stats.R")
source("functions/normalize.R")

## Basic statistics


vars <- c("BALANCE",
          "BALANCE_FREQUENCY",
          "PURCHASES",
          "ONEOFF_PURCHASES",
          "INSTALLMENTS_PURCHASES",          
          "CASH_ADVANCE",
          "PURCHASES_FREQUENCY",
          "ONEOFF_PURCHASES_FREQUENCY",
          "PURCHASES_INSTALLMENTS_FREQUENCY",
          "CASH_ADVANCE_FREQUENCY",
          "CASH_ADVANCE_TRX",
          "PURCHASES_TRX",
          "CREDIT_LIMIT",
          "PAYMENTS",
          "MINIMUM_PAYMENTS",
          "PRC_FULL_PAYMENT",
          "TENURE")


describe_stats <- t(data.frame(apply(cc_general[vars], 2, cc_stats)))
#describe_stats

# many outliers, but they can represent the true behavior of customer. We leave that

#outliers

library(lattice)
bwplot(~BALANCE, data = cc_general)
### stats <- boxplot.stats(cc_general$BALANCE)
### stats


# With respect a NA, they are not in the same row

## NOT IN THE SAME ROW

sum(is.na(cc_general$CREDIT_LIMIT) & is.na(cc_general$MINIMUM_PAYMENTS))

## Strategy for unknown values

### remove the cases
### fill in the unknowns using some strategy
### use tools that handle these types of values


## Proportion Unknow values

nas <- cc_general[which(is.na(cc_general$CREDIT_LIMIT) | is.na(cc_general$MINIMUM_PAYMENTS)), c("CREDIT_LIMIT", "MINIMUM_PAYMENTS")]

## Delete from the data set

cc_general <- cc_general[-which(is.na(cc_general$CREDIT_LIMIT) | is.na(cc_general$MINIMUM_PAYMENTS)),]

summary(cc_general)
str(cc_general)

# Exploratory analysis
library(corrplot)

MC <- cor(cc_general)
corrplot(MC, type ="upper")

#####################################
#CJ
#####################################

# 1 normalize for BD

cc_general_norm <- data.frame(apply(cc_general[vars], 2, normalize))

model_HC <- hclust(dist(cc_general_norm), method = "complete")

plot(model_HC, labels = FALSE)
rect.hclust(model_HC, k = 6, border = "blue")

## sAVE DATA WITH THE GROUP

cluster <- cutree(model_HC, k = 6)

cc_general_cluster <-cbind(cc_general_norm, cluster)

library(rattle)

cc_general_center <- centers.hclust(cc_general_norm,
                                    model_HC,
                                    nclust = 6,
                                    use.median = FALSE)

barplot(cc_general_center[1,],las = 2)
barplot(cc_general_center[6,],las = 2)


barplot(t(cc_general_center),
        beside = TRUE,
        main = "Cluster Interpretation")


library(fmsb)


center  <- as.data.frame(cc_general_center)
maximos <- apply(center,2,max)
minimos <- apply(center,2,min)
center  <- rbind(minimos,center)
center  <- rbind(maximos,center)


radarchart(center,
           maxmin = TRUE,
           axistype = 4,
           axislabcol = "slategray4",
           centerzero = FALSE,
           seg = 8,
           cglcol = "gray67",
           pcol=c("green","blue","red", "purple", "black", "brown"),
           plty = 1,
           plwd = 5,
           title = "Comparación de clústeres")


legenda <-legend(1.5,1, legend=c("Cluster 1","Cluster 2","Cluster 3",
                                 "Cluster 4","Cluster 5","Cluster 6"),
                 seg.len=-1.4,
                 title="Clústeres",
                 pch=21, 
                 bty="n" ,lwd=3, y.intersp=1, horiz=FALSE,
                 col=c("green","blue","red", "purple", "black", "brown")
                 )



# Convert hclust into a dendrogram and plot
hcd <- as.dendrogram(modelo)

# Default plot
plot(hcd, type = "rectangle", ylab = "Height")

plot(hcd, ylim = c(5000,20000))



library("ggplot2")
library("ggdendro")
library("dendextend")

ggdendrogram(modelo)




#####################################
#k-means
#####################################

library(snow)

## How many k?

cl <- makeCluster(4, type="SOCK")

# carga el paquete MASS en cada peón
# haciendo visibles la Tabla de Datos Boston en cada peón o procesador

ignore <- clusterEvalQ(cl, {library(MASS); NULL}) 

  #Hartigan-Wong
  results_HW <- lapply(seq(1,30), function(x) kmeans(cc_general,
                                                     centers = x,
                                                     algorithm = "Hartigan-Wong",
                                                     nstart = 20))
  
  variance_HW <- sapply(results_HW, function(results_HW) results_HW$tot.withinss)
  
  # MacQueen
  results_MQ <- lapply(seq(1,30), function(x) kmeans(cc_general,
                                                     centers = x,
                                                     algorithm = "MacQueen",
                                                     nstart = 20))
  
  variance_MQ <- sapply(results_MQ, function(results_MQ) results_MQ$tot.withinss)
  
  # Lloyd
  results_Ll <- lapply(seq(1,30), function(x) kmeans(cc_general,
                                                     centers = x,
                                                     algorithm = "Lloyd",
                                                     nstart = 20))
  
  variance_Ll <- sapply(results_Ll, function(results_Ll) results_Ll$tot.withinss)
  
  # Forgy
  results_Fg <- lapply(seq(1,30), function(x) kmeans(cc_general,
                                                     centers = x,
                                                     algorithm = "Forgy",
                                                     nstart = 20))
  
  variance_Fg <- sapply(results_Fg, function(results_Fg) results_Fg$tot.withinss)

stopCluster(cl)

plot(variance_HW, col = "red", type = "b", xlab = "No. of centers", ylab = "Variance", main = "Elbow Method: No. of clusters by algorithm")
points(variance_MQ, col = "blue", type = "b")
points(variance_Ll, col = "green", type = "b")
points(variance_Fg, col = "magenta", type = "b")
legend("topright",
       legend = c("Hartigan","MacQueen","Lloyd","Forgy"), 
       col = c("red", "blue", "green", "magenta"), 
       lty = 1, 
       lwd = 1)


## Which method:


# Hartigan-Wong
results_HW <- lapply(seq(1,30), function(x) kmeans(cc_general, 
                                                   centers = 8,
                                                   nstart = 20,
                                                   algorithm = "Hartigan-Wong"))
betweenss_HW <- sapply(results_HW, function(results_HW) results_HW$betweenss)

# MacQueen
results_MQ <- lapply(seq(1,30), function(x) kmeans(cc_general, 
                                                   centers = 8,
                                                   nstart = 20,
                                                   algorithm = "MacQueen"))
betweenss_MQ <- sapply(results_MQ, function(results_MQ) results_MQ$betweenss)


# Lloyd
results_Ll <- lapply(seq(1,30), function(x) kmeans(cc_general, 
                                                   centers = 8,
                                                   nstart = 20,
                                                   algorithm = "Lloyd"))
betweenss_Ll <- sapply(results_Ll, function(results_Ll) results_Ll$betweenss)


# Forgy
results_Fg <- lapply(seq(1,30), function(x) kmeans(cc_general, 
                                                   centers = 8,
                                                   nstart = 20,
                                                   algorithm = "Forgy"))
betweenss_Fg <- sapply(results_Fg, function(results_Fg) results_Fg$betweenss)


cbind(Method= "Hartigan-Wong", AVG_Between_Class =  mean(betweenss_HW))
mean(betweenss_MQ)
mean(betweenss_Ll)
mean(betweenss_Fg)

AVG_Between_Class <- data.frame(Method = c("Hartigan-Wong", "MacQueen", "Lloyd", "Forgy"),
                                AVG_Between_Class = c(mean(betweenss_HW), mean(betweenss_MQ),
                                                mean(betweenss_Ll),mean(betweenss_Fg))
                               )

## With Forgy method

Cluster_FG <- kmeans(x = cc_general,
                     centers =  8,
                     iter.max = 100,
                     nstart = 20,
                     algorithm = "Forgy")

Cluster_FG

# Points in each cluster
Cluster_FG$size

#Cluster center
Cluster_FG$centers


barplot(t(Cluster_FG$centers),
        main = "Centers by cluster",
        xlab = "Cluster",
        beside = TRUE, 
        col = rainbow(17))




#PCA

library(FactoMineR)
cc_general_PCA <-  PCA(cc_general, scale.unit = TRUE, graph = FALSE)


cos2_IND <- (cc_general_PCA$ind$cos2[,1] + cc_general_PCA$ind$cos2[,2])*100
# Individuos mal representado
cos2_IND[cos2_IND < 10]


cos2_VAR <- (cc_general_PCA$var$cos2[,1] + cc_general_PCA$var$cos2[,2])*100
# Variable mal representadas
cos2_VAR[cos2_VAR < 10]


res_hcpc <- HCPC(cc_general_PCA, nb.clust = -1, consol = TRUE, min=3, max=3, graph=FALSE)

cluster <- res_hcpc$data.clust[,ncol(res_hcpc$data.clust), drop=F]

cc_general_Cluster <- cbind(cc_general, cluster)

str(cc_general_Cluster)

cc_general_PCA_Cluster <- PCA(cc_general_Cluster, quali.sup= 10, scale.unit = TRUE, ncp = 9, graph = FALSE)
#plot(res.pca,choix="ind",habillage=10,select="cos2 0.1")
plot(cc_general_PCA, axes=c(1, 2), choix="ind",habillage=10,new.plot=TRUE,select="cos2 0.1")
