
##############################################################
# site: https://www.kaggle.com/arjunbhasin2013/ccdata
##############################################################


##############################################################
# Format
options("scipen" = 100, "digits" = 2)

# seed
set.seed(1234)



##############################################################
# Load the file
##############################################################

cc_general <- read.table("data/CC_General.csv",
                         header = TRUE,
                         sep = ",",
                         dec = ".",
                         row.names = 1)


# Load Functions  

source("functions/stats.R")
source("functions/normalize.R")

##############################################################
## Basic statistics
##############################################################

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

### nas <- cc_general[which(is.na(cc_general$CREDIT_LIMIT) | is.na(cc_general$MINIMUM_PAYMENTS)), c("CREDIT_LIMIT", "MINIMUM_PAYMENTS")]

## Delete from the data set

cc_general <- cc_general[-which(is.na(cc_general$CREDIT_LIMIT) | is.na(cc_general$MINIMUM_PAYMENTS)),]

summary(cc_general)
str(cc_general)


### Scaling and normalize the data
cc_general_norm     <- data.frame(apply(cc_general[vars], 2, normalize))
summary(cc_general_norm)


### Guardar la tabla generada

write.table(x = cc_general_norm,
            file = "output/data/cc_general_norm.csv"
            , sep = ",", dec = ".")


##############################################################
# Exploratory analysis
##############################################################

library(corrplot)

MC <- cor(cc_general)
corrplot(MC, type ="upper")

MC_norm <- cor(cc_general_norm)
corrplot(MC_norm, type ="upper")




##############################################################
#k-means
##############################################################

library(snow)

cl <- makeCluster(4, type="SOCK")
# Read data in each cluster
ignore <- clusterEvalQ(cl, 
                       { data <- read.csv("output/data/cc_general_norm.csv",
                                           header = TRUE,
                                           sep = ",",
                                           dec = "."
                                          )
                                  NULL}) 

# Hartigan-Wong
results_HW <- clusterApply(cl,
                           seq(1,20),
                           function(x) kmeans(data,
                                              centers = x,
                                              algorithm = "Hartigan-Wong",
                                              nstart = 50))

withinss_HW <-  sapply(results_HW, function(results_HW) results_HW$tot.withinss)

# MacQueen
results_MQ <- clusterApply(cl,
                           seq(1,20),
                           function(x) kmeans(data,
                                              centers = x,
                                              algorithm = "MacQueen",
                                              nstart = 50))

withinss_MQ <-  sapply(results_MQ, function(results_MQ) results_MQ$tot.withinss)

# Lloyd
results_Ll <- clusterApply(cl,
                           seq(1,20),
                           function(x) kmeans(data,
                                              centers = x,
                                              algorithm = "Lloyd",
                                              nstart = 50))

withinss_Ll <-  sapply(results_Ll, function(results_Ll) results_Ll$tot.withinss)


# Forgy
results_FG <- clusterApply(cl,
                           seq(1,20),
                           function(x) kmeans(data,
                                              centers = x,
                                              algorithm = "Forgy",
                                              nstart = 50))

withinss_FG <-  sapply(results_FG, function(results_FG) results_FG$tot.withinss)

## end cluster
stopCluster(cl)

plot(withinss_HW, col = "red", type = "b", xlab = "Number of cluster k", ylab = "Sum of squares", main = "Elbow Method: No. of clusters by algorithm")
points(withinss_MQ, col = "blue", type = "b")
points(withinss_Ll, col = "green", type = "b")
points(withinss_FG, col = "magenta", type = "b")
legend("topright",
       legend = c("Hartigan","MacQueen","Lloyd","Forgy"), 
       col = c("red", "blue", "green", "magenta"), 
       lty = 1, 
       lwd = 1)


## 5 groups base on jambu
library(snow)

cl <- makeCluster(4, type="SOCK")
# Read data in each cluster
ignore <- clusterEvalQ(cl, 
                       { data <- read.csv("output/data/cc_general_norm.csv",
                                          header = TRUE,
                                          sep = ",",
                                          dec = "."
                       )
                       set.seed(1234)
                       NULL}) 

# Evaluate all the posible algorithms
results_Algorithm <- clusterApply(cl,
                           c("Hartigan-Wong","MacQueen","Lloyd","Forgy"),
                           function(algorithm) kmeans(data,
                                              centers = 5,
                                              algorithm = algorithm,
                                              nstart = 50))
for(i in 1:4) print(results_Algorithm[[i]]$betweenss)
# end cluster
stopCluster(cl)



## Hartigan-Wong 

cc_general_norm <- read.csv("output/data/cc_general_norm.csv",
header = TRUE,
sep = ",",
dec = "."
)

Cluster_HW <- kmeans(x = cc_general_norm,
                     centers =  5,
                     nstart = 250,
                     algorithm = "Hartigan-Wong")

Cluster_HW

# Points in each cluster
Cluster_HW$size

#Cluster center
colnames(Cluster_HW$centers)


color = grDevices::colors()[grep('gr(a|e)y', grDevices::colors(), invert = T)]
set.seed(1234)
palette <- sample(color, 17)

palette <- c("orange4", "red2", "tomato", "darkseagreen1",  "deeppink3", "steelblue2", "gold1",         
             "wheat", "tan4", "springgreen2", "darkred", "darkseagreen3", "lightskyblue", "darkorange2",   
             "plum3", "darkgoldenrod4", "skyblue1") 



barplot(t(Cluster_HW$centers),
        main = "Centers by cluster K-means clustering",
        xlab = "Cluster",
        beside = TRUE, 
        col = palette,
        ylim = c(0, 1.5)
        )
abline(h = 0.5, col=3, lty=3)
text(5, 0.55, labels= "Center > 0.5", col = "blue", adj = c(0, 0), font=2, cex= 0.8 )

legend("topright", 
       legend = colnames(Cluster_HW$centers),
       fill = palette, ncol = 2,
       cex = 0.6)



##############################################################
#CJ
##############################################################

# 1 normalize for BD

model_avg <- hclust(dist(cc_general_norm), method = "average")

plot(model_avg, labels = FALSE)
rect.hclust(model_avg, k = 4, border = "blue")

###


model_Ward <- hclust(dist(cc_general_norm), method = "ward.D2")

plot(model_Ward, labels = FALSE)
rect.hclust(model_Ward, k = 5, border = "blue")


## sAVE DATA WITH THE GROUP

cluster <- cutree(model_Ward, k = 5)

cc_general_norm_cluster <-cbind(cc_general_norm, cluster)

library(rattle)

cc_general_center <- centers.hclust(cc_general_norm,
                                    model_Ward,
                                    nclust = 5,
                                    use.median = FALSE)


str(cc_general_center)

colnames(cc_general_center)

barplot(t(cc_general_center),
        beside = TRUE,
        main = "Cluster Interpretation",
        col = palette
        )
legend("topright", 
       legend = colnames(cc_general_center),
       fill = palette, ncol = 2,
       cex = 0.6)



## radar graph
center  <- as.data.frame(cc_general_center)
maximos <- apply(center,2,max)
minimos <- apply(center,2,min)
center  <- rbind(minimos,center)
center  <- rbind(maximos,center)

colnames(center) <- c("BAL", "BAL_FREQ", "PURCH", "ONEOFF_PURCH","INST_PURCH.","CASH_ADV","PURCH._FREQ","ONEOFF_PURCH_FREQ","PURCH_INSTALL_FREQ" ,"CASH_ADV_FREQ","CASH_ADV_TRX","PURCH_TRX","CRED_LIM","PAY","MIN_PAY","PRC_PAY","TENURE")



library(fmsb)
radarchart(center,
           maxmin = TRUE,
           axistype = 4,
           axislabcol = "slategray4",
           centerzero = FALSE,
           seg = 8,
           cglcol = "gray67",
           pcol= palette,
           plty = 1,
           plwd = 5,
           title = "Comparacion de clusteres")
legenda <-legend(1.5,1, legend=c("Cluster 1","Cluster 2","Cluster 3", "Cluster 4", "Cluster 5"),
                 seg.len=-1.4,
                 title="Clusteres",
                 pch=21, 
                 bty="n" ,lwd=3, y.intersp=1, horiz=FALSE,
                 col= palette
                 )


install.packages("devtools")
devtools::install_github("ricardo-bion/ggradar")


# Convert hclust into a dendrogram and plot
hcd <- as.dendrogram(modelo)

# Default plot
plot(hcd, type = "rectangle", ylab = "Height")

plot(hcd, ylim = c(5000,20000))



library("ggplot2")
library("ggdendro")
library("dendextend")

ggdendrogram(modelo)




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
