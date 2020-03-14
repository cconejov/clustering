library(MASS)

summary(Boston)


plot(Boston)



grupos <- kmeans(Boston,
                 centers = 2,
                 iter.max = 100)  ## iter.max por defecto es 10, center es grupos
grupos

grupos$cluster
grupos$centers


grupos$totss         # Inercia Total
grupos$withinss      # Inercia Intra-clases por grupo (una para cada grupo)
grupos$size


#plot(grupos$centers[,c(1,2)],pch=19,col="blue",cex=2)
#points(Boston[,c(1,2)], pch=19)

# How many clusters? jambu elbow

InerciaIC <- rep(0,30)

for(k in 1:30) {

    grupos <-  kmeans(Boston, k, nstart = 25 )
    InerciaIC[k]<-grupos$tot.withinss
}

plot(InerciaIC,col="blue",type="b")


# apply function
resultados <- lapply(seq(1,30), function(x) kmeans(Boston, x, nstart = 200))
inercias <- sapply(resultados, function(resultados) resultados$tot.withinss)
plot(inercias,col="red",type="b")

# pararel

library(snow)

# usaremos 4 "peones" en una máquina local usando un "socket"
cl <- makeCluster(4, type="SOCK")

# carga el paquete MASS en cada peón
# haciendo visibles la Tabla de Datos Boston en cada peón o procesador
ignore <- clusterEvalQ(cl, {library(MASS); NULL}) 
resultados <- lapply(seq(1,30), function(x) kmeans(Boston, x, nstart = 200))
inercias <- sapply(resultados, function(resultados) resultados$tot.withinss)

stopCluster(cl)

plot(inercias,col="red",type="b")


# nstart=Número de muestras aleatorias iniciales
# Repite 4 veces kmeans con nstart=25 con un "for", o sea código tipo estructurado
resultados <- list()
inercias   <- rep(0,4)

for(k in 1:4) {

    resultados[[k]] <- kmeans(Boston, 4, nstart=25) 
    inercias[k]     <- resultados[[k]]$tot.withinss
}
resultados <- resultados[[which.min(inercias)]]
inercias 



kk <- kmeans(Boston, 4, nstart=25)
kk$tot.withins


InerciaIC.Hartigan = rep(0,30)

for(k in 1:30) {
  grupos = kmeans(Boston,k,iter.max=100,algorithm = "Hartigan-Wong")
  InerciaIC.Hartigan[k] = grupos$tot.withinss

}

plot(InerciaIC.Hartigan,col="blue",type="b")