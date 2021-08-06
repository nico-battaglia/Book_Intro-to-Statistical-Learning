states=row.names(USArrests)
states
names(USArrests)
str(USArrests)

# aplicamos funcion por columnas "2" a dataFrame USArrest
apply(USArrests, 2, mean)
apply(USArrests, 2, var)
# vemos que las variables tienen mucha diferencia entre medias y varianzas
# La variable Assault es la de mayor mean y var, es preciso standarizar antes de aplicar PCA

#PCA usando funcion: prcomp y scale=TRUE (media=0 y var=1)
pr.out=prcomp(USArrests, scale. = TRUE)
pr.out
names(pr.out)

pr.out$center   #means before scaling
pr.out$scale    #dev standard before scaling

pr.out$rotation #Los principal components loadings. Cada col es el loading vector 

dim(pr.out$x)   #x tiene por col los score vectors. Zi=þ1*X1+þ2*X2+þ3*X3+þ4*X4

biplot(pr.out, scale = 0)
 
pr.out$sdev   #std dev of each principal component
pr.var= pr.out$sdev^2
pve=pr.var/sum(pr.var)  #porcentaje de la varianza explicada por cada ppal comp

plot(pve)
pve

plot(pve, 
     xlab="Principal Component", 
     ylab="Proportion of Variance explained", ylim=c(0,1), type="b")

plot(cumsum(pve), 
     xlab="Principal Component", 
     ylab="Cumulative prop. of Var. explained", ylim=c(0,1), type="b")


#####################################################################################
## K-Means
#####################################################################################
## func kmeans()  , first in a simulated example where are k=2
## nstart=nro es la cantidad de multiples inicios al azar, se queda con el de mejor resultado.

set.seed(2)
x=matrix(rnorm(50*2), ncol=2)
x[1:25, 1] = x[1:25, 1] + 3
x[1:25, 1] = x[1:25, 1] - 4

plot(x[,1], x[,2])

## k=2
km.out = kmeans(x, 2, nstart = 50)
km.out

km.out$cluster
plot(x, col=(km.out$cluster), main = "K-Means Clustering Results with K=2",
     pch=20, cex=2)


set.seed(2)
## k=3
km.out.3 = kmeans(x, 3, nstart = 50)
km.out.3
plot(x, col=(km.out.3$cluster), main = "K-Means Clustering Results with K=3",
     pch=20, cex=2)


#####################################################################################
## Hierarchical Clustering
#####################################################################################
## hclust(disimilarity structure, method=)

hc.complete = hclust(dist(x), method = "complete")
hc.average = hclust(dist(x), method = "average")
hc.single = hclust(dist(x), method = "single")

par(mfrow=c(1,3))
plot(hc.complete, main = "Complete Linkage")
plot(hc.average, main = "Average Linkage")
plot(hc.single, main = "Single Linkage")

#func cutree para determinar los labels para un determinado corte del dendograma
cutree(hc.complete, 2)


#####################################################################################
## PCA and Clustering on NCI60 data
## NCI60 cancer cell array --> 64 cancer cell lines of 6830 genes expressions
#####################################################################################

library(ISLR)
nci.labs=NCI60$labs
nci.data=NCI60$data

dim(nci.data)
str(NCI60)

table(nci.labs)

##  PCA
############

pr.out = prcomp(nci.data, scale. = TRUE)

#funcion para asignar colores a cada una de las 64 lines segun el tipo de cancer que corresp.
Cols = function(vec){
  cols=rainbow(length(unique(vec)))
  return(cols[as.numeric(as.factor(vec))])
}

## graficamos en funcion de los tres primeros ppal comp Z1, Z2, Z3
par(mfrow=c(1,2))
plot(pr.out$x[,1:2], col=Cols(nci.labs), pch=19, xlab="Z1", ylab="Z2")
plot(pr.out$x[,c(1,3)], col=Cols(nci.labs), pch=19, xlab="Z1", ylab="Z3")

par(mfrow=c(1,1))
summary(pr.out)  #tira el Cumulative Prop of Variance explained
plot(pr.out)

par(mfrow=c(1,2))
names(summary(pr.out))
plot(summary(pr.out)$importance[3,] )  #acumulado
plot(summary(pr.out)$importance[2,] )  #pve por Zi


##  Clustering on NCI60 data
##############################

sd.data = scale(nci.data)  #solosi quisieramos escalar datos

par(mfrow=c(1,3))
data.dist = dist(sd.data)

plot(hclust(data.dist), labels = nci.labs, main = "Complete linkage")
plot(hclust(data.dist, method = "average"), labels = nci.labs, main = "Average linkage")
plot(hclust(data.dist, method = "single"), labels = nci.labs, main = "Single linkage")

##analisis sobre complete linkage
hc.out = hclust(dist(sd.data))
hc.clusters = cutree(hc.out, 4)  #cortamos dendograma a una altura para que nos de k=4
table(hc.clusters, nci.labs)    #vemos que todos lo de leukemia caen en cluster 3 por ej.

par(mfrow=c(1,1))
plot(hc.out, labels = nci.labs)
abline(h=139, col="red")   #cortamos el dendograma a la altura que da k=4

hc.out

#comparando con los 4 cluster que obtendriamos por K-means vemos que son diferentes
set.seed(2)
km.out= kmeans(sd.data, 4, nstart = 20)
km.clusters = km.out$cluster
table(km.clusters, hc.clusters)
#cluster 4 en km es igual al 3 en hc. pero el resto difieren. El metodo varia

##Ahora performing Hierarchical Clust in Ppal Components
hc.out = hclust(dist(pr.out$x[,1:5]))
plot(hc.out, labels = nci.labs, main = "Hier. Clust in first 5 score vects")
table(cutree(hc.out, 4), nci.labs)
