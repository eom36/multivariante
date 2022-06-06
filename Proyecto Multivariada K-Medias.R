
data("USArrests")
arrestos =USArrests
# Exploración de la matriz 
dim(arrestos)
names(arrestos)
str(arrestos)
anyNA(arrestos)


#---------------------------------
#    Metodo k-means
#---------------------------------

#1.- Separacion de filas y columnas.

n<-dim(arrestos)[1]
p<-dim(arrestos[2])

# 2.- Estandarizacion univariante.
a.s<-scale(arrestos)

# 3.- Algoritmo k-medias (2 grupos)
# nstar es cantidad de subconjuntos aleatorios que se escogen
# para realizar los calculos de algoritmo.
# el 3 es el nmero de clouster o de agrpupaciones, en este caso se utilizan 3
Kmeans.2<-kmeans(arrestos, 2, nstart=25)

# centroides
Kmeans.2$centers

# cluster de pertenencia
Kmeans.2$cluster

# 4.- SCDG
#hasta aqui llego el minimo de scdg la idea es llegar a 0 
SCDG<-sum(Kmeans.2$withinss)
SCDG

# 5.- Clusters
cl.kmeans<-Kmeans.2$cluster
cl.kmeans

# 6.- Scatter plot con la division de grupos
# obtenidos (se utiliza la matriz de datos centrados).
col.cluster<-c("blue", "red")[cl.kmeans]
pairs(a.s, col=col.cluster, main="k-medias", pch=23)

#-----------------------------------------
#  Visualizacion con las dos componentes principales
#-----------------------------------------
#
library(cluster) 

clusplot(a.s, cl.kmeans, 
         main="Dos primeras componentes principales")

text(princomp(a.s)$score[,1:2],
     labels=rownames(a.s), pos=1, col="blue")
# de aqui se puede tomar la descicio para aumentar el numero de clousters
#-------------------------------------
#  Silhouette
#--------------------------------------
# Representacion grafica de la eficacia de
# clasificacion de una observacion dentro de un
# grupo.

# 1.- Generacion de los calculos
dist.Euc<-dist(a.s, method = "euclidean")
Sil.kmeans<-silhouette(cl.kmeans, dist.Euc)

#2.- Generacion del grafico
#los ultimos numeros de la derecha son la probabilidad si es bajo es decir que la clasificacion es baja 
plot(Sil.kmeans, main="Silhouette para k-medias", 
     col="blue")
