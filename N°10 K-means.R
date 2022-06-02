
#__________ K-MEANS____________

# Cargar la matriz de datos.
#aqui se cnsieran las medianas
#busca k objetos representativos

X<-as.data.frame(state.x77)
#X
#-------------------------------------
#     Transformacion de datos
#-------------------------------------

#1.- Transformacion de las variables x1,x3 y x8
# con la funcion de logaritmo.

X[,1]<-log(X[,1])
colnames(X)[1]<-"Log-Population"

X[,3]<-log(X[,3])
colnames(X)[3]<-"Log-Illiteracy"

X[,8]<-log(X[,8])
colnames(X)[8]<-"Log-Area"

#---------------------------------
#    Metodo k-means
#---------------------------------

#1.- Separacion de filas y columnas.

dim(X)
n<-dim(X)[1]
p<-dim(X[2])

# 2.- Estandarizacion univariante.
X.s<-scale(X)

# 3.- Algoritmo k-medias (6 grupos)
# nstar es cantidad de subconjuntos aleatorios que se escogen
# para realizar los calculos de algoritmo.
# el 3 es el nmero de clouster o de agrpupaciones, en este caso se utilizan 3
Kmeans.6<-kmeans(X.s, 6, nstart=25)

# centroides
Kmeans.6$centers

# cluster de pertenencia
Kmeans.6$cluster


# 4.- SCDG
#hasta aqui llego el minimo de scdg la idea es llegar a 0 
SCDG<-sum(Kmeans.6$withinss)
SCDG

# 5.- Clusters
cl.kmeans<-Kmeans.6$cluster
cl.kmeans

# 6.- Scatter plot con la division de grupos
# obtenidos (se utiliza la matriz de datos centrados)
col.cluster<-c("blue", "red", "green","brown","darkblue","cyan")[cl.kmeans]
pairs(X.s, col=col.cluster, main="k-means", pch=19)

#-----------------------------------------
#  Visualizacion con las dos componentes principales
#-----------------------------------------
#
library(cluster) 

clusplot(X.s, cl.kmeans, 
         main="Dos primeras componentes principales")

text(princomp(X.s)$score[,1:2],
     labels=rownames(X.s), pos=1, col="blue")

#-----------------------------------------
#  Visualizacion con las dos componentes principales
#-----------------------------------------
#
library(cluster) 

clusplot(X.s, cl.kmeans, 
         main="Dos primeras componentes principales")

text(princomp(X.s)$score[,1:2],
     labels=rownames(X.s), pos=1, col="blue")

# de aqui se puede tomar la descicio para aumentar el numero de clousters
#-------------------------------------
#  Silhouette
#--------------------------------------
# Representacion grafica de la eficacia de
# clasificacion de una observacion dentro de un
# grupo.

# 1.- Generacion de los calculos
dist.Euc<-dist(X.s, method = "euclidean")
#el cl.kmeans es dode se se encuentran los closters 
Sil.kmeans<-silhouette(cl.kmeans, dist.Euc)

#2.- Generacion del grafico
#los ultimos numeros de la derecha son la probabilidad si es bajo es decir que la clasificacion es baja 
plot(Sil.kmeans, main="Silhouette for k-means", 
     col="blue")



#replicar el scrip y sugerir un numeo diferente de closter que no sea ni 3 ni 1 

#se utilizo un nuevo numero de clousters en este caso fueron 6, y se disminuyo significativamente la suma de cuadrados dentro del grupo 
#pero la probailidad de agrupamiento es muy baja para la mayoria de los grupos, el unico mas significativo es 6 y 4
# no es probabilidad, es el ancho de silhouet el promedio de siluedt debe ser alto, en este caso es de 0.27 por lo que se debe busacar otro numero de clousters 
# se recomienda bajar el numero de clouster a 2 y volver a correr el codigo 

