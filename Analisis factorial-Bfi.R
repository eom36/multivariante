# descarga de paquetes y librerias 

#install.packages("psych")
library(psych)


#install.packages("polycor")
library(polycor)
#1.- Lectura de la matriz de datos
x1<-as.data.frame(bfi)
#se seleccionaron las primeras 8 variables 
x= x1[1:200,1:8]

# se eliminaron los NA de la matriz 
x <- na.omit(x)
#3.- Separa n (estados) y p (variables)

n<-dim(x)[1]
p<-dim(x)[2]

#4.- Generacion de un scater plot para la
# visualizaci髇 de variables originales
pairs(x, col="blue", pch=19, main="matriz original")

#---------------------------------
#   Transformaci髇 de algunas varibles
#----------------------------------

#1.- Aplicamos logaritmo para las columnas 1,4 y 8
x[,1]<-log(x[,1])
colnames(x)[1]<-"Log-A1"

x[,4]<-log(x[,4])
colnames(x)[4]<-"Log-A3"

x[,7]<-log(x[,7])
colnames(x)[7]<-"Log-c3"


# Grafico scater para la visualizacion de la 
# matriz original con 3 variables que se incluyeron
pairs(x,col="blue", pch=19, main="Matriz original")


# se va a implementar la matriz de
# correlaciones para estimar la matriz de carga

#-------------------------------------
#   Reduccion de la dimensionalidad 
#  An醠sis Factorial de componentes principales (PCFA)
#-----------------------------------

#1.- Calcular la matriz de medias y de correlaciones
# Matriz de medias
mu<-colMeans(x)
mu

#Matriz de correlaciones
R<-cor(x)
R

# 2.- Reducci贸n de la dimensionalidad mediante
# An谩lisis factorial de componentes principales (PCFA).

# 1.- Calcular los valores y vectores propios.
eR<-eigen(R)

# 2.- Valores propios
eigen.val<-eR$values
eigen.val

# 3.- Vectores propios
eigen.vec<-eR$vectors
eigen.vec

# 4.- Calcular la proporcion de variabilidad
prop.var<-eigen.val/sum(eigen.val)
prop.var

# 5.- Calcular la proporcion de variabilidad acumulada
prop.var.acum<-cumsum(eigen.val)/sum(eigen.val)
prop.var.acum

#-------------------------------
# Estimacion de la matriz de carga
#---------------------------------

# Nota: se estima la matriz de carga usando los 
# autovalores y autovectores.
# se aplica la rotaci贸n varimax

# Primera estimaci贸n de Lamda mayuscula
# se calcula multiplicando la matriz de los 
# 3 primeros autovectores por la matriz diagonal
# formada por la raiz cuadrada de los primeros
# 3 autovalores.

L.est.1<-eigen.vec[,1:3] %*% diag(sqrt(eigen.val[1:3]))
L.est.1

# Rotaci贸n varimax
L.est.1.var<-varimax(L.est.1)
L.est.1.var

#----------------------------
# Estimaci贸n de la matriz de los errores
#-----------------------------

#1.- Estimaci贸n de la matriz de perturbaciones
Psi.est.1<-diag(diag(R-as.matrix(L.est.1.var$loadings)%*% t(as.matrix(L.est.1.var$loadings))))
Psi.est.1

# 2.- Se utiliza el m茅todo An谩lisis de factor principal (PFA)
# para estimaci贸n de autovalores y autovectores
RP<-R-Psi.est.1
RP

# Calculo de la matriz de autovalores y autovectores
eRP<-eigen(RP)

# Autovalores
eigen.val.RP<-eRP$values
eigen.val.RP

# Autovectores
eigen.vec.RP<-eRP$vectors
eigen.val.RP

# Proporcion de variabilidad
prop.var.RP<-eigen.val.RP/ sum(eigen.val.RP)
prop.var.RP

# Proporcion de variabilidad acumulada
prop.var.RP.acum<-cumsum(eigen.val.RP)/ sum(eigen.val.RP)
prop.var.RP.acum

# Estimaci贸n de la matriz de cargas
# con rotaci贸n varimax
L.est.2<-eigen.vec.RP[,1:3] %*% diag(sqrt(eigen.val.RP[1:3]))
L.est.2

# Rotacion varimax
L.est.2.var<-varimax(L.est.2)

# Estimaci贸n de la matriz de covarianzas de los errores.
Psi.est.2<-diag(diag(R-as.matrix(L.est.2.var$loadings)%*% t(as.matrix(L.est.2.var$loadings))))
Psi.est.2

#----------------------------
#   Obtencion de los scores de ambos m茅todos
#------------------------------

# PCFA
FS.est.1<-scale(x)%*% as.matrix(L.est.1.var$loadings)
FS.est.1

# PFA
FS.est.2<-scale(x)%*% as.matrix (L.est.2.var$loadings)
FS.est.2

# graficamos ambos scores
par(mfrow=c(2,1))

# Factor I y II
pl1<-plot(FS.est.1[,1], FS.est.1[,2], xlab="primer factor",
          ylab="segundo factor", main="scores con factor I y II con PCFA",
          pch=19, col="blue")
text(FS.est.1[,1], FS.est.1[,2], labels = rownames(x), pos=4, col="blue")

# Factor I y III
pl2<-plot(FS.est.1[,1], FS.est.1[,3], xlab="Primer factor",
          ylab="Tercer factor", main="scores con factor I y III con PCFA",
          pch=19, col="blue")
text(FS.est.1[,1], FS.est.1[,3], labels = rownames(x), pos=4, col="blue")

# Factor II y III
pl3<-plot(FS.est.1[,2], FS.est.1[,3], xlab="Segundo factor",
          ylab="Tercer factor", main="scores con factor II y III con PCFA",
          pch=19, col="blue")
text(FS.est.1[,2], FS.est.1[,3], labels = rownames(x), pos=4, col="blue")

