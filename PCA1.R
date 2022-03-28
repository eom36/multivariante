rm(list=ls())
#install.packages("datos")
library(datos)
datos::diamantes
view(diamantes)
str(diamantes)
x=diamantes [,c(1,2,6,7,8,9,10)]
x
# 3.- Se definen n (numero de estados) y p (variables)
dim(x)

n<-dim(x)[1]
p<-dim(x)[2]


# 4.- Generación de un scatterplot
# de las variables originales
pairs(x,col="blue", pch=19, 
     main="Variables originales")

#5.- Obtención de los componentes principales
# con base en la matriz de covarianza muestral
mu<-colMeans(x)
s<-cov(x)
s

#6.- Obtención de los componentes principales 
# con base a la matriz de covarianza muestral
es<-eigen(s)
es

# 7.- Matriz de auto-valores
eigen.val<-es$values

# 8.- Matriz de auto-vectores
eigen.vec<-es$vectors

# Proporción de variabilidad para cada vector
pro.var<-eigen.val/sum(eigen.val)

# Proporción de variabilidad acumulada
pro.var.acum<-cumsum(eigen.val)/sum(eigen.val)

#----------------------------------
# Obtencion de los componentes principales con
# base en la matriz de correlaciones muestrales
#---------------------------------------

R<-cor(x)
eR<-eigen(R)
eR

# Obtención de auto-valores
eigen.val<-eR$values

# Obtención de auto-vectores
eigen.vec<-eR$vectors

# Proporcion de variablidad
pro.var<-eigen.val/sum(eigen.val)

# Proporcion de variabilidad acumulada
pro.var.acum<-cumsum(eigen.val)/sum(eigen.val)

# Media de los auto-valores
mean(eigen.val)

#---------------------------
# Obtencion de los coeficientes (nuevas variables)
# 
#--------------------------

# 1.- Centrar los datos con respecto a la media
ones<-matrix(rep(1,n),nrow=n, ncol=1)
ones
# 2.- Construccion de la matriz centrada
X.cen<-as.matrix(x)-ones%*%mu
X.cen

# 3.- Construccion de la matriz diagonal de las 
# varianzas
Dx<-diag(diag(s))
Dx

# 4.- Construccion de la matriz centrada multiplicada
# por Dx^1/2

Y<-X.cen%*%solve(Dx)^(1/2)
Y #datos normalizados

# 5.- Construccion de los coeficientes o scores
# eigen.vec matriz de autovectores
scores<-Y%*%eigen.vec

# Nombramos las columnas PC1...PC8
colnames(scores)<-c("PC1","PC2","PC3","PC4","PC5",
                    "PC6", "PC7")

# visualizamos
scores

# Generacion del grafico de los scores
pairs(scores, main="scores", col="blue", pch=19)

#--------------------------------------
#  PCA sintetizado
#-------------------------------------

View(x)
head(x)

# Aplicar el cálculo de la varianza a las columnas 
# 1=filas, 2=columnas
apply(x, 2, var)

# centrado por la media y escalada por 
# la desviacion standar (dividir entre sd).

acp<-prcomp(x, center=TRUE, scale=TRUE)
acp

# Generación del gráfico screeplot
plot(acp, type="l")

# Visualizar el resumen
summary(acp)