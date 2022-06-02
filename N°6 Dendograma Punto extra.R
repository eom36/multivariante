# Dendrograma
# Cargamos librerias
install.packages("cluster.datasets")

library("cluster.datasets")

# Bajamos la matriz de datos
 data(rda.meat.fish.fowl.1959)
crimen = rda.meat.fish.fowl.1959
anyNA(crimen)
# Cambiamos el nombre de la matriz
crimen 
# Calculo de la matriz de distancia de Mahalonobis
dist.crimen<-dist(crimen[,2:6])

# Convertir los resultados del Calculo de la distancia a una matriz de datos y me indique 3 digitos.
round(as.matrix(dist.crimen)[1:6, 1:6],3)

# Calculo del dendrograma
dend.crimen<-as.dendrogram(hclust(dist.crimen))

# Generacion del dendrograma
plot(dend.crimen)

# Agregar etiquetas al Grafico
crimen.nombres=crimen
rownames(crimen.nombres)= crimen.nombres$name
crimen.nombres=crimen.nombres[,-1]

# Construimos de nuevo el Grafico
plot(as.dendrogram(hclust(dist(crimen.nombres))))

#  Modificar el dendrograma
#install.packages("dendextend")
library(dendextend)

# Guardar las etiquetas en un objeto "L"
L=labels(dend.crimen)
labels(dend.crimen)=crimen$name[L]

# Cambiar el tama???o de las etiquetas
dend.crimen %>%
  set(what="labels_col", "red") %>% #Colores etiqueta
  set(what="labels_cex", 0.8) %>%
  plot(main="Dendrograma de Mamiferos")

#Dendograma de Circulo
#install.packages("circlize")
library("circlize")

circlize_dendrogram(dend.crimen,labels_track_height=NA,
                    dend_track_height=0.1)
