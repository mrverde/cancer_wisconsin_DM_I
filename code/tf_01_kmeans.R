################## K-MEANS ##############################
library(RCurl)
datos <- read.csv(text=getURL("https://raw.githubusercontent.com/mrverde/cancer_wisconsin_DM_I/master/data/cancer_winsconsin.csv"), header = TRUE)

bdkmeans <- datos[ -c(1:2) ]
plot(bdkmeans[,])

bdkmeans <- log10(bdkmeans)
bdkmeans <- scale(bdkmeans)
bdkmeans <- data.frame(bdkmeans[,colSums(is.na(bdkmeans))<nrow(bdkmeans)])

#Para quitar los outliers. Percentilup y down marcan el valor del percentil con el 
#que vamos a sustituir los valores mas extremos
percentilup <- 85
percentildown <- 15
for (columna in colnames(bdkmeans)){
  up <-c("bdkmeans$",columna,"[bdkmeans$",columna,">quantile(bdkmeans$",columna,",", 
         percentilup*0.01,")] <- quantile(bdkmeans$",columna,",", percentilup*0.01,")")
  down <-c("bdkmeans$",columna,"[bdkmeans$",columna,"<quantile(bdkmeans$",columna,", ", 
           percentildown*0.01,")] <- quantile(bdkmeans$",columna,", ", percentildown*0.01,")")
  up <-paste(up, collapse="")
  down <-paste(down, collapse="")
  eval(parse(text=up))
  eval(parse(text=down))
}

#Dibujo el diagrama de dispersion de los datos tras hacer las transformaciones
plot(bdkmeans[,])

#Hacemos el kmeans
kmeans_cancer <-  kmeans(bdkmeans,2, iter.max =1000, nstart = 1000)

#Hago una tabla de contingencia
tabla <- table(datos$diagnosis, kmeans_cancer$cluster)

#Vemos el resultado de la clasificacion
tabla 

#Para contabilizar el numero de aciertos y fallos selecciono los selecciono
aciertos <- max(tabla[1,]) + max(tabla[2,])
fallos <- min(tabla[1,])+ min(tabla[2,])
por_ac = aciertos*100/569
por_fa = 100-por_ac
por_ac

###################### RESULTADOS ###################
#mayor acierto por ahora, datos en bruto (85,41% de acierto)
#mayor acierto datos normalizados con scale (91,03% de acierto)
#mayor acierto 1) log10 2) datos normalizados con scale (93,5% de acierto)

#mayor acierto> 1) datos sin outliers 5% (86,82% de acierto)
#mayor acierto> 1) datos sin outliers 10% (87,34% de acierto)
#mayor acierto> 1) datos sin outliers 15% (89,10% de acierto)
#mayor acierto> 1) datos sin outliers 20% (90,86% de acierto)
#mayor acierto> 1) datos sin outliers 20% (88,75% de acierto)

#mayor acierto> 1) Normalizados con scale 2) datos sin outliers 5% (91,74% de acierto)
#mayor acierto> 1) Normalizados con scale 2) datos sin outliers 10% (93,67% de acierto)
#mayor acierto> 1) Normalizados con scale 2) datos sin outliers 15% (93,84% de acierto)


#mayor acierto> 1) datos sin outliers 5% 2) Normalizados con scale (91,22% de acierto)
#mayor acierto> 1) datos sin outliers 10% 2) Normalizados con scale (92,44% de acierto)
#mayor acierto> 1) datos sin outliers 15% 2) Normalizados con scale (93,32% de acierto)
#mayor acierto> 1) datos sin outliers 20% 2) Normalizados con scale (92,97% de acierto)


#mayor acierto> 1) datos sin outliers 5% 2) Calculo el Log10 3) Normalizados con scale (92,97% de acierto)
#mayor acierto> 1) datos sin outliers 10% 2) Calculo el Log10 3) Normalizados con scale (93,14% de acierto)
#mayor acierto> 1) datos sin outliers 15% 2) Calculo el Log10 3) Normalizados con scale (92,97% de acierto)

#mayor acierto> 1) Calculo el Log10 2) Normalizados con scale 3) datos sin outliers 15% (93,5% de acierto)
#mayor acierto> 1) Calculo el Log10 2) Normalizados con scale 3) datos sin outliers 15% (94,03% de acierto)
#####mayor acierto> 1) Calculo el Log10 2) Normalizados con scale 3) datos sin outliers 15% (94,37% de acierto)
#mayor acierto> 1) Calculo el Log10 2) Normalizados con scale 3) datos sin outliers 20% (94,03% de acierto)


########### CLASIFICADO DE NUEVOS CASOS ##############
#El clasificador es una funcion que coge los valores del logaritmo de las 30 variables normalizadas, 
#y comprueba cual es el centroide que tiene mas cerca. Si es el benigno devuelve B y si es el maligno 
#devuelve M
clasificador <- function(nuevo_punto){
  if (dist(rbind(kmeans_cancer$centers[1],nuevo_punto))
      <dist(rbind(kmeans_cancer$centers[2],nuevo_punto))){
    return ("M")
    
  }else if (dist(rbind(kmeans_cancer$centers[2],nuevo_punto))
            <dist(rbind(kmeans_cancer$centers[1],nuevo_punto))){
    return ("B")
  }
}

clasificador(bdkmeans[1,])
clasificador(bdkmeans[507,])