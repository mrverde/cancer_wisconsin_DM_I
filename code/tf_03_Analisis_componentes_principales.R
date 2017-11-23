################## ANALISIS DE COMPONENTES PRINCIPALES ##############################
library(rgl)
library(corrplot)
library(RCurl)

options(digits=2)

datos <- read.csv(text=getURL("https://raw.githubusercontent.com/mrverde/cancer_wisconsin_DM_I/master/data/cancer_winsconsin.csv"), header = TRUE)

bdkmeans <- datos[ -c(1,2) ]
mat_cov1 <- cov(bdkmeans)
bdkmeans <- data.frame(scale(bdkmeans))

#Para quitar los outliers
percentilup <- 85
percentildown <- 15
for (columna in colnames(bdkmeans)){
  up <-c("bdkmeans$",columna,"[bdkmeans$",columna,">quantile(bdkmeans$",columna,",", percentilup*0.01,")] <- quantile(bdkmeans$",columna,",", percentilup*0.01,")")
  down <-c("bdkmeans$",columna,"[bdkmeans$",columna,"<quantile(bdkmeans$",columna,", ", percentildown*0.01,")] <- quantile(bdkmeans$",columna,", ", percentildown*0.01,")")
  up <-paste(up, collapse="")
  down <-paste(down, collapse="")
  eval(parse(text=up))
  eval(parse(text=down))
}

mat_cov <- cov(bdkmeans)
mat_cor <- cov2cor(mat_cov)
#Como los resultados de la matriz de covarianza estan en escalas muy diferentes uso la matriz de correlacion

#Dibujo una matriz de correlacion para ver que variables tienen relacion entre si
corrplot(mat_cor,cl.cex=2,tl.col="black", tl.srt=45)

#Hacemos el analisis de componentes principales
bd_pca <- prcomp(mat_cor)

#Comprobamos el resultado
#Para ver la informacion, usamos summary y print
print(bd_pca)
summary(bd_pca)


#Dibujamos el biplot de los componentes
biplot(bd_pca, scale = 0)

#Comprobamos la cantidad de la varianza que explica cada componente principal
plot(bd_pca, xlab = "Principal Component", type = "b")

#Ahora vamos a comprobar como pesan los componentes principales en los datos
bdkmeans_pca <- scale(as.matrix(bdkmeans),center = bd_pca$center, scale = bd_pca$scale) %*% bd_pca$rotation[ , 1:4]


#Dibujamos las 3 primeras componentes principales de cada caso

#Para diferenciar con el color a benignos y a malignos creo un vector en el que sustituyo las letras por numeros que equivalen a colores
diagnosis <- as.character(datos$diagnosis)
diagnosis[diagnosis=="B"] <- 3
diagnosis[diagnosis=="M"] <- 2
plot3d(bdkmeans_pca[,1],bdkmeans_pca[,2],bdkmeans_pca[,3], col=diagnosis, size=5, xlab= "PC1",  ylab= "PC2",  zlab= "PC3")

#Hago un hierarchical cluster con los resultados
for (i in c("complete","average")){
  hc <- hclust(dist(bdkmeans_pca), method=i) 
  plot(hc, hang = -1,labels=datos$diagnosis, main=paste(c("Hierarchical cluster - ", i, "method"),collapse=""))
  #dev.copy(png,paste(c("03 - Hierarchical cluster - ", i, "method - PCA.png"),collapse=""), width=25000, height=2000, units="px", res=300)
  #dev.off()
}