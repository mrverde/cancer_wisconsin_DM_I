################## HIERARCHICAL CLUSTER ##############################
require(RCurl)
datos <- read.csv(text=getURL("https://raw.githubusercontent.com/mrverde/cancer_wisconsin_DM_I/master/data/cancer_winsconsin.csv"), header = TRUE)

#Creo una segunda bd eliminando la columna id (es un numero identificativo) y la columna diagnosis (es cualitativa)
bdkmeans <- datos[ -c(1:2) ]

#Para normalizar los datos
bdkmeans <- log10(bdkmeans)
bdkmeans <- scale(bdkmeans)
bdkmeans <- data.frame(bdkmeans[,colSums(is.na(bdkmeans))<nrow(bdkmeans)])

#Hago un bucle que dibuja el hierarchical cluster con 7 metodos diferentes
for (i in c("ward.D","single","complete","average","mcquitty","median","centroid")){
  hc <- hclust(dist(bdkmeans), method=i) 
  plot(hc, hang = -1,labels=datos$diagnosis, main=paste(c("Hierarchical cluster - ", i, "method"),collapse=""))
  #dev.copy(png,paste(c("02 - Hierarchical cluster - ", i, "method.png"),collapse=""), width=25000, height=2000, units="px", res=300)
  #dev.off()
}

#Para quitar los outliers
#Percentilup y down marcan el valor del percentil con el que vamos a sustituir los valores mas extremos
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
