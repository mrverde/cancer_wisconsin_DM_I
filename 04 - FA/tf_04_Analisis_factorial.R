################## ANALISIS FACTORIAL ##############################
library(psych)
library(RCurl)
options(digits=2)

datos <- read.csv(text=getURL("https://raw.githubusercontent.com/mrverde/cancer_wisconsin_DM_I/master/data/cancer_winsconsin.csv"), header = TRUE)

bdkmeans <- datos[ -c(1:2) ]
bdkmeans <- data.frame(scale(bdkmeans))

mat_cov <- cov(bdkmeans)
mat_cor <- cov2cor(mat_cov)


KMO(mat_cor)
###### RESULTADOS KMO ######
#Valores +90% - concave.points_mean, concavity_worst, 
#Valores 80-90% - radius_mean, perimeter_mean, area_mean, smoothness_mean, compactness_mean, concavity_mean, symmetry_mean, 
#fractal_dimension_mean, radius_se, perimeter_se, area_se, compactness_se, concavity_se, concave.points_se, fractal_dimension_se, 
#radius_worst, perimeter_worst, area_worst, compactness_worst, concave.points_worst, fractal_dimension_worst

#Valores 70-80% - smoothness_worst, symmetry_worst
#Valores 60-70% - texture_mean, somoothness_se, texture_worst
#Valores -60% - texture_se, symmetry_se

bdkmeans <- bdkmeans[ -c(12,19) ]
mat_cov <- cov(bdkmeans)
mat_cor <- cov2cor(mat_cov)
KMO(mat_cor)

##### TEST DE BARTLETT #####
cortest.bartlett(mat_cor, n=569)

#chisq = 37763; p-value = 0. Rechazamos H0

####### PROCESO PARA ESCOGER EL Nº DE FACTORES #######
fa.parallel(mat_cor, n.obs=569, fa="both", n.iter=100,
            main="Scree plots with parallel analysis")
#Parallel analysis suggests that the number of factors =  5  and the number of components =  5 
fa <- fa(mat_cor, nfactors=5, rotate="none", fm="pa")
fa
factor.plot(fa, labels=rownames(fa$loadings))
fa.diagram(fa,simple=FALSE)

#Rotamos los factores con el metodo varimax (rotacion ortogonal) para aumentar su capacidad explicativa
fa.varimax <- fa(mat_cor, nfactors=5, rotate="varimax", fm="pa")
fa.varimax
factor.plot(fa.varimax, labels=rownames(fa$loadings))
fa.diagram(fa.varimax,simple=FALSE)

#Hacemos otro ejemplo con una rotacion diferente, la promax, que es oblicua
fa.promax <- fa(mat_cor, nfactors=5, rotate="promax", fm="pa")
fa.promax
factor.plot(fa.promax, labels=rownames(fa$loadings))
fa.diagram(fa.promax,simple=FALSE)

#########
#PA1
#area_mean,radius_mean,area_worst,perimeter_mean,radius_worst,perimeter_worst,area_se,radius_se,perimeter_se,concave.points_mean,concave.points_worst,concavity_mean
#PA2
#concavity_mean,fractal_dimension_worst,fractal_dimension_mean,compactness_worst,compactness_mean,concavity_se,compactness_se,fractal_dimension_se,concave.points_se,concavity_worst
#PA3 -> Variables SE -> Desviaciones estandar
#area_se,radius_se,perimeter_se,smoothness_mean,smoothness_se
#PA4 -> Textura
#texture_worst,texture_mean
#PA5 -> 
#concave.points_mean,concave.points_worst,smoothness_worst,smoothness_mean,symmetry_worst,fractal_dimension_worst,fractal_dimension_mean,symmetry_mean,compactness_worst,compactness_mean,concavity_worst