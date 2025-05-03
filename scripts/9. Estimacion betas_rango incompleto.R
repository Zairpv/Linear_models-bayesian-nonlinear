#importar datos
setwd("C:/Users/zaira/Documents/01. PhD en Ciencias Forestales/1. Cursos/3. Verano/EST-642 - Modelos lineales/Ejercicios de clase")

#Con  la matriz diseño
datos<-read.csv("8. Datospesovacas.csv")
View(datos)                
(Y=datos[,1])
(X=datos[,2:8])
Y=as.matrix(Y)
X=as.matrix(X)

#Ecuaciones normales X'X
(xtx=t(X)%*%X)
(xty=t(X)%*%Y)

library(matlib)
g_xtx=Ginv(xtx)
(betas=g_xtx%*%xty)
betas

# BETAS 
(dif_AN_HE=betas[6]-betas[7])

##### Con los datos originales-
(datos<-read.csv("9. Datospesovacas.csv"))
datos$Age=as.factor(datos$Age)
datos$Breed=as.factor(datos$Breed)

#Generar modelo
mod1=lm(PWG~Age+Breed,data = datos)
summary(mod1)

library(lmtest)
coeftest(mod1)
model.matrix(mod1)
