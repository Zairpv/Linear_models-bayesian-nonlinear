#importar datos
setwd("C:/Users/zaira/Documents/01. PhD en Ciencias Forestales/1. Cursos/3. Verano/EST-642 - Modelos lineales/Ejercicios de clase")

#Con  los datos originales
datos<-read.csv("8. Datospesovacas.csv")
View(datos) 

#Crear modelo
mod=lm(PWG~Age+Breed,data = datos)
summary(mod)
anova(mod)

#Interpretación
#Hay diferencias significativas de la edad 3 con respecto a todas
#las edades. Hay diferencias de la edad 4 con respecto a todas las edades
#y asi sucesivamente. 
model.matrix(mod)


datos2=read.csv("11. Datos_pesovacas_dummy_and_matrizdiseno.csv")
Y=as.matrix(datos2$PWG)
X=as.matrix(datos2[,11:18])
W=as.matrix(cbind(datos2$mu,datos2[,5:9]))

#Obtener betas con inversa generalizada

beta=ginv(t(X)%*%X)%*%t(X)%*%Y
beta

library(matlib)
(beta2=Ginv(t(X)%*%X)%*%t(X)%*%Y) #Esta es la del ejercicio

#AHORA CON REPARAMETRIZACIÓN
(beta_est=inv(t(W)%*%W)%*%t(W)%*%Y) #Coincide con los obtenidos en la linea 10


#NOTA ALGEBRAICAMENTE:::
#Xbeta=Wbeta
#W es de rango completo ¿Que significa theta?
#W'X beta=W'W Theta
#(W'W)^-1 %*% W'X Beta = (w'w)^-1 %*% W'W Theta 
#theta= (W'W)^-1 %*% W'X Beta
#beta=(mu,ed2,ed3,ed4,ed5,rAN,rHE,rSM)
#y theta????

round(inv(t(W)%*%W)%*%t(W)%*%X)
#Entonces cada parametro obtenido en beta_est significa
beta_est
#347.13 es la suma de mu+edad2
#9.02 es la dif entre edad3-edad2
#13.56 es la dif entre edad4 y edad 2
#el parametro 53.5991 es la dif entre la raza SM Y AN