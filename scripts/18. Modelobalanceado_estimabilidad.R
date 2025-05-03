setwd("C:/Users/zaira/Documents/01. PhD en Ciencias Forestales/1. Cursos/3. Verano/EST-642 - Modelos lineales/Ejercicios de clase")
datos=read.csv("17. Modelo balanceado_Estimabilidad.csv")
View(datos)
(Y=as.matrix(datos$Y))
(X=as.matrix(datos[,2:9]))

library(matlib)
#Combinaciones lineales
#mu+a1+b2
#betas=m,a1,a2,a3,b1,b2,b3,b4

lambda_t=c(1,1,0,0,0,1,0,0)


#Utilizando criterio de rango
R(X)
(A=rbind(X,lambda_t))
R(A)
#RangoX=RangoA --- Es estimable

#Con el criterio de inversa generalizada
(XtX=t(X)%*%X)
(g=Ginv(XtX))
round(lambda_t%*%g%*%t(X)%*%X)
lambda_t
#Es estimable!

#Matriz escalonada
echelon(X)
#Son combinaciones lineales estimables

library(estimability)
(N=nonest.basis(X))
lambda_t
is.estble(lambda_t,N)


#Utilizando el modelo en crudo
Y=c(12,24,30,6,11,21,15,16)
A=c("A1","A1","A1","A2","A2","A3","A3","A3")
B=c("B1","B3","B4","B3","B4","B1","B2","B3")
mod=lm(Y~A+B)
summary(mod)
library(estimability)
library(lsmeans)

(estimacionA=lsmeans(mod,"A"))
#Da las medias de cada factor de A

#Definir contrastes
#c1=a1-a2
#c2=a2-a3
#c3=a1+a2+a3
contraste1=list(c1=c(1,-1,0),
                c2=c(0,1,-1),
                c3=c(1,1,1))
contrast(estimacionA,contraste1)
#el contraste 1 y el contraste 2 no muestran 
#diferencias significativas


