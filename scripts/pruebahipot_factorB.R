setwd("C:/Users/zaira/Documents/01. PhD en Ciencias Forestales/1. Cursos/3. Verano/EST-642 - Modelos lineales/Ejercicios de clase")
datos=read.csv("datos_ds.csv")


mod1=lm(resp~factor(A)+factor(B)+factor(A)*factor(B),data = datos)
library(car)
Anova(mod1,type=2)
anova(mod1)

#X
(X=as.matrix(datos[4:15]))
(Y=as.matrix(datos[,3]))
View(X)
#sUMA DE CUADRADOS TOTAL

(J=matrix(1,nrow(X),nrow(X)))
(n=nrow(X))
(I=diag(1,nrow(X)))
(SCT=t(Y)%*%(I-(1/n)*J)%*%(Y))
#Checar

#Grados de libertad total
library(matlib)
(glt=R(I-(1/n)*J))

#Suma de cuadrados del error
library(MASS)
(Px=X%*%ginv(t(X)%*%X)%*%t(X))
(SCE=t(Y)%*%(I-Px)%*%Y)


#Suma de cuadrados del modelo
(SCM=t(Y)%*%(Px-(1/n)*J)%*%Y)
130.67+626.33+36.33 #De la tabla de anova (linea7-8)

#Efecto del factor A
#Efecto del factor A
XA=X[,1:3]
(gle=R(I-Px))
(JA=matrix(1,nrow(XA),nrow(XA)))
(IA=diag(1,nrow(XA)))
(PXA=XA%*%ginv(t(XA)%*%XA)%*%t(XA))
(SCA=t(Y)%*%(PXA-(1/n)*JA)%*%Y)

#Efecto del factor B
XB=X[,c(1,4:6)]
(JB=matrix(1,nrow(XB),nrow(XB)))
(IB=diag(1,nrow(XB)))
(PXB=XB%*%ginv(t(XB)%*%XB)%*%t(XB))
(SCA=t(Y)%*%(PXB-(1/n)*JB)%*%Y)

#Efecto del factor AB
XAB=X[,c(1,7:12)]
View(XAB)
(JAB=matrix(1,nrow(XAB),nrow(XAB)))
(IAB=diag(1,nrow(XAB)))
(PXAB=XAB%*%ginv(t(XAB)%*%XAB)%*%t(XAB))
AB=Px-PXA-PXB+(1/n)*J
(SCA=t(Y)%*%(I-(PXA-(1/n)*JA)-(PXB-(1/n)*JB))%*%Y)
t(Y)%*%AB%*%Y


(AB+(1-Px)+(PXA-(1/n)*JA)+(PXB-(1/n)*JB))
#Debe de coincidir
(I-(1/n)*J)