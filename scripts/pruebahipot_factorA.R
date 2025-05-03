setwd("C:/Users/zaira/Documents/01. PhD en Ciencias Forestales/1. Cursos/3. Verano/EST-642 - Modelos lineales/Ejercicios de clase")
datoslht=read.csv("datoslht.csv")
#View(datoslht)
#pander::pander(datoslht)

X=as.matrix(datoslht[,1:8]) #matrix X
pander::pander(X)
(Y=as.matrix(datoslht[,9])) # vector Y
pander::pander(Y)
library(matlib)
(rango_X=R(X))
XtX=t(X)%*%X
gXtX=Ginv(XtX)
gXtX
beta=gXtX%*%t(X)%*%Y
#Hipotesis H0: b1=b2=b3 (No hay efecto de B)
#Ha:b1-b2=0 y b1-b3=0 
R=rbind(c(0,0,0,0,0,1,-1,0),c(0,0,0,0,0,1,0,-1))
# verificar estimabilidad en matrix X aumentada con R
pander::pander(R)
X_R=rbind(X,R)
# Rango de X_R
(rango_X_R=R(X_R)) # es estimable rango 6
Rbeta=R%*%beta
Rbeta
#Numerador de la Fc;
Numerador=t(Rbeta)%*%inv(R%*%gXtX%*%t(R))%*%Rbeta
Numerador
rangonum=R(inv(R%*%gXtX%*%t(R)))
#denominador de Fc;
Px=X%*%gXtX%*%t(X)
R(Px)
Id=as.matrix(diag(nrow(X)))
denom=t(Y)%*%(Id-Px)%*%Y
rangoden=R(Id)-R(Px)
# Fc calculada;
Fc=(Numerador/rangonum)/(denom/rangoden)
Fc
#Valor de tablas y pvalue;
alfa=0.05
valorFtablas=qf(1-alfa,rangonum, rangoden)
valorFtablas
pvalue=1-pf(Fc, rangonum, rangoden)
pvalue
