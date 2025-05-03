###Problema 1. 
### 0. Capturar informacion

(Y=matrix(c(22.3,19.7,24.3,23.9,34.6,35.7,32.2,38.1)))
(B0=matrix(c(1,1,1,1,1,1,1,1)))
(x1=matrix(c(5.3,6.2,5.8,12.2,10.0,11.1,8.3,5.4)))
(x2=matrix(c(0,0,1,1,0,0,1,1)))
(x3=matrix(c(65,46,23,77,9,101,45,88)))
(x4=matrix(c(11.8,10.8,8.1,19.9,10.9,21.2,12.8,14.2)))


### 1. Matriz diseño
(matrizX=cbind(B0,x1,x2,x3,x4))


### 2. Rango de la matrix X
library(matlib)
R(matrizX)
(XtX=t(matrizX)%*%(matrizX))
R(XtX)


###3. Inversa generalizada
(GX=Ginv(XtX))


###4. Estimar beta con MCO
(betas=GX%*%t(matrizX)%*%(Y))


###4.1 Comprobar con lo estimado con lm
mod1=lm(Y~x1+x2+x3+x4)
summary(mod1) #Los valores de Beta estimados coinciden 


###5. Muestre que B1 es no estable
(lambda1=matrix(c(0,1,0,0,0)))
(lambda1_t=t(lambda1))
#Probando estimabilidad con el teorema 1
round(lambda1_t%*%GX%*%XtX) #No es estimable
#Utilizando el teorema 2
R(matrizX) 
R(XtX)
(X_lambda1=rbind(matrizX,lambda1_t))
R(X_lambda1) #No es estimable

###7. Muestre que theta2=b1+2b2+b4 es estimable y encuentre estimador de MCO
(lambda2=matrix(c(0,1,2,0,1)))
(lambda2_t=t(lambda2))
#Probando estimabilidad con el teorema 1
round(lambda2_t%*%GX%*%XtX) #Es estimable
#Probando estimabilidad con el teorema 2
R(matrizX)
R(XtX)
(X_lambda2=rbind(matrizX,lambda2_t))
R(X_lambda2) #Es estimable

###Otra forma de verificar si es o no estimable
library(estimability)
N=nonest.basis(matrizX)
funciones<-rbind(c(0,1,0,0,0),
                 c(0,1,2,0,1))
is.estble(funciones,N)



### 8.. Hallar el estimador de MCO
(Mco1=lambda2_t%*%betas) #ES MELI
#Verificando
betas[2]+2*betas[3]+betas[5]


### 9. Estimando sigma^2 = CME
#Aplicando el teorema de Gauss Marcov
(MELI1=Mco1)

#CME=((Y-X beta_hat)'(Y-X beta_hat))/(n-r)
CME=(t(Y-(matrizX%*%betas))%*%(Y-(matrizX%*%betas)))/(length(Y)-R(matrizX))
CME
sqrt(CME)

#Estimando la varianza de theta
#??'(X' X)- ?? ??^2
(lambda2_t%*%GX%*%lambda2)*CME



### 10. Intervalos de confianza para theta
(ts=qt(1-0.05/2,8-4))
#Aplicando la formula en partes 
#Lambda2*beta_hat +- raiz((lambda2_t*GX*lambda*CME)) * ts
parte1=lambda2_t%*%GX%*%lambda2
parte2=sqrt(parte1*CME)
parte3=parte2*ts

MELI1
(Ic1=MELI1+parte3)
(Ic2=MELI1-parte3)


### 11. Prueba de hipotesis Ho= theta igual a 0
(Tc=(MELI1-0)/parte2)
ts
#No se rechaza Ho
