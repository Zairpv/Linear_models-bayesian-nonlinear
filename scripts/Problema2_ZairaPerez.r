#### Problema 2
### 1. Captura de info
rm(list=ls())
problema2 <- read.csv("~/01. PhD en Ciencias Forestales/1. Cursos/3. Verano/EST-642 - Modelos lineales/Examen/problema2.csv")
View(problema2)

### Matriz diseño
(X=as.matrix(problema2[,4:10]))
(Y=as.matrix(problema2[,1]))


####-------------------------------------------------------------------
### 1. Obtenga la distribución de Y


#Suma de cuadrados para factor A (tipo de queso)
(((155.78^2)+(142.22^2)+(144.76^2))/4)-((442.76^2)/12)
#Suma de cuadrados para factor B (metodo de elaboración)
((((221.98)^2)+((220.78)^2))/6)-((442.76^2)/(3*2*2))
#Formas cuadraticas
Y
(J=matrix(1,nrow(X),nrow(X)))
(I=diag(1,nrow(X)))
(n=nrow(X))
(PX=X%*%ginv(t(X)%*%X)%*%t(X))
#Suma de cuadrados del error
(SCE=t(Y)%*%(I-PX)%*%Y)
#Grados de libertad del error
(gle=R(I-PX))
#Suma de cuadrados del modelo
(SCM=t(Y)%*%(PX-(1/n)*J)%*%Y)
#Grados de libertad del modelo
(glm=R(PX-(1/n)*J))
#Suma de cuadrados total corregida
(SCT=t(Y)%*%(I-(1/n)*J)%*%Y)
#Grados de libertad totales
(glt=R(I-(1/n)*J)) #Son 12 observaciones


####-------------------------------------------------------------------
### 2. Escribe los parámetros del modelo utilizando inversas generalizadas
library(matlib)
library(MASS)
# Rango de X
R(X)
# Rango de X'X
(XtX=t(X)%*%X)
R(XtX)
# Inversa generalizada de X'X (libreria MASS)
(G_XtX=ginv(XtX))
# Estimacion de parametros MCO
(alphas=G_XtX%*%t(X)%*%Y) #Esta solución no es única

#Forma cuadratica Y'AY
(sigma2=SCE/6)
A=(1/0.108533)*(I-(1/n)*J)
R(A)
lambda1=0.5*(t(X%*%alphas)%*%A%*%(X%*%alphas))
hist(Y)


####-------------------------------------------------------------------
### 3. Prueba de hipotesis
## a) Ho: a1=a2=a3
#Estimabilidad
(R1=rbind(c(0,1,-1,0,0,0,0),
            c(0,1,0,-1,0,0,0)))
(XR1=rbind(X,R1))
R(X)
R(XR1) #Es estimable
#Valores
(R1Beta=R1%*%alphas)
#Prueba de F 
(numerador=t(R1Beta)%*%inv(R1%*%G_XtX%*%t(R1))%*%R1Beta)
(rangonumerador=R(inv(R1%*%G_XtX%*%t(R1))))
PX=X%*%G_XtX%*%t(X)
R(PX)
I=as.matrix(diag(nrow(X)))
(denominador=t(Y)%*%(I-PX)%*%Y)
(rangodenominador=R(I)-R(PX))
(Fc=(numerador/rangonumerador)/(denominador/rangodenominador))
(Ftabla=qf(1-0.05,rangonumerador,rangodenominador))
(pvalue=1-pf(Fc,rangonumerador,rangodenominador))


# b) Ho: a1=a2=a3=0
(R2=rbind(c(0,1,0,0,0,0,0),
          c(0,0,1,0,0,0,0),
          c(0,0,0,1,0,0,0)))
(XR2=rbind(X,R2))
R(X)
R(XR2) #No es estimable



# c) Ho: m+a1=39  &  m+a4=39
(R3=rbind(c(1,1,0,0,0,0,0),
          c(1,0,0,0,1,0,0)))
(XR3=rbind(X,R3))
R(X)
R(XR3) #Si es estimable
#Bajo la hipotesis Ho:Rbeta=r --> Ho:Rbeta-r=0
#Valores
(R3Beta=(R3%*%alphas)-39)
#Prueba de F 
(numerador3=t(R3Beta)%*%inv(R3%*%G_XtX%*%t(R3))%*%R3Beta)
(rangonumerador3=R(inv(R3%*%G_XtX%*%t(R3))))
PX=X%*%G_XtX%*%t(X)
R(PX)
I=as.matrix(diag(nrow(X)))
(denominador=t(Y)%*%(I-PX)%*%Y)
(rangodenominador=R(I)-R(PX))
(Fc3=(numerador3/rangonumerador3)/(denominador/rangodenominador))
(Ftabla3=qf(1-0.05,rangonumerador3,rangodenominador))
(pvalue3=1-pf(Fc3,rangonumerador3,rangodenominador))


# d) Ho: 0 1 -1 0 -1 1 0     beta=0
#        0 1 0 -1 -1 0 1          0

(R4=rbind(c(0,1,-1,0,-1,1,0),
         c(0,1,0,-1,-1,0,1)))
(XR4=rbind(X,R4))
R(X)
R(XR4) #Es estimable
#Valores
(R4Beta=R4%*%alphas)
#Prueba de F 
(numerador4=t(R4Beta)%*%inv(R4%*%G_XtX%*%t(R4))%*%R4Beta)
(rangonumerador4=R(inv(R4%*%G_XtX%*%t(R4))))
PX=X%*%G_XtX%*%t(X)
R(PX)
I=as.matrix(diag(nrow(X)))
(denominador=t(Y)%*%(I-PX)%*%Y)
(rangodenominador=R(I)-R(PX))
(Fc4=(numerador4/rangonumerador4)/(denominador/rangodenominador))
(Ftabla4=qf(1-0.05,rangonumerador4,rangodenominador))
(pvalue=1-pf(Fc4,rangonumerador4,rangodenominador))



####-------------------------------------------------------------------
### 4. Efectos del factor A y B

#Estimar parametros del modelo completo Yijk=??+??i+??j+??ij+eijk
(X_completo=as.matrix(problema2[,c(4,11:21)]))
(Y=as.matrix(problema2[,1]))
# Rango de X
R(X_completo)
# Rango de X'X
(XtX_completo=t(X_completo)%*%X_completo)
R(XtX_completo)
# Inversa generalizada de X'X (libreria MASS)
(G_XtX_completo=ginv(XtX_completo))
# Estimacion de parametros MCO
(betas_completo=G_XtX_completo%*%t(X_completo)%*%Y) #Esta solución no es única

#Efecto del tipo de queso
#¿ES ESTIMABLE C?
(C1=rbind(c(0,1,0,-1,0,0,1/2,1/2,0,0,-1/2,-1/2),
          c(0,0,1,-1,0,0,0,0,1/2,1/2,-1/2,-1/2)))
(XC1=rbind(X_completo,C1))
R(X_completo)
R(XC1) #Es estimable
#Valores
(C1Beta=C1%*%betas_completo)
#Prueba de F 
(numerador_c1=t(C1Beta)%*%inv(C1%*%G_XtX_completo%*%t(C1))%*%C1Beta)
(rangonumerador_c1=R(inv(C1%*%G_XtX_completo%*%t(C1))))
PX_C=X_completo%*%G_XtX_completo%*%t(X_completo)
R(PX_C)
I_C=as.matrix(diag(nrow(X_completo)))
(denominador_c=t(Y)%*%(I_C-PX_C)%*%Y)
(rangodenominador_c=R(I_C)-R(PX_C))
(Fc_tipo_queso=(numerador_c1/rangonumerador_c1)/(denominador_c/rangodenominador_c))
(Ftabla=qf(1-0.05,rangonumerador_c1,rangodenominador_c))
(pvalue=1-pf(Fc_tipo_queso,rangonumerador_c1,rangodenominador_c))


#Efecto del método de elaboración
#¿ES ESTIMABLE C?
(C2=rbind(c(0,0,0,0,0,0,1,1,1,1,0,0),
          c(0,0,0,0,0,0,0,0,1,1,1,1)))
(XC2=rbind(X_completo,C2))
R(X_completo)
R(XC2) #No Es estimable en el modelo reducido

## Es estimable en el modelo de interacciones
(R5=rbind(c(0,1,-1,0,-1,1,0),
         c(0,0,1,-1,0,-1,1)))
(XR5=rbind(X,R5))
R(X)
R(XR5) #Es estimable
#Valores
(R5Beta=R5%*%alphas)

#Prueba de F 
(numerador5=t(R5Beta)%*%inv(R5%*%G_XtX%*%t(R5))%*%R5Beta)
(rangonumerador5=R(inv(R5%*%G_XtX%*%t(R5))))
PX=X%*%G_XtX%*%t(X)
R(PX)
I=as.matrix(diag(nrow(X)))
(denominador=t(Y)%*%(I-PX)%*%Y)
(rangodenominador=R(I)-R(PX))
(Fc5=(numerador5/rangonumerador5)/(denominador/rangodenominador))
(Ftabla5=qf(1-0.05,rangonumerador5,rangodenominador))
(pvalue=1-pf(Fc5,rangonumerador5,rangodenominador))


####-------------------------------------------------------------------
### 5. REPARAMETRIZACIÓN DEL MODELO
(W=as.matrix(problema2[,c(4,22:26)]))
(alphas_est=inv(t(W)%*%W)%*%t(W)%*%Y)











































