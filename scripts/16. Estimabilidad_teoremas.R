#EJEMPLO 1  CON LA MATRIZ ECHELON

library(matlib)
(A=matrix(c(1,1,0,1,0,
           1,1,0,0,1,
           1,0,1,1,0,
           1,0,1,0,1),ncol = 5,nrow = 4,byrow = TRUE))
echelon(A,verbose=TRUE,fractions=TRUE)
#Nos da otras tres funciones lineales estimables independientes
#lambda1_beta=mu+alpha2+beta2
#lambda2_beta=alpha1-alpha2
#lambda3_beta=beta1-beta2


#EJEMPLO 2 INVERSA GENERALIZADA
library(matlib)
(X=matrix(c(1,1,0,1,0,
           1,1,0,0,1,
           1,0,1,1,0,
           1,0,1,0,1),nrow = 4,ncol = 5,byrow = TRUE))
R(X)
(XtX=t(X)%*%X)
g_=Ginv(XtX,fractions = T)

#beta=(m,a1,a2,b1,b2)
#Es estimable m+a1-3*b1??
(I=as.matrix(c(1,1,0,0,-3)))
(It=t(I))

#Es estimable utilizando el teorema 1
It%*%g_%*%XtX
#No es estimable

#Utilizando el teorema 2
xa=rbind(X,It)
R(xa)
R(X)
#No es estimable