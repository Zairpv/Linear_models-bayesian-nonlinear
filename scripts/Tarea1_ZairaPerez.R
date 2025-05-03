#### Tarea 1.
##Presenta: Zaira Rosario Pérez Vázquez

#---------------------
#Problema 1
rm(list=ls())

#Sea
(A<-matrix(c(7,4,-3,9,2,5),nrow = 2,ncol = 3))

#   Encuentre A'
(At<-t(A))

#   Encuentre (A')'=A
t(At)
(t(At)==A)

#   Obtenga A'A=AA'
At%*%A
A%*%At
#Matrices con diferente num. de filas y columnas
###---------------------------------------------------




###---------------------------------------------
#### Problema 2
rm(list=ls())
#Sean
(A=matrix(c(2,-1,4,3),nrow = 2,ncol = 2))
(B=matrix(c(1,2,3,-1),nrow = 2,ncol = 2))

#   Encuentre AB y BA
(AB=A%*%B)
(BA=B%*%A)

# 	Encuentre |A|, |B| y |AB| (los determinantes)
det(A)
det(B)
det(AB)

#	  Obtenga |BA| y compare con |AB|
det(BA)
det(AB)
#R. Es igual su determinante

# 	Obtenga la traza(AB) y compare con la traza (BA)
sum(diag(AB))
sum(diag(BA))
#R. Es igual su traza

#	  Obtenga los eigenvalores de AB y de BA
eigen(AB)
eigen(BA)

#	  Obtenga el rango de A y de B
qr(A)$rank
qr(B)$rank

#	Obtenga si existe la matriz inversa de A^(-1) y de B^(-1)
solve(A)
solve(B)


###---------------------------------------------------
#### Problema 3
rm(list=ls())
#Sean
(A=matrix(c(5,-1,3,-1,1,2,3,2,7),nrow = 3,ncol = 3))
(B=matrix(c(6,7,2,-2,1,-3,3,0,5),nrow = 3,ncol = 3))
(C=matrix(c(2,-1,3,-3,4,1),nrow = 3,ncol = 2))
(x=matrix(c(3,-1,2),nrow = 3,ncol = 1))
(y=matrix(c(3,2,4),nrow = 3,ncol = 1))
(z=matrix(c(2,5),nrow = 2,ncol = 1))

# Obtenga

# 	Bx
(Bx=B%*%x)

#	  y' B
(yt=t(y))
(yt_B=t(y)%*%B)

#	  x' Ax
(xt=t(x))
(Ax=A%*%x)
(xt_Ax=t(x)%*%Ax)

#   x' Cz
(Cz=C%*%z)
(xt_Cz=xt%*%Cz)

#   x' x
xt%*%x

#	  x' y
xt%*%y

#   xx'
x%*%xt

#   xy'
x%*%yt

#   B'B
(Bt=t(B))
Bt%*%B

#   yz'
zt=t(z)
y%*%zt

#   zy'
z%*%yt

#   raiz(y'y)
sqrt(yt%*%y)

#   B'B
Bt%*%B

#Encuentre
#   x+y
x+y

#   x-y
x-y
###---------------------------------------------------------------



###----------------------------------------
### Problema 4
rm(list=ls())

#     Inciso 1. 
#Matriz A
(A=matrix(c(1,1,1,2,-1,0,3,0,-1),nrow = 3,ncol = 3))
(b=matrix(c(6,2,-1),nrow = 3,ncol = 1))

#Matriz A aumentada
(A2=cbind(A,b))

#Comparando rangos
qr(A)$rank
qr(A2)$rank
#R. Rangos iguales - Sistema compatible

#Obtención de la solución
(soluc=solve(A,b))
dimnames(soluc)<-list(c("x1", "x2","x3"), NULL)
soluc

#-------
#Inciso B
rm(list=ls())
(A=matrix(c(1,1,2,-1,-1,-2,2,-1,1),nrow = 3,ncol = 3))
(b=matrix(c(2,-1,2),nrow = 3,ncol = 1))

#Matriz A aumentada
(A2=cbind(A,b))

#Comparando rangos
qr(A)$rank
qr(A2)$rank
#R. Rangos diferentes - Sistema incompatible

#Obtención de la solución
(soluc=solve(A,b))


##------
#Inciso C
rm(list=ls())
(A=matrix(c(1,1,3,1,-1,1,1,-1,1,1,-1,1),nrow = 3,ncol = 4))
(b=matrix(c(8,6,22),nrow = 3,ncol = 1))

#Matriz A aumentada
(A2=cbind(A,b))

#Comparando rangos
qr(A)$rank
qr(A2)$rank
 #R. Rangos iguales - Sistema compatible

#Obtención de la solución
qr(t(A)%*%(A))$rank
qr(A)$rank
library(MASS)
ginv(t(A)%*%A)%*%t(A)%*%b
