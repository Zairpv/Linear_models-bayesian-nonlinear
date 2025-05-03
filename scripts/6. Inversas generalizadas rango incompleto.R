#Definir matriz
A=rbind(c(4,1,2),c(1,1,5),c(3,1,3))
b=c(3,9,5)

#Matriz A*
(Ab=cbind(A,b))

library(matlib)
R(A)
R(Ab)

#Existe la matriz inversa de A
solve(A)

#y la matriz generalizada??
Ginv(A)

#Multiplicar A (A^-1) A  
round(A%*%Ginv(A)%*%A)
#Nota ¿Es igual a A?

      
#Primer forma de solucionarlo x=A^-1 b
#Utilizando la libreria matlib
(x=Ginv(A)%*%b)

#Segunda forma de solucionarlo
#Utilizando la libreria MASS
library(MASS)
(X=ginv(A)%*%b)
