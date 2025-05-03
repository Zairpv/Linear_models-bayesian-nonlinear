#### Problema 3
### 1. Definiendo las matrices
rm(list=ls())
(mu=matrix(c(-1,0,3),ncol = 1))
(sigma=matrix(c(0.75,0,0.25,0,1,0,0.25,0,0.75),ncol = 3))
(A=matrix(c(1.5,0,-0.5,0,1,0,-0.5,0,1.5),ncol = 3))

### 2. E(Y^t AY)= tr(A??)+ ??' A??
(AS=A%*%sigma)
(tr=sum(diag(AS)))
(mAm=t(mu)%*%A%*%mu)
(E=tr+mAm)

### 3. V(Y^T AY)= 2tr[(A??)^2 ]+4??' A????
(AS2=AS^2)
(tr2=2*sum(diag(AS2)))
(Var=tr2+(4*t(mu)%*%AS%*%mu))

### 4. Se distribuye como x2?
library(matlib)
(R=R(A))
(NC=0.5*mAm)
AS
R(AS)


### 5. Distribución de Y'AY/sig2
#grados de libertad
R(A)

#Parametro de no centralidad
0.5%*%t(mu)%*%(A/sigma)%*%mu


