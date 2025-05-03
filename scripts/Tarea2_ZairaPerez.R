#Nota. El documento de texto se realizo en LaTEX
#Zaira Rosario Pérez Vázquez
#Tarea 2. Art. A note on estimability in linear models

# Definir matriz diseño en R
y<-matrix(c(36.6,37.9,21.49,23.8,8.0,0.29,41.9,31.6,17.68,29.05,28.33,31.6,
            15.1,15.13,33.7,9.4,7.07,20.41,29.52,2.8,12.7,6.22,4.41,2.99,
            13.42,9.84,19.85,15.81,1.1,20.88))

X<-matrix(c(1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,
            1,1,1,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,
            0,0,0,1,1,1,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,
            0,0,0,0,0,0,1,1,1,1,1,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,
            0,0,0,0,0,0,0,0,0,0,0,1,1,1,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,
            0,0,0,0,0,0,0,0,0,0,0,0,0,0,1,1,1,1,1,0,0,0,0,0,0,0,0,0,0,0,
            0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,1,1,1,1,1,0,0,0,0,0,0,
            0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,1,1,0,0,0,0,
            0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,1,1,0,0,
            0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,1,1),
          nrow = 30,ncol = 10)
View(X)

#Obteniendo X'X
(Xt_X=t(X)%*%X)   #Nota: Es la misma que Adeyemo & Nowi (2014)

#Obteniendo X'y
(Xt_y=t(X)%*%y) #Nota: Es la misma que Adeyemo & Nowi (2014)

#Rangos de X y X'X
qr(Xt_X)$rank

#Inversa de X'X
solve(Xt_X) #Nota: requiere utilizar inversa generalizada

#Obteniendo la inversa generalizada G- de X'X
library(MASS)
(G_=ginv(Xt_X)) #Nota: Es la misma que Adeyemo & Nowi (2014)


#Para obtener H=G-X'X
(H=G_%*%Xt_X) #Nota: Es la misma que Adeyemo & Nowi (2014)

#Para estimar \hat(beta)=G-X'Y
(beta_hat=G_%*%Xt_y) #Nota: Es la misma que Adeyemo & Nowi (2014)

#Combinaciones lineales de Bi ¿Son estimables?
#Crear matriz T
T=diag(1,nrow = 10,ncol = 10)
View(T)
#Ti es de dimension 1 x p
#ti_1=T[1,]
#ti_2=T[2,]  
#--- hasta 9..

#Es estimable?
(round(T%*%G_%*%Xt_X))
#R. Si es estimable #Nota. Los autores obtienen otra matriz??


#Si ahora con contrastes B1-B2=T'B Y 
(T2=matrix(c(0,1,-1,0,0,0,0,0,0,0),nrow = 1,ncol = 10))
#es estimable??
(round(T2%*%G_%*%Xt_X))
#R. Tambien es estimable 

