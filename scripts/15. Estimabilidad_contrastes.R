(Y=matrix(c(18,12,25,27,26,39,28,40,33,30,41,37,30,41,56,41,42,38,48,45),ncol = 1))
(dosis=matrix(c(15,15,15,15,15,25,25,25,25,25,35,35,35,35,35,45,45,45,45,45),ncol = 1)
(X=matrix(c(1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,
           1,1,1,1,1,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,
           0,0,0,0,0,1,1,1,1,1,0,0,0,0,0,0,0,0,0,0,
           0,0,0,0,0,0,0,0,0,0,1,1,1,1,1,0,0,0,0,0,
           0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,1,1,1,1,1),ncol = 5,nrow = 20))

#Crear modelo
dosis<-as.factor(dosis)
levels(dosis)
  
mod1<-lm(Y~dosis)
anova(mod1)

#Encontrar el IC para cada contraste
library(lsmeans)
(esti_mco=lsmeans(mod1,"dosis"))
contrastes=list(Lineal=c(-3,-1,1,3),
                cuadratico=c(1,-1,-1,1),
                cubico=c(-1,3,-3,1))
contrast(esti_mco,contrastes)

library(estimability)
(N=nonest.basis(X))
(funciones=rbind(c(0,-3,-1,1,3),
                c(0,1,-1,-1,1),
                c(0,-1,3,-3,1),
                c(8,0,0,-1,0)))
is.estble(funciones,N)
