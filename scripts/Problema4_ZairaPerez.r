#### Problema 4
### 1. Captura de info
rm(list=ls())

### Estimando 
problema4 <- read.csv("~/01. PhD en Ciencias Forestales/1. Cursos/3. Verano/EST-642 - Modelos lineales/Examen/problema4.csv")
View(problema4)
problema4$Prot=as.factor(problema4$Prot)
problema4$Alimento=as.factor(problema4$Alimento)
(X_reducido=as.matrix(problema4[,4:15]))
(X_completo=as.matrix(problema4[,4:16]))
(Y=as.matrix(problema4[,1]))


library(matlib)
library(MASS)
library(car)

#Modelo reducido: No considera a la variable peso inicial
#----------------------------------------------------------------------------
#Solución de ecuaciones normales
R(X_reducido)
XtX_red=t(X_reducido)%*%X_reducido
GXtX_red=ginv(XtX_red)
beta_hat_red=GXtX_red%*%t(X_reducido)%*%Y
modelo_red=lm(Y~Prot+Alimento+Prot*Alimento,data = problema4)
summary(modelo_red)
anova(modelo_red)

#Formas cuadraticas
PX_red=X_reducido%*%GXtX_red%*%t(X_reducido)
J_red=matrix(1,nrow(PX_red),nrow(PX_red))
I_red=diag(1,nrow(X_reducido))
n_red=nrow(X_reducido)
#Suma de cuadrados total corregida
(SCT_red=t(Y)%*%(I_red-(1/n_red)*J_red)%*%Y)
#Grados de libertad totales
(glt_red=R(I_red-(1/n_red)*J_red)) #Son 14 observaciones
#Suma de cuadrados del modelo
(SCM_red=t(Y)%*%(PX_red-(1/n_red)*J_red)%*%Y)
#Grados de libertad del modelo
(glm_red=R(PX_red-(1/n_red)*J_red))
#Suma de cuadrados del error
(SCE_red=t(Y)%*%(I_red-PX_red)%*%Y)
#Grados de libertad del error
(gle_red=R(I_red-PX_red))
#Cuadrado medio del modelo reducido
(CMM_red=SCM_red/glm_red)
#Cuadrado medio del error del modelo reducido
(CME_red=SCE_red/gle_red)
#Comprobación 
anova(modelo_red)
SCM_red+SCE_red
SCT_red #Son iguales
glm_red+gle_red
glt_red #Son iguales

#Calculo de F 
#Ho: ninguno de los factores influye sobre Y
#Ha: al menos un factor influye
(Fc_red=CMM_red/CME_red)
#Valor de tablas y pvalue
(ftabla_red=qf(1-0.05,glm_red,gle_red))
(pvalue_red=1-pf(Fc_red,glm_red,gle_red))
#No se rechaza Ho: por lo tanto
#se asume que los factores proteína y tipo de alimento no 
#influyen sobre Y


###Modelo completo: Si considera a la variable peso inicial
#----------------------------------------------------------------------------
X_completo
Y
#Solución de ecuaciones normales
R(X_completo)
XtX_comp=t(X_completo)%*%X_completo
GXtX_comp=ginv(XtX_comp)
beta_hat_comp=GXtX_comp%*%t(X_completo)%*%Y

modelo_comp=lm(Y~Prot+Alimento+Prot*Alimento+Peso_inicial,data = problema4)
summary(modelo_comp)
anova(modelo_comp)

#Formas cuadraticas
PX_comp=X_completo%*%GXtX_comp%*%t(X_completo)
J_comp=matrix(1,nrow(PX_comp),nrow(PX_comp))
I_comp=diag(1,nrow(X_completo))
n_comp=nrow(X_completo)
#Suma de cuadrados total corregida
(SCT_comp=t(Y)%*%(I_comp-(1/n_comp)*J_comp)%*%Y)
#Grados de libertad totales
(glt_comp=R(I_comp-(1/n_comp)*J_comp)) #Son 14 observaciones
#Suma de cuadrados del modelo
(SCM_comp=t(Y)%*%(PX_comp-(1/n_comp)*J_comp)%*%Y)
#Grados de libertad del modelo
(glm_comp=R(PX_comp-(1/n_comp)*J_comp))
#Suma de cuadrados del error
(SCE_comp=t(Y)%*%(I_comp-PX_comp)%*%Y)
#Grados de libertad del error
(gle_comp=R(I_comp-PX_comp))
#Cuadrado medio del modelo completo
(CMM_comp=SCM_comp/glm_comp)
#Cuadrado medio del error del modelo completo
(CME_comp=SCE_comp/gle_comp)

#Comprobación 
anova(modelo_comp)
SCM_comp+SCE_comp
SCT_comp #Son iguales
glm_comp+gle_comp
glt_comp #Son iguales

#Calculo de F
#Ho: ninguno de los factores influye sobre Y
#Ha: al menos un factor influye
(Fc_comp=CMM_comp/CME_comp)
#Valor de tablas y pvalue
(ftabla_comp=qf(1-0.05,glm_comp,gle_comp))
(pvalue_red=1-pf(Fc_comp,glm_comp,gle_comp))
#No se rechaza Ho: por lo tanto
#se asume que los factores proteína y tipo de alimento no 
#influyen sobre Y

###Comparando los modelos con F parcial
#----------------------------------------------------------------------------
(SCM_peso=SCM_comp-SCM_red)
#Ho. la variable peso inicial no influye en Y 
#Ha. lo contrario
(Fparcial=((SCM_comp-SCM_red)/(glm_comp-glm_red))/CME_comp)
(Ftabla_mod=(qf(1-0.05,glm_comp-glm_red,gle_comp)))
(pvalueFp=1-pf(Fparcial,(glm_comp-glm_red),gle_comp))
#No se rechaza Ho: la variable peso inicial no influye
#sobre Y. 
#para complementar, se obtuvo una grafica de dispersión
#Se observa que no hay correlacion entre ambas variables
cor(problema4$Y,problema4$Peso_inicial)
AIC(modelo_red,modelo_comp)






