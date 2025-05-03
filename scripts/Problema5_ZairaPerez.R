#### Problema 
### 1. Captura de info
rm(list=ls())

### Estimando 
problema5 <- read.csv("~/01. PhD en Ciencias Forestales/1. Cursos/3. Verano/EST-642 - Modelos lineales/Examen/problema5.csv")
View(problema5)
X=model.matrix(~factor(problema5$Tratamiento)-1)
Z=model.matrix(~problema5$Vaca-1)
boxplot(problema5$Y~problema5$Vaca)

library(lme4)
library(nlme)


### 1. ML VS REML
#----------------------------------------------------------------
### 1. MODELO MIXTO CON EFECTO FIJO Y ALEATORIO - ML
mod1_ML=lme(Y ~ factor(Tratamiento), random = ~ 1|factor(Vaca), data = problema5,method = "ML")
summary(mod1_ML)
ranef(mod1_ML) #BLUP
fixef(mod1_ML) #BLUE
anova(mod1_ML) 


### 2. MODELO MIXTO CON EFECTO FIJO Y ALEATORIO - REML
mod2_REML=lme(Y ~ factor(Tratamiento), random = ~ 1|factor(Vaca), data = problema5,method = "REML")
summary(mod2_REML)
ranef(mod2_REML) #BLUP
fixef(mod2_REML) #BLUE
anova(mod2_REML)

### 3. COMPARACIÓN ENTRE MODELOS ML VS REML
AIC(mod1_ML,mod2_REML)
BIC(mod1_ML,mod2_REML)
fixef(mod1_ML) #BLUE
fixef(mod2_REML) #BLUE
ranef(mod1_ML)
ranef(mod2_REML)



### 2. MODELO COMPLETO VS REDUCIDOS
#----------------------------------------------------------------
### 1. MODELO SIN EFECTO ALEATORIO - REML
mod3_reduc=gls(Y ~  factor(Tratamiento), data = problema5,method = "REML")
summary(mod3_reduc)
#Ho: el efecto aleatorio no influye en el modelo
#Ha: el efecto aleatorio si influye en el modelo
anova(mod3_reduc,mod2_REML)


### 2. MODELO SIN EFECTO FIJO (Modelo anidado)
mod4_red=lme(Y~1, random = ~1|Vaca,method = "ML",data = problema5)
summary(mod4_red)
anova(mod4_red,mod1_ML)

### 2. MODELO SIN EFECTO FIJO (Modelo anidado)
mod4_red=lme(Y~1, random = ~1|Vaca,method = "ML",data = problema5)
summary(mod4_red)
anova(mod4_red,mod1_ML)


#Hay diferencias entre tratamientos
library(multcomp)
mmod.ttest=glht(mod2_REML)
summary(mmod.ttest)
#Variabilidad explicada en modelo mixto
#install.packages("MuMIn",dep=T)
library(MuMIn)
r.squaredGLMM(mod1_ML)
r.squaredGLMM(mod2_REML)
