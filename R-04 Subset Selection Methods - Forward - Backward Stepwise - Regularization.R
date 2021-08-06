########################################################################################
## BEST SUBSET SELECTION METHODS  
########################################################################################

library(ISLR)
names(Hitters)
dim(Hitters)

sum(is.na(Hitters))   #vemos cuantos elementos faltantes hay en data Hitters

Hitters=na.omit(Hitters)  #eliminamos filas donde haya elementos faltantes

dim(Hitters)
sum(is.na(Hitters))

## regsubset(Y~X, data) funcion con misma sintaxis que lm(), selecciona el mejor subset según RSS
##    parte de library(leaps)
library(leaps)
regfit.full=regsubsets(Salary~., Hitters)
summary(regfit.full)

# por defecto tira modelos hasta con ocho variables (abajo ahora se cambia esto).
# la forma de leer el summary es por filas. 
# Cada fila indica la cantidad de variables del modelo. y el "*" indica que variables incluye

# con nvmax cambiamos la cantidad maxima de variables a incluir en el modelo
regfit.full=regsubsets(Salary~., data=Hitters, nvmax=19)
reg.summary=summary(regfit.full)
names(reg.summary)

reg.summary$rsq  #el R^2 va de 32% (con una variable) hasta 54.6% (con todas)

par(mfrow=c(2,2))
plot(reg.summary$rss, 
     xlab="Number of Variables",
     ylab="RSS",
     type = "l")
plot(reg.summary$adjr2 , 
     xlab="Number of Variables",
     ylab="R^2 adjust",
     type = "l")   #type="l" hace conectar los puntos con linea

which.max(reg.summary$adjr2)  #identifica la location del punto máximo de un vector
points(11, reg.summary$adjr2[11], col="red", cex=2, pch=20)  #marca en grafico punto de maximo R^2

plot(reg.summary$cp, 
     xlab="Number of Variables",
     ylab="Cp",
     type = "l")
plot(reg.summary$bic , 
     xlab="Number of Variables",
     ylab="BIC",
     type = "l")

## Grafica ordenada según estadístico seleccionado en scale=""
##  Se lee por fila, la cual indica el valor del estadístico. Si el casillero está negro, quiere decir
##  que el modelo incluye esa variable:
plot(regfit.full, scale = "r2")
plot(regfit.full, scale = "adjr2")
plot(regfit.full, scale = "Cp")
plot(regfit.full, scale = "bic")

#Asi se ve que el de menor R2ajustado es el modelo con Atbat,Hits,Walks,Catbat, etc... 6 variables
#Para saber los coeficientes del modelo:

coef(regfit.full,6)


####################################################################################
## FORWARD and BACKWARD STEPWISE
####################################################################################
### A la funcion regsubsets() se le especifica el method= "forward" o "backward"

regfit.fwd=regsubsets(Salary~., data = Hitters,nvmax = 19, method = "forward")
summary(regfit.fwd)

regfit.bwd=regsubsets(Salary~., data = Hitters,nvmax = 19, method = "backward")
summary(regfit.bwd)

plot(regfit.fwd, scale = "adjr2", main ="Fwd Step")
plot(regfit.bwd, scale = "adjr2", main = "Bwd Step")
plot(regfit.full, scale = "adjr2", main = "full de a una variable")

#obteniendo coeficientes segun cant de variables del modelo:
coef(regfit.fwd,11)

names(summary(regfit.fwd))
plot(regfit.fwd$rss)


####################################################################################
## Choosing Among Models using: VALIDATION and CROSS-VALIDATION
####################################################################################
### Primero dividimos el dataset en trainingSet and tesSet
library(ISLR)
Hitters=na.omit(Hitters) 
set.seed(1)
train=sample(c(TRUE,FALSE), nrow(Hitters), replace = T)
test=!train   #Obs: Asi los divide casi a la mitad

## metodo AranaFiuba --> train=sample(nrow(Hitters), 0.8*nrow(Hitters))
##      trainFiuba=sample(nrow(Hitters), 0.8*nrow(Hitters))
##        Hitters[trainFiuba,]   --> el trainingSet
##        Hitters[-trainFiuba,]  --> el testStet


regfit.best=regsubsets(Salary~., data=Hitters[train,], nvmax = 19)  #entreno modelo con trainSet
summary(regfit.best)

test.mat=model.matrix(Salary~., data=Hitters[test,])  #model.matrix() construye "X" matrix con datos del testSet

## ahora un loop para cada tamaño "i" de modelo, se extraen los coeficientes del best Model 
## y se multiplican por la matrix X para obtener las predicciones y computar el Test MSE
## esto es porque no hay predict method para regsubsets

val.errors=rep(NA,19)
for (i in 1:19) {
  coefi=coef(regfit.best,id=i)
  pred=test.mat[,names(coefi)]%*%coefi
  val.errors[i]=mean((Hitters$Salary[test]-pred)^2)
  
}
val.errors
which.min(val.errors)

coef(regfit.best,7)

##########################################################
### RIDGE REGRESSION and the LASSO
##########################################################
## sintaxis diferente a Y~X , aca hay que pasar "x" matriz and "y" vector
## lo hacemos con func model.matrix que transforma tambien las variables cuali en dummies

##  glmnet(x, y, alpha, lambda=grid)       
##  si alpha=0 --> Ridge,  alpha=1 --> Lasso

library(ISLR)
Hitters=na.omit(Hitters)


x=model.matrix(Salary~.,Hitters)[,-1]
y=Hitters$Salary

library(glmnet)
# si alpha=0 --> Ridge,  alpha=1 --> Lasso

grid=10^seq(10,-2,length=100)   #estos seran los valores de lamba desde 10^10 hasta 10^-2
ridge.mod=glmnet(x,y,alpha = 0,lambda = grid)

names(ridge.mod)
#coeficientes de cada modelo segun el lambda ordenados en columnas (19 varibles + intecept)
dim(coef(ridge.mod))

#vemos modelo para cierto valor de lambda en posicion 50
ridge.mod$lambda[50]
coef(ridge.mod)[,50]

#otra forma mas practica de obtener coeficientes para un valor de lambda "s"
predict(ridge.mod, s=11497.57, type = "coefficients")

predict(ridge.mod, s=50, type = "coefficients")


## Ahora dividiendo training y test
set.seed(1)
train=sample(1:nrow(x),131)
test=(-train)
y.test=y[test]

ridge.mod=glmnet(x[train,],y[train], alpha=0, lambda = grid)
ridge.pred=predict(ridge.mod, s=4, newx = x[test,])           ##s=lambda, newx=newdataTipox
mean((ridge.pred-y.test)^2)

#con otro valor de lambda, uno muy grande
ridge.mod=glmnet(x[train,],y[train],alpha = 0, lambda = grid)
ridge.pred=predict(ridge.mod, s=1e10, newx = x[test,])
mean((ridge.pred-y.test)^2)

mean((mean(y[train])-y.test)^2)  ## valores muy grandes de lambda hacen cero a los coeficientes. dando lo mismo

## Ahora, en vez de elegir un valor arbitrario de lambda, hacemos crossValidation con la funcion cv.glmnet()
set.seed(1)
cv.out=cv.glmnet(x[train,], y[train], alpha=0)
plot(cv.out)

bestLam=cv.out$lambda.min
bestLam

##calculamos el test error con este modelo (valor de lambda)
ridge.pred=predict(ridge.mod, s=bestLam, newx=x[test,])
mean((ridge.pred-y.test)^2)

#comparamos vs modelo sin lambdas
ridge.pred=predict(ridge.mod,s=0,newx = x[test,])
mean((ridge.pred-y.test)^2)

##Con lambda=bestLam da mejor (mas chico el test MSE)

##Finalmente usamos todos los datos para entrenar el modelo, con el mejor lambda encontrado

out=glmnet(x,y, alpha=0)
predict(out, type = "coefficients", s=bestLam)
## Se ve que ninguno de los coeficientes es cero. A difirencia de Lasso (a continucion), que si selecciona variables
## haciendo coeficientes igual a cero


##########################################################
### The LASSO
##########################################################

lasso.mod=glmnet(x[train,],y[train], alpha = 1, lambda=grid)
plot(lasso.mod)   #algunos coef se hacen cero segun lambda

set.seed(1)
cv.out=cv.glmnet(x[train,],y[train], alpha=1)
plot(cv.out)
bestLam=cv.out$lambda.min
bestLam

lasso.pred=predict(lasso.mod, s=bestLam, newx=x[test,])
mean((lasso.pred-y[test])^2)

#ahora modelo con todos los datos con este valor de lambda optimo

out=glmnet(x,y, alpha = 1, lambda = grid)
lasso.coef=predict(out, s=bestLam, type = "coefficients")[1:20,]
lasso.coef

# Se ve que hay coeficientes iguales a cero. 
# Por lo que este modelo queda con menos variables explicativas que el anterior

