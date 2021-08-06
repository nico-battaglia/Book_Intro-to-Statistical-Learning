################################################################################################
##  RESAMPLING METHODS   Cross Validation
################################################################################################

### Validation Set Approach
#----------------------------
library(ISLR)  #Auto dataSet
set.seed(1)

dim(Auto)

train=sample(392,196) #elije al azar 196 elementos del 1:932
train

#en vez de usar todo el dataSet usamos la mitad
#con la opcion subset, le pasamos 196 indices tomados al azar del total de elementos de "Auto"

lm.fit=lm(mpg~horsepower, data = Auto, subset = train)   
lm.fit
dim(Auto)

y.validation=predict(lm.fit,Auto[-train,])

(sum((Auto$mpg[-train]-y.validation)^2))/196  #MSE --> [SUM(Yi-Yestimado)^2]/n

mean((Auto$mpg-predict(lm.fit,Auto))[-train]^2) #otra formade calcular MSE.
#Que es el error del testSet para el modelo lineal


#ahora para cuadratico y cubico
lm.fit2=lm(mpg~poly(horsepower,2),data = Auto, subset = train)
mean((Auto$mpg-predict(lm.fit2,Auto))[-train]^2)

lm.fit3=lm(mpg~poly(horsepower,3),data = Auto, subset=train)
mean((Auto$mpg-predict(lm.fit3,Auto))[-train]^2)

lm.fit4=lm(mpg~poly(horsepower,4),data = Auto, subset=train)
mean((Auto$mpg-predict(lm.fit4,Auto))[-train]^2)

errorTabla=data.frame(GradoPol=c(1,2,3,4),errorTestSet=0)
errorTabla[1,"errorTestSet"]=mean((Auto$mpg-predict(lm.fit,Auto))[-train]^2)
errorTabla[2,"errorTestSet"]=mean((Auto$mpg-predict(lm.fit2,Auto))[-train]^2)
errorTabla[3,"errorTestSet"]=mean((Auto$mpg-predict(lm.fit3,Auto))[-train]^2)
errorTabla[4,"errorTestSet"]=mean((Auto$mpg-predict(lm.fit4,Auto))[-train]^2)
errorTabla

plot(errorTabla)
lines(errorTabla)   
#se ve que el error disminuye considerablemente al pasar de linear a ^2, pero para grados mayores no gran mejora


### LOOCV - Leave One Out Cross Validation ##
#----------------------------------------------

attach(Auto)

glm.fit=glm(mpg~horsepower, data=Auto)
lm.fit=lm(mpg~horsepower)

glm.fit
lm.fit

## glm() es una funcion más general, para modelos linearizados.
## familiy= "binomial" te hace reg logistica y otros opciones
## si no se especifica nada hace un modelo reg lineal comun
## pero tiene opcion cv.glm() --> que te devuelve automaticamente crossValidation results

library(boot)  ##Contiene cv.glm()
glm.fit=glm(mpg~horsepower, data=Auto)
cv.err=cv.glm(Auto,glm.fit)

names(cv.err)
cv.err$delta  ##delta devuelve error al hacer cross validation 

##repetimos el procedimiento para polinomios hasta grado 5
cv.error=1:5
for (i in 1:5) {
  glm.fit=glm(mpg~poly(horsepower,i))
  cv.error[i]=cv.glm(Auto,glm.fit)$delta[1]
  
}
cv.error

plot(cv.error, pch=20)  ##igual que antes, baja mucho el error de lineal a cuadratico, despues no suma demasiado



### k-Fold Cross Validation
#----------------------------------------------
# puede ser implementado autom. a traves de la funcion glm() con opcion cv.glm(Data, modelo, K=nro de Folds)

library(boot)  #contiene la func glm()

set.seed(17)
cv.error=1:10

for (i in 1:10) {
  glm.fit=glm(mpg~poly(horsepower,i))
  cv.error[i]=cv.glm(Auto, glm.fit, K=10)$delta[1]
  
}
cv.error
plot(cv.error, pch=20, col=2)
lines(cv.error, col=2)
