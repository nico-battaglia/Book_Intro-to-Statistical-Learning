#Loading Data
#----------------------------------------------------
Auto=read.table("./Auto.data", 
                header = T,
                na.strings = "?")

fix(Auto)
dim(Auto)
names(Auto)
head(Auto)

Auto=na.omit(Auto)  #omite valores Faltantes
dim(Auto)

plot(Auto$cylinders,Auto$mpg)  #Referirse a variables con signo $ --> Data$NameVariable

attach(Auto)  #Hace las variables disponibles por nombre solamente

plot(cylinders, weight)

cylinders=as.factor(cylinders)  #transforma variable numerica en factor/categoría
plot(cylinders,mpg)

hist(mpg, col=2)  #Histograma
hist(mpg, col=6, breaks = 15)  #Histograma

pairs(Auto)
pairs(~mpg+displacement+horsepower, Auto)

plot(horsepower,mpg)
identify(horsepower,mpg,name) #al hacer click en grafico identifica punto

summary(Auto)
summary(mpg)

#----------------------------------------------------------------------------------

## Regresion Lineal ##
#----------------------------------------------------------------------------------
library(MASS)
library(ISLR)

fix(Boston)
names(Boston)
dim(Boston)
?Boston
attach(Boston)  #pone variables disponibles para ser llamadas sin Boston$VariableName

##Sintaxis lm(Y~X , data= ) o attach(data) antes para no referenciar cada vez las variables
lm.fit=lm(medv~lstat, data = Boston)  #modelo solo con lstat como varX

lm.fit
summary(lm.fit)
names(lm.fit)
names(summary(lm.fit))

lm.fit$coefficients       #coeficientes de la regresion
confint(lm.fit)           #intervalosde confianza

predict(lm.fit,data.frame(lstat=c(5,10,15)),interval = "confidence")  #prediccion (modelo,datos,intervaloSi/No)
                                                            #newData que se le pasa tiene que ser formato DataFrame

predict(lm.fit, data.frame(lstat=c(5,10,15)),interval = "prediction")

predict(lm.fit)  #si no se especifican datos, predice con el modelo "lm.fit"
                  #todos los datos con los que fue entrenado el modelo

plot(lstat,medv, col="blue", pch=20)
abline(lm.fit, lwd=3, col=2)   #agrega linea con a= intercep b=slope, u objeto con coef a,b como lm.fit

plot(1:20,1:20,pch=1:20)  #distintos caracteres para dibujar del pch= 1 al 20

plot(lm.fit)  #plotea residuos de la regresion vs fitted values

par(mfrow=c(2,2))  #divide pantalla en grilla 2x2 para ver graficos todos juntos
plot(lm.fit)

par(mfrow=c(2,2))
plot(predict(lm.fit),residuals(lm.fit))               #graficos de valorespredichos vs.
plot(lm.fit$fitted.values,lm.fit$residuals)           #residuos, hechos de dos maneras diferentes

#-----------------------------------------------------------------------------------------

##Regresion Lineal Multiple
#------------------------------------------------------------------------------------------
#lm() syntax --> lm(Y~X1+X2+X3)
#            -->  ~. para usar todas las variables del data --> lm(Y~.)

lm.fit=lm(medv~lstat+age, data=Boston)
summary(lm.fit)

lm.fit=lm(medv~., data=Boston)   #Usando todas las variables con ~.
summary(lm.fit)

names(lm.fit)
names(summary(lm.fit))  #El summary contiene R^2 y sigma (el RSE)

lm.fit=lm(medv~.-age, data=Boston)  #Todas las variables excluyendo a "age"
summary(lm.fit)

##Interaction terms---------------------------------------------
#   Syntax -->  X1:X2
#   Syntax -->  X1*X2 es shorthand para (X1+X2+X1:X2)

lm.fit=lm(medv~lstat*age)
summary(lm.fit)

##Non Linear Transformations-----------------------------
#   Dado X creamos predictor X^2 --> syntax I(X^2) --> hay que usar I() no solo ^2

lm.fit2=lm(medv~lstat+I(lstat^2))
summary(lm.fit2)

par(mfrow=c(2,2))
plot(lm.fit2)

par(mfrow=c(1,1))
plot(lstat, medv, col="blue", pch=20)
points(lstat, lm.fit2$fitted.values, lwd=3, col=2)  #points() para superponer al anterior


##Polinomios grado superior - funcion poly()------------------------

lm.fit5=lm(medv~poly(lstat,5))
summary(lm.fit5)

points(lstat,lm.fit5$fitted.values)

#----------------------------------------------------------------------------------

##QUALITATIVE PREDICTORS
#----------------------------------------------------------------------------------
attach(Carseats)
fix(Carseats)
names(Carseats)

contrasts(Carseats$ShelveLoc)  #Genera dummies variables for cualitatives automaticamente
                                #Constrasts() solo los muestra, pero los genera automaticamente R

plot(ShelveLoc ,Sales)

lm.fitQual=lm(Sales~.+Income:Advertising+Price:Age, data=Carseats)
summary(lm.fitQual)


#En este caso se crearon variables dummies ShelveLocGood y ShelveLocMedium (bad es cuando estas dos son cero)

#----------------------------------------------------------------------------------

##WRITING FUNCTIONS
#----------------------------------------------------------------------------------

NombreFuncion=function(){
  print("Esto es una funcion en ejecucion")
  x=1:10
  print(x)
}

NombreFuncion()

