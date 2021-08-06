library(ISLR)
names(Smarket)
dim(Smarket)
summary(Smarket)
##Queremos predecir Up or Down

pairs(Smarket)
cor(Smarket)  #matriz de correlaciones - la unica correlacion substancial parece ser entre "Year" y "Volume"
cor(Smarket[,-9])  #Hay que sacar $Direction ya que es variable cualitativa

attach(Smarket)
plot(Volume)


#############################################################################################
##LOGISTIC REGRESSION
#############################################################################################
##Syntax --> glm(Y~X, data=, family=binomial)  hay que poner family=binomial para indicar logistic

glm.fit=glm(Direction~Lag1+Lag2+Lag3+Lag4+Lag5+Volume, data=Smarket, family=binomial)
summary(glm.fit)

names(glm.fit)
names(summary(glm.fit))

glm.fit$coefficients
summary(glm.fit)$coefficients
contrasts(Direction)  #P(y=1|x)=valor será la prob de que suba ("Direction=Up" definido como 1)

glm.probs=predict(glm.fit, type = "response")  #type response -->output probabilites p(y=1|x)
glm.probs[1:10]                                 #si no se indican datos predice sobre todo el trainingSet


#Convirtiendo probabilidades a "Up" or "Down", estableciendo a "Decision Boundary"
glm.pred=rep("Down",1250)  #crea vector de todos Down igual a la cant de elementos predichos
glm.pred[glm.probs>0.5]="Up"

table(glm.pred, Direction)  #Matriz de Confusion - Diagonal elements = correct predictions

(145+507)/1250              #A cuantos le pega el modelo (calculado de dos formas)
mean(glm.pred==Direction)
1-mean(glm.pred==Direction) #Error en training Set (los que NO acierta)

#Ahora vamos a entrenar el modelo con los datos anteriores al 2005 (TrainingSet)
#Y dejar los del 2005 como TestSet
train=Year<2005                 #Vector Boolean donde indica como TRUE las posiciones en que Year<2005
Smarket.2005=Smarket[!train,]   #Usamos esas posiciones como filas en nuestro Data.Frame Smarket p/separar
Direction.2005=Direction[!train]

glm.fit=glm(Direction~Lag1+Lag2+Lag3+Lag4+Lag5+Volume, data=Smarket, family=binomial, subset = train)
#En vez de crear un nuevo data.frame subset year<2005 lo metemos como opcion "subset" -->vector de posiciones a tomar del data=Smarket

glm.probs=predict(glm.fit, Smarket.2005 ,type="response")
#Predecimos sobre TestSet

glm.pred=rep("Down", length(glm.probs))
glm.pred[glm.probs>0.5]="Up"

table(glm.pred,Direction.2005)
mean(glm.pred==Direction.2005)   #Los que acierta el modelo
1-mean(glm.pred==Direction.2005) #Error en TestSet --> los que no acierta el modelo (Muy Alto!)

####Ahora entrenamos el modelo sólo con dos variables
glm.fit=glm(Direction~Lag1+Lag2, data=Smarket, family = binomial, subset = train)
glm.probs=predict(glm.fit, Smarket.2005, type = "response")
glm.pred=rep("Down",length(glm.probs))
glm.pred[glm.probs>0.5]="Up"

table(glm.pred, Direction.2005)

##Predict con valores especificos para Lag1 y Lag2
predict(glm.fit, data.frame(Lag1=c(1.2, 1.5), Lag2=c(1.1, -0.8)), type="response")


####################################################################################################
##LDA
####################################################################################################
library(MASS)  #contiene lda()

lda.fit=lda(Direction~Lag1+Lag2, data=Smarket, subset = train)
lda.fit

par(mfrow=c(1,1))
plot(lda.fit)

lda.pred=predict(lda.fit, Smarket.2005)
names(lda.pred)   #"class" = prediccion de la clase
                  #"posterior" = probabilidad posterior P(Y=k|X=x)
                  #"x" = linear discriminants

lda.pred$class
table(lda.pred$class, Direction.2005)

mean(lda.pred$class==Direction.2005) #los que acierta


####################################################################################################
##QDA
####################################################################################################
library(MASS)  #contiene qda()

qda.fit=qda(Direction~Lag1+Lag2, data=Smarket, subset=train)
qda.fit

qda.pred=predict(qda.fit, Smarket.2005)

qda.pred
names(qda.pred)
qda.pred$class

table(qda.pred$class, Direction.2005)
mean(qda.pred$class==Direction.2005) #aciertos modelo


####################################################################################################
## K-NEAREST NEIGHBORS
####################################################################################################
## knn() requiere 4 inputs:
## 1. Una matriz con los predictores del training set (X inputs)
## 2. Una matriz con los predictores del test set (X inputs para los que queremos hacer predicciones)
## 3. Un vector con los class labels for training observations del punto 1. --> los valores de y
## 4. Un valor de K para clasificar
## Luego knn() tira directamente las predicciones

library(class)
## 1. y 2.
train.X=cbind(Lag1,Lag2)[train,]   #Combina columnas x1 y x2 de trainingSet
test.X=cbind(Lag1,Lag2)[!train,]   #Combina columnas x1 y x2 de testSet

## 3.
train.Direction=Direction[train]

set.seed(1)
knn.pred=knn(train.X, test.X, train.Direction, k=1)
knn.pred

table(knn.pred, Direction.2005)
(43+83)/(43+58+68+83)  #La pega 0,5 muy bajo, probamos con k más altos

knn.pred=knn(train.X, test.X, train.Direction, k=3)
knn.pred

table(knn.pred, Direction.2005)
mean(knn.pred==Direction.2005)

##------------------------------------------------------------------------------------------------
# KNN a CARAVAN INSURANCE DATA
##------------------------------------------------------------------------------------------------
attach(Caravan)
dim(Caravan)
names(Caravan)

summary(Purchase)  #Purchase es el Y, the "response variable"
348/(5474+348)    #El porcentaje de gente que compra (muy bajo--> 6%)

######## STANDARIZAR LOS DATOS PARA EVITAR QUE LAS DIFERENCIAS DE ESCALA AFECTEN "DISTANCE" ENTRE OBSERVACIONES
### FUNCION scale() standariza datos --> media=0 y var=1

standarized.X=scale(Caravan[,-86])  #Dejamos afuera col 86 que es "Purchase"

#dividimos en train y test los datos
test=1:1000  
train.X=standarized.X[-test,]
test.X=standarized.X[test,]

train.Y=Purchase[-test]
test.Y=Purchase[test]

set.seed(1)

knn.pred=knn(train.X, test.X, train.Y, k=1)
knn.pred

table(knn.pred, test.Y)
mean(knn.pred!=test.Y)  #error rate --> (knn.pred!=test.Y) es vector boolean sobre el total de test obs.

#yo quiero enganchar a los que compran Purchase = yes
#Me interese el ratio de cuantos predigo que sí le pego:
9/(68+9)
sum(knn.pred=="Yes" & test.Y=="Yes")/sum(knn.pred=="Yes")  #hecho logicamente la cuenta anterior

## Con K=3
knn3.pred=knn(train.X, test.X, train.Y, k=3)
table(knn3.pred, test.Y)

## Con K=5
knn5.pred=knn(train.X, test.X, train.Y, k=5)
table(knn5.pred, test.Y)


