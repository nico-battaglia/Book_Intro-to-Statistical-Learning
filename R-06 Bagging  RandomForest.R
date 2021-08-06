# Bagging and Random Forests
install.packages("randomForest")
library(randomForest)
library(MASS)

str(Boston)
?Boston


set.seed(1)

train = sample(nrow(Boston), 0.8*nrow(Boston))

# recordemos que bagging es un caso especial de RF en el que m = p.
#  Por consiguiente la función randomForest() se puede usar tanto para generar modelos 
#  de bagging como de RF

bag.boston = randomForest(medv~.,data=Boston,subset=train,mtry=13,importance=TRUE)
bag.boston

# El argumento mtry = 13 indica que los 13 predictores deben considerarse
# para cada partición del árbol, en otras palabras, o sea: bagging 
# ¿Funciona bien el modelo de bagging en el set de validación?

y.bag = predict(bag.boston,newdata=Boston[-train,])

mean((y.bag-Boston[-train,"medv"])^2)

plot(y.bag, Boston[-train,"medv"], xlab="Predicciones", ylab="Observados")
abline(0,1)


# Podriamos cambiar
# el número de árboles generados por randomForest () usando el argumento ntree:
# que es cantidad de trees grown by RandomForest
# cuál es la cant de árboles por default?
?randomForest  #ntree=500

#  Voy a probar con menos cantidad de árboles 
bag.boston=randomForest(medv~.,data=Boston,subset=train,mtry=13,ntree=40)
y.bag = predict(bag.boston,newdata=Boston[-train,])

mean((y.bag - Boston[-train,"medv"])^2)

# Ahora probemos con Random Forest. El único argumento que modificaremos es la cant de predictores
# a utilizar para entrenar cada árbol:  mtry. Por defecto, randomForest
# usa  p/3 variables al construir un RF de árboles de regresión y
# raizcuadrada(p) al construir un RF de árboles de clasificación. Nosotros usaremos
# mtry = 6.

set.seed(1)
rf.boston=randomForest(medv~., data=Boston, subset=train, mtry=6, importance=TRUE)

# Usando la función importance(), podemos ver la importancia de cada predictor

y.rf = predict(rf.boston, newdata=Boston[-train,])
mean((y.rf-Boston[-train,"medv"])^2)  #Si mejora, es mas chico. Con el mtry default es mejor aun
importance(rf.boston)


# Se utilizan dos medidas de performance por variable predictora. La primera , %IncMSE
# es el promedio de la disminución de la exactitud (accuracy) en las predicciones 
# cuando la  variable en cuestión se excluye del modelo. La segunda mide 
# la disminución total en la impureza del nodo que resulta de las particiones realizadas 
# sobre esa variable, promediada en todos los árboles.
# En el caso de árboles de regresión, la impureza del nodo se mide por el SCR, y 
# para árboles de clasificación por el error de clasificaión. 

# ordenada por %IncMSE
importance(rf.boston)[rev(order(importance(rf.boston)[,1])),]

# ordenada por IncNodePurity
importance(rf.boston)[rev(order(importance(rf.boston)[,2])),]

# Lo grafico
varImpPlot(rf.boston)

#  Vamos a ver como varían los CME de la estimación del medv probando con mtry= 1 a 13 (o sea de un RF
# con una sóla variable a un bagging, o sea m=p)

train_error = double(13)
test_error = double(13)

for (i in 1:13){
  rf.boston.i=randomForest(medv~. , data=Boston , subset=train , mtry=i , ntree=400)
  # train_error = mean(rf.boston.i$mse)

  y.rf.train = predict(rf.boston.i , newdata=Boston[train,])
  y.rf.test = predict(rf.boston.i , newdata=Boston[-train,])
  
  train_error[i] = mean((y.rf.train - Boston[train,"medv"])^2)
  test_error[i] = mean((y.rf.test - Boston[-train,"medv"])^2)
  }

# Graficamos
par(mfrow=c(1,1))
matplot(1:13,cbind(train_error,test_error), 
        pch=19, 
        col=c("red","blue"), 
        type = "b",
        xlab = "Cant var Predictoras", ylab="Cuadrado Medio esperado")

legend("right", 
       legend=c("Error de Entrenamiento","Error de Validación"),
       pch=19, 
       col=c("red","blue"), 
       horiz = F)

#  ¿Cua´l es el mínimo error en el set de validación?
min(test_error)

#  Veo cual es el tamaño que minimiza el CME test
which(test_error==min(test_error))

which.min(test_error) #mismo arriba forma mas linda

