#Para arboles de decision sobre variables categoricas ver 
# Cursos\DataMining FIUBA\Clases\06 - Árboles de Decisión\Árboles de Decisión.R

##########################################################################################
### REGRESSION TREES
##########################################################################################

library(MASS)
set.seed(1)
train=sample(1:nrow(Boston),nrow(Boston)/2)

tree.Boston=tree(medv~., Boston, subset = train)
summary(tree.Boston)

plot(tree.Boston)
text(tree.Boston, pretty = 0)

## CrosValidation + pruning
## Con cv.tree() elegimos cant de nodos que minimizan error
## Luego con prune.tree() podamos el arbol

cv.Boston=cv.tree(tree.Boston)
plot(cv.Boston)
plot(cv.Boston$size, cv.Boston$dev, type = "b")

which.min(cv.Boston$dev)
cv.Boston$dev
bestCantNodos=cv.Boston$size[which.min(cv.Boston$dev)]

prune.Boston=prune.tree(tree.Boston, best=6)
plot(prune.Boston)
text(prune.Boston, pretty = 0)

#Prediction in the testSet

y.pred=predict(prune.Boston,newdata=Boston[-train,])
y.test=Boston$medv[-train]

accuracy=mean((y.pred-y.test)^2)
accuracy

#sin pruning
y.pred.Complete.Tree=predict(tree.Boston, newdata = Boston[-train,])

accuracy.Complete.Tree=mean((y.pred.Complete.Tree-y.test)^2)
accuracy.Complete.Tree

plot(y.pred.Complete.Tree, y.test)
abline(0,1)

##Sigue con random forest en Cursos\DataMining FIUBA\Clases\07 - Ensambles de Árboles de Decisión