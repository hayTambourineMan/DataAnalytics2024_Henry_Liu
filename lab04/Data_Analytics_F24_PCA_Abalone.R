##########################################
### Principal Component Analysis (PCA) ###
##########################################

library(ggfortify)
library(e1071)
library(class)
library(psych)

# PCA with iris dataset
## read data
abalone <- read.csv("~/Courses/Data Analytics/Fall24/labs/Lab02_2/abalone/abalone.data", header=FALSE)

## rename columns
colnames(abalone) <- c("sex", "length", 'diameter', 'height', 'whole_weight', 'shucked_wieght', 'viscera_wieght', 'shell_weight', 'rings' ) 

## derive age group based in number of rings
abalone$age.group <- cut(abalone$rings, br=c(0,8,11,35), labels = c("young", 'adult', 'old'))

abalone <- abalone[,-c(1,9)]
abalone <- abalone[-which(abalone$age.group=="old"),]

abalone <- abalone[-c(1418,2052),]

# abalone <- abalone[,-c(1,2,4,6)]

pairs.panels(abalone[,-8],gap = 0,bg = c("red", "yellow", "blue")[abalone$age.group],pch=21)

ggplot(abalone, aes(x = length, y = whole_weight, colour = age.group)) +
  geom_point()

ggplot(scale(abalone[,-8], scale = FALSE, center = TRUE), aes(x = length, y = whole_weight, colour = abalone$age.group)) +
  geom_point()

ggplot(scale(abalone[,-8], scale = TRUE, center = FALSE), aes(x = length, y = whole_weight, colour = abalone$age.group)) +
  geom_point()

# abalone.pc <- princomp(abalone[,-8], cor = TRUE, score = TRUE)

abalone.pc <- prcomp(abalone[,-8], center = TRUE, scale. = TRUE)

attributes(abalone.pc)
summary(abalone.pc)
abalone.pc$rotation

# using the plot() function, we can plot the principal components.
plot(abalone.pc)

# plotting the abalone.pc using the a line in plot() functions 
# plot(abalone.pc, type = "l")

# using rhw biplot() function we can plot the components
# biplot(abalone.pc)

## using autoplot() function to plot the components
autoplot(abalone.pc, data = abalone, colour = 'age.group',
         loadings = TRUE, loadings.colour = 'blue',
         loadings.label = TRUE, loadings.label.size = 3)


k=65
knn.pred <- knn(train = abalone[,-8], test = abalone[,-8], cl = abalone$age.group, k = k)

## evaluate
cm <- table(Predicted=knn.pred, Actual = abalone$age.group, dnn=list('predicted','actual'))

cm

n = sum(cm) # number of instances
nc = nrow(cm) # number of classes
diag = diag(cm) # number of correctly classified instances per class 
rowsums = apply(cm, 1, sum) # number of instances per class
colsums = apply(cm, 2, sum) # number of predictions per class
p = rowsums / n # distribution of instances over the actual classes
q = colsums / n # distribution of instances over the predicted 

precision = diag / colsums 
recall = diag / rowsums 
f1 = 2 * precision * recall / (precision + recall) 

data.frame(recall, precision, f1)


k=65
knn.pred <- knn(train = abalone.pc$x[,c(1,2)], test = abalone.pc$x[,c(1,2)], cl = abalone$age.group, k = k)

## evaluate
cm <- table(Predicted=knn.pred, Actual = abalone$age.group, dnn=list('predicted','actual'))

cm

n = sum(cm) # number of instances
nc = nrow(cm) # number of classes
diag = diag(cm) # number of correctly classified instances per class 
rowsums = apply(cm, 1, sum) # number of instances per class
colsums = apply(cm, 2, sum) # number of predictions per class
p = rowsums / n # distribution of instances over the actual classes
q = colsums / n # distribution of instances over the predicted 

precision = diag / colsums 
recall = diag / rowsums 
f1 = 2 * precision * recall / (precision + recall) 

data.frame(recall, precision, f1)


k=3
knn.pred <- knn(train = abalone.pc$scores[,c(1,2,3)], test = abalone.pc$scores[,c(1,2,3)], cl = abalone$Type, k = k)

## evaluate
cm <- table(Predicted=knn.pred, Actual = abalone$Type, dnn=list('predicted','actual'))

cm

n = sum(cm) # number of instances
nc = nrow(cm) # number of classes
diag = diag(cm) # number of correctly classified instances per class 
rowsums = apply(cm, 1, sum) # number of instances per class
colsums = apply(cm, 2, sum) # number of predictions per class
p = rowsums / n # distribution of instances over the actual classes
q = colsums / n # distribution of instances over the predicted 

precision = diag / colsums 
recall = diag / rowsums 
f1 = 2 * precision * recall / (precision + recall) 

data.frame(recall, precision, f1)


k=3
knn.pred <- knn(train = abalone.pc$scores[,c(1,2,3)], test = abalone.pc$scores[,c(1,2,3)], cl = abalone$age.group, k = k)

## evaluate
cm <- table(Predicted=knn.pred, Actual = abalone$age.group, dnn=list('predicted','actual'))

cm

n = sum(cm) # number of instances
nc = nrow(cm) # number of classes
diag = diag(cm) # number of correctly classified instances per class 
rowsums = apply(cm, 1, sum) # number of instances per class
colsums = apply(cm, 2, sum) # number of predictions per class
p = rowsums / n # distribution of instances over the actual classes
q = colsums / n # distribution of instances over the predicted 

precision = diag / colsums 
recall = diag / rowsums 
f1 = 2 * precision * recall / (precision + recall) 

data.frame(recall, precision, f1)
