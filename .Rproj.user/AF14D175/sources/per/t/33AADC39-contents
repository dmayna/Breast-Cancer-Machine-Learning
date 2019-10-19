# importing the dataset
dataset = read.csv('breastCancerWisconsin.csv')
dataset <- subset(dataset, select = -c(1))
names(dataset) <- c("Clump Thickness","Cell Size", "Cell Shape", "Marginal Adhesion", "Epithelial Cell Size", "Bare Nuclei", "Bland Chromatin","Normal Nucleoli","Mitoses","Class")

#check if any empty values
indx <- apply(dataset, 2, function(x) any(is.na(x) | is.infinite(x)))
dataset$`Bare Nuclei` = ifelse(dataset$`Bare Nuclei` == "?",
                               ave(dataset$`Bare Nuclei`, FUN = function(x) mean(x,na.rm = TRUE)),
                               dataset$`Bare Nuclei`)
dataset <- data.frame(lapply(dataset, function(x) {gsub("?", 0, x)}))

#(2 for benign, 4 for malignant)
dataset$Class = ifelse(dataset$Class == 2,1,0)
#(1 for benign, 0 for malignant)

# splitting the dataset into training set and test set
#install.packages('caTools')
library(caTools)
set.seed(123) 
split = sample.split(dataset$Class,SplitRatio=0.8)
training_set = subset(dataset, split ==TRUE)
test_set = subset(dataset, split ==FALSE)

# Fitting data to training set
#install.packages('e1071')
library(e1071)
classifier = svm(formula = Class~.,
                 data = training_set,
                 type = 'C-classification',
                 kernel = 'linear')


# Predicting the Test set results
prob_pred = predict(classifier,type = 'response', test_set[-10])
y_pred = predict(classifier,newdata = test_set[-10])

# making the confusion matrix
cm = table(test_set[,10],y_pred)



####### Decision Tree #####################
#install.packages('rpart')
library(rpart)
treeClassifier = rpart(formula = Class ~.,
                       data = training_set)
treeY_pred = predict(treeClassifier,newdata = test_set[-10],type='class')
tree_cm = table(test_set[,10],treeY_pred)
plot(treeClassifier)
text(treeClassifier)
#0.655 confidence

############# Random forest ################
#install.packages('randomForest')
library(randomForest)
forestClassifier = randomForest(x = training_set[-10],
                                y = training_set$Class,
                                ntree = 10)
y_pred_forest = predict(forestClassifier,newdata = test_set[-10])
cm_forest = table(test_set[,10],y_pred_forest)

############### KNN ##############################
library(class)
knn_y_pred = knn(train = training_set[,-10],
                 test = test_set[,-10],
                 cl = training_set[,10],
                 k = 2)
cm_knn = table(test_set[,10],knn_y_pred)





# visualizing the training set reslults
#install.packages('ElemStatLearn')
library(ElemStatLearn)
set = training_set
X1 = seq(min(set[,1])-1,max(set[,1])+1, by = 0.01)
X2 = seq(min(set[,10])-1,max(set[,10])+1, by = 0.01)
grid_set = expand.grid(X1,X2)
colnames(grid_set) = c('Clump Thickness','Class')
prob_set = predict(classifier,type = 'response',newdata = grid_set)
y_grid = ifelse(prob_set > 0.5, 1, 0)
plot(set[,10],
     main = 'Logistic Regresion (Training Set)',
     xlab = "Clump Thickness", ylab = 'Class',
     xlim = range(X1), ylim = range(X2))
contour(X1,X2,matrix(as.numeric(y_grid), length(X1), length(X2)), add = TRUE)
points(grid_set, pch = '.', col = ifelse(y_grid == 1,'springgreen3', 'tomato'))
points(set, pch = 21, bg = ifelse(set[,10] == 1,'green4', 'red3'))

set = test_set
X1 = seq(min(set[,1])-1,max(set[,1])+1, by = 0.01)
X2 = seq(min(set[,10])-1,max(set[,10])+1, by = 0.01)
grid_set = expand.grid(X1,X2)
colnames(grid_set) = c('Clump Thickness','Class')
prob_set = predict(classifier,type = 'response',newdata = grid_set)
y_grid = ifelse(prob_set > 0.5, 1, 0)
plot(set[,10],
     main = 'Logistic Regresion (Training Set)',
     xlab = "Clump Thickness", ylab = 'Class',
     xlim = range(X1), ylim = range(X2))
contour(X1,X2,matrix(as.numeric(y_grid), length(X1), length(X2)), add = TRUE)
points(grid_set, pch = '.', col = ifelse(y_grid == 1,'springgreen3', 'tomato'))
points(set, pch = 21, bg = ifelse(set[,10] == 1,'green4', 'red3'))



# other visuals
library(ggplot2)

#p=ggplot(dataset,aes(y =dataset["Clump Thickness"] ,x=seq.int(nrow(dataset)),color=as.factor(Class), shape = Mitoses ,size = dataset["Cell Size"])) +
 # geom_point(fill="blue") + scale_shape_identity() + scale_shape_discrete(solid=TRUE, legend=TRUE)+ xlab("Clump Thickness") +
  #ylab("ID Number")

#+theme(legend.position="top")

p = ggplot(dataset, aes(x=dataset["Clump Thickness"], y=seq.int(nrow(dataset)))) +
  geom_point(aes(shape=Mitoses, color=as.factor(Class), size=dataset["Cell Size"]))+
  scale_shape_manual(values=c(3, 16, 17))+
  scale_color_manual(values=c('#78c3f0','#d20a0a'))+
  theme(legend.position="top") +
  scale_shape_identity() +
  xlab("Clump Thickness") +
  ylab("ID Number") 

#+ 
  
 # scale_shape_manual(name = "Legend",
  #                   labels = c(1,2,3,4,5,6,7,8,9,10),
   #                  values = c(1,2,3,4,5,6,7,8,9,10))