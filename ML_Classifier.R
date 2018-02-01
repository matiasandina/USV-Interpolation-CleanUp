library(caret)



mycontrol = trainControl(method="repeatedcv", number=10, repeats=3)

# parameter fine tuning for gbm

grid <- expand.grid(.interaction.depth = seq(1,7,2),
                    .n.trees = seq(100, 1000, 50),
                    .shrinkage = c(0.01, 0.1),
                    .n.minobsinnode = c(10))


full_model = train(label~., data=Full_data , method="rpart",
                   trControl=mycontrol)


# mymodel = train(churn~., data=trainset, method="rpart",
#                preProcess="scale", trControl=control)

set.seed(1)

# The event probability is stratified within the data, it keeps the distribution in
# the original set

inTrainingSet <- createDataPartition(Full_data$label, p=0.30,list = F)

TrSet <- Full_data[inTrainingSet,]
TestSet <- Full_data[-inTrainingSet]



TrSet$label <- as.numeric(TrSet$label)
TestSet$label <- as.numeric(TestSet$label)



# This breaks with variables that have full zero
# procValues <- preProcess(TrSet, method = "pca", thresh = 80, scale=F)

model_30_percent = train(label~., data=TrSet , method="rpart",
                   trControl=mycontrol)

gmbTune <- train(label~., data=TrSet, method='gbm',
                 verbose=FALSE, trControl=mycontrol, tuneGrid=grid)



#### neural net

# Get data without adaptors
to.nnet<-data.frame(QQ, duration=filtered_50$duration, label=filtered_50$label) 

# Fix label to numbers
to.nnet$num.label<-as.numeric(as.factor(to.nnet$label))

# Group categories into broader categories
to.nnet<-tidyr::separate(to.nnet, label,remove=FALSE,into=c("main","secondary"),by='-')
to.nnet$main.label<-as.numeric(as.factor(to.nnet$main))

# Get the matrix (large and small regards the number of 'y' categories)

X_nnet_large <- select(to.nnet, f1:f50,duration, num.label)
X_nnet_small <- select(to.nnet, f1:f50,duration, main.label)

# Partition data

set.seed(825)

inTR_large <- createDataPartition(X_nnet_large$num.label, p=0.7,list = F)


net_Tr_large <- X_nnet_large[inTR_large,]
net_Test_large <- X_nnet_large[-inTR_large,]

# Round the prediction to the nearest integer
pred_large <- round(predict(annFit_large, newdata=net_Test_large),0)

# append to test set with the original levels so we can do the confusion matrix
net_Test_large$pred_large <- factor(pred_large, levels=levels(factor(net_Test_large$num.label)))
confusionMatrix(net_Test_large$num.label, net_Test_large$pred_large)



inTR_small <- createDataPartition(X_nnet_small$main.label, p=0.7,list = F)

net_Tr_small <- X_nnet_small[inTR_small,]
net_Test_small <- X_nnet_small[-inTR_small,]

# Get grid for tunning parameters and fit data

tunGrid <-  expand.grid(size  = c(5, 6, 7),
                        decay = c(0, 10^(-2), 10^(-3), 10^(-4)))

fitControl <- trainControl(method = "repeatedcv", number = 5, repeats=3)


annFit_large <- train(num.label~., data = net_Tr_large,
                 method = "nnet",
                 trControl = fitControl,
                 tuneGrid  = tunGrid,
                 preProcess = c('center', 'scale'),
                 linout = TRUE) # linout = True is because we are not modeling logistic but n categories as output

annFit_small <- train(main.label~., data = net_Tr_small,
                      method = "nnet",
                      trControl = fitControl,
                      tuneGrid  = tunGrid,
                      preProcess = c('center', 'scale'),
                      linout = TRUE)

# Round the prediction to the nearest integer
pred_small <- round(predict(annFit_small, newdata=net_Test_small),0)

# append to test set with the original levels so we can do the confusion matrix
net_Test_small$pred_small <- factor(pred_small, levels=levels(factor(net_Test_small$main.label)))
confusionMatrix(net_Test_small$main.label, net_Test_small$pred_small)


## THREE LABELS
# Encoding 1 = flat; 2 = short ; 3 = FM

to.nnet$CallClass <- ifelse(to.nnet$main=="flat",
                            1,
                            ifelse(to.nnet$main=="short",
                                   2, 3)
)


X <- to.nnet[, 1:51]
Y <- to.nnet[, 53:57]

X_part <- createDataPartition(Y$CallClass, p=0.7,list = F)

X_train <- X[X_part,]
X_test <- X[-X_part,]

Y_train <- Y[X_part,]
Y_test <- Y[-X_part,]


annFit_Class <- train(y = Y_train$CallClass, x=X_train,
                      method = "nnet",
                      trControl = fitControl,
                      tuneGrid  = tunGrid,
                      preProcess = c('center', 'scale'),
                      maxit = 1000,
                      linout = TRUE)


# Round the prediction to the nearest integer
pred_class <- factor(round(predict(annFit_Class, newdata=X_test),0),
                     levels=levels(factor(Y_test$CallClass)))

# append to test set with the original levels so we can do the confusion matrix
confusionMatrix(data = pred_class, reference = Y_test$CallClass)


tunGridSVM <-  data.frame(C=c(0.25, 0.5, 1, 2, 4, 16, 32))


## pcaNNET ?
source('src/Summarise_Me.R')
source('src/Auto_Corr.R')

to.pca <- Summarise_Me(QQ, 1)
to.pca$duration <- log(filtered_50$duration)
to.pca$label    <- filtered_50$label
to.pca$CallClass <- factor(to.nnet$CallClass)

pca_train <- to.pca[inTR_small,]
pca_test <- to.pca[-inTR_small,]


pcaNNET <- train(y = pca_train$CallClass, x=pca_train[,1:8],
                      method = 'pcaNNet',
                      trControl = fitControl,
                      tuneGrid  = tunGrid,
                      preProcess = c('center', 'scale'),
                      maxit = 500,
                      linout = TRUE)


# Round the prediction to the nearest integer
pred_pcaNNET <- predict(pcaNNET, newdata=pca_test[,1:8])

# append to test set with the original levels so we can do the confusion matrix
confusionMatrix(data = pred_pcaNNET, reference = pca_test$CallClass)


rgl::plot3d(my.PCA$x[,1:3], col=ggplot2::alpha(to.pca$CallClass,0.5),pch=19)
rgl::plot3d(my.PCA$x[,2:4], col=ggplot2::alpha(to.pca$CallClass,0.5),pch=19)
rgl::plot3d(my.PCA$x[,c(1,2,4)], col=ggplot2::alpha(to.pca$CallClass,0.5),pch=19)


## Plot decision boundary
# decisionplot might be useless

decisionplot <- function(model, data, class = NULL, predict_type = "class",
                         resolution = 100, showgrid = TRUE, ...) {
  
  if(!is.null(class)) cl <- data[,class] else cl <- 1
  data <- data[,1:2]
  k <- length(unique(cl))
  
  plot(data, col = as.integer(cl)+1L, pch = as.integer(cl)+1L, ...)
  
  # make grid
  r <- sapply(data, range, na.rm = TRUE)
  xs <- seq(r[1,1], r[2,1], length.out = resolution)
  ys <- seq(r[1,2], r[2,2], length.out = resolution)
  g <- cbind(rep(xs, each=resolution), rep(ys, time = resolution))
  colnames(g) <- colnames(r)
  g <- as.data.frame(g)
  
  ### guess how to get class labels from predict
  ### (unfortunately not very consistent between models)
  p <- predict(model, g, type = predict_type)
  if(is.list(p)) p <- p$class
  p <- as.factor(p)
  
  if(showgrid) points(g, col = as.integer(p)+1L, pch = ".")
  
  z <- matrix(as.integer(p), nrow = resolution, byrow = TRUE)
  contour(xs, ys, z, add = TRUE, drawlabels = FALSE,
          lwd = 2, levels = (1:(k-1))+.5)
  
  invisible(z)
}

decisionplot(pcaNNET, data=pred_pcaNNET, "CallClass",main = "NNet Decision")

http://www.cmap.polytechnique.fr/~lepennec/R/Learning/Learning.html


# SVM TAKES TOO MUCH TIME

model_SVM <- train(y=Y_train$CallClass,x=X_train,
                 method='svmLinear',
                 trControl = fitControl,
                 tuneGrid  = tunGridSVM,
                 preProcess = c('center', 'scale'))
