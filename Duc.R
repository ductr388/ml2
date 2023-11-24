library(tree)
library(knitr)
library(ggplot2)
# Questions                                         ####

# General: Does not say which variables to use, so use all but change categorical variables to factor?
# What about pdays, it says missing values are 999 but data is -1?

# 2.2c: Only min deviance and not min node size?

# 2.5: Should we estimate a NEW model with same number of leaves found in 2.3 
# but with a different loss function?


# 2.1                                               ####
# Index 12 is the variable duration
data <- read.csv2("bank-full.csv", stringsAsFactors = TRUE)[, -12]

n=dim(data)[1]
set.seed(12345)
id=sample(1:n, floor(n*0.4))
train=data[id,]

id1=setdiff(1:n, id)
set.seed(12345)
id2=sample(id1, floor(n*0.3))
valid=data[id2,]

id3=setdiff(id1,id2)
test=data[id3,] 




# 2.2a                                              ####
tree_a <- tree(y ~., data=train)
plot(tree_a)
text(tree_a, pretty=0)
# Default setting in tree if we don't specify any values for tree control?
# Need to double check but looks the same. 
tree_a <- tree(y ~., data=train, 
               control=tree.control(nobs = nrow(train), # Observations in training set
                                    mincut = 5,     # Min size for a leaf?
                                    minsize = 10,   # Min size for a node
                                    mindev = 0.01)) # max 10% misclassification
plot(tree_a)
text(tree_a, pretty=0)

pred_train <- predict(tree_a, newdata=train, type="class")
table_train <- table(train$y, pred_train)
miss_train <- 1 - (sum(diag(table_train)) / sum(table_train))

pred_valid <- predict(tree_a, newdata=valid, type="class")
table_valid <- table(valid$y, pred_valid)
miss_valid <- 1 - (sum(diag(table_valid)) / sum(table_valid))

model_a <- c(miss_train, miss_valid)
# 2.2b                                              ####
tree_b <- tree(y ~., data=train, 
               control=tree.control(nobs = nrow(train),
                                    minsize = 7000))
plot(tree_b)
text(tree_b, pretty=0)

pred_train <- predict(tree_b, newdata=train, type="class")
table_train <- table(train$y, pred_train)
miss_train <- 1 - (sum(diag(table_train)) / sum(table_train))

pred_valid <- predict(tree_b, newdata=valid, type="class")
table_valid <- table(valid$y, pred_valid)
miss_valid <- 1 - (sum(diag(table_valid)) / sum(table_valid))

model_b <- c(miss_train, miss_valid)

# 2.2c                                              ####
tree_c <- tree(y ~., data=train, 
               control=tree.control(nobs = nrow(train),
                                    mindev = 0.0005))
plot(tree_c)
text(tree_c, pretty=0)

pred_train <- predict(tree_c, newdata=train, type="class")
table_train <- table(train$y, pred_train)
miss_train <- 1 - (sum(diag(table_train)) / sum(table_train))

pred_valid <- predict(tree_c, newdata=valid, type="class")
table_valid <- table(valid$y, pred_valid)
miss_valid <- 1 - (sum(diag(table_valid)) / sum(table_valid))

model_c <- c(miss_train, miss_valid)

table <- data.frame(rbind(model_a, model_b, model_c))
colnames(table) <- c("Training error", "Validation error")
rownames(table) <- c("Default setting", "Smallest node 7000", "Min deviance 0.0005")

kable(table)
# 2.3                                               ####
fit <- tree(y ~., data=train, 
            control=tree.control(nobs = nrow(train),
                                 mindev = 0.0005))

# This will give tree with 4 leaves, so index to 50 will give 50 leaves
prunedTree=prune.tree(fit,best=4)
plot(prunedTree)

trainScore=rep(0,50)
testScore=rep(0,50)
for(i in 2:50){
  prunedTree=prune.tree(fit,best=i)
  pred=predict(prunedTree, newdata=valid,
               type="tree")
  trainScore[i]=deviance(prunedTree)
  testScore[i]=deviance(pred)
}

# Finds min, add +1 since index 1 is tree with 2 leaves.
which(min(testScore[2:50]) == testScore[2:50])+1

plot(2:50, trainScore[2:50], type="b", col="red",ylim=c(4000,12000))
points(2:50, testScore[2:50], type="b", col="blue")
# Fill the point with the min value on validation
points(22, testScore[22], pch=16, col="blue")

best_fit <- prune.tree(fit, best=22)
plot(best_fit)
text(best_fit, pretty=0)



# 2.4                                               ####
pred_test <- predict(best_fit, newdata=test, type="class")
table_test <- table(test$y, pred_test)
# Confusion matrix
table_test
# Confusion matrix indicate that we have unbalanced data, which makes the 
# model be able to predict no (the dominant class) better than yes. 
# Accuracy is not good, since the model is bad on predicting the small class. 


# Accuracy
acc_test <- sum(diag(table_test)) / sum(table_test)
acc_test

# F1-score
# True positive
tp <- table_test[2,2]
# false positve = We predict positive(yes) but real value is negative(no).
fp <- table_test[1,2]
# false negative = We predict negative(no) but real value is positive(yes)
fn <- table_test[2,1]

precision <- tp / (tp+fp)
recall <- tp / (tp+fn)
# F-score is between 0 and 1, where the closer to 1, the better. 
f_score <- 2 * (precision*recall) / (precision + recall)
f_score
# 2.5                                               ####

# Predicts the probability of each class. 
pred_best <- predict(best_fit, test, type="vector")
prediction <- as.vector(ifelse(pred_best[,1]/pred_best[,2] > 5, "no", "yes"))
table(test$y, prediction)

# 2.6                                               ####
d <- data.frame(TPR = 1, FPR = 1)
roc_logistic <- d[FALSE, ]
roc_tree <- d[FALSE, ]
# Tree
pred_best <- predict(best_fit, test, type="vector")
probs <- seq(from=0.05, to=0.95, by=0.05)
for(i in 1:length(probs)){
  prediction <- ifelse(pred_best[,2] > probs[i], "yes", "no")
  conf_mat <- table(test$y, prediction)
  if(dim(conf_mat)[2] == 1){
    TP <- 0
    FP <- 0
  } else {
    TP <- conf_mat[2,2]
    FP <- conf_mat[1,2]
  }
  TN <- conf_mat[1,1]
  FN <- conf_mat[2,1]

  
  # TPR, true positive rate
  roc_tree[i,1] <- TP / (TP+FN)
  # FPR, false positive rate
  roc_tree[i,2] <- FP / (FP+TN)
}

plot_data <- data.frame(roc_tree)
ggplot(plot_data) +
  geom_point(aes(x=FPR, y=TPR)) +
  scale_x_continuous(limits=c(0,1)) +
  geom_abline(intercept = 0, slope = 1) +
  theme_bw()


model_logistic <- glm(y~., family="binomial", train)
pred_logistic <- predict(model_logistic, test, type="response")
d <- data.frame(TPR = 1, FPR = 1)
roc_logistic <- d[FALSE, ]
probs <- seq(from=0.05, to=0.95, by=0.05)
roc_logistic <- d[FALSE, ]

for(i in 1:length(probs)){
  pred <- ifelse(pred_logistic > probs[i], "yes", "no")
  conf_mat <- table(test$y, pred)
  TP <- conf_mat[2,2]
  TN <- conf_mat[1,1]
  FN <- conf_mat[2,1]
  FP <- conf_mat[1,2]
  
  # TPR, true positive rate
  roc_logistic[i,1] <- TP / (TP+FN)
  # FPR, false positive rate
  roc_logistic[i,2] <- FP / (FP+TN)
}
plot_data <- data.frame(roc_logistic)
ggplot(plot_data) +
  geom_point(aes(x=FPR, y=TPR)) +
  scale_x_continuous(limits=c(0,1)) +
  geom_abline(intercept = 0, slope = 1) +
  theme_bw()
