#install.packages("C50", repos="http://R-Forge.R-project.org")
#install.packages("caret", dependencies = c("Depends", "Suggests"))

churn = data(churn)

summary(churnTrain)
x = churnTrain[,-20] #delet the label
y = churnTrain$churn #label
treemodel <- C5.0(x,y)
summary(treemodel)

treemodel_no_prun <- C5.0(x,y,control = C5.0Control(noGlobalPruning = TRUE))
summary(treemodel_no_prun)
pre_no_prun <- predict.C5.0(treemodel_no_prun, churnTest[,-20])
no_prun_table = table(pre_no_prun, churnTest$churn)
confusionMatrix(no_prun_table)

treemodel_prun <- C5.0(x,y,control = C5.0Control(noGlobalPruning = FALSE))
summary(treemodel_prun)
pre_prun <- predict.C5.0(treemodel_prun, churnTest[,-20])
prun_table = table(pre_prun, churnTest$churn)
confusionMatrix(prun_table)




data(GermanCredit)

set.seed(1234)
ind = sample(2,nrow(GermanCredit),replace=TRUE,prob=c(0.7,0.3))
trainData = GermanCredit[ind==1,]
testData = GermanCredit[ind==2,]
GermanCreditj48 = J48(Class ~., data = trainData,control = Weka_control(R = TRUE, N = 10, M = 5))
summary(GermanCreditj48)
GermanCreditj48
nrow(trainData)
testj48 = evaluate_Weka_classifier(GermanCreditj48, newdata=testData, class = TRUE)
testj48


WOW(J48)
data(GermanCredit)
summary(GermanCredit)
set.seed(1234)
ind = sample(2,nrow(GermanCredit),replace=TRUE,prob=c(0.7,0.3))
trainData = GermanCredit[ind==1,]
testData = GermanCredit[ind==2,]
GermanCreditj48 = J48(Class ~., data = trainData)
summary(GermanCreditj48)
GermanCreditj48
testj48 = evaluate_Weka_classifier(GermanCreditj48, newdata=testData, class = TRUE)
testj48


college = read.csv("/Users/zhangyifei/Desktop/DM_assignment3/college.csv")
View(college)
dim(college)
summary(college)

college = college[, -c(0,1)]
college = college[, -c(0,1)]
levels(college$Private) <- c(0,1)

set.seed(1235)
ind_college = sample(2,nrow(college),replace=TRUE,prob=c(0.7,0.3))
trainData_college = college[ind_college==1,]
testData_college = college[ind_college==2,]


#classification differnt algrothms: 
#J48
collegej48 = J48(isElite ~., data = trainData_college)
testj48_college = evaluate_Weka_classifier(collegej48, newdata=testData_college, class = TRUE)
testj48_college

#c50
tree_college <- C5.0(trainData_college[, -6],trainData_college$isElite)
predict_tree <- predict.C5.0(tree_college, testData_college[,-6])
tree_table = table(predict_tree, testData_college$isElite)
confusionMatrix(tree_table)

#knn
KNN_college = knn3(isElite ~., data = trainData_college, k = 2)
predict_knn = predict(KNN_college, testData_college[,-8], type = "class")
knn_table = table(predict_knn, testData_college$isElite)
confusionMatrix(knn_table)

#ksvm
library(kernlab)
svm_college = ksvm(isElite ~., data = trainData_college)
predict_svm = predict(svm_college, testData_college[,-8], type = "response")
svm_table = table(predict_svm, testData_college$isElite)
confusionMatrix(svm_table)

#NB
library(e1071)
nb_college = naiveBayes(isElite ~., data = trainData_college, k = 5)
predict_nb = predict(nb_college, testData_college[,-8], type = "class")
nb_table = table(predict_nb, testData_college$isElite)
confusionMatrix(nb_table)

#parameter changes
collegej48 = J48(isElite ~., data = trainData_college)
testj48_college = evaluate_Weka_classifier(collegej48, newdata=testData_college, class = TRUE)
testj48_college

collegej48 = J48(isElite ~., data = trainData_college)
testj48_college = evaluate_Weka_classifier(collegej48, newdata=testData_college, cost = matrix(c(0,1,4,0), nrow=2), numFolds = 15, complexity =
                                             TRUE, seed = 123, class = TRUE)
testj48_college


tree_college <- C5.0(trainData_college[, -6],trainData_college$isElite)
predict_tree <- predict.C5.0(tree_college, testData_college[,-6])
tree_table = table(predict_tree, testData_college$isElite)
confusionMatrix(tree_table)

costMatrix <- matrix(c(0, 4, 1, 0), nrow=2)
tree_college <- C5.0(trainData_college[, -6],trainData_college$isElite,costs=costMatrix)
predict_tree <- predict.C5.0(tree_college, testData_college[,-6])
tree_table = table(predict_tree, testData_college$isElite)
confusionMatrix(tree_table)


collegej48 = J48(isElite ~., data = trainData_college)
summary(collegej48)
plot(collegej48)
testj48_college = evaluate_Weka_classifier(collegej48, newdata=testData_college, cost = matrix(c(0,1,4,0), nrow=2), numFolds = 20, complexity =
                                     TRUE, seed = 123, class = TRUE)
testj48_college

hist(college[,c(1)])
college_rate <- cut(college[,c(1)], c(-Inf, 0.1,0.2, 0.3, 0.4, 0.5,0.6,0.7,0.8,0.9,Inf), labels=1:10)
college$accept_rate <- college_rate

hist(college[, c(2)])
college_outsate <- cut(college[,c(2)], c(-Inf,1000,2000, 3000, 4000, 5000,6000,7000,8000,9000,10000,11000,12000,13000,14000,15000,16000,17000,18000,19000,20000,Inf), labels=1:21)
college$Outstate <- college_outsate
View(college)
hist(college[, c(3)])
college_enroll <- cut(college[,c(3)], c(-Inf,500,1000, 1500, 2000, 2500,3000,3500,4000,4500,5000,5500,6000,Inf), labels=1:13)
college$Enroll <- college_enroll

hist(college[, c(4)])
college_grad <- cut(college[,c(4)], c(-Inf,20,30, 40, 50, 60,70,80,90,100,110,Inf), labels=1:11)
college$Grad.Rate <- college_grad

KNN_college = knn3(isElite ~., data = trainData_college, k = 2)
predict_knn = predict(KNN_college, testData_college[,-8], type = "class")
knn_colege_accurancy = sum(predict_knn == testData_college$isElite)/length( predict_knn )
summary(KNN_college)
knn_table = table(predict_knn, testData_college$isElite)
confusionMatrix(knn_table)


nb_college = naiveBayes(isElite ~., data = trainData_college, k = 5)
summary(nb_college)
predict_nb = predict(nb_college, testData_college[,-8], type = "class")
nb_table = table(predict_nb, testData_college$isElite)
confusionMatrix(nb_table)

library(kernlab)
svm_college = ksvm(isElite ~., data = trainData_college)
summary(svm_college)
predict_svm = predict(svm_college, testData_college[,-8], type = "response")
svm_table = table(predict_svm, testData_college$isElite)
confusionMatrix(svm_table)

costMatrix <- matrix(c(0, 4, 1, 0), nrow=2)
tree_college <- C5.0(trainData_college[, -6],trainData_college$isElite,costs=costMatrix)
predict_tree <- predict.C5.0(tree_college, testData_college[,-6])
tree_table = table(predict_tree, testData_college$isElite)
confusionMatrix(tree_table)

tree_college <- C5.0(trainData_college[, -6],trainData_college$isElite)
summary(tree_college)
plot(tree_college)
predict_tree <- predict.C5.0(tree_college, testData_college[,-6])
tree_table = table(predict_tree, testData_college$isElite)
confusionMatrix(tree_table)

summary(college)
college_cluster <- SimpleKMeans(college[,-6])
plot(college_cluster)
summary(college_cluster)
cluster_table <- table(predict(college_cluster), college$isElite)
confusionMatrix(cluster_table)

plot(college[c("Outstate", "accept_rate")], col =
       c(college_cluster$clusterer))

cluster_table.result

kmeans.result = kmeans(college[,-6],2)
kmeans.result

plot(college[c("Outstate", "accept_rate")], col =
         kmeans.result$cluster)

plot(college[c("Outstate", "accept_rate")], col =
       college$isElite, main = "real labeled data")
points(kmeans.result$centers[,c("Outstate", "accept_rate")], col = 1:3,pch = 8, cex=2)

plot(college[c("Outstate", "accept_rate")], col =
       kmeans.result$cluster, main = "cluster labeled data")
points(kmeans.result$centers[,c("Outstate", "accept_rate")], col = 1:3,pch = 8, cex=2)


plot(college[c("Grad.Rate", "Enroll")], col =
       kmeans.result$cluster, main = "real labeled data")
points(kmeans.result$centers[,c("Grad.Rate", "Enroll")], col = 1:3,pch = 8, cex=2)


library(cluster)

pam.result = pam(college,2)
pam.result
table(pam.result$clustering,college$isElite)
plot(pam.result)

college$Grad.Rate
