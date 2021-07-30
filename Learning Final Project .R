library(data.table)
library(MASS)
library(e1071)
library(nnet)
library(dplyr)
library(corrplot)
library(scatterplot3d)
library(glmnet)
#Import Data

names<-fread("/Users/vincentpeng/Documents/Stat Learning/HAR_dataset /features.txt",header=FALSE,col.names=c("id","names"))

train_data<-read.csv("/Users/vincentpeng/Documents/Stat Learning/HAR_dataset /HAR_train.csv",header=TRUE,nrows=7352)
test_data<-read.csv("/Users/vincentpeng/Documents/Stat Learning/HAR_dataset /HAR_test.csv",header=TRUE,nrows=2947)


#Question 1 

Laying_data<-filter(train_data,Activity=="LAYING")
Walking_data<-filter(train_data,Activity=="WALKING_UPSTAIRS")
hist(Laying_data$tBodyAccmeanX,seq(-1,1,length=50),prob=TRUE,main="LAYING")
hist(Laying_data$tBodyAccmeanY,seq(-1,1,length=50),prob=TRUE,main="LAYING")
hist(Laying_data$tBodyAccmeanZ,seq(-1,1,length=50),prob=TRUE,main="LAYING")
hist(Walking_data$tBodyAccmeanX,seq(-1,1,length=50),prob=TRUE,main="WALKING_UPSTAIRS")
hist(Walking_data$tBodyAccmeanY,seq(-1,1,length=50),prob=TRUE,main="WALKING_UPSTAIRS")
hist(Walking_data$tBodyAccmeanZ,seq(-1,1,length=50),prob=TRUE,main="WALKING_UPSTAIRS")

#Question 2  

hist(Laying_data$tBodyGyromeanX,seq(-1,1,length=50),prob=TRUE,main="LAYING")
hist(Laying_data$tBodyGyromeanY,seq(-1,1,length=50),prob=TRUE,main="LAYING")
hist(Laying_data$tBodyGyromeanZ,seq(-1,1,length=50),prob=TRUE,main="LAYING")
hist(Walking_data$tBodyGyromeanX,seq(-1,1,length=50),prob=TRUE,main="WALKING_UPSTAIRS")
hist(Walking_data$tBodyGyromeanY,seq(-1,1,length=50),prob=TRUE,main="WALKING_UPSTAIRS")
hist(Walking_data$tBodyGyromeanZ,seq(-1,1,length=50),prob=TRUE,main="WALKING_UPSTAIRS")

#Question 3 

col.index<-colorRampPalette(c("red","green","blue","yellow","white","darkorchid","seagreen1","goldenrod","darkred"))
Cor_Matrix<-cor(train_data[,2:562])
corrplot(Cor_Matrix,method="color",tl.pos="n",col=col.index(100))

#Question 4 

Train_matrix<-as.matrix(train_data[1:2947,2:562])
Train_class_matrix<-as.factor(train_data[1:2947,563])
Test_matrix<-as.matrix(test_data[,2:562])
Test_class_matrix<-as.factor(test_data[,563])


#Principal Component Analysis For Dimension Reduction and easier Computation 

pc<-prcomp(Train_matrix,center=TRUE,scale=TRUE)

#Choose Dimension of 30 and extract projections 

pc_train<-data.frame(pc$x[,1:30])
pc_test<-as.data.frame(predict(pc,newdata=Test_matrix)[,1:30])
dim(pc_test)
dim(pc_train)

#SVM test 

lambda_values<-seq(40,54)   #Choose lambda between 40 and 54 
Index_Fold<-sample(1:10,size=2947,prob=rep(1,10)/10,replace=TRUE)
Percent_Correct<-rep(0,15)


# Decided to use 10 folds 
for(k in 1:15) {
  for(i in 1:10){
      Train_data<-pc_train[Index_Fold!=i,]
      Train_class<-Test_class_matrix[Index_Fold!=i]
      Test_data<-pc_test[Index_Fold==i,]
      Test_class<-Test_class_matrix[Index_Fold==i]
  
      cv_model<-svm(Train_data,Train_class,kernel="radial",cost=lambda_values[k])
      predict_class<-predict(cv_model,Test_data)
      Percent_Correct[k]<-Percent_Correct[k]+mean(predict_class==Test_class)
  }
    
}


plot(lambda_values,Percent_Correct,type="l")

opt_lambda<-lambda_values[which.max(Percent_Correct)]

#Question 5 

Opt_model<-svm(Train_matrix,Train_class_matrix,kernel="radial",cost=opt_lambda)
predict_opt_model<-predict(Opt_model,Test_matrix)

table(Test_class_matrix,predict_opt_model)

#Question 7 


color_3d<-c("red","green","blue","yellow","darkorchid","pink")
c_3d<-color_3d[as.numeric(train_data[,563])]
s_plot<-scatterplot3d(train_data$tBodyAccmeanX,train_data$tBodyAccmeanY,train_data$tBodyAccmeanZ,color=c_3d,pch=1)
legend(s_plot$xyz.convert(0.7,0,1),legend=levels(train_data[,563]),col=color_3d,cex=0.3,pch=1)



#Question 9 



s_plot2<-scatterplot3d(train_data$tBodyGyromeanX,train_data$tBodyGyromeanY,train_data$tBodyGyromeanZ,color=c_3d,pch=1)
legend(s_plot2$xyz.convert(0.7,0,1),legend=levels(train_data[,563]),col=color_3d,cex=0.3,pch=1)

############################## Part 2 




#Problem 11 



mat_A<-as.matrix(Walking_data[,2:562])
pc_A<-prcomp(mat_A,center=TRUE,scale=TRUE)

percent_var<-1-cumsum(pc_A$sdev^2/sum(pc_A$sdev^2))

plot(percent_var[1:100],xlim=c(1,100),ylim=c(0,1),xlab="Number of principal Component",col="red",main="Walking Upstairs")
mtext(side=2,line=1.8,"Percent of Residual Variance",font=1,cex=1)

#Problem 12 

scatterplot3d(pc_A$x[,1],pc_A$x[,2],pc_A$x[,3],color="green",xlim=c(-10,10),ylim=c(-10,10),zlim=c(-10,10),main="Walking Upstairs")


#Problem 13 

mat_A2<-as.matrix(Laying_data[,2:562])
pc_A2<-prcomp(mat_A2,center=TRUE,scale=TRUE)
percent_var2<-1-cumsum(pc_A2$sdev^2/sum(pc_A2$sdev^2))



plot(percent_var2[1:100], xlim=c(1,100), ylim=c(0,1), xlab="Number of principal Component",col="red",main="Laying")
mtext(side=2,line=1.8,"Percent of Residual Variance",font=1,cex=1)

scatterplot3d(pc_A2$x[,1],pc_A2$x[,2],pc_A2$x[,3],color="blue",xlim=c(-10,10),ylim=c(-10,10),zlim=c(-10,10),main="Laying")

#Problem 14 

pc_A3<-prcomp(as.matrix(train_data[,2:562]),center=TRUE,scale=TRUE)
percent_var3<-1-cumsum(pc_A3$sdev^2/sum(pc_A3$sdev^2))
plot(percent_var3[1:100], xlim=c(1,100), ylim=c(0,1), xlab="Number of principal Component",col="red",main="XTrain")
mtext(side=2,line=1.8,"Percent of Residual Variance",font=1,cex=1)

#Problem 15 


#90% value at 58th column according to plot. 
opt_vector<-pc_A3$x[,58]



#Problem 16 


color_3d<-c("red","green","blue","yellow","darkorchid","pink")
c_3d<-color_3d[as.numeric(train_data[,563])]
s_plot<-scatterplot3d(pc_A3$x[,1],pc_A3$x[,2],pc_A3$x[,3],color=c_3d,pch=1)
legend(s_plot$xyz.convert(20,-120,1),legend=levels(train_data[,563]),col=color_3d,cex=0.3,pch=1)



#Problem 17 


#For k=30 

Train_matrix<-as.matrix(train_data[,2:562])
Train_class_matrix<-as.factor(train_data[,563])
Train_class_matrix<-as.factor(train_data[,563])
pc<-prcomp(Train_matrix,center=TRUE,scale=TRUE)




x_train_30<-data.frame(pc$x[,1:30])
x_test_30<-predict(pc,Test_matrix)
x_test_30<-as.data.frame(x_test_30[,1:30])

glm_30<-cv.glmnet(as.matrix(x_train_30),Train_class_matrix,alpha=1,family="multinomial")


y_30_pred<-predict(glm_30,newx=as.matrix(x_test_30),s="lambda.min",type="class")


table(Test_class_matrix,y_30_pred)


#For k=40 


x_train_40<-data.frame(pc$x[,1:40])
x_test_40<-predict(pc,Test_matrix)
x_test_40<-as.data.frame(x_test_40[,1:40])

glm_40<-cv.glmnet(as.matrix(x_train_40),Train_class_matrix,alpha=1,family="multinomial")


y_40_pred<-predict(glm_40,newx=as.matrix(x_test_40),s="lambda.min",type="class")


table(Test_class_matrix,y_40_pred)

#k=50 


x_train_50<-data.frame(pc$x[,1:50])
x_test_50<-predict(pc,Test_matrix)
x_test_50<-as.data.frame(x_test_50[,1:50])

glm_50<-cv.glmnet(as.matrix(x_train_50),Train_class_matrix,alpha=1,family="multinomial")


y_50_pred<-predict(glm_50,newx=as.matrix(x_test_50),s="lambda.min",type="class")


table(Test_class_matrix,y_50_pred)



#k=60 


x_train_60<-data.frame(pc$x[,1:60])
x_test_60<-predict(pc,Test_matrix)
x_test_60<-as.data.frame(x_test_60[,1:60])

glm_60<-cv.glmnet(as.matrix(x_train_60),Train_class_matrix,alpha=1,family="multinomial")


y_60_pred<-predict(glm_60,newx=as.matrix(x_test_60),s="lambda.min",type="class")


table(Test_class_matrix,y_60_pred)

#Problem 18 
y_table<-c(sum(diag(table(Test_class_matrix,y_30_pred))),sum(diag(table(Test_class_matrix,y_40_pred))),sum(diag(table(Test_class_matrix,y_50_pred))),sum(diag(table(Test_class_matrix,y_60_pred))))/2947
x_table<-c(30,40,50,60)



plot(x_table,y_table,type="l",main="Logistic Regression ",xlab="K",ylab="Percent_Correct")
mtext(side=2,line=1.8,"Percent of Accuracy",font=1,cex=1)

#Problem 19 

#k=30


qda_30<-qda(as.matrix(x_train_30),Train_class_matrix)
pd_qda_30<-predict(qda_30,as.matrix(x_test_30))
table(Test_class_matrix,pd_qda_30$class)

#k=40 

qda_40<-qda(as.matrix(x_train_40),Train_class_matrix)
pd_qda_40<-predict(qda_40,as.matrix(x_test_40))
table(Test_class_matrix,pd_qda_40$class)


#k=50 

qda_50<-qda(as.matrix(x_train_50),Train_class_matrix)
pd_qda_50<-predict(qda_50,as.matrix(x_test_50))
table(Test_class_matrix,pd_qda_50$class)

#k=60 


qda_60<-qda(as.matrix(x_train_60),Train_class_matrix)
pd_qda_60<-predict(qda_60,as.matrix(x_test_60))
table(Test_class_matrix,pd_qda_60$class)


#Problem 20 
 
y_table_2<-c(sum(diag(table(Test_class_matrix,pd_qda_30$class))),sum(diag(table(Test_class_matrix,pd_qda_40$class))),sum(diag(table(Test_class_matrix,pd_qda_50$class))),sum(diag(table(Test_class_matrix,pd_qda_60$class))))/2947
plot(x_table,y_table_2,main="QDA",xlab="K",ylab="Percent Correct",type="l")
mtext(side=2,line=1.8,"Percent of Accuracy",font=1,cex=1)

#Problem 21 

#k=30


n_30<-nnet(as.matrix(x_train_30),class.ind(Train_class_matrix),decay=0.01,size=10,maxit=5000,softmax=TRUE)
n_30_pred<-predict(n_30,as.matrix(x_test_30),type="class")
table(Test_class_matrix,n_30_pred)

#k=40 



n_40<-nnet(as.matrix(x_train_40),class.ind(Train_class_matrix),decay=0.01,size=10,maxit=5000,softmax=TRUE)
n_40_pred<-predict(n_40,as.matrix(x_test_40),type="class")
table(Test_class_matrix,n_40_pred)


#k=50


n_50<-nnet(as.matrix(x_train_50),class.ind(Train_class_matrix),decay=0.01,size=10,maxit=5000,softmax=TRUE)
n_50_pred<-predict(n_50,as.matrix(x_test_50),type="class")
table(Test_class_matrix,n_50_pred)





#k=60 

n_60<-nnet(as.matrix(x_train_60),class.ind(Train_class_matrix),decay=0.01,size=10,maxit=5000,softmax=TRUE)
n_60_pred<-predict(n_60,as.matrix(x_test_60),type="class")
table(Test_class_matrix,n_60_pred)


#Problem 22 


y_table_3<-c(sum(diag(table(Test_class_matrix,n_30_pred))),sum(diag(table(Test_class_matrix,n_40_pred))),sum(diag(table(Test_class_matrix,n_50_pred))),sum(diag(table(Test_class_matrix,n_60_pred))))/2947

plot(x_table,y_table_3,main="NNet",xlab="K",ylab="Percent Correct",type="l")
mtext(side=2,line=1.8,"Percent of Accuracy",font=1,cex=1)



