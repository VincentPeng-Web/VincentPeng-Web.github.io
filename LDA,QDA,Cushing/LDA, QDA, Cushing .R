set.seed(233)

library(MASS)

mu_1<-cbind(c(1,2))
mu_2<-cbind(c(6,6))
mu_3<-cbind(c(6,-2))

cov_matrix_1<-cbind(c(1,0),c(0,4))
cov_matrix_2<-cbind(c(9,0),c(0,1))
cov_matrix_3<-cbind(c(2.25,0),c(0,4))


X_Gauss_1<-mvrnorm(20,mu_1,cov_matrix_1)
X_Gauss_2<-mvrnorm(30,mu_2,cov_matrix_2)
X_Gauss_3<-mvrnorm(50,mu_3,cov_matrix_3)

plot(X_Gauss_1,col="red",xlim=range(c(-10,10)),ylim=range(c(-10,10)))
par(new=TRUE)
plot(X_Gauss_2,col="blue",xlim=range(c(-10,10)),ylim=range(c(-10,10)))
par(new=TRUE)
plot(X_Gauss_3,col="magenta",xlim=range(c(-10,10)),ylim=range(c(-10,10)))
X_Sample<-rbind(X_Gauss_1,X_Gauss_2,X_Gauss_3)



Little_x_values<-X_Sample[,1]
Little_y_values<-X_Sample[,2]
y_subi<-matrix(0L,nrow=49,ncol=1)
x_subi<-matrix(0L,nrow=49,ncol=1)
for(i in 1:49) {
  x_subi[i]<-min(Little_x_values)+(i/50)*(max(Little_x_values)-min(Little_x_values))
}
for(i in 1:49) {
  y_subi[i]<-min(Little_y_values)+(i/50)*(max(Little_y_values)-min(Little_y_values))
}
x_subi2<-rbind(min(Little_x_values),x_subi)
y_subi2<-rbind(min(Little_y_values),y_subi)

X_Grid_Final<-expand.grid(x_subi2,y_subi2)


mvdnorm2<-function(x,mu,sigma,porp) {
  z_value<-(porp/sqrt(pi^2*det(sigma)))*(exp(-(t((x-mu))%*%solve(sigma)%*%(x-mu))))
  return(z_value)
  
}

Density_values_1<-matrix(0L,nrow=2500,ncol=1)

X_Grid_vector<-cbind(X_Grid_Final$Var1,X_Grid_Final$Var2)

for(i in 1:2500) {
Density_values_1[i]<-mvdnorm2(X_Grid_vector[i,],mu_1,cov_matrix_1,0.2) 
}

Density_values_2<-matrix(0L,nrow=2500,ncol=1)

for(i in 1:2500) {
Density_values_2[i]<-mvdnorm2(X_Grid_vector[i,],mu_2,cov_matrix_2,0.3) 
}

Density_values_3<-matrix(0L,nrow=2500,ncol=1)

for(i in 1:2500) {
Density_values_3[i]<-mvdnorm2(X_Grid_vector[i,],mu_3,cov_matrix_3,0.5) 
}

g1_x<-0.2*Density_values_1-pmax(0.3*Density_values_2,0.5*Density_values_3)
g2_x<-0.3*Density_values_2-pmax(0.2*Density_values_1,0.5*Density_values_3)
g3_x<-0.5*Density_values_3-pmax(0.2*Density_values_1,0.3*Density_values_2)



g1_matrix<-matrix(g1_x,nrow=50,ncol=50)
g2_matrix<-matrix(g2_x,nrow=50,ncol=50)
g3_matrix<-matrix(g3_x,nrow=50,ncol=50)



plot(X_Gauss_1,col="red",xlim=range(c(-10,10)),ylim=range(c(-10,10)))
par(new=TRUE)
plot(X_Gauss_2,col="blue",xlim=range(c(-10,10)),ylim=range(c(-10,10)))
par(new=TRUE)
plot(X_Gauss_3,col="magenta",xlim=range(c(-10,10)),ylim=range(c(-10,10)))



contour(x=x_subi2,y=y_subi2,z=g1_matrix,add=TRUE,levels=0,xlim=range(c(-10,10)),ylim=range(c(-10,10)))
contour(x=x_subi2,y=y_subi2,z=g2_matrix,add=TRUE,levels=0,xlim=range(c(-10,10)),ylim=range(c(-10,10)))
contour(x=x_subi2,y=y_subi2,z=g3_matrix,add=TRUE,levels=0,xlim=range(c(-10,10)),ylim=range(c(-10,10)))




Class_1<-rep("I",20)
Class_2<-rep("II",30)
Class_3<-rep("III",50)
Classes<-c(Class_1,Class_2,Class_3)
Data_Linear<-data.frame(Label=Classes,Var1=X_Sample[,1],Var2=X_Sample[,2])
theClass<-as.factor(Data_Linear$Label)
X.lda<-lda(X_Sample,theClass)
guess<-predict(X.lda,X_Grid_Final)
predict_class<-guess$class
proba_class<-guess$post


z_values_1<-proba_class[,1]-pmax(proba_class[,2],proba_class[,3])
z_values_2<-proba_class[,2]-pmax(proba_class[,1],proba_class[,3])
z_values_3<-proba_class[,3]-pmax(proba_class[,1],proba_class[,2])


z_Matrix_1<-matrix(z_values_1,nrow=50,ncol=50)
z_Matrix_2<-matrix(z_values_2,nrow=50,ncol=50)
z_Matrix_3<-matrix(z_values_3,nrow=50,ncol=50)



plot(X_Gauss_1,col="red",xlim=range(c(-10,10)),ylim=range(c(-10,10)))
par(new=TRUE)
plot(X_Gauss_2,col="blue",xlim=range(c(-10,10)),ylim=range(c(-10,10)))
par(new=TRUE)
plot(X_Gauss_3,col="magenta",xlim=range(c(-10,10)),ylim=range(c(-10,10)))
contour(x=x_subi2,y=y_subi2,z=g1_matrix,add=TRUE,levels=0,xlim=range(c(-10,10)),ylim=range(c(-10,10)))
contour(x=x_subi2,y=y_subi2,z=g2_matrix,add=TRUE,levels=0,xlim=range(c(-10,10)),ylim=range(c(-10,10)))
contour(x=x_subi2,y=y_subi2,z=g3_matrix,add=TRUE,levels=0,xlim=range(c(-10,10)),ylim=range(c(-10,10)))
contour(x=x_subi2,y=y_subi2,z=z_Matrix_1,add=TRUE,levels=0,col="magenta")
contour(x=x_subi2,y=y_subi2,z=z_Matrix_2,add=TRUE,levels=0,col="magenta")
contour(x=x_subi2,y=y_subi2,z=z_Matrix_3,add=TRUE,levels=0,col="magenta")

#Cushing Analysis 

Cushings_1<-read.csv('datasets/Cushings.csv', header=TRUE, row.names=1, nrows=21)
X <- log(as.matrix(Cushings_1[1:21,1:2]))
theClass   <- factor(Cushings_1$Type[1:21])

len <- 50
xp <- seq(0.6, 4.0, length=len)
yp <- seq(-3.25, 2.45, length=len)

tst_grid <- expand.grid(Tetrahydrocortisone=xp, Pregnanetriol=yp)


## LDA

Bio_lda<-lda(X,theClass)
guess_lda<-predict(Bio_lda,tst_grid)
prob_lda<-guess_lda$post
V_1<-prob_lda[,1]-pmax(prob_lda[,2],prob_lda[,3])
V_2<-prob_lda[,2]-pmax(prob_lda[,1],prob_lda[,3])
V_3<-prob_lda[,3]-pmax(prob_lda[,1],prob_lda[,2])
lda_matrix_1<-matrix(V_1,nrow=50,ncol=50)
lda_matrix_2<-matrix(V_2,nrow=50,ncol=50)
lda_matrix_3<-matrix(V_3,nrow=50,ncol=50)



plot.new()

plot(X,col=c("green","blue","red")[theClass])



contour(x=xp,y=yp,z=lda_matrix_1,add=TRUE,levels=0)
contour(x=xp,y=yp,z=lda_matrix_2,add=TRUE,levels=0)
contour(x=xp,y=yp,z=lda_matrix_3,add=TRUE,levels=0)

## QDA

Bio_qda<-qda(X,theClass)
guess_qda<-predict(Bio_qda,tst_grid)
prob_qda<-guess_qda$post
V_1q<-prob_qda[,1]-pmax(prob_qda[,2],prob_qda[,3])
V_2q<-prob_qda[,2]-pmax(prob_qda[,1],prob_qda[,3])
V_3q<-prob_qda[,3]-pmax(prob_qda[,1],prob_qda[,2])
qda_matrix_1<-matrix(V_1q,nrow=50,ncol=50)
qda_matrix_2<-matrix(V_2q,nrow=50,ncol=50)
qda_matrix_3<-matrix(V_3q,nrow=50,ncol=50)
plot.new()

plot(X,col=c("green","blue","red")[theClass])
contour(x=xp,y=yp,z=qda_matrix_1,add=TRUE,levels=0)
contour(x=xp,y=yp,z=qda_matrix_2,add=TRUE,levels=0)
contour(x=xp,y=yp,z=qda_matrix_3,add=TRUE,levels=0)










