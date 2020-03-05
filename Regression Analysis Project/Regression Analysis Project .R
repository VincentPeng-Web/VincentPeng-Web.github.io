

epsilon<-rnorm(30, mean=0, sd=0.05)

x<-linspace(0,1,100)

x2<-rep(0,30)



y<-f+epsilon


for(i in 0:30){
  x2[i]<-x2[i]+i/30
  }

f2<-1-x2+2*x2^2-0.8*x2^3+0.6*x2^4-x2^5+epsilon 
f<-1-x2+2*x2^2-0.8*x2^3+0.6*x2^4-x2^5

plot(f2,col="green",ylim=range(c(0,1.5)),xlim=range(c(0,30)))
par(new=TRUE)
lines(f,ylim=range(c(0,1.5)),xlim=range(c(0,30)))


X_final<-matrix(0L,nrow=30,ncol=1)

for(i in 0:30){
  X_final[i]<-X_final[i]+i/30
}

p1<-5

X_1<-matrix(X_final,nrow=30, ncol=p1)

X2<-cbind(rep(1,30),X_1[,1],X_1[,2]^2,X_1[,3]^3,X_1[,4]^4,X_1[,5]^5)
data_x3<-data.frame(y=f2,x_1=X2[,2],x_2=X2[,3],x_3=X2[,4],x_4=X2[,5],x_5=X2[,6])
coefficeint_5<-lm(y~1+x_1+x_2+x_3+x_4+x_5, data=data_x3)
print(coefficeint_5)
            
p2<-1

X_p21<-matrix(X_final,nrow=30,ncol=p2)
X_p2<-cbind(rep(1,30),X_p21[,1])
data_x4<-data.frame(y=f2,x_1=X_p2[,2])
coefficient_1<-lm(y~1+x_1, data=data_x4)
print(coefficient_1)


p3<-4

X4<-matrix(X_final,nrow=30, ncol=p3)
X5<-cbind(rep(1,30),X4[,1],X4[,2]^2,X4[,3]^3,X4[,4]^4)
data_x5<-data.frame(y=f2,x_1=X5[,2],x_2=X5[,3],x_3=X5[,4],x_4=X5[,5])
coefficient_4<-lm(y~1+x_1+x_2+x_3+x_4, data=data_x5)
print(coefficient_4)

p4<-15
X7<-matrix(X_final,nrow=30, ncol=p4)
X6<-cbind(rep(1,30),X7[,1], X7[,2]^2, X7[,3]^3, X7[,4]^4, X7[,5]^5, X7[,6]^6, X7[,7]^7, X7[,8]^8, X7[,9]^9, X7[,10]^10, X7[,11]^11, X7[,12]^12, X7[,13]^13, X7[,14]^14, X7[,15]^15)
data_X6<-data.frame(y=f2, x_1=X6[,2], x_2=X6[,3], x_3=X6[,4], x_4=X6[,5], x_5=X6[,6], x_6=X6[,7], x_7=X6[,8], x_8=X6[,9], x_9=X6[,10], x_10=X6[,11], x_11=X6[,12], x_12=X6[,13], x_13=X6[,14], x_14=X6[,15], x_15=X6[,16])
coefficient_15<-lm(y~1+x_1+x_2+x_3+x_4+x_5+x_6+x_7+x_8+x_9+x_10+x_11+x_12+x_13+x_14+x_15, data=data_X6)
print(coefficient_15)





lines(f,ylim=range(c(0,1.5)),xlim=range(c(0,30)))

par(new=TRUE)
lines(predict(coefficeint_5,newdata=data_x3),lwd=2, col="red",ylim=range(c(0,1.5)),xlim=range(c(0,30)))
par(new=TRUE)
lines(predict(coefficient_1,newdata=data_x4),lwd=1, col="blue",ylim=range(c(0,1.5)),xlim=range(c(0,30)))
par(new=TRUE)
lines(predict(coefficient_4,newdata=data_x5),lwd=2,col="yellow",ylim=range(c(0,1.5)),xlim=range(c(0,30)))
par(new=TRUE)
lines(predict(coefficient_15,newdata=data_X6),lwd=2,col="orange",ylim=range(c(0,1.5)),xlim=range(c(0,30)))
legend("bottomright",legend=c("p=1","p=4","p=5","p=15"), col=c("blue","yellow","red","orange"),pch=15)


r2<-2

X_r2<-matrix(X_final,nrow=30,ncol=r2)
X_r22<-cbind(rep(1,30),X_r2[,1],X_r2[,2]^2)
data_r2<-data.frame(y=f2,x_1=X_r22[,2],x_2=X_r22[,3])
coefficient_r2<-lm(y~1+x_1+x_2, data=data_r2)

r3<-3

X_r3<-matrix(X_final,nrow=30,ncol=r3)
X_r33<-cbind(rep(1,30),X_r3[,1],X_r3[,2]^2, X_r3[,3]^3)
data_r3<-data.frame(y=f2,x_1=X_r33[,2],x_2=X_r33[,3], x_3=X_r33[,4])
coefficient_r3<-lm(y~1+x_1+x_2+x_3, data=data_r3)

r6<-6

X_r6<-matrix(X_final,nrow=30,ncol=r6)
X_r66<-cbind(rep(1,30),X_r6[,1],X_r6[,2]^2, X_r6[,3]^3, X_r6[,4]^4, X_r6[,5]^5, X_r6[,6]^6)
data_r6<-data.frame(y=f2,x_1=X_r66[,2],x_2=X_r66[,3],x_3=X_r66[,4],x_4=X_r66[,5],x_5=X_r66[,6],x_6=X_r66[,7])
coefficient_r6<-lm(y~1+x_1+x_2+x_3+x_4+x_5+x_6, data=data_r6)

r7<-7

X_r7<-matrix(X_final,nrow=30,ncol=r7)
X_r77<-cbind(rep(1,30),X_r7[,1],X_r7[,2]^2, X_r7[,3]^3, X_r7[,4]^4, X_r7[,5]^5, X_r7[,6]^6, X_r7[,7]^7)
data_r7<-data.frame(y=f2,x_1=X_r77[,2],x_2=X_r77[,3],x_3=X_r77[,4],x_4=X_r77[,5],x_5=X_r77[,6],x_6=X_r77[,7], x_7=X_r77[,8])
coefficient_r7<-lm(y~1+x_1+x_2+x_3+x_4+x_5+x_6+x_7, data=data_r7)

r8<-8

X_r8<-matrix(X_final,nrow=30,ncol=r8)
X_r88<-cbind(rep(1,30),X_r8[,1],X_r8[,2]^2, X_r8[,3]^3, X_r8[,4]^4, X_r8[,5]^5, X_r8[,6]^6, X_r8[,7]^7, X_r8[,8]^8)
data_r8<-data.frame(y=f2,x_1=X_r88[,2],x_2=X_r88[,3],x_3=X_r88[,4],x_4=X_r88[,5],x_5=X_r88[,6],x_6=X_r88[,7],x_7=X_r88[,8],x_8=X_r88[,9])
coefficient_r8<-lm(y~1+x_1+x_2+x_3+x_4+x_5+x_6+x_7+x_8, data=data_r8)

r9<-9

X_r9<-matrix(X_final,nrow=30,ncol=r9)
X_r99<-cbind(rep(1,30),X_r9[,1],X_r9[,2]^2, X_r9[,3]^3, X_r9[,4]^4, X_r9[,5]^5, X_r9[,6]^6, X_r9[,7]^7, X_r9[,8]^8, X_r9[,9]^9)
data_r9<-data.frame(y=f2,x_1=X_r99[,2],x_2=X_r99[,3],x_3=X_r99[,4],x_4=X_r99[,5],x_5=X_r99[,6],x_6=X_r99[,7],x_7=X_r99[,8],x_8=X_r99[,9],x_9=X_r99[,10])
coefficient_r9<-lm(y~1+x_1+x_2+x_3+x_4+x_5+x_6+x_7+x_8+x_9, data=data_r9)

r10<-10

X_r10<-matrix(X_final,nrow=30,ncol=r10)
X_r10<-cbind(rep(1,30),X_r10[,1],X_r10[,2]^2, X_r10[,3]^3, X_r10[,4]^4, X_r10[,5]^5, X_r10[,6]^6, X_r10[,7]^7, X_r10[,8]^8, X_r10[,9]^9, X_r10[,10]^10)
data_r10<-data.frame(y=f2,x_1=X_r10[,2],x_2=X_r10[,3],x_3=X_r10[,4],x_4=X_r10[,5],x_5=X_r10[,6],x_6=X_r10[,7],x_7=X_r10[,8],x_8=X_r10[,9],x_9=X_r10[,10],x_10=X_r10[,11])
coefficient_r10<-lm(y~1+x_1+x_2+x_3+x_4+x_5+x_6+x_7+x_8+x_9+x_10, data=data_r10)


R_1<-sum(summary(coefficient_1)$residuals^2)
R_2<-sum(summary(coefficient_r2)$residuals^2)
R_3<-sum(summary(coefficient_r3)$residuals^2)
R_4<-sum(summary(coefficient_4)$residuals^2)
R_5<-sum(summary(coefficeint_5)$residuals^2)
R_6<-sum(summary(coefficient_r6)$residuals^2)
R_7<-sum(summary(coefficient_r7)$residuals^2)
R_8<-sum(summary(coefficient_r8)$residuals^2)
R_9<-sum(summary(coefficient_r9)$residuals^2)
R_10<-sum(summary(coefficient_r10)$residuals^2)


Res_vec<-c(R_1,R_2,R_3,R_4,R_5,R_6,R_7,R_8,R_9,R_10)
p_xs<-c(1,2,3,4,5,6,7,8,9,10)

plot.new()
plot(x=p_xs,y=Res_vec)
lines(x=p_xs,y=Res_vec,col="red")

which.min(Res_vec)

sigma1<-0.05

C_p<-Res_vec+(2*(p_xs+1)*sigma1^2)
dev.off()

lines(x=p_xs,y=C_p,col="purple")
legend("toprigh",legend=c("Residual","C_P"), col=c("red","purple"),pch=15)
which.min(C_p)




Y_values_p1<-matrix(0L,nrow=30,ncol=100)

p_17<-1


for(i in 0:100){
epsilon1<-rnorm(30, mean=0, sd=0.05)
f3<-1-x2+2*x2^2-0.8*x2^3+0.6*x2^4-x2^5+epsilon1
X_17<-matrix(X_final,nrow=30,ncol=p_17)
X_p17<-cbind(rep(1,30),X_17[,1])
data_p17<-data.frame(y=f3,x_1=X_p17[,2])
coefficient_p17<-lm(y~1+x_1, data=data_p17)
Y_values_p1[,i]<-predict(coefficient_p17, newdata=data_p17)
}
Y_mean_p1<-mean(Y_values_p1)

Var_p1<-(1/99)*(sum(abs(Y_values_p1-Y_mean_p1)^2))

bias_squared_1<-abs(f-Y_mean_p1)^2

Risk_p1<-Var_p1+bias_squared_1


Y_values_p2<-matrix(0L,nrow=30,ncol=100)

p_v2<-2


for(i in 0:100){
  epsilon2<-rnorm(30, mean=0, sd=0.05)
  f4<-1-x2+2*x2^2-0.8*x2^3+0.6*x2^4-x2^5+epsilon2
  X_p22<-matrix(X_final,nrow=30,ncol=p_v2)
  X_pv2<-cbind(rep(1,30),X_p22[,1],X_p22[,2]^2)
  data_pv2<-data.frame(y=f4,x_1=X_pv2[,2],x_2=X_pv2[,3])
  coefficient_pv2<-lm(y~1+x_1+x_2, data=data_pv2)
  Y_values_p2[,i]<-predict(coefficient_pv2, newdata=data_pv2)
}

Y_mean_p2<-mean(Y_values_p2)

Var_p2<-(1/99)*(sum(abs(Y_values_p2-Y_mean_p2)^2))

bias_squared_2<-abs(f-Y_mean_p2)^2

Risk_p2<-Var_p2+bias_squared_2


Y_values_p2<-matrix(0L,nrow=30,ncol=100)

p_v2<-2


for(i in 0:100){
  epsilon2<-rnorm(30, mean=0, sd=0.05)
  f4<-1-x2+2*x2^2-0.8*x2^3+0.6*x2^4-x2^5+epsilon2
  X_p22<-matrix(X_final,nrow=30,ncol=p_v2)
  X_pv2<-cbind(rep(1,30),X_p22[,1],X_p22[,2]^2)
  data_pv2<-data.frame(y=f4,x_1=X_pv2[,2],x_2=X_pv2[,3])
  coefficient_pv2<-lm(y~1+x_1+x_2, data=data_pv2)
  Y_values_p2[,i]<-predict(coefficient_pv2, newdata=data_pv2)
}

Y_mean_p2<-mean(Y_values_p2)

Var_p2<-(1/99)*(sum(abs(Y_values_p2-Y_mean_p2)^2))

bias_squared_2<-abs(f-Y_mean_p2)^2

Risk_p2<-Var_p2+bias_squared_2



Y_values_p3<-matrix(0L,nrow=30,ncol=100)

p_v3<-3


for(i in 0:100){
  epsilon2<-rnorm(30, mean=0, sd=0.05)
  f4<-1-x2+2*x2^2-0.8*x2^3+0.6*x2^4-x2^5+epsilon2
  X_pv33<-matrix(X_final,nrow=30,ncol=p_v3)
  X_pv3<-cbind(rep(1,30),X_pv33[,1],X_pv33[,2]^2, X_pv33[,3]^3)
  data_pv3<-data.frame(y=f4,x_1=X_pv3[,2],x_2=X_pv3[,3], x_3=X_pv3[,4])
  coefficient_pv3<-lm(y~1+x_1+x_2+x_3, data=data_pv3)
  Y_values_p3[,i]<-predict(coefficient_pv3, newdata=data_pv3)
}

Y_mean_p3<-mean(Y_values_p3)

Var_p3<-(1/99)*(sum(abs(Y_values_p3-Y_mean_p3)^2))

bias_squared_3<-abs(f-Y_mean_p3)^2

Risk_p3<-Var_p3+bias_squared_3


Y_values_p4<-matrix(0L,nrow=30,ncol=100)

p_v4<-4


for(i in 0:100){
  epsilon2<-rnorm(30, mean=0, sd=0.05)
  f4<-1-x2+2*x2^2-0.8*x2^3+0.6*x2^4-x2^5+epsilon2
  X_pv44<-matrix(X_final,nrow=30, ncol=p_v4)
  X_pv4<-cbind(rep(1,30),X_pv44[,1],X_pv44[,2]^2,X_pv44[,3]^3,X_pv44[,4]^4)
  data_pv4<-data.frame(y=f4,x_1=X_pv4[,2],x_2=X_pv4[,3],x_3=X_pv4[,4],x_4=X_pv4[,5])
  coefficient_pv4<-lm(y~1+x_1+x_2+x_3+x_4, data=data_pv4)
  Y_values_p4[,i]<-predict(coefficient_pv4, newdata=data_pv4)
}

Y_mean_p4<-mean(Y_values_p4)

Var_p4<-(1/99)*(sum(abs(Y_values_p4-Y_mean_p4)^2))

bias_squared_4<-abs(f-Y_mean_p4)^2

Risk_p4<-Var_p4+bias_squared_4


Y_values_p5<-matrix(0L,nrow=30,ncol=100)

p_v5<-5


for(i in 0:100){
  epsilon2<-rnorm(30, mean=0, sd=0.05)
  f4<-1-x2+2*x2^2-0.8*x2^3+0.6*x2^4-x2^5+epsilon2
  X_pv55<-matrix(X_final,nrow=30, ncol=p1)
  X_pv5<-cbind(rep(1,30),X_pv55[,1],X_pv55[,2]^2,X_pv55[,3]^3,X_pv55[,4]^4,X_pv55[,5]^5)
  data_pv5<-data.frame(y=f4,x_1=X_pv5[,2],x_2=X_pv5[,3],x_3=X_pv5[,4],x_4=X_pv5[,5],x_5=X_pv5[,6])
  coefficient_pv5<-lm(y~1+x_1+x_2+x_3+x_4+x_5, data=data_pv5)
  Y_values_p5[,i]<-predict(coefficient_pv5, newdata=data_pv5)
}

Y_mean_p5<-mean(Y_values_p5)

Var_p5<-(1/99)*(sum(abs(Y_values_p5-Y_mean_p5)^2))

bias_squared_5<-abs(f-Y_mean_p5)^2

Risk_p5<-Var_p5+bias_squared_5



Y_values_p6<-matrix(0L,nrow=30,ncol=100)

p_v6<-6


for(i in 0:100){
  epsilon2<-rnorm(30, mean=0, sd=0.05)
  f4<-1-x2+2*x2^2-0.8*x2^3+0.6*x2^4-x2^5+epsilon2
  X_pv66<-matrix(X_final,nrow=30,ncol=p_v6)
  X_pv6<-cbind(rep(1,30),X_pv66[,1],X_pv66[,2]^2, X_pv66[,3]^3, X_pv66[,4]^4, X_pv66[,5]^5, X_pv66[,6]^6)
  data_pv6<-data.frame(y=f4,x_1=X_pv6[,2],x_2=X_pv6[,3],x_3=X_pv6[,4],x_4=X_pv6[,5],x_5=X_pv6[,6],x_6=X_pv6[,7])
  coefficient_pv6<-lm(y~1+x_1+x_2+x_3+x_4+x_5+x_6, data=data_pv6)
  Y_values_p6[,i]<-predict(coefficient_pv6, newdata=data_pv6)
}

Y_mean_p6<-mean(Y_values_p6)


Var_p6<-(1/99)*(sum(abs(Y_values_p6-Y_mean_p6)^2))

bias_squared_6<-abs(f-Y_mean_p6)^2

Risk_p6<-Var_p6+bias_squared_6


Y_values_p7<-matrix(0L,nrow=30,ncol=100)

p_v7<-7


for(i in 0:100){
  epsilon2<-rnorm(30, mean=0, sd=0.05)
  f4<-1-x2+2*x2^2-0.8*x2^3+0.6*x2^4-x2^5+epsilon2
  X_pv77<-matrix(X_final,nrow=30,ncol=p_v7)
  X_pv7<-cbind(rep(1,30),X_pv77[,1],X_pv77[,2]^2, X_pv77[,3]^3, X_pv77[,4]^4, X_pv77[,5]^5, X_pv77[,6]^6, X_pv77[,7]^7)
  data_pv7<-data.frame(y=f4,x_1=X_pv7[,2],x_2=X_pv7[,3],x_3=X_pv7[,4],x_4=X_pv7[,5],x_5=X_pv7[,6],x_6=X_pv7[,7], x_7=X_pv7[,8])
  coefficient_pv7<-lm(y~1+x_1+x_2+x_3+x_4+x_5+x_6+x_7, data=data_pv7)
  Y_values_p7[,i]<-predict(coefficient_pv7, newdata=data_pv7)
}

Y_mean_p7<-mean(Y_values_p7)


Var_p7<-(1/99)*(sum(abs(Y_values_p7-Y_mean_p7)^2))

bias_squared_7<-abs(f-Y_mean_p7)^2

Risk_p7<-Var_p7+bias_squared_7


Y_values_p8<-matrix(0L,nrow=30,ncol=100)

p_v8<-8


for(i in 0:100){
  epsilon2<-rnorm(30, mean=0, sd=0.05)
  f4<-1-x2+2*x2^2-0.8*x2^3+0.6*x2^4-x2^5+epsilon2
  X_pv88<-matrix(X_final,nrow=30,ncol=r8)
  X_pv8<-cbind(rep(1,30),X_pv88[,1],X_pv88[,2]^2, X_pv88[,3]^3, X_pv88[,4]^4, X_pv88[,5]^5, X_pv88[,6]^6, X_pv88[,7]^7, X_pv88[,8]^8)
  data_pv8<-data.frame(y=f4,x_1=X_pv8[,2],x_2=X_pv8[,3],x_3=X_pv8[,4],x_4=X_pv8[,5],x_5=X_pv8[,6],x_6=X_pv8[,7],x_7=X_pv8[,8],x_8=X_pv8[,9])
  coefficient_pv8<-lm(y~1+x_1+x_2+x_3+x_4+x_5+x_6+x_7+x_8, data=data_pv8)
  
  Y_values_p8[,i]<-predict(coefficient_pv8, newdata=data_pv8)
}

Y_mean_p8<-mean(Y_values_p8)


Var_p8<-(1/99)*(sum(abs(Y_values_p8-Y_mean_p8)^2))

bias_squared_8<-abs(f-Y_mean_p8)^2

Risk_p8<-Var_p8+bias_squared_8


Y_values_p9<-matrix(0L,nrow=30,ncol=100)

p_v9<-9


for(i in 0:100){
  epsilon2<-rnorm(30, mean=0, sd=0.05)
  f4<-1-x2+2*x2^2-0.8*x2^3+0.6*x2^4-x2^5+epsilon2
  X_pv99<-matrix(X_final,nrow=30,ncol=p_v9)
  X_pv9<-cbind(rep(1,30),X_pv99[,1],X_pv99[,2]^2, X_pv99[,3]^3, X_pv99[,4]^4, X_pv99[,5]^5, X_pv99[,6]^6, X_pv99[,7]^7, X_pv99[,8]^8, X_pv99[,9]^9)
  data_pv9<-data.frame(y=f4,x_1=X_pv9[,2],x_2=X_pv9[,3],x_3=X_pv9[,4],x_4=X_pv9[,5],x_5=X_pv9[,6],x_6=X_pv9[,7],x_7=X_pv9[,8],x_8=X_pv9[,9],x_9=X_pv9[,10])
  coefficient_pv9<-lm(y~1+x_1+x_2+x_3+x_4+x_5+x_6+x_7+x_8+x_9, data=data_pv9)
  Y_values_p9[,i]<-predict(coefficient_pv9, newdata=data_pv9)
}

Y_mean_p9<-mean(Y_values_p9)


Var_p9<-(1/99)*(sum(abs(Y_values_p9-Y_mean_p9)^2))

bias_squared_9<-abs(f-Y_mean_p9)^2

Risk_p9<-Var_p9+bias_squared_9


Y_values_p10<-matrix(0L,nrow=30,ncol=100)

p_v10<-10


for(i in 0:100){
  epsilon2<-rnorm(30, mean=0, sd=0.05)
  f4<-1-x2+2*x2^2-0.8*x2^3+0.6*x2^4-x2^5+epsilon2
  X_pv110<-matrix(X_final,nrow=30,ncol=p_v10)
  X_pv10<-cbind(rep(1,30),X_pv110[,1],X_pv110[,2]^2, X_pv110[,3]^3, X_pv110[,4]^4, X_pv110[,5]^5, X_pv110[,6]^6, X_pv110[,7]^7, X_pv110[,8]^8, X_pv110[,9]^9, X_pv110[,10]^10)
  data_pv10<-data.frame(y=f4,x_1=X_pv10[,2],x_2=X_pv10[,3],x_3=X_pv10[,4],x_4=X_pv10[,5],x_5=X_pv10[,6],x_6=X_pv10[,7],x_7=X_pv10[,8],x_8=X_pv10[,9],x_9=X_pv10[,10],x_10=X_pv10[,11])
  coefficient_pv10<-lm(y~1+x_1+x_2+x_3+x_4+x_5+x_6+x_7+x_8+x_9+x_10, data=data_r10)
  Y_values_p10[,i]<-predict(coefficient_pv10, newdata=data_pv10)
}

Y_mean_p10<-mean(Y_values_p10)


Var_p10<-(1/99)*(sum(abs(Y_values_p10-Y_mean_p10)^2))

bias_squared_10<-abs(f-Y_mean_p10)^2

Risk_p10<-Var_p10+bias_squared_10



plot.new()


plot(1,Var_p1,ylim=range(c(0,0.15)),xlim=range(c(0,10)))
par(new=TRUE)
plot(2,Var_p2,ylim=range(c(0,0.15)),xlim=range(c(0,10)))
par(new=TRUE)
plot(3,Var_p3,ylim=range(c(0,0.15)),xlim=range(c(0,10)))
par(new=TRUE)
plot(4,Var_p4,ylim=range(c(0,0.15)),xlim=range(c(0,10)))
par(new=TRUE)
plot(5,Var_p5,ylim=range(c(0,0.15)),xlim=range(c(0,10)))
par(new=TRUE)
plot(6,Var_p6,ylim=range(c(0,0.15)),xlim=range(c(0,10)))
par(new=TRUE)
plot(7,Var_p7,ylim=range(c(0,0.15)),xlim=range(c(0,10)))
par(new=TRUE)
plot(8,Var_p8,ylim=range(c(0,0.15)),xlim=range(c(0,10)))
par(new=TRUE)
plot(9,Var_p9,ylim=range(c(0,0.15)),xlim=range(c(0,10)))
par(new=TRUE)
plot(10,Var_p10,ylim=range(c(0,0.15)),xlim=range(c(0,10)))
title(main="Variance of Y hat")


plot.new()
plot(rep(1,30),bias_squared_1,ylim=range(c(0,0.01)),xlim=range(c(0,10)))
par(new=TRUE)
plot(rep(2,30),bias_squared_2,ylim=range(c(0,0.01)),xlim=range(c(0,10)))
par(new=TRUE)
plot(rep(3,30),bias_squared_3,ylim=range(c(0,0.01)),xlim=range(c(0,10)))
par(new=TRUE)
plot(rep(4,30),bias_squared_4,ylim=range(c(0,0.01)),xlim=range(c(0,10)))
par(new=TRUE)
plot(rep(5,30),bias_squared_5,ylim=range(c(0,0.01)),xlim=range(c(0,10)))
par(new=TRUE)
plot(rep(6,30),bias_squared_6,ylim=range(c(0,0.01)),xlim=range(c(0,10)))
par(new=TRUE)
plot(rep(7,30),bias_squared_7,ylim=range(c(0,0.01)),xlim=range(c(0,10)))
par(new=TRUE)
plot(rep(8,30),bias_squared_8,ylim=range(c(0,0.01)),xlim=range(c(0,10)))
par(new=TRUE)
plot(rep(9,30),bias_squared_9,ylim=range(c(0,0.01)),xlim=range(c(0,10)))
par(new=TRUE)
plot(rep(10,30),bias_squared_10,ylim=range(c(0,0.01)),xlim=range(c(0,10)))
title(main="Bias squared of Y hat")

plot.new()


plot(rep(1,30),Risk_p1,ylim=range(c(0,0.15)),xlim=range(c(0,10)),col="red")
par(new=TRUE)
lines(rep(1,30),Risk_p1,ylim=range(c(0,0.15)),xlim=range(c(0,10)),col="red")
par(new=TRUE)
lines(rep(2,30),Risk_p2,ylim=range(c(0,0.15)),xlim=range(c(0,10)),col="red")
par(new=TRUE)
lines(rep(3,30),Risk_p3,ylim=range(c(0,0.15)),xlim=range(c(0,10)),col="red")
par(new=TRUE)
lines(rep(4,30),Risk_p4,ylim=range(c(0,0.15)),xlim=range(c(0,10)),col="red")
par(new=TRUE)
lines(rep(5,30),Risk_p5,ylim=range(c(0,0.15)),xlim=range(c(0,10)),col="red")
par(new=TRUE)
lines(rep(6,30),Risk_p6,ylim=range(c(0,0.15)),xlim=range(c(0,10)),col="red")
par(new=TRUE)
lines(rep(7,30),Risk_p7,ylim=range(c(0,0.15)),xlim=range(c(0,10)),col="red")
par(new=TRUE)
lines(rep(8,30),Risk_p8,ylim=range(c(0,0.15)),xlim=range(c(0,10)),col="red")
par(new=TRUE)
lines(rep(9,30),Risk_p9,ylim=range(c(0,0.15)),xlim=range(c(0,10)),col="red")
par(new=TRUE)
lines(rep(10,30),Risk_p10,ylim=range(c(0,0.15)),xlim=range(c(0,10)),col="red")
title(main="Risk of Y hat")



f_s<-matrix(f,nrow=30,ncol=100)

Res_s1matrix<-matrix(0L,nrow=1,ncol=100)

for(i in 0:100){
  Res_s1matrix[,i]<-sum((f_s[,i]-Y_values_p1[,i])^2)
  
}

Res_s2matrix<-matrix(0L,nrow=1,ncol=100)

for(i in 0:100){
  Res_s2matrix[,i]<-sum((f_s[,i]-Y_values_p2[,i])^2)
  
}

Res_s3matrix<-matrix(0L,nrow=1,ncol=100)

for(i in 0:100){
  Res_s3matrix[,i]<-sum((f_s[,i]-Y_values_p3[,i])^2)
  
}


Res_s4matrix<-matrix(0L,nrow=1,ncol=100)

for(i in 0:100){
  Res_s4matrix[,i]<-sum((f_s[,i]-Y_values_p4[,i])^2)
  
}

Res_s5matrix<-matrix(0L,nrow=1,ncol=100)

for(i in 0:100){
  Res_s5matrix[,i]<-sum((f_s[,i]-Y_values_p5[,i])^2)
  
}

Res_s6matrix<-matrix(0L,nrow=1,ncol=100)

for(i in 0:100){
  Res_s6matrix[,i]<-sum((f_s[,i]-Y_values_p6[,i])^2)
  
}

Res_s7matrix<-matrix(0L,nrow=1,ncol=100)

for(i in 0:100){
  Res_s7matrix[,i]<-sum((f_s[,i]-Y_values_p7[,i])^2)
  
}

Res_s8matrix<-matrix(0L,nrow=1,ncol=100)

for(i in 0:100){
  Res_s8matrix[,i]<-sum((f_s[,i]-Y_values_p8[,i])^2)
  
}

Res_s9matrix<-matrix(0L,nrow=1,ncol=100)

for(i in 0:100){
  Res_s9matrix[,i]<-sum((f_s[,i]-Y_values_p9[,i])^2)
  
}

Res_s10matrix<-matrix(0L,nrow=1,ncol=100)

for(i in 0:100){
  Res_s10matrix[,i]<-sum((f_s[,i]-Y_values_p10[,i])^2)
  
}


plot.new()

plot(1:100, Res_s1matrix,col="green",ylim=range(c(0,0.1)),xlim=range(c(0,100)))
par(new=TRUE)
plot(1:100, Res_s2matrix,col="yellow",ylim=range(c(0,0.1)),xlim=range(c(0,100)))
par(new=TRUE)
plot(1:100, Res_s3matrix,col="orange",ylim=range(c(0,0.1)),xlim=range(c(0,100)))
par(new=TRUE)
plot(1:100, Res_s4matrix,col="blue",ylim=range(c(0,0.1)),xlim=range(c(0,100)))
par(new=TRUE)
plot(1:100, Res_s5matrix,col="rosybrown",ylim=range(c(0,0.1)),xlim=range(c(0,100)))
par(new=TRUE)
plot(1:100, Res_s6matrix,col="purple",ylim=range(c(0,0.1)),xlim=range(c(0,100)))
par(new=TRUE)
plot(1:100, Res_s7matrix,col="red",ylim=range(c(0,0.1)),xlim=range(c(0,100)))
par(new=TRUE)
plot(1:100, Res_s8matrix,col="lightsalmon",ylim=range(c(0,0.1)),xlim=range(c(0,100)))
par(new=TRUE)
plot(1:100, Res_s9matrix,col="navy",ylim=range(c(0,0.1)),xlim=range(c(0,100)))
par(new=TRUE)
plot(1:100, Res_s10matrix,col="violet",ylim=range(c(0,0.1)),xlim=range(c(0,100)))
title(main="Residuals vs s")


plot.new()

plot(1:100, Res_s1matrix+(2*0.05^2*2),col="green",ylim=range(c(0,0.15)),xlim=range(c(0,100)))
par(new=TRUE)
plot(1:100, Res_s2matrix+(2*0.05^2*3),col="yellow",ylim=range(c(0,0.15)),xlim=range(c(0,100)))
par(new=TRUE)
plot(1:100, Res_s3matrix+(2*0.05^2*4),col="orange",ylim=range(c(0,0.15)),xlim=range(c(0,100)))
par(new=TRUE)
plot(1:100, Res_s4matrix+(2*0.05^2*5),col="blue",ylim=range(c(0,0.15)),xlim=range(c(0,100)))
par(new=TRUE)
plot(1:100, Res_s5matrix+(2*0.05^2*6),col="rosybrown",ylim=range(c(0,0.15)),xlim=range(c(0,100)))
par(new=TRUE)
plot(1:100, Res_s6matrix+(2*0.05^2*7),col="purple",ylim=range(c(0,0.15)),xlim=range(c(0,100)))
par(new=TRUE)
plot(1:100, Res_s7matrix+(2*0.05^2*8),col="red",ylim=range(c(0,0.15)),xlim=range(c(0,100)))
par(new=TRUE)
plot(1:100, Res_s8matrix+(2*0.05^2*9),col="lightsalmon",ylim=range(c(0,0.15)),xlim=range(c(0,100)))
par(new=TRUE)
plot(1:100, Res_s9matrix+(2*0.05^2*10),col="navy",ylim=range(c(0,0.15)),xlim=range(c(0,100)))
par(new=TRUE)
plot(1:100, Res_s10matrix+(2*0.05^2*11),col="violet",ylim=range(c(0,0.15)),xlim=range(c(0,100)))
title(main="C_p vs s")




