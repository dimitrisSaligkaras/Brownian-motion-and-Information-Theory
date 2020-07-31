
library("entropy")

#-----------------------------------------------------------------
#                 ??????? ?????? brown
#-----------------------------------------------------------------
  
#?=0.05 ??? ?= 0.15
t <- seq(0, 100, length.out=1001)# time
sig2 <- 0.01
#??????? ????????????? ???????? brown
nsim = 100
X1 <- matrix(rnorm(mean = 0.05, n = nsim * (length(t) - 1), sd = 0.15), 
             nsim, length(t) - 1)
X1 <- cbind(rep(0, nsim), t(apply(X1, 1, cumsum)))
#????? ?????? brown
plot(t, X1[1, ], xlab = "time", ylab = "X1", ylim = c(-50, 50), type = "l")
t <- seq(0, 100, length.out=1001)# time
sig2 <- 0.01
#??????? ????????????? ???????? brown
nsim = 100
X1 <- matrix(rnorm(mean = 0.05, n = nsim * (length(t) - 1), sd = 0.15), 
             nsim, length(t) - 1)
X1 <- cbind(rep(0, nsim), t(apply(X1, 1, cumsum)))
#????? ?????? brown
plot(t, X1[1, ], xlab = "time", ylab = "X1", ylim = c(-50, 50), type = "l")
#????????? 99 ???????? brown
apply(X1[2:nsim, ], 1, function(x, t) lines(t, x), t = t)


#?=0.05 ??? ?= 0.3
t <- seq(0, 100, length.out=1001)# time
sig2 <- 0.01
#??????? ????????????? ???????? brown
nsim = 100
X2 <- matrix(rnorm(mean = 0.05, n = nsim * (length(t) - 1), sd = 0.3), 
             nsim, length(t) - 1)
X2 <- cbind(rep(0, nsim), t(apply(X2, 1, cumsum)))
#????? ?????? brown
plot(t, X2[1, ], xlab = "time", ylab = "X2", ylim = c(-50, 50), type = "l")


#????????? 99 ???????? brown
apply(X2[2:nsim, ], 1, function(x, t) lines(t, x), t = t)


#?=0.2 ??? ?= 0.15
t <- seq(0, 100, length.out=1001)# time
sig2 <- 0.01
#??????? ????????????? ???????? brown
nsim = 100
X3 <- matrix(rnorm(mean = 0.2, n = nsim * (length(t) - 1), sd = 0.15), 
               nsim, length(t) - 1)
X3 <- cbind(rep(0, nsim), t(apply(X3, 1, cumsum)))
#????? ?????? brown
plot(t, X3[1, ], xlab = "time", ylab = "X3", ylim = c(-50, 50), type = "l")

#????????? 99 ???????? brown
apply(X3[2:20, ], 1, function(x, t) lines(t, x), t = t)

  
#------------------------------------------------------------------
#     ???????? ??? ???? ??????? ?????? ???? ??????? Brown
#------------------------------------------------------------------
library(entropy)    
#??? ?1
y11<-matrix(rep(0, 100000),nrow = 100, ncol = 1000)
for (j in 1:100) {
  for (i in 2:1000){
    w1=list()
    w1=discretize(X1[j,1:i], numBins=1+3.3*log(i), r=c(min(X1[j,1:i]),max(X1[j,1:i])))   
    w1=as.data.frame(w1)
    y11[j,i]<-entropy( w1$Freq, method = c("ML"))
  }
}



for (i  in 2:100) {
  plot(t,c(0,y11[i,]) ,xlab = "time", ylab = "ML - ?1", ylim = c(0, 5), type = "l" )
  par(new=TRUE)
}
  
  
  
# ??? ?2
y12<-matrix(rep(0, 100000),nrow = 100, ncol = 1000)
for (j in 1:100) {
  for (i in 2:1000){
    w1=list()
    w1=discretize(X2[j,1:i], 1+3.3*log(i), r=c(min(X2[j,1:i]),max(X2[j,1:i])))
    w1=as.data.frame(w1)
    y12[j,i]<-entropy( w1$Freq, method = c("ML"))
  }
}

y12[]

for (i  in 2:100) {
  plot(t,c(0,y12[i,]) ,xlab = "time", ylab = "ML - ?2", ylim = c(0, 5), type = "l" )
  par(new=TRUE)
}
  
  
  
# ??? ?3
y13<-matrix(rep(0, 100000),nrow = 100, ncol = 1000)
for (j in 1:100) {
  for (i in 2:1000){
    w1=list()
    w1=discretize(X3[j,1:i], numBins=1+3.3*log(i), r=c(min(X3[j,1:i]),max(X3[j,1:i])))
    w1=as.data.frame(w1)
    y13[j,i]<-entropy( w1$Freq, method = c("ML"))
  }
}

y13[]

for (i  in 1:100) {
  plot(t,c(0,y13[i,]) ,xlab = "time = 1000", ylab = "ML - ?3", ylim = c(2, 4), type = "l" )
  par(new=TRUE)
}
 

  
#------------------------------------------------------------------
#              ???????? ????????? 
#------------------------------------------------------------------
    
#??? ?1    
par1=matrix(rep(0, 100000),nrow = 20, ncol = 1000)
for(j in 1:20){
  for (i in 1:999) {
     par1[j,i]<-y11[j+1,i+1]-y11[j+1,i]          
  }
}

  
for (i  in 1:20) {
  plot(t,c(0,par1[i,]) ,xlab = "time", ylab = "paragogi entropias - X1", ylim = c(-1, 1), type = "l" )
  par(new=TRUE)
}
  
  
#??? ?2
par2=matrix(rep(0, 100000),nrow = 20, ncol = 1000)
for(j in 1:20){
  for (i in 1:999) {
    par2[j,i]<-y12[j+1,i+1]-y12[j+1,i]
  }
}
par2[2,]

for (i  in 2:20) {
  plot(t,c(0,par2[i,]) ,xlab = "time", ylab = "paragogi entropias - X2", ylim = c(-1, 1), type = "l" )
  par(new=TRUE)
}
  
  
#??? ?3
par3=matrix(rep(0, 100000),nrow = 20, ncol = 1000)
for(j in 1:20){
  for (i in 1:999) {
    par3[j,i]<-y13[j+1,i+1]-y13[j+1,i]
  }
}
par3[]

for (i  in 1:20) {
  plot(t,c(0,par3[i,]) ,xlab = "time", ylab = "paragogi entropias - X3", ylim = c(0, 0.5),xlim = c(0, 40), type = "l" )
  par(new=TRUE)
}

#________________________________________________________________________________________
#                        ?????? ????????? ?????????
#________________________________________________________________________________________
  
ds1<-matrix(rep(0, 4000),nrow = 4, ncol = 1000)
for(j in 1:4){
  for(i in 2:1000){
    ds1[j,i]=(y11[j,i]-y11[j,i-1])/0.1
  }
}
tab1=c("1","2","3","4")
tab1=as.data.frame(tab1)
dim(tab1)
plot(t,c(0,ds1[1,]),type="l",ylim=c(-1,8),col="red",lty=1,ylab="dS",lwd=1,xlab="time")
lines(t,c(0,ds1[2,]),type="l",col="black",lwd=1)
lines(t,c(0,ds1[3,]),type="l",col="blue",lwd=1)
lines(t,c(0,ds1[4,]),type="l",col="green",lwd=1)
grid()
legend("topright",legend=rownames(tab1),lty=c(1,2,3,4),col=c("red","black","blue","green"),bg="white",lwd=2)
  
plot(t,c(0,ds1[2,]),type="l",ylim=c(-1,8),col="black",lty=1,ylab="dS",lwd=1,xlab="time")
plot(t,c(0,ds1[3,]),type="l",ylim=c(-1,8),col="blue",lty=1,ylab="dS",lwd=1,xlab="time")
plot(t,c(0,ds1[4,]),type="l",ylim=c(-1,8),col="green",lty=1,ylab="dS",lwd=1,xlab="time")

  
ds2<-matrix(rep(0, 4000),nrow = 4, ncol = 1000)
for(j in 1:4){
  for(i in 2:1000){
    ds2[j,i]=(y12[j,i]-y12[j,i-1])/0.1
  }
}
  
plot(t,c(0,ds2[1,]),type="l",ylim=c(-1,8),col="red",lty=1,ylab="dS",lwd=1,xlab="time",xlim = c(0,30))
lines(t,c(0,ds2[2,]),type="l",col="black",lwd=1)
lines(t,c(0,ds2[3,]),type="l",col="blue",lwd=1)
lines(t,c(0,ds2[4,]),type="l",col="green",lwd=1)
grid()
legend("topright",legend=rownames(tab1),lty=c(1,2,3,4),col=c("red","black","blue","green"),bg="white",lwd=2)
plot(t,c(0,ds2[2,]),type="l",ylim=c(-1,8),col="black",lty=1,ylab="dS",lwd=1,xlab="time")
plot(t,c(0,ds2[3,]),type="l",ylim=c(-1,8),col="blue",lty=1,ylab="dS",lwd=1,xlab="time")
plot(t,c(0,ds2[4,]),type="l",ylim=c(-1,8),col="green",lty=1,ylab="dS",lwd=1,xlab="time")

ds3<-matrix(rep(0, 4000),nrow = 4, ncol = 1000)
  
for(j in 1:4){
  for(i in 2:1000){
    ds3[j,i]=(y13[j,i]-y13[j,i-1])/0.1
  }
}
  
plot(t,c(0,ds3[1,]),xlim=c(0,100),type="l",ylim=c(-1,8),col="red",lty=1,ylab="dS",lwd=1,xlab="time")
lines(t,c(0,ds3[2,]),type="l",col="black",lwd=1)
lines(t,c(0,ds3[3,]),type="l",col="blue",lwd=1)
lines(t,c(0,ds3[4,]),type="l",col="green",lwd=1)
grid()
legend("topright",legend=rownames(tab1),lty=c(1,2,3,4),col=c("red","black","blue","green"),bg="white",lwd=2)
plot(t,c(0,ds3[2,]),type="l",ylim=c(-1,8),col="black",lty=1,ylab="dS",lwd=1,xlab="time")
plot(t,c(0,ds3[3,]),type="l",ylim=c(-1,8),col="blue",lty=1,ylab="dS",lwd=1,xlab="time")
plot(t,c(0,ds3[4,]),type="l",ylim=c(-1,8),col="green",lty=1,ylab="dS",lwd=1,xlab="time")

  


#------------------------------------------------------------------
#   ???????? ?????????? (?????? ??? ???????? brown)   
#------------------------------------------------------------------
 library(entropy)   

#??? ?1
Gm1<-matrix(rep(0, 1000),nrow = 1000, ncol =1)


for(i in 2:1000){
  y2d = discretize2d(X1[2,1:i], X1[2,1:i], numBins1=1+3.3*log(i), numBins2=1+3.3*log(i),r1=range(X1[2,1:i]),r2=range(X1[2,1:i]))
  Gm1[i,1]= mi.empirical(y2d)
}

plot(Gm1[1:1000,1],xlim = c(1, 1000),ylim = c(0, 5),type="l",ylab ="mutual information",xlab="time",col="blue",lwd=2)

Gm2<-matrix(rep(0, 1000),nrow = 1000, ncol =1)

for(i in 2:1000){
  y2d = discretize2d(X1[2,1:i], X1[3,1:i], numBins1=1+3.3*log(i), numBins2=1+3.3*log(i),r1=range(X1[2,1:i]),r2=range(X1[3,1:i]))
  Gm2[i,1]= mi.empirical(y2d)
}

plot(Gm2[1:1000,1],xlim = c(1, 1000),ylim = c(0, 5),type="l",ylab ="mutual information",xlab="time",col="blue",lwd=2)
Gm3<-matrix(rep(0, 1000),nrow = 1000, ncol =1)
for(i in 2:1000){
  y2d = discretize2d(X1[2,1:i], X1[4,1:i], numBins1=1+3.3*log(i), numBins2=1+3.3*log(i),r1=range(X1[2,1:i]),r2=range(X1[4,1:i]))
  Gm3[i,1]= mi.empirical(y2d)
}

plot(Gm3[1:1000,1],xlim = c(1, 1000),ylim = c(0, 5),type="l",ylab ="",xlab="time",col="blue",lwd=2)

Gm4<-matrix(rep(0, 1000),nrow = 1000, ncol =1)
for(i in 2:1000){
  y2d = discretize2d(X1[2,1:i], X1[5,1:i], numBins1=1+3.3*log(i), numBins2=1+3.3*log(i),r1=range(X1[2,1:i]),r2=range(X1[5,1:i]))
  Gm4[i,1]= mi.empirical(y2d)
}

plot(Gm1[1:1000,1],xlim = c(1, 1000),ylim = c(0, 5),type="l",ylab ="mutual information",xlab="time",col="yellow",lwd=2)
lines(Gm2[1:1000,1],type="l",col="red",lty=1,ylab="mutual information",lwd=2,xlab="time")
lines(Gm3[1:1000,1],type="l",col="black",lty=1,lwd=2,ylab="mutual information")
lines(Gm4[1:1000,1],type="l",col="blue",lty=1,lwd=2,ylab="mutual information")

#??? ?2

Gm1<-matrix(rep(0, 1000),nrow = 1000, ncol =1)
for(i in 2:1000){
  y2d = discretize2d(X2[2,1:i], X2[2,1:i], numBins1=1+3.3*log(i), numBins2=1+3.3*log(i),r1=range(X2[2,1:i]),r2=range(X2[2,1:i]))
  Gm1[i,1]= mi.empirical(y2d)
}

plot(Gm1[1:1000,1],xlim = c(1, 1000),ylim = c(0, 5),type="l",ylab ="mutual information",xlab="time",col="blue",lwd=2)

Gm2<-matrix(rep(0, 1000),nrow = 1000, ncol =1)

for(i in 2:1000){
  y2d = discretize2d(X2[2,1:i], X2[3,1:i], numBins1=1+3.3*log(i), numBins2=1+3.3*log(i),r1=range(X2[2,1:i]),r2=range(X2[3,1:i]))
  Gm2[i,1]= mi.empirical(y2d)
}

plot(Gm2[1:1000,1],xlim = c(1, 1000),ylim = c(0, 5),type="l",ylab ="mutual information",xlab="time",col="blue",lwd=2)
Gm3<-matrix(rep(0, 1000),nrow = 1000, ncol =1)
for(i in 2:1000){
  y2d = discretize2d(X2[2,1:i], X2[4,1:i], numBins1=1+3.3*log(i), numBins2=1+3.3*log(i),r1=range(X2[2,1:i]),r2=range(X2[4,1:i]))
  Gm3[i,1]= mi.empirical(y2d)
}

plot(Gm3[1:1000,1],xlim = c(1, 1000),ylim = c(0, 5),type="l",ylab ="mutual information",xlab="time",col="blue",lwd=2)

Gm4<-matrix(rep(0, 1000),nrow = 1000, ncol =1)
for(i in 2:1000){
  y2d = discretize2d(X2[2,1:i], X2[5,1:i], numBins1=1+3.3*log(i), numBins2=1+3.3*log(i),r1=range(X2[2,1:i]),r2=range(X2[5,1:i]))
  Gm4[i,1]= mi.empirical(y2d)
}

plot(Gm4[1:1000,1],xlim = c(1, 1000),ylim = c(0, 5),type="l",ylab ="mutual information",xlab="time",col="blue",lwd=2)
plot(Gm1[1:1000,1],xlim = c(1, 1000),ylim = c(0, 5),type="l",ylab ="mutual information",xlab="time",col="yellow",lwd=2)
lines(Gm2[1:1000,1],type="l",col="red",lty=1,ylab="mutual information",lwd=2,xlab="time")
lines(Gm3[1:1000,1],type="l",col="black",lty=1,lwd=2,ylab="mutual information")
lines(Gm4[1:1000,1],type="l",col="blue",lty=1,lwd=2,ylab="mutual information")
#??? ?3
Gm1<-matrix(rep(0, 1000),nrow = 1000, ncol =1)
for(i in 2:1000){
  y2d = discretize2d(X3[2,1:i], X3[2,1:i], numBins1=1+3.3*log(i), numBins2=1+3.3*log(i),r1=range(X3[2,1:i]),r2=range(X3[2,1:i]))
  Gm1[i,1]= mi.empirical(y2d)
}

plot(Gm1[1:1000,1],xlim = c(1, 1000),ylim = c(0, 5),type="l",ylab ="mutual information",xlab="time",col="blue",lwd=2)

Gm2<-matrix(rep(0, 1000),nrow = 1000, ncol =1)

for(i in 2:1000){
  y2d = discretize2d(X3[2,1:i], X3[3,1:i], numBins1=1+3.3*log(i), numBins2=1+3.3*log(i),r1=range(X3[2,1:i]),r2=range(X3[3,1:i]))
  Gm2[i,1]= mi.empirical(y2d)
}

plot(Gm2[1:1000,1],xlim = c(1, 1000),ylim = c(0, 5),type="l",ylab ="mutual information",xlab="time",col="blue",lwd=2)
Gm3<-matrix(rep(0, 1000),nrow = 1000, ncol =1)
for(i in 2:1000){
  y2d = discretize2d(X3[2,1:i], X3[4,1:i], numBins1=1+3.3*log(i), numBins2=1+3.3*log(i),r1=range(X3[2,1:i]),r2=range(X3[4,1:i]))
  Gm3[i,1]= mi.empirical(y2d)
}

plot(Gm3[1:1000,1],xlim = c(1, 1000),ylim = c(0, 5),type="l",ylab ="mutual information",xlab="time",col="blue",lwd=2)

Gm4<-matrix(rep(0, 1000),nrow = 1000, ncol =1)
for(i in 2:1000){
  y2d = discretize2d(X3[2,1:i], X3[5,1:i], numBins1=1+3.3*log(i), numBins2=1+3.3*log(i),r1=range(X3[2,1:i]),r2=range(X3[5,1:i]))
  Gm4[i,1]= mi.empirical(y2d)
}

plot(Gm4[1:1000,1],xlim = c(1, 1000),ylim = c(0, 5),type="l",ylab ="mutual information",xlab="time",col="blue",lwd=2)
plot(Gm1[1:1000,1],xlim = c(1, 1000),ylim = c(0, 5),type="l",ylab ="mutual information",xlab="time",col="yellow",lwd=2)
lines(Gm2[1:1000,1],type="l",col="red",lty=1,ylab="mutual information",lwd=2,xlab="time")
lines(Gm3[1:1000,1],type="l",col="black",lty=1,lwd=2,ylab="mutual information")
lines(Gm4[1:1000,1],type="l",col="blue",lty=1,lwd=2,ylab="mutual information")

  
#-----------------------------------------------------------------
#                       ???????? kulback (??? ????????)
#-----------------------------------------------------------------
    
    
#install.packages("LaplacesDemon")
library(LaplacesDemon)
library(LaplacesDemon)

#??? ?1
k1<-matrix(rep(y11[2,1000], 1000),nrow = 1, ncol = 1000)
Gsumkl11<-matrix(rep(0, 1000),nrow = 1, ncol = 1000)

for (i in 2:1000){
  w1=list()
  w2=list()
  w1=discretize(k1[1,1:i], 1+3.3*log(i), r=c(min(y11[2,1:1000]),max(y11[2,1:1000])))
  w2=discretize(y11[2,1:i], 1+3.3*log(i), r=c(min(y11[2,1:1000]),max(y11[2,1:1000])))
  w1=as.data.frame(w1)
  w2=as.data.frame(w2)
  Gsumkl11[1,i]=KLD(w1$Freq/i,w2$Freq/i)$sum.KLD.px.py    
}
  
plot(Gsumkl11[1,2:1000],xlim = c(1, 1000),ylim = c(0, 0.6),type="l",ylab ="?. ????????? - ?.?????????? - ?1",xlab="time",lwd=2)
  

#??? ?2
k2<-matrix(rep(y12[2,1000], 1000),nrow = 1, ncol = 1000)
Gsumkl12<-matrix(rep(0, 1000),nrow = 1, ncol = 1000)

for (i in 2:1000){
  w1=list()
  w2=list()
  w1=discretize(k2[1,1:i], 1+3.3*log(i), r=c(min(y12[2,1:1000]),max(y12[2,1:1000])))
  w2=discretize(y12[2,1:i], 1+3.3*log(i), r=c(min(y12[2,1:1000]),max(y12[2,1:1000])))
  w1=as.data.frame(w1)
  w2=as.data.frame(w2)
  Gsumkl12[1,i]=KLD(w1$Freq/i,w2$Freq/i)$sum.KLD.px.py    
}

plot(Gsumkl12[1,2:1000],xlim = c(1, 1000),ylim = c(0, 0.6),type="l",ylab ="?. ????????? - ?.?????????? - ?2",xlab="time",col="red",lwd=2)


#??? ?3
k3<-matrix(rep(y13[2,1000], 1000),nrow = 1, ncol = 1000)
Gsumkl13<-matrix(rep(0, 1000),nrow = 1, ncol = 1000)

for (i in 2:1000){
  w1=list()
  w2=list()
  w1=discretize(k3[1,1:i], numBins=1+3.3*log(i), r=c(min(y13[2,1:1000]),max(y13[2,1:1000])))
  w2=discretize(y13[2,1:i], numBins=1+3.3*log(i), r=c(min(y13[2,1:1000]),max(y13[2,1:1000])))
  w1=as.data.frame(w1)
  w2=as.data.frame(w2)
  Gsumkl13[1,i]=KLD(w1$Freq/i,w2$Freq/i)$sum.KLD.px.py    
}

plot(Gsumkl13[1,2:1000],xlim = c(1, 1000),ylim = c(0, .5),type="l",ylab ="?. ????????? - ?.?????????? - ?3",xlab="time",col="blue",lwd=2)



tab=c("K.L ???????? ?1","K.L ???????? ?2 ", "K.L ???????? ?3")
tab=as.data.frame(tab)
dim(tab)
plot(Gsumkl11[1,2:1000],type="l",ylim=c(0,0.6),col="red",lty=1,ylab="kullback divergence",lwd=2,xlab="time")
lines(Gsumkl12[1,2:1000],type="l",col="black",lty=2,lwd=2)
lines(Gsumkl13[1,2:1000],type="l",col="blue",lty=3,lwd=2)
grid()
legend("topright",legend=rownames(tab),lty=c(1,2,3),col=c("red","black","blue"),bg="white",lwd=2)

  

#------------------------------------------------------------------
#                       ?????????? ???????? (S[A;A]-S[A;B])
#------------------------------------------------------------------
for (i in 1:1) {
  for (j in 1:100) {
    I<-discretize2d(X1[i,], X1[i,], numBins1=1+3.3*log(1000), numBins2=1+3.3*log(1000))
    y2d = discretize2d(X1[j,], X1[i,], numBins1=1+3.3*log(1000), numBins2=1+3.3*log(1000))
    I1<-mi.empirical(I)
    I2<-mi.empirical(y2d)
    plot(j,I1-I2,xlim=c(0,100),ylim=c(0,2),xlab = "i", ylab = "X1")
    par(new=TRUE)
  }
}

for (i in 1:1) {
  for (j in 1:100) {
    I<-discretize2d(X2[i,], X2[i,], numBins1=1+3.3*log(1000), numBins2=1+3.3*log(1000))
    y2d = discretize2d(X2[j,], X2[i,], numBins1=1+3.3*log(1000), numBins2=1+3.3*log(1000))
    I1<-mi.empirical(I)
    I2<-mi.empirical(y2d)
    plot(j,I1-I2,xlim=c(0,100),ylim=c(0,2),xlab = "i", ylab = "X2")
    par(new=TRUE)
  }
}

for (i in 1:1) {
  for (j in 1:100) {
    I<-discretize2d(X3[i,], X3[i,], numBins1=1+3.3*log(1000), numBins2=1+3.3*log(1000))
    y2d = discretize2d(X3[j,], X3[i,], numBins1=1+3.3*log(1000), numBins2=1+3.3*log(1000))
    I1<-mi.empirical(I)
    I2<-mi.empirical(y2d)
    plot(j,I1-I2,xlim=c(0,100),ylim=c(0,2),xlab = "i", ylab = "X3")
    par(new=TRUE)
  }
}


