data=read.csv("BF.csv",header=TRUE, dec=".")
str(data) 

pairs(data[,2:4])
model<-lm(BodyFat~.,data=data)
summary(model)
summary(model)$r.squared
summary(model)$adj.r.squared


step(model)
model1=lm(BodyFat~ Density+Age+Chest,data=data)
summary(model1)
AIC(model1)
plot(predict(model1),resid(model1))
shapiro.test(resid(model1))
ks.test(resid(model1),pnorm)
x0=rep(1,109)
M=matrix(c(x0,data$Density,data$Age,data$Chest),109,4)
ul=matrix(0,109,1)
ll=matrix(0,109,1)
m=matrix(0,109,4)
j=1
for (i in 1:109) {
  ul[i]=predict(model1)[i]+1.98*1.766*sqrt(1+M[i,]%*%solve(t(M)%*%M)%*%M[i,])
  ll[i]=predict(model1)[i]-1.98*1.766*sqrt(1+M[i,]%*%solve(t(M)%*%M)%*%M[i,])
  if (data$bwt[i]>ul[i] | data$bwt[i]<ll[i]) {
    m[j,]<-c(i,data$bwt[i],ul[i],ll[i])
    j<-j+1 
  }                 
}

seqx=seq(1,109,length=109)
sd=sqrt(deviance(model1)/df.residual(model1))
sd
abr=abs(data$bwt-predict(model1))/1.766
plot(seqx,abr)
abline(h=2, lty=2,col=2)

summary(model1)$r.squared
summary(model1)$adj.r.squared

###Procédure de séléction Fisher
#####Etape1
nva<-ncol(data)
Fish = rep(0,nva)
for (i in  2:ncol(data) ) {
  mod<-lm(data[,1]~data[,i])
  Fish[i]=var(predict(mod))*(nrow(data)-1)/(deviance(mod)/df.residual(mod))
}
Fish
df2=nrow(data)-2
df2
1-pf(max(Fish),1,df2)

### Introduction de la variable Density

##### Etape 2 -Inroduction-
nva=ncol(data)-1
Fish = rep(0,nva)
SCR1<-deviance(lm(data[,1]~data[,2]))
for (i in 3:ncol(data)) {
  mod<-lm(data[,1]~data[,2]+data[,i])
  SCR2=deviance(mod)
  Fish[i]=(SCR1-SCR2)/(SCR2/(nrow(data)-3))
}
Fish
df2=nrow(data)-3
df2
1-pf(max(Fish),1,df2)
##### Introduction de Chest

###### Etape 2 -Retrait-
Fish = rep(0,2)
SCR2=deviance(lm(data[,1]~data[,2]+data[,7]))
mod<-lm(data[,1]~data[,2])
SCR1<-deviance(mod)
Fish[1]=(SCR1-SCR2)/(SCR2/(nrow(data)-3))

mod<-lm(data[,1]~data[,7])
SCR1=deviance(mod)
Fish[2]=(SCR1-SCR2)/(SCR2/(nrow(data)-3))
Fish
df2=nrow(data)-3
df2
1-pf(min(Fish),1,df2)

## Aucune variable n'est retitée, les F sont significatifs 



##### Etape 3 -Introduction-
Fish = rep(0,nva)
SCR2<-deviance(lm(data[,1]~data[,2]+data[,7]))
for (i in 3:ncol(data)) {
  mod<-lm(data[,1]~data[,2]+data[,7]+data[,i])
  SCR3=deviance(mod)
  Fish[i]=(SCR2-SCR3)/(SCR3/(nrow(data)-4))
}
Fish
df2=nrow(data)-4
df2
1-pf(max(Fish),1,df2)
########## Introduction d 'Age

#### Etape 3 "Retrait"
SCR3<-deviance(lm(data[,1]~data[,2]+data[,3]+data[,7]))

Fish<-rep(0,3)
mod<-lm(data[,1]~data[,2]+data[,3])
SCR2<-deviance(mod)
Fish[1]=(SCR2-SCR3)/(SCR3/(nrow(data)-4))

mod<-lm(data[,1]~data[,2]+data[,7])
SCR2<-deviance(mod)
Fish[2]=(SCR2-SCR3)/(SCR3/(nrow(data)-4))

mod<-lm(data[,1]~data[,3]+data[,7])
SCR2<-deviance(mod)
Fish[3]=(SCR2-SCR3)/(SCR3/(nrow(data)-4))
Fish

df2=nrow(data)-4
df2

1-pf(min(Fish),1,df2)

##### Aucune variable n'est retitée

##### Etape 4 -Introduction-
Fish<-rep(0,nva)
SCR4<-deviance(lm(data[,1]~data[,2]+data[,7]+data[,3]))
for (i in 3:ncol(data)) {
  mod<-lm(data[,1]~data[,2]+data[,7]+data[,3]+data[,i])
  SCR5=deviance(mod)
  Fish[i]=(SCR4-SCR5)/(SCR5/(nrow(data)-4))
}

Fish
df2=nrow(data)-5
df2
1-pf(max(Fish),1,df2)
###########Introduction de biceps

#### Etape 4 -Retrait-
Fish = rep(0,4)
SCR4<-deviance(lm(data[,1]~data[,2]+data[,3]+data[,7]+data[,13]))
mod<-lm(data[,1]~data[,2]+data[,3]+data[,7])
SCR3<-deviance(mod)
Fish[1]<-(SCR3-SCR4)/(SCR4/(nrow(data)-5))

mod<-lm(data[,1]~data[,2]+data[,3]+data[,13])
SCR3<-deviance(mod)
Fish[2]<-(SCR3-SCR4)/(SCR4/(nrow(data)-5))

mod<-lm(data[,1]~data[,2]+data[,7]+data[,13])
SCR3<-deviance(mod)
Fish[3]<-(SCR3-SCR4)/(SCR4/(nrow(data)-5))

mod<-lm(data[,1]~data[,3]+data[,7]+data[,13])
SCR3<-deviance(mod)
Fish[4]<-(SCR3-SCR4)/(SCR4/(nrow(data)-5))

Fish
df2=nrow(data)-5
df2
1-pf(min(Fish),1,df2)
###### Aucune variable n'est retirée


###### On s'arrete car l'ajout des variables explicatives n'est plus signicatif

mod<-lm(data[,1]~data[,7]+data[,2]+data[,3])
summary(mod)
summary(mod)$adj.r.squared

mod<-lm(data[,1]~data[,2]+data[,3]+data[,7])
summary(mod)




