#centrer et réduire les données
data<-read.csv("BF.csv",header=TRUE, dec=".")
str(data)
data_cr<-scale(data,center=TRUE,scale=TRUE)
as.data.frame(data_cr)->data_cr

disc_var1=kmeans(data_cr[,1],centers=3 ,nstart=5)
disc_var2=kmeans(data_cr[,2],centers=3 ,nstart=5)
disc_var3=kmeans(data_cr[,3],centers=3 ,nstart=5)
disc_var4=kmeans(data_cr[,4],centers=3 ,nstart=5)
disc_var5=kmeans(data_cr[,5],centers=3 ,nstart=5)
disc_var6=kmeans(data_cr[,6],centers=3 ,nstart=5)
disc_var7=kmeans(data_cr[,7],centers=3 ,nstart=5)
disc_var8=kmeans(data_cr[,8],centers=3 ,nstart=5)
disc_var9=kmeans(data_cr[,9],centers=3 ,nstart=5)
disc_var10=kmeans(data_cr[,10],centers=3 ,nstart=5)
disc_var11=kmeans(data_cr[,11],centers=3 ,nstart=5)
disc_var12=kmeans(data_cr[,12],centers=3 ,nstart=5)
disc_var13=kmeans(data_cr[,13],centers=3 ,nstart=5)
disc_var14=kmeans(data_cr[,14],centers=3 ,nstart=5)
disc_var15=kmeans(data_cr[,15],centers=3 ,nstart=5)
#affichage de taux d'homogénité:
taux_inertie=disc_var1$betweenss/disc_var1$totss
print(paste("le taux d'inertie de transformation à 3  modalités pour la variable  ",colnames(data_cr)[1],': ',taux_inertie))
taux_inertie=disc_var2$betweenss/disc_var2$totss
print(paste("le taux d'inertie de transformation à 3 modalités pour la variable  ",colnames(data_cr)[2],': ',taux_inertie))
taux_inertie=disc_var3$betweenss/disc_var3$totss
print(paste("le taux d'inertie de transformation à 3 modalités pour la variable  ",colnames(data_cr)[3],': ',taux_inertie))
taux_inertie=disc_var4$betweenss/disc_var4$totss
print(paste("le taux d'inertie de transformation à 3 modalités pour la variable  ",colnames(data_cr)[4],': ',taux_inertie))
taux_inertie=disc_var5$betweenss/disc_var5$totss
print(paste("le taux d'inertie de transformation à 3 modalités pour la variable  ",colnames(data_cr)[5],': ',taux_inertie))
taux_inertie=disc_var6$betweenss/disc_var6$totss
print(paste("le taux d'inertie de transformation à 3 modalités pour la variable  ",colnames(data_cr)[6],': ',taux_inertie))
taux_inertie=disc_var7$betweenss/disc_var7$totss
print(paste("le taux d'inertie de transformation à 3 modalités pour la variable  ",colnames(data_cr)[7],': ',taux_inertie))
taux_inertie=disc_var8$betweenss/disc_var8$totss
print(paste("le taux d'inertie de transformation à 3 modalités pour la variable  ",colnames(data_cr)[8],': ',taux_inertie))
taux_inertie=disc_var9$betweenss/disc_var9$totss
print(paste("le taux d'inertie de transformation à 3 modalités pour la variable  ",colnames(data_cr)[9],': ',taux_inertie))
taux_inertie=disc_var10$betweenss/disc_var10$totss
print(paste("le taux d'inertie de transformation à 3 modalités pour la variable  ",colnames(data_cr)[10],': ',taux_inertie))
taux_inertie=disc_var11$betweenss/disc_var11$totss
print(paste("le taux d'inertie de transformation à 3 modalités pour la variable  ",colnames(data_cr)[11],': ',taux_inertie))
taux_inertie=disc_var12$betweenss/disc_var12$totss
print(paste("le taux d'inertie de transformation à 3 modalités pour la variable  ",colnames(data_cr)[12],': ',taux_inertie))
taux_inertie=disc_var13$betweenss/disc_var13$totss
print(paste("le taux d'inertie de transformation à 3 modalités pour la variable  ",colnames(data_cr)[13],': ',taux_inertie))
taux_inertie=disc_var14$betweenss/disc_var14$totss
print(paste("le taux d'inertie de transformation à 3 modalités pour la variable  ",colnames(data_cr)[14],': ',taux_inertie))
taux_inertie=disc_var15$betweenss/disc_var15$totss
print(paste("le taux d'inertie de transformation à 3 modalités pour la variable  ",colnames(data_cr)[15],': ',taux_inertie))
#creation tableau d'AMC
#pour 3 modalités
BodyFat=factor(disc_var1$cluster,labels=c('BodyFat_mod1','BodyFat_mod2','BodyFat_mod3'))
Density=factor(disc_var2$cluster,labels=c('Density_mod1','Density_mod2','Density_mod3'))
Age=factor(disc_var3$cluster,labels=c('Age_mod1','Age_mod2','Age_mod3'))
Weight=factor(disc_var4$cluster,labels=c('Weight_mod1','Weight_mod2','Weight_mod3'))
Height=factor(disc_var5$cluster,labels=c('Height_mod1','Height_mod2','Height_mod3'))
Neck=factor(disc_var6$cluster,labels=c('Neck_mod1','Neck_mod2','Neck_mod3'))
Chest=factor(disc_var7$cluster,labels=c('Chest_mod1','Chest_mod2','Chest_mod3'))
Abdomen=factor(disc_var8$cluster,labels=c('Abdomen_mod1','Abdomen_mod2','Abdomen_mod3'))
Hip=factor(disc_var9$cluster,labels=c('Hip_mod1','Hip_mod2','Hip_mod3'))
Thigh=factor(disc_var10$cluster,labels=c('Thigh_mod1','Thigh_mod2','Thigh_mod3'))
Knee=factor(disc_var11$cluster,labels=c('Knee_mod1','Knee_mod2','Knee_mod3'))
Ankle=factor(disc_var12$cluster,labels=c('Ankle_mod1','Ankle_mod2','Ankle_mod3'))
Biceps=factor(disc_var13$cluster,labels=c('Biceps_mod1','Biceps_mod2','Biceps_mod3'))
Forearm=factor(disc_var14$cluster,labels=c('Forearm_mod1','Forearm_mod2','Forearm_mod3'))
Wrist=factor(disc_var15$cluster,labels=c('Wrist_mod1','Wrist_mod2','Wrist_mod3'))


data.amc=data.frame(BodyFat,Density,Age,Weight,Height,Neck,Chest,Abdomen,Hip,Thigh,Knee,Ankle,Biceps,Forearm,Wrist)
#écrire dans un fichier csv
write.csv(data.amc,file="data.amc.csv")
#écrire dans un fichier csv
#écrire les données quantitatives
write.csv(data,file ="data.amc.csv",sheetName="les variables quantitatives",append=FALSE)
#écrire les données qualitatives
write.csv(data.amc,file ="data.amc.csv",sheetName="les variables qualitatives",append=TRUE)
#écrire le taux de discrétisation
write.csv(taux_inertie,file ="data.amc.csv",sheetName="le  taux de discrétisation de chaque transformation ",append=TRUE)
library(FactoMineR)
#tableau de disjonction
table_disj=tab.disjonctif(data.amc)
table_disj


#Existe t-il des modalités de faibles fréquences ?
propmod=apply(table_disj,2,sum)/(nrow(K))
propmod

#nombre de modalité
ncol(table_disj)

#nombre de variables
ncol(data.amc)

#nombre d'individus
nrow(data.amc)


#appliquer ACM
res<-MCA(data.amc,ncp=15,graph=FALSE,axes=c(2.3))
res

#Calcul des valeurs propres, le pourcentage d’inertie de chaque valeur propre ainsi que le cumul des pourcentages d’inertie 
res$eig

#nombre de valeurs propres non nulles
min(nrow(data.amc)-1,ncol(table_disj)-ncol(data.amc))

#Tracez le graphique des valeurs propres.
plot(res$eig[,1],type="b",main="Scree plot")

#choix de dimension de sous espace
#dim = 3
var(res$eig[4:16,1])*12/(var(res$eig[,1])*15)
#dim = 4
var(res$eig[5:16,1])*11/(var(res$eig[,1])*15)
#dim = 5
var(res$eig[6:16,1])*10/(var(res$eig[,1])*15)
#dim = 6
var(res$eig[7:16,1])*9/(var(res$eig[,1])*15)

#=====================Nuage de Modalités
#le cos2 des modalités sur le sous espace
res$var$cos2[,1:5]
print(t(apply(res$var$cos2[,1:5],1,cumsum)),digit=6)

#la contribution des modalités sur le sous espace
contrib<-res$var$contrib[,1:5]
contrib

plot(res,choix="var")

#Appliquez la CAH au tableau des contributions des modalités 
#aux axes du sous espace
Cat_var<-HCPC(as.data.frame(contrib),nb.clust=-1)
plot(Cat_var,choice="tree")
plot(Cat_var,choice="map",draw.tree=False)
plot(Cat_var,choice="3D.map")
Cat_var$desc.var
res$var
#le nuage des modalités projeté sur les 2 premiers axes
plot(res, invisible = c("ind", "quali.sup", "quanti.sup"),cex = 0.8,title="Projection des modalités sur le plan 1-2")
#plot(res,choix="var", cex=0.75,title="Projection des modalités sur le plan 1-2")
#=================================Nuage des individus
#le cos2 des individus sur le sous espace
res$ind$cos2[,1:5]
print(t(apply(res$ind$cos2[,1:5],1,cumsum)),digit=6)
#la contribution des individus dans chaque axe du sous espace
res$ind$contrib[,1:5]
#Application de la CAH au tableau des contributions des individus
# aux axes du sous espace
cont<-res$ind$contrib[,1:5]
cont
Cat_ind<-HCPC(as.data.frame(cont),nb.clust=-1)
plot(Cat_ind,choice="tree")
plot(Cat_ind,choice="map",draw.tree=False)
plot(Cat_ind,choice="3D.map")
Cat_ind$desc.var
Cat_ind$desc.axes
#=============================Nuage des variables
#Calcul des coefficients de corrélation des variables avec les projections sur les axes
res$var$eta2[,1:5]
#graphique des coefficients de corrélation des variables avec les facteurs du 1er plan factoriel
plot(res,choix="var", cex=0.75,title="Projection des variables sur le plan 1-2") 

