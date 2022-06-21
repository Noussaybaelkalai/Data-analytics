data=read.csv("BF.csv",header=TRUE, dec=".")

#Lecture des données
data<-read.csv("BF.csv",header=TRUE, dec=".")
str(data)
data_cr<-scale(data,center=TRUE,scale=TRUE)
as.data.frame(data_cr)->data_cr

#=======Application du kmeans 
#k-means avec les donn??es centr??es et r??duites
library(factoextra)
elbow_method<-fviz_nbclust(
  data_cr,
  FUNcluster=kmeans,
  method="wss"
)
elbow_method
library(NbClust)
NbClust(
  data=data_cr,
  distance= "euclidean",
  method="kmeans"
)

set.seed(12)
groupes.kmeans <- kmeans(data_cr,centers=14,nstart=5)
#affichage des r??sultats
print(groupes.kmeans)
write.table(data_cr, "data_cr.csv", sep=';', col.names=TRUE, row.names=TRUE)

fviz_cluster(groupes.kmeans,data = data_cr)

data$cluster<-as.factor(groupes.kmeans$cluster) 
View(data)
#=======Application du kmeans avec un nb de groupe qui varie de  1 ?? 61
#k-means avec les donn??es centr??es et r??duites
#??valuer la proportion d'inertie expliqu??e
inertie.expl <- rep(0,times=61)
for (k in 1:61){
  clus <- kmeans(data_cr,centers=k,nstart=5)
  inertie.expl[k] <- clus$betweenss/clus$totss
}
print(clus)
max(inertie.expl)

#graphique
plot(1:61,inertie.expl,type="b",xlab="Nb. de groupes",ylab="% inertie expliqu??e")
inertie.expl


var(inertie.expl[15:61])*46*100/(var(inertie.expl)*60)

#donc on va choisir 14 classes /le nbre de classes ?? retenir est 14

library(FactoMineR)


#CAH
# l'arbre est automatiquement coupÃ© au niveau suggÃ©rÃ©
Res<-HCPC(data_cr , nb.clust=-1)
plot(Res,choice="tree")
plot(Res,choice="map",draw.tree=False)
plot(Res,choice="3D.map")
Res
Res$data.clust
Res$desc.var
Res$desc.ind

##Calcul du taux d'inertie
I<-Res$call
I
#le taux d'inertie : Inertie Inter/Inertie total,avant la consolidation de la CAH.
I$bw.before.consol
#le taux d'inertie : Inertie Inter/Inertie total,aprÃ¨s la consolidation de la CAH.
I$bw.after.consol

#gain d'inerite
Res$call$'t'$inert.gain

