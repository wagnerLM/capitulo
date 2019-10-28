### SF12
# carregando o banco de dados
sf12<-read.csv("https://raw.githubusercontent.com/wagnerLM/capitulo_sf12/master/sf_12",sep=";")
sf12_nomes<-scan("https://raw.githubusercontent.com/wagnerLM/capitulo_sf12/master/nomes",what = "character", sep = "\n")
# visualizando o banco de dados
View(sf12)
View(sf12_nomes)
# instalando e carregando os pacotes 
install.packages("psych")
install.packages("qgraph")
install.packages("igraph")
library(psych)
library(qgraph)
library(igraph)
# visualizando a matriz de correlações
cor.plot(cor_auto(sf12),numbers = T,upper = F)
# modelo gráfico gaussiano com aproximação não-paramétrica
sf12_g<-qgraph(cor_auto(sf12),layout="spring",graph="glasso",sampleSize=nrow(na.omit(sf12)),posCol="blue",threshold=T,edge.labels=T,labels=colnames(sf12),nodeNames=sf12_nomes,legend.cex=0.3,cut=0.3,edge.label.position=0.4,vsize=6,GLratio=1.5,0.5)
cor.plot(getWmat(sf12_g),numbers = T,upper = F)
centralityPlot(sf12_g,include = "ExpectedInfluence",orderBy = "ExpectedInfluence")
# análise de comunidades
sf12_ig<-as.igraph(sf12_g)
sf12_sgc<-spinglass.community(sf12_ig,implementation = "neg")
sf12_sgc$membership
qgraph(sf12_g,groups=as.factor(sf12_sgc$membership),labels=colnames(sf12),legend=T,legend.cex=0.3,GLratio=1.5,0.5)
# flow chart
par(mfrow=c(1,1))
flow(sf12_g2,"sf1")
flow(sf12_g2,"sf7a")
flow(sf12_g2,"sf3b")
sf12_g2<-qgraph(cor_auto(sf12),layout="spring",graph="glasso",sampleSize=nrow(na.omit(sf12)),posCol="blue",threshold=T,edge.labels=T,labels=colnames(sf12),legend.cex=0.3,cut=0.3,edge.label.position=0.4,vsize=6,GLratio=1.5,0.5)
