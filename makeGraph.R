require(igraph) #grabs some extra files

nodeList<-read.table("webOutput3.txt") #go get the file that haskel made
nodeList <- graph.data.frame(nodeList) #make the graph
lo <- layout.fruchterman.reingold(nodeList, vertex.size = 4, edge.arrow.size = 0.5, edge.width = .5)#make a better layout for the graph

nodeList2<-read.table("webOutput2.txt") #go get the file that haskel made
nodeList2 <- graph.data.frame(nodeList2) #make the graph
lo2 <- layout.fruchterman.reingold(nodeList2, vertex.size = 4, edge.arrow.size = 0.5, edge.width = .5)#make a better layout for the graph 

pdf('graph.pdf') # open the pdf we will start writing to

plot(nodeList, vertex.size = 4, edge.arrow.size = 0.5, edge.width = .5, layout=lo, vertex.label.cex=.3) # make the picture

plot(nodeList2, vertex.size = 4, edge.arrow.size = 0.5, edge.width = .5, layout=lo2, vertex.label.cex=.3) # make the picture

dev.off()#close the pdf
