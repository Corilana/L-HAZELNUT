#EXPLORATORY ANALISYS_annual shoot
setwd("C:/Users/franc/Google Drive/PhD/Deruta/R/auto/AnnualShoot")
library(RColorBrewer)
library(dplyr)
#shoot level df
auto=read.csv("C:/Users/franc/Google Drive/PhD/Deruta/DF/auto/mtp use/2020shoot_level_DEVELOPED_fin.csv")

length(auto$shoot)#104 shoots
table(auto$class)# 26Sh, 25Me, 28Lo, 25VLo

tot_=grep("tot_buds", colnames(auto))
# sum(auto$tot_buds)#1677
# sum(auto[auto$class=="Sh",tot_])#133 in Sh shoots
# sum(auto[auto$class=="Me",tot_])#296 in Me shoots
# sum(auto[auto$class=="Lo",tot_])#577 in Lo shoots
# sum(auto[auto$class=="VLo",tot_])#671 in VLo shoots

#CREATE THE CONTINGENCY TABLE
max=max(auto$node)#max nodes
freq_node=as.data.frame(matrix(nrow=1, ncol=max))#new df for the contingency table
colnames(freq_node)=c(1:max)#columns name=ranks
table=as.data.frame.matrix(t(table(auto$node)))#coningency table without zeros

for (i in 1:max) {
  node=colnames(freq_node)[i]#node of frewq_node
  node_tab=grep(paste0("^",i,"$"), colnames(table))#corresponding node of table
  if (length(node_tab!=0)) {#if that node exist in table
  freq_table=table[1,node_tab]
  freq_node[1,node]=freq_table#paste the value that there is in table
  } else {#if not exist
    freq_node[1,node]=0#it means that there are zero shoots with that node length
  }
}


#1distribution length (number of nodes)
png("frequencelength.png",width=1200, height=900, res=150)# save plot
cols=colors()[577]
barplot(as.matrix(freq_node),col = cols,main="distribution of annual shoot", xlab = "length(number of nodes)", ylab="number of shoots", ylim = c(0,13))
dev.off()

#2
#from this graph we can think about making just 2 class length (from 0 to 11 nodes and >11 nodes)

# #relationship between parent length and number of nodes
# png("node~length.png",width=1200, height=900, res=150)# save plot
# par(mar=c(5,5,5,3))
# col=brewer.pal(4, name="Set2")
# plot(auto$node~auto$Length, pch=16,cex=1.2,main="relationship #node/length", xlab="parent length(cm)", ylab="parent #nodes", col=col[factor(auto$class, levels = c("Sh","Me", "Lo", "VLo"))])
# legend("topleft", legend=c("Sh", "Me", "Lo", "VLo"), lwd=5, cex= 1, col=col)
# dev.off()
