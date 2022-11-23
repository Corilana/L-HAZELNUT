#AIM1: freq dist per each rank --> nb of shoots
#AIM2: relationship length (node) ~ length (cm)
#AIM3: relationship length(cm) ~ diameter (mm)
#data:deruta 2020
#PhD: Francesca Grisafi
source("Scripts/Modify_dataset/import_dataset.R")
library(gridExtra)
library(grid)

# AIM1: freq dist per each rank --> nb of c,v,m,b, nuts, and sum of buds --------
max=max(shoot$node)
freq.rank=as.data.frame(seq(1:max))

for (i in 1:max) {
  freq.rank[i,"nb_shoots"]=length(shoot[shoot$node==i,"node"])
  }
freq.rank=as.matrix(freq.rank)
print(freq.rank)

# graph
png("Outputs/Plots/1_length_dist.png",width=1200, height=900, res=150)# save plot
barplot(freq.rank[,2],names.arg = freq.rank[,1],main="length distribution of annual shoots",
        xlab = "length(nodes)", ylab="nb of shoots", ylim = c(0,13))
dev.off()

#from this graph we can think about making just 2 rank length (from 0 to 11 nodes and >11 nodes)

# AIM2: relationship length (node) ~ length (cm) --------------------------
png("Outputs/Plots/1_node~length.png",width=1200, height=900, res=150)# save plot
par(mar=c(5,5,5,3))
plot(shoot$node~shoot$Length, pch=c(1:4)[shoot$class],
     cex=1.2,main="relationship #node/length", xlab="parent length(cm)", 
     ylab="parent #nodes")
legend("topleft", legend=c("Sh", "Me", "Lo", "VLo"), pch=c(1:4),bty='n')
dev.off()


# AIM3: relationship length(cm) ~ diameter (mm) --------------------------
#data visualization
plot(Diam~Length, shoot,pch=19)
#MODEL
model = nls(Diam ~ a * (Length ^ b) + 0,
            data = shoot,
            start = c(a = 1, b = 1))
summary(model)
# visualize
lines(seq(0,75,1), predict(model, data.frame(Length=seq(0,75,1))),lwd=3)
#save outputs
out=capture.output(summary(model))
cat("1_length~diameter", out, file="Outputs/Tables/1_length~diameter.txt", sep="\n")


