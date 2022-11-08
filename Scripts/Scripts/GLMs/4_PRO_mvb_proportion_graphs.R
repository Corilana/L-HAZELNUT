#AIM: nb buds of B, V and M in proleptic shoots
#data: DERUTA 2020
#PhD: Francesca Grisafi
source("Scripts/GLMs/4_PRO_mvb_proportion_analyses.R")

library(RColorBrewer)

#relative frequency table of M,V and B with rank node
tab = as.matrix(prop.table(table(data.poly$rank_node, data.poly$fate), 1))
tab
#predicted value
df = data.frame(
  rank_node = p_rank_node,
  rank_node0.5 = p_rank_node ** 0.5,
  rank_node2 = p_rank_node ** 2,
  rank_node3 = p_rank_node ** 3,
  rank_node4 = p_rank_node ** 4
)
pred = cbind(df, predict(test, newdata = df, "probs", se = T))
pred

#graph
png("Outputs/Plots/4_PRO_prop_MVB.png",width=1200, height=900, res=150)# save plot
par(mar=c(5,5,5,5))
cols<-brewer.pal(n=3,name="Set2")
r=barplot(t(tab),
          col = cols, names.arg = row.names(tab),
          xlab = "rank node",
          ylab="proportion of B,M,V", ylim = c(0,1))
with(pred, lines(x = r,pred$V,lwd=5, col="green"))
with(pred, lines(x = r,pred$B,lwd=5, col="orange"))
with(pred, lines(x = r,pred$M,lwd=5, col="blue"))
legend("topright",
       inset=c(-0.13,0),xpd = TRUE,
       title = "bud fate",
       legend = c(colnames(tab),paste0("predict",colnames(tab))),
       lty=c(NA,NA,NA,1,1,1),fill = c(cols, c("green","orange","blue")), cex=0.8)
dev.off()
