#logigram:MOD4
#AIM: nb buds of B, V and M in proleptic shoots
#data: DERUTA 2020
#PhD: Francesca Grisafi
source("Own_rooted_young/Scripts/GLMs/4_PRO_mv_proportion_analyses.R")

library(paletteer)

#relative frequency table of M,V and B with rank node
tab = as.matrix(prop.table(table(data.poly$rank_node, data.poly$fate), 1))
tab
#predicted value
df = data.frame(
  rank_node = unique(sort(data.poly$rank_node)),
  rank_node0.5 = unique(sort(data.poly$rank_node)) ** 0.5,
  rank_node2 = unique(sort(data.poly$rank_node)) ** 2,
  rank_node3 = unique(sort(data.poly$rank_node)) ** 3,
  rank_node4 = unique(sort(data.poly$rank_node)) ** 4
)
df
pred = cbind(df, predict(model, newdata = df, "probs", se = T))
colnames(pred)[ncol(pred)] = "M"
pred$V = 1 - pred$M

#graph
# png("Own_rooted_young/Outputs/Plots/4_PRO_prop_MV.png",width=1200, height=900, res=150)# save plot
par(mar=c(5,5,5,5))
cols<-paletteer_d("ggsci::alternating_igv")
r=barplot(t(tab),
          col = cols, names.arg = row.names(tab),
          xlab = "rank node",
          ylab="Bud fate proportion", ylim = c(0,1))
points(r, pred$V, type = "l", lwd = 7, col = "black")
with(pred, lines(x = r,pred$V,lwd=5, col=cols[1]))
points(r, pred$M, type = "l", lwd = 7, col = "black")
with(pred, lines(x = r,pred$M,lwd=5, col=cols[2]))
legend("topright",
       inset=c(-0.13,-0.1),xpd = TRUE,
       title = "bud fate",
       legend = c(colnames(tab),paste0("predict",colnames(tab))),
       lty=c(NA,NA,NA,1,1,1),fill = c(cols, cols), cex=0.8,)
# dev.off()
