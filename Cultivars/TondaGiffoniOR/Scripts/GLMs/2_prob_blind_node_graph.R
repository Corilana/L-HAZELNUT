#logigram:MOD1
#AIM: graph of probability of having a sylleptic shoot on that rank 
#data: DERUTA 2020
#PhD: Francesca Grisafi
library(RColorBrewer)
source("Own_rooted_young/Scripts/GLMs/2_prob_blind_node.R")

#relative frequency table of proportion of sylleptic related to narmal_distance
syl_norm=t(table(met$b, met$rank_node))
syl_norm
syl_norm.freq=prop.table(syl_norm, 1)
syl_norm.freq.matrix = as.data.frame.matrix(syl_norm.freq)
syl_norm.freq.matrix$rank_node = as.numeric(rownames(syl_norm.freq.matrix))
head(syl_norm.freq.matrix)

colnames(syl_norm.freq.matrix)[1]="Blind"

met$b = factor(met$b)
plot(met$b~met$rank_node)
str(met$shoot_type)

#graph
# png("Own_rooted_young/Outputs/Plots/2_proba_blind_node.png",width = 1200,height = 900,res = 150)# save plot
rbPal <- brewer.pal(n=6, name="Set1")
with(
  syl_norm.freq.matrix,
  plot(
    Blind ~ rank_node,
    xlab = "rank node",
    ylab = "proportion of blind nodes",
    ylim = c(0, 1),
    type = "h",
    lwd = 4,
    col="orange"
  )
)
new_x = seq(min(syl_norm.freq.matrix$rank_node), max(syl_norm.freq.matrix$rank_node), 0.05)
new_y = predict(model, newdata = data.frame(rank_node = new_x),"response")
lines(new_x, new_y,lwd = 5)

pred = predict(model, newdata = data.frame(rank_node = new_x),"response",se.fit = T)
upr <- pred$fit + qnorm(0.975)*pred$se.fit
lwr <- pred$fit + qnorm(0.025)*pred$se.fit
cols = adjustcolor("black", alpha.f=0.5)
polygon(c(rev(new_x), new_x), c(rev(upr), lwr), col = cols, border = NA)

legend("topright", lty=c(1,1),lwd=c(4,5), col = c("orange","black"),legend = c("observed", "predicted"))
# dev.off()

