#logigram:MOD1
#AIM: graph of probability of having a sylleptic shoot on that rank 
#data: DERUTA 2020
#PhD: Francesca Grisafi
library(RColorBrewer)
source("Own_rooted_young/Scripts/GLMs/3_PRO_shoot_type.R")

#relative frequency table of proportion of sylleptic related to narmal_distance
parameters
syl_norm=table(met$abs_norm_median_distance, met$shoot_type)
syl_norm
syl_norm.freq=prop.table(syl_norm, 1)
syl_norm.freq.matrix = as.data.frame.matrix(syl_norm.freq)
syl_norm.freq.matrix$abs_norm_median_distance = as.numeric(rownames(syl_norm.freq.matrix))
head(syl_norm.freq.matrix)

plot(met$shoot_type~met$abs_norm_median_distance)
str(met$shoot_type)
#graph
# png("Own_rooted_young/Outputs/Plots/3_proba_sylleptic.png",width = 1200,height = 900,res = 150)# save plot
rbPal <- brewer.pal(n=6, name="Set1")
with(
  syl_norm.freq.matrix,
  plot(
    SYLLEPTIC ~ abs_norm_median_distance,
    xlab = "|normalized distance from median rank node|",
    ylab = "proportion of sylleptic shoots",
    ylim = c(0, 1),
    type = "h",
    lwd = 4,
    col="orange"
  )
)
new_x = seq(min(syl_norm.freq.matrix$abs_norm_median_distance), max(syl_norm.freq.matrix$abs_norm_median_distance), 0.05)
new_y = predict(model, newdata = data.frame(abs_norm_median_distance = new_x),"response")
lines(new_x, new_y,lwd = 5)

pred = predict(model, newdata = data.frame(abs_norm_median_distance = new_x),"response",se.fit = T)
upr <- pred$fit + qnorm(0.975)*pred$se.fit
lwr <- pred$fit + qnorm(0.025)*pred$se.fit
cols = adjustcolor("black", alpha.f=0.5)
polygon(c(rev(new_x), new_x), c(rev(upr), lwr), col = cols, border = NA)
legend("topright", lty=c(1,1),lwd=c(4,5), col = c("orange","black"),legend = c("observed", "predicted"))
# dev.off()

