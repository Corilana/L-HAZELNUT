#logigram:MOD1
#AIM: graph of probability of having a sylleptic shoot on that rank 
#data: DERUTA 2020
#PhD: Francesca Grisafi
source("Scripts/GLMs/2_PRO_shoot_type.R")

library(RColorBrewer)

#relative frequency table of proportion of sylleptic related to narmal_distance
parameters
syl_norm=table(met$normal_distance, met$shoot_type)
syl_norm
syl_norm.freq=prop.table(syl_norm, 1)
syl_norm.freq.matrix = as.data.frame.matrix(syl_norm.freq)
syl_norm.freq.matrix$normal_distance = as.numeric(rownames(syl_norm.freq.matrix))
head(syl_norm.freq.matrix)

plot(met$shoot_type~met$normal_distance)
str(met$shoot_type)
#graph
png("Outputs/Plots/2_proba_sylleptic.png",
    width = 1200,
    height = 900,
    res = 150)# save plot
rbPal <- brewer.pal(n=6, name="Set1")
with(
  syl_norm.freq.matrix,
  plot(
    SYLLEPTIC ~ normal_distance,
    xlab = "normalized distance from median rank node",
    ylab = "proportion of sylleptic shoots",
    ylim = c(0, 1),
    type = "h",
    lwd = 4
  )
)
lines(seq(min(syl_norm.freq.matrix$normal_distance), max(syl_norm.freq.matrix$normal_distance), 0.05),
      predict(model, newdata = data.frame(normal_distance = seq(
        min(syl_norm.freq.matrix$normal_distance), max(syl_norm.freq.matrix$normal_distance), 0.05)),"response"),
      lwd = 5)
dev.off()

