#AIM: length of new shoots from buds in proleptic buds
#data: DERUTA 2020
#PhD: Francesca Grisafi

source("Scripts/GLMs/6_PRO_length_new_shoots_analyses.R")
library(plotfunctions)
library(RColorBrewer)
library(plotrix)

summary(MV.bud.PRO$median_distance_norm)
nline = dim(MV.bud.PRO)[1]
a = -0.5
b = -0.25
c = 0.25
d = 0.5
for (i in 1:nline) {
  if (MV.bud.PRO[i, "median_distance_norm"] >= a &
      MV.bud.PRO[i, "median_distance_norm"] < b) {
    MV.bud.PRO[i, "class_distance"] = "proximal"
  }
  if (MV.bud.PRO[i, "median_distance_norm"] >= b &
      MV.bud.PRO[i, "median_distance_norm"] <= c) {
    MV.bud.PRO[i, "class_distance"] = "median"
  }
  if (MV.bud.PRO[i, "median_distance_norm"] > c &
      MV.bud.PRO[i, "median_distance_norm"] <= d) {
    MV.bud.PRO[i, "class_distance"] = "distal"
  }
}
MV.bud.PRO$class_distance = as.factor(MV.bud.PRO$class_distance)

#frequence distribution
prop = MV.bud.PRO[0, 0]
for (i in levels(MV.bud.PRO$fate)) {
  fate = i
  df = droplevels(MV.bud.PRO[MV.bud.PRO$fate == i, ])
  for (j in levels(df$class_distance)) {
    position = j
    dt = droplevels(df[df$class_distance == j, ])
    for (z in unique(dt$Length)) {
      Length = z
      dx = dt[dt$Length == z, ]
      av = mean(dx$length2yo.cm.)
      se = sd(dx$length2yo.cm.) / sqrt(length(dx$length2yo.cm.))
      tot = cbind(fate, position, Length, av, se)
      prop = rbind(prop, tot)
    }
  }
}
prop
str(prop)
prop[1:2] = lapply(prop[1:2], as.factor)
prop[3:5] = lapply(prop[3:5], as.numeric)
str(prop)
#create a sequence with random numbers between 1 and maximum sib
df = data.frame(Length = rep(seq(
  min(prop$Length),
  max(prop$Length),
  length.out = length(unique(prop$Length))
), 2))
df$fate = c(rep("V", length(unique(prop$Length))), rep("M", length(unique(prop$Length))))
df
#quindi metto prima +0.5(distal) e poi proximal(-0.5)
for (j in c(0.5, 0.25, 0, -0.25,-0.5)) {
  df = cbind(df, data.frame(rep(j, length(df$Length))))
}
#rename columns
head(df)
str(df)
names(df)[-c(1, 2)] = "median_distance_norm"
conf_int = df
#predict model according to normal_median, for each type of sequence of distance (0-10)
for (i in 3:ncol(df)) {
  pred = predict(model,
                 newdata = df[c(1, 2, i)],
                 type = "response",
                 se.fit = T)
  df = cbind(df, pred$fit)
}
head(df)
#remove negative values
names(df)
dupli = df[8:12]
dupli[dupli < 0] = NA
df[8:12] = dupli
head(df)
#rename columns
#quindi metto prima +0.5(distal) e poi proximal(-0.5)
colnames(df)[8:12] = c(0.5, 0.25, 0, -0.25, -0.5)
#confidence intervel
for (i in grep("median_distance_norm", colnames(df))) {
  pred = predict(model, newdata = conf_int[c(1, 2, i)], se.fit = T)
  lw = pred$fit + qnorm(0.025) * pred$se.fit
  up = pred$fit + qnorm(0.975) * pred$se.fit
  lim = as.data.frame(cbind(lw, up))
  conf_int = cbind(conf_int, lim)
}
head(conf_int)
names(conf_int)
conf_int = conf_int[c(1, 2, 10, 9, 14, 11, 16, 15)]
colnames(conf_int)[c(3, 5, 7)] = "lw"
colnames(conf_int)[c(4, 6, 8)] = "up"
#remove negative values
head(conf_int)
dupl = conf_int[c(3:8)]
dupl[dupl < 0] = 0
conf_int[c(3:8)] = dupl

#graph
png("Outputs/Plots/6_PRO_length_new_shoots.png",
    width = 1200,
    height = 900,
    res = 150)# save plot
par(oma = c(4, 4, 2.5, 4))
par(mfrow = c(1, 2), mai = c(0.2, 0.1, 0.1, 0.1))
rbPal <- brewer.pal(n = 6, name = "Set1")
transp = alphaPalette(rbPal, rep(0.25, 6))
V = droplevels(prop[prop$fate == "V", ])
with(
  V,
  plot(
    av ~ Length,
    col = rbPal[V$position],
    xlab = "fate (V or M)",
    ylab = NA,
    yaxt="n",
    ylim = c(0, 20),
    xlim = c(0, 72),
    pch = c(15:17)[V$position]
  ),
  na.rm = T
)
axis(2,
     at = seq(0,20,5))
mtext(
  "Shoot length (cm)",
  side = 1,
  line = 2,
  cex = 1,
  outer = F
)
mtext(
  "new shoots length (cm)",
  side = 2,
  line = 2,
  cex = 1,
  outer = F
)
mtext(
  "fate V",
  side = 3,
  line = 1,
  cex = 1,
  outer = F
)
# for (i in levels(V$position)) {
#   q=droplevels(V[V$position==i,])
#   for (j in unique(q$siblings)) {
#     p=droplevels(q[q$siblings==j,])
#     plotCI(p$siblings,p$av,
#          li = p$av-p$se,
#          ui = p$av+p$se,
#          add=T)
#   }
# }
VV = droplevels(df[df$fate == "V", ])
for (i in 1:3) {
  t = colnames(VV)[c(8, 10, 12)][i]
  new = cbind(VV[1], VV[t])
  new = new[order(new$Length), ]
  with(VV, lines(new[, 2] ~ new$Length, col = rbPal[i], lwd = 3))
}
VVV = droplevels(conf_int[conf_int$fate == "V", ])
for (i in 1:length(grep("lw", colnames(VVV)))) {
  j = grep("lw", colnames(VVV))[i]
  id = 1
  t_lw = j
  t_up = j + 1
  with(VVV[c(id, t_lw:t_up)], polygon(
    x = c(rev(VVV$Length), VVV$Length),
    y = c(rev(lw), up),
    col = transp[i],
    border = NA
  ))
}
M = droplevels(prop[prop$fate == "M", ])
with(
  M,
  plot(
    av ~ Length,
    col = rbPal[M$position],
    xlab = "fate (V or M)",
    ylab = NA,
    yaxt="n",
    ylim = c(0, 20),
    xlim = c(0, 72),
    pch = c(15:17)[M$position]
  ),
  na.rm = T
)
axis(4,
     at = seq(0,20,5))
mtext(
  "Shoot length (cm)",
  side = 1,
  line = 2,
  cex = 1,
  outer = F
)
mtext(
  "new shoots length (cm)",
  side = 4,
  line = 2,
  cex = 1,
  outer = F
)
mtext(
  "fate M",
  side = 3,
  line = 1,
  cex = 1,
  outer = F
)
# for (i in leMels(M$position)) {
#   q=dropleMels(M[M$position==i,])
#   for (j in unique(q$siblings)) {
#     p=dropleMels(q[q$siblings==j,])
#     plotCI(p$siblings,p$aM,
#          li = p$aM-p$se,
#          ui = p$aM+p$se,
#          add=T)
#   }
# }
MM = droplevels(df[df$fate == "M", ])
for (i in 1:3) {
  t = colnames(MM)[c(8, 10, 12)][i]
  new = cbind(MM[1], MM[t])
  new = new[order(new$Length), ]
  with(MM, lines(new[, 2] ~ new$Length, col = rbPal[i], lwd = 3))
}
MMM = droplevels(conf_int[conf_int$fate == "M", ])
for (i in 1:length(grep("lw", colnames(MMM)))) {
  j = grep("lw", colnames(MMM))[i]
  id = 1
  t_lw = j
  t_up = j + 1
  with(MMM[c(id, t_lw:t_up)], polygon(
    x = c(rev(MMM$Length), MMM$Length),
    y = c(rev(lw), up),
    col = transp[i],
    border = NA
  ))
}
legend(
  "topleft",
  horiz = F,
  title = "position in the shoot",
  xpd = TRUE,
  c("distal", "median", "proximal"),
  pch = c(15:17),
  col = rbPal[1:3],
  cex = 0.8,
  bty = 'n'
)
dev.off()

