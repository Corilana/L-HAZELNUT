#logigram:MOD5ù
#AIM: proportion of new shoots from buds from proleptic buds
#data: DERUTA 2020
#PhD: Francesca Grisafi
source("Scripts/GLMs/5_PRO_new_shoots_proba_analyses.R")

library(plotfunctions)
library(RColorBrewer)
library(plotrix)

#create categories for median distance norm
summary(MV.bud.PRO$median_distance_norm)
str(MV.bud.PRO$presence_new_shoots)
MV.bud.PRO$presence_new_shoots=as.numeric(as.character(MV.bud.PRO$presence_new_shoots))
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

#new shoots proportion according to distance and fate
prop = MV.bud.PRO[0, 0]
for (j in levels(MV.bud.PRO$class_distance)) {
  position = j
  dt = droplevels(MV.bud.PRO[MV.bud.PRO$class_distance == j, ])
  for (i in levels(dt$fate)) {
    fate=i
    df=droplevels(dt[dt$fate==i,])
    for (z in unique(df$siblings_mv)) {
      siblings_mv = z
      dx = df[df$siblings_mv == z, ]
      av = mean(dx$presence_new_shoots)
      se = sd(dx$presence_new_shoots) / sqrt(length(dx$presence_new_shoots))
      tot = cbind(position,fate, siblings_mv, av, se)
      prop = rbind(prop, tot)
    }
  }
}

prop
str(prop)
prop[1:2] = lapply(prop[1:2], as.factor)
prop[3:5] = lapply(prop[3:5], as.numeric)
#create a sequence with random numbers between 1 and maximum sibling buds
df = data.frame(siblings_mv = rep(seq(
  min(prop$siblings_mv),
  max(prop$siblings_mv),
  length.out = length(unique(prop$siblings_mv))
),2))
df$fate=c(rep("V",length(df$siblings_mv)/2 ),rep("M",length(df$siblings_mv)/2 ))
#quindi metto prima +0.5(distal) e poi proximal(-0.5)
for (j in c(0.5, 0.25, 0, -0.25,-0.5)) {
  df = cbind(df, data.frame(rep(j, length(df$siblings_mv))))
}
#rename columns
df
str(df)
names(df)[-c(1,2)] = "median_distance_norm"
conf_int = df
#predict model according to normal_median, for each type of sequence of distance (0-10)
for (i in 3:ncol(df)) {
  pred = predict(model,
                 newdata = df[c(1,2, i)],
                 type = "response",
                 se.fit = T)
  df = cbind(df, pred$fit)
}
df
#rename columns
#quindi metto prima +0.5(distal) e poi proximal(-0.5)
colnames(df)[8:12] = c(0.5, 0.25, 0, -0.25, -0.5)
#confidence intervel
for (i in 1:length(colnames(df)[8:12])) {
  h = i + 2
  pred = predict(model, newdata = conf_int[c(1,2, h)], se.fit = T)
  lw = plogis(pred$fit + qnorm(0.025) * pred$se.fit)
  up = plogis(pred$fit + qnorm(0.975) * pred$se.fit)
  lim = as.data.frame(cbind(lw, up))
  conf_int = cbind(conf_int, lim)
}
conf_int = conf_int[c(1,2, 10, 9, 14, 11, 16, 15)]
colnames(conf_int)[c(3, 5, 7)] = "lw"
colnames(conf_int)[c(4, 6, 8)] = "up"

#split dataframes
V=prop[prop$fate=="V",]
M=prop[prop$fate=="M",]
df.V=df[df$fate=="V",]
df.M=df[df$fate=="M",]
conf_V=conf_int[conf_int$fate=="V",]
conf_M=conf_int[conf_int$fate=="M",]
#graph
png(
  "Outputs/Plots/5_PRO_proba_new_shoots_glm.png",
  width = 1200,
  height = 900,
  res = 150
)# save plot
par(oma = c(4, 4, 2.5, 4))
par(mfrow = c(1, 2), mai = c(0.2, 0.1, 0.1, 0.1))
rbPal <- brewer.pal(n = length(levels(prop$position)), name = "Set1")
transp = alphaPalette(rbPal, rep(0.25, length(levels(prop$position))))
# fateV
with(
  V,
  plot(
    av ~ siblings_mv,
    col = rbPal[V$position],
    ylim = c(0, 1),
    xlim = c(0, 8),
    yaxt = 'n',
    pch = c(15:17)[V$position]
  ),
  na.rm = T
)
axis(2,
     at = seq(0,1,0.2))
mtext(
  "nb other buds (M,V)",
  side = 1,
  line = 2,
  cex = 1,
  outer = F
)
mtext(
  "new shoots proportion",
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
#   for (j in unique(q$siblings_mv)) {
#     p=droplevels(q[q$siblings_mv==j,])
#     plotCI(p$siblings_mv,p$av,
#          li = p$av-p$se,
#          ui = p$av+p$se,
#          add=T)
#   }
# }
for (i in 1:3) {
  t = colnames(df.V)[c(8,10,12)][i]
  new = cbind(df.V[1], df.V[t])
  new = new[order(new$siblings_mv), ]
  with(df.V, lines(new[, 2] ~ new$siblings_mv, col = rbPal[i], lwd = 3))
}
for (i in 1:length(grep("lw", colnames(conf_V)))) {
  j = grep("lw", colnames(conf_V))[i]
  id = 1
  t_lw = j
  t_up = j + 1
  with(conf_V[c(id, t_lw:t_up)], polygon(
    x = c(seq(8, 0, by = -2), seq(0, 8, by = 2)),
    y = c(rev(lw), up),
    col = transp[i],
    border = NA
  ))
}
# fateM
with(
  M,
  plot(
    av ~ siblings_mv,
    col = rbPal[M$position],
    ylim = c(0, 1),
    xlim = c(0, 8),
    yaxt = 'n',
    pch = c(15:17)[M$position]
  ),
  na.rm = T
)
axis(4,
     at = seq(0,1,0.2))
mtext(
  "nb other buds (M,V)",
  side = 1,
  line = 2,
  cex = 1,
  outer = F
)
mtext(
  "new shoots proportion",
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
# for (i in levels(M$position)) {
#   q=droplevels(M[M$position==i,])
#   for (j in unique(q$siblings_mv)) {
#     p=droplevels(q[q$siblings_mv==j,])
#     plotCI(p$siblings_mv,p$av,
#            li = p$av-p$se,
#            ui = p$av+p$se,
#            add=T)
#   }
# }
for (i in 1:3) {
  t = colnames(df.M)[c(8,10,12)][i]
  new = cbind(df.M[1], df.M[t])
  new = new[order(new$siblings_mv), ]
  with(df.M, lines(new[, 2] ~ new$siblings_mv, col = rbPal[i], lwd = 3))
}
for (i in 1:length(grep("lw", colnames(conf_M)))) {
  j = grep("lw", colnames(conf_M))[i]
  id = 1
  t_lw = j
  t_up = j + 1
  with(conf_M[c(id, t_lw:t_up)], polygon(
    x = c(seq(8, 0, by = -2), seq(0, 8, by = 2)),
    y = c(rev(lw), up),
    col = transp[i],
    border = NA
  ))
}
legend(
  "topright",
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
