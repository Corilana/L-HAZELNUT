#AIM: does M buds have CLUSTERS?
#data: DERUTA 2020
#PhD: Francesca Grisafi
source("Own_rooted_young/Scripts/Modify_dataset/import_dataset_OR.R")

library(RColorBrewer);library(dplyr);library(plotrix);library(plotfunctions)

mix=bud[bud$fate=="M",]

toremove <- grep("^mix$", ls(), invert = TRUE, value = TRUE)
rm(list = c(toremove, "toremove"))

mix[mix$cl>1,"cl"]=1
cluster_set=sum(mix$cl)/nrow(mix)
nut_set=sum(mix$nu)/nrow(mix)

plot(as.factor(mix$cl)~mix$length)
plot(as.factor(mix$cl)~mix$`Length(node)`)
plot(as.factor(mix$cl)~mix$rank_node)
plot(as.factor(mix$cl)~mix$median_distance)
plot(as.factor(mix$cl)~mix$abs_norm_median_distance)
plot(as.factor(mix$cl)~mix$siblings_mv)#interessante
plot(as.factor(mix$cl)~mix$shoot_type)#interessante
plot(as.factor(mix$cl)~mix$length2yo)#interessante

names(mix)
parameters = c("length","Length_node",
               "rank_node", "siblings_mv","abs_norm_median_distance","shoot_type")
str(mix$cl)#glm family binomial

names(mix)[9]="Length_node"

#model1
model = glm(cl ~ length + Length_node +
              rank_node+siblings_mv + abs_norm_median_distance+shoot_type+0,
            family = "binomial",
            data = mix
)
summary(model)
#AIC:812.35

#model2
model = glm(cl ~ length + Length_node +
              siblings_mv + abs_norm_median_distance+shoot_type+0,
            family = "binomial",
            data = mix
)
summary(model)
#AIC:810.38

#model3
model = glm(cl ~ length + Length_node +
              siblings_mv + abs_norm_median_distance,
            family = "binomial",
            data = mix
)
summary(model)
#AIC:811.81

model = glm(cl ~ length +  siblings_mv + abs_norm_median_distance +0,
            family = "binomial",
            data = mix
)
summary(model)

#FINAL MODEL
#save outputs
out=capture.output(summary(model))
# cat("clusters", out, file="Own_rooted_young/Outputs/Tables/clusters.txt", sep="\n")

#AIC:835.89

#FINAL MODEL
model = glm(cl ~ length +  siblings_mv + 0,
            family = "binomial",
            data = mix
)
summary(model)

#real data
summary(mix$siblings_mv)
nline = dim(mix)[1]
a = 0
b = 2
c = 4
d = 6
for (i in 1:nline) {
  if (mix[i, "siblings_mv"] == a) {
    mix[i, "class_siblings"] = "0"
  }
  if (mix[i, "siblings_mv"] > a &
      mix[i, "siblings_mv"] <= b) {
    mix[i, "class_siblings"] = "2"
  }
  if (mix[i, "siblings_mv"] > b &
      mix[i, "siblings_mv"] <= c) {
    mix[i, "class_siblings"] = "4"
  }
  if (mix[i, "siblings_mv"] > c &
      mix[i, "siblings_mv"] <= d) {
    mix[i, "class_siblings"] = "6"
  }
  if (mix[i, "siblings_mv"] > d) {
    mix[i, "class_siblings"] = "8"
  }
}
mix$class_siblings = as.factor(mix$class_siblings)

#mean of values with the same length
mix$class_length=round(mix$length/10)

mean.freq = mix[0, 0]
for (class_length in unique(sort(mix$class_length))) {
  df=mix[mix$class_length==class_length,]
  length = mean(df$length)
  for (j in levels(df$class_siblings)) {
    sib = j
    DF = df[df$class_siblings == j, ]
    if (nrow(DF)==0) {
      clusters=NA
      se=NA
    } else {
      clusters = mean(DF$cl)
      se = sd(DF$cl)/sqrt(length(DF$cl))
    }
    mean.freq = rbind(mean.freq, cbind(class_length,length, sib, clusters, se))
  }
}

str(mean.freq)
mean.freq[c(3)] = lapply(mean.freq[c(3)],as.factor)
mean.freq[c(1,2,4,5)]=lapply(mean.freq[c(1,2,4,5)], as.numeric)

#create a sequence with random numbers between 1 and maximum siblings
summary(mix$siblings_mv)
for (j in seq(0, 8, 2)) {
  mean.freq = cbind(mean.freq, j)
}

names(mean.freq)
#rename columns
se=grep("se",names(mean.freq))
names(mean.freq)[c((se+1):(ncol(mean.freq)))] = "siblings_mv"
conf_int = mean.freq
#predict model
len=grep("^length",names(mean.freq))

for (i in grep("siblings_mv", colnames(mean.freq))) {
  pred = predict(model,
                 newdata = mean.freq[c(len, i)],
                 type = "response",
                 se.fit = T)
  mean.freq = cbind(mean.freq, pred$fit)
}
#rename columns
se=last(grep("mv$",names(mean.freq)))
names(mean.freq)[c((se+1):(ncol(mean.freq)))] = seq(0, 8, 2)

#confidence intervel
for (i in grep("siblings_mv", colnames(mean.freq))) {
  pred = predict(model,
                 newdata = mean.freq[c(len, i)],
                 type = "response",
                 se.fit = T)
  lw = pred$fit + qnorm(0.025) * pred$se.fit
  up = pred$fit + qnorm(0.975) * pred$se.fit
  lim = as.data.frame(cbind(lw, up))
  conf_int = cbind(conf_int, lim)
}

# graph
# png("Own_rooted_young/Outputs/Plots/7_clusters.png",width=1200, height=900, res=150)# save plot
rbPal <- brewer.pal(n=6, name="Set1")
transp<-alphaPalette(rbPal, rep(0.25,6))
par(mfrow=c(1,1))
par(mar=c(5,4,1,0)+0.1)
#real data
with(mean.freq,plot(clusters~length,
                    ylab = "cluster set (cluster/mixed buds",
                    xlab = "parent length(cm)",
                    ylim=c(0,1),
                    pch=20,cex=2,
                    xlim=c(0,72)))
for (i in 0:max(mean.freq$class_length)) {
  plotCI(
    mean.freq[mean.freq$class_length==i,"length"],
    mean.freq[mean.freq$class_length==i,"clusters"],
    li = mean.freq[mean.freq$class_length==i,"clusters"] - mean.freq[mean.freq$class_length==i,"se"],
    ui = mean.freq[mean.freq$class_length==i,"clusters"] + mean.freq[mean.freq$class_length==i,"se"],
    add = T,
    sfrac=0.01,
    lwd=1.5,
    pch=NA
  )
}
for (i in 1:length((mean.freq)[grep("0|2|4|6|8",names(mean.freq))])) {
  t=grep("0|2|4|6|8",names(mean.freq))[i]
  with(mean.freq,lines(mean.freq[,t]~mean.freq$length,col=rbPal[i], lwd=3))
}
for (i in 1:length(grep("lw", colnames(conf_int)))) {
  j=grep("lw", colnames(conf_int))[i]
  t_lw=j
  t_up=j+1
  with(conf_int[c(1,t_lw:t_up)], polygon(x=c(rev(conf_int$length),conf_int$length),
                                         y=c(rev(lw),up),
                                         col=transp[i], border = NA))
}
legend("topleft",
       horiz=T,
       title="number of sibling buds",
       xpd = TRUE,
       legend = c(seq(0,8,2)),
       fill = rbPal[1:6],
       bty="n",
       cex=1)
dev.off()

