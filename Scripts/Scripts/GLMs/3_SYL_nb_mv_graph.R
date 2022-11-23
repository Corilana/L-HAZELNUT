#logigram:"how many buds?"
#AIM: graphs nb buds in sylleptic shoots
#data: DERUTA 2020
#PhD: Francesca Grisafi
source("Scripts/Modify_dataset/import_dataset.R")
source("Scripts/GLMs/3_SYL_nb_mv_analysis.R")

library(RColorBrewer)
library(plotrix)
head(met.sylleptic)

#CREATE A DF WHERE, FOR EACH LENGTH I HAVE MEAN AND SE OF NUMBER OF BUDS
nline = length(unique(sort(met.sylleptic$parent_length_cm)))
dt = met[0, 0]
#for loop to store mean and se
for (i in 1:nline) {
  L = unique(sort(met.sylleptic$parent_length_cm))[i]
  av = round(mean(met.sylleptic[met.sylleptic$parent_length_cm == L, "tot_buds_m.v"]), digits = 2)
  n = length(met.sylleptic[met.sylleptic$parent_length_cm == L, "tot_buds_m.v"])
  se = round(std.error(met.sylleptic[met.sylleptic$parent_length_cm == L, "tot_buds_m.v"]), digits =
               2)
  if (n == 1) {
    se = 0
    marg = 0
  }
  else{
    t = qt(0.975, df = n - 1)
    #confidence interval = coef * se
    marg = se * t
  }
  d = cbind(L, av, se, marg)
  dt = rbind(dt, d)
}
#CREATE A new DF IN WHICH I HAVE THE LENGHT AND DIFFERENT normal distances
df = data.frame(parent_length_cm = seq(1, max(met.sylleptic$parent_length_cm),
                                       length.out = length(unique(
                                         met.sylleptic$parent_length_cm
                                       ))))
#create 5 repetitions of sequence with random numbers between 0 and 0.5 (sequence to simulate distance)
for (i in seq(0, 0.5, by = 0.1)) {
  df = cbind(df, data.frame(rep(i, length(
    df$parent_length_cm
  ))))
}
names(df)
#rename columns
colnames(df)[-1] = "normal_distance"
#predict model according to parent length, for each type of sequence of distance (0-10)
for (i in 1:(ncol(df) - 1)) {
  h = i + 1
  df = cbind(df, predict(model, newdata = df[c(1, h)], type = "response"))
}
names(df)
#rename columns
colnames(df)[8:13] = seq(0, 0.5, by = 0.1)

#graph
png("Outputs/Plots/3_SYL_nb_buds.png",width=1200, height=900, res=150)# save plot
rbPal <- brewer.pal(n=6, name="Set1")
with(dt, plot(dt$av~dt$L,
                         pch=19,
                         xlab= "parent length(cm)",
                         ylab="vegetative + mixed buds",
              ylim=c(0,7)))
with(arrows(x0 = dt$L,                           # Add error bars
       y0 = dt$av + dt$se,
       y1 = dt$av - dt$se,
       angle = 90,
       code = 3,
       length = 0.05))
for (i in 1:length(grep("[0-9]", colnames(df)))) {
  t=grep("[0-9]", colnames(df))[i]
  with(df,lines(df[,t]~df$parent_length_cm,col=rbPal[i], lwd=3))
}
legend("topright",
       bty="n",
       xpd = TRUE,c("real", "dist=0","dis=0.1","dis=0.2","dis=0.3","dis=0.4","dis=0.5"),
       pch = c(19,NA,NA,NA,NA,NA,NA),lty=c(NA,1,1,1,1,1,1), lwd=5,
       col = c("black", rbPal),
       cex=0.6)
dev.off()

