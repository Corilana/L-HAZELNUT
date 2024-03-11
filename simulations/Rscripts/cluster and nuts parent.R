#confronto tra dati simulati e dati reali su variabili globali
#numero di nocciole e clusters
#numero gemme
#lunghezza nuovi germogli
#2023
#francesca Grisafi
library(plotrix);library(data.table)

real_shoot=data.frame(fread("data/real_annual_shoot_deruta.csv",stringsAsFactors = T))
#dataset where, for each simulation, without sampling 26sh, 25me, 28lo,25vlo shoots 
all_sim=data.frame(fread("data/parent_sampled_as_deruta.csv",stringsAsFactors = T))

#fai la somma, per ogni simulazione, del numero di gemme DATI SIMULATED
buds_nb_sim = data.frame()
new_df = data.frame()
nb_sim = sort(unique(all_sim$simulation_nb))

for (sim in nb_sim) {
  df = droplevels(all_sim[all_sim$simulation_nb==sim,c("simulation_nb","clusters","nuts")])
  buds = data.frame(t(colSums(df[c("clusters","nuts")],na.rm = T)))
  new_df = cbind(data.frame(sim),buds)
  buds_nb_sim = rbind(buds_nb_sim,new_df)
}

print(buds_nb_sim)
#fai la media del numero di gemme DATI SIMULATI
av_buds_sim =  data.frame(round(colMeans(buds_nb_sim[-c(1)]),2))
colnames(av_buds_sim) = "av"
av_buds_sim$sd = round(sapply(buds_nb_sim[-c(1)], sd),2)
av_buds_sim$n = length(buds_nb_sim$sim)
av_buds_sim$se  = round(av_buds_sim$sd/sqrt(av_buds_sim$n),2)
av_buds_sim$bud_type = factor(colnames(buds_nb_sim)[-c(1)], levels= c(as.character(colnames(buds_nb_sim)[-c(1)])))

print(av_buds_sim)
#somma del numero di gemme DATI REALI
buds_nb_real = data.frame(round(colSums(real_shoot[c("cl","nu")],na.rm = T),2))
colnames(buds_nb_real) = "sum"
buds_nb_real$bud_type = factor(colnames(buds_nb_sim)[-c(1)], levels= c(as.character(colnames(buds_nb_sim)[-c(1)])))

print(buds_nb_real)
#prendendo spunto da V_Mango, Figura 7C faccio un grafico

#media spmma numero gemme SIMULATI
bp=barplot(av_buds_sim$av[c(1:2)], names.arg = av_buds_sim$bud_type[c(1:2)],ylim = c(0,1000),ylab = "number of nuts and clusters (sum)")
#barplot con distribuzione numero di gemme SIMULATI
for (i in c(1:2)) {  boxplot(buds_nb_sim[i+1],at = bp[i], add = T,col = NA,border = "black") }
#punta somma numero di gemme DATI REALI
points.default(x = bp,y = buds_nb_real$sum[c(1:2)], xlab = NA, ylim = c(0,1000), pch=19, col="red")
legend("topleft", legend = c("real","simulated"), pch=c(16,15), col=c("red","grey"))
