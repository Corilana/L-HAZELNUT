#confronto numero gemme (B,M,V e syl) in rami parentali prolettici
#2023
#francesca Grisafi
source("scripts/useful_functions.R")

library(plotrix);library(data.table)

# importa dataset REAL ---------------------------------------------------
real_shoot=data.frame(fread("data/real_buds_deruta.csv",stringsAsFactors = T))
head(real_shoot)
levels(real_shoot$fate)[levels(real_shoot$fate)=="C"] = "S"
#la scala è a livello di GEMME 
#rimuoviamo le gemme M e V che sono nel syllettico
real_shoot = real_shoot[!(real_shoot$shoot_type=="SYLLEPTIC" & real_shoot$fate!="S"),]
str(real_shoot$fate)

# importa dataset SIMULATED come Deruta -----------------------------------
all_sim=data.frame(fread("data/parent_sampled_as_deruta.csv",stringsAsFactors = T))
head(all_sim)
str(all_sim$fate)
#la scala è a livello di GEMME
all_sim$fate = factor(all_sim$fate, levels = levels(real_shoot$fate))

# contiamo gemme ----------------------------------------------------------
buds_nb_sim = data.frame()

nline = length(unique(all_sim$simulation_nb))
for (i in 1:nline) {
    sim = unique(all_sim$simulation_nb)[i]
    a = table(all_sim[all_sim$simulation_nb==sim,"fate"])
    tab = t(data.frame(a,row.names = T))
    buds_nb_sim = rbind(buds_nb_sim, tab)
}
buds_nb_sim = data.frame(buds_nb_sim)
col_s = grep("S", colnames(buds_nb_sim))
buds_nb_sim = buds_nb_sim[-(col_s)]

#fai la media del numero di gemme DATI SIMULATI
av_buds_sim =  data.frame(round(colMeans(buds_nb_sim),2))
colnames(av_buds_sim) = "av"
av_buds_sim$sd = round(sapply(buds_nb_sim, sd),2)
av_buds_sim$n = nrow(buds_nb_sim)
av_buds_sim$se  = round(av_buds_sim$sd/sqrt(av_buds_sim$n),2)
av_buds_sim$bud_type = factor(rownames(av_buds_sim), levels= c(as.character(rownames(av_buds_sim))))

print(av_buds_sim)

#somma del numero di gemme DATI REALI
buds_nb_real = t(data.frame(table(real_shoot$fate),row.names = T))
buds_nb_real = data.frame(buds_nb_real)
rownames(buds_nb_real) = "real"
col_s = grep("S", colnames(buds_nb_real))
buds_nb_real = buds_nb_real[-(col_s)]
#prendendo spunto da V_Mango, Figura 7C faccio un grafico

#media spmma numero gemme SIMULATI
bp=barplot(av_buds_sim$av, names.arg = av_buds_sim$bud_type,ylim = c(0,600),ylab = "number of observations (sum)")
#barplot con distribuzione numero di gemme SIMULATI
for (i in c(1:ncol(buds_nb_sim))) {  boxplot(buds_nb_sim[i],at = bp[i], add = T,col = NA,border = "black") }
#punta somma numero di gemme DATI REALI
points.default(x = bp,y = buds_nb_real[c(1:ncol(buds_nb_real))], xlab = NA, ylim = c(0,500), pch=19, col="red")
legend("topleft", legend = c("real","simulated"), pch=c(16,15), col=c("red","grey"))
