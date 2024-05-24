#confronto tra dati simulati e dati reali:
#-distribuzione altre gemme
#2023
#francesca Grisafi
source("simulations/Rscripts/useful_functions.R")
library(plotrix);library(data.table)

# importa dataset REAL ----------------------------------------------------
real_shoot=data.frame(fread("simulations/data/real_buds_deruta.csv",stringsAsFactors = T))
head(real_shoot)
#la scala è a livello di GEMME 

# importa dataset SIMULATED come Deruta -----------------------------------
all_sim=data.frame(fread("simulations/data/parent_sampled_as_deruta.csv",stringsAsFactors = T))
head(all_sim)
#la scala è a livello di GEMME

# sistema dataset ---------------------------------------------------------
#prendi solo rami prolettici
real_shoot = droplevels(real_shoot[real_shoot$shoot_type == "PROLEPTIC",c("length", "shoot_ID","rank_node","fate")])
head(real_shoot)

all_sim = droplevels(all_sim[all_sim$fate != "S",c("length_cm", "shoot_id","rank","fate")])
head(all_sim)

# Calcola frequenze -------------------------------------------------------
freq_sim = table(all_sim$rank,all_sim$fate)
print(freq_sim)

freq_obs = table(real_shoot$rank_node,real_shoot$fate)
print(freq_obs)

#prop
rel_freq_sim = prop.table(freq_sim,1)
rel_freq_sim = rel_freq_sim[,1]
rel_freq_sim = data.frame(t(rel_freq_sim))
colnames(rel_freq_sim) = c(1:ncol(rel_freq_sim))

rel_freq_obs = prop.table(freq_obs,1)
rel_freq_obs = rel_freq_obs[,1]
rel_freq_obs = data.frame(t(rel_freq_obs))
colnames(rel_freq_obs) = c(1:ncol(rel_freq_obs))

while (ncol(rel_freq_obs)!=ncol(rel_freq_sim)){
  rel_freq_sim[ncol(rel_freq_obs)] = NA
  colnames(rel_freq_sim) = c(1:ncol(rel_freq_sim))
}

#realizza un datet unificato con REAL e SIMULATED
unique_df = rbind(rel_freq_obs,rel_freq_sim)
row.names(unique_df) = c("Observed", "Simulated")
unique_df = as.matrix(unique_df)

#dataset per prop test
freq_sim = as.data.frame.matrix(freq_sim)
freq_sim$sum = rowSums(freq_sim)
freq_sim = freq_sim[c(1,4)]

freq_obs = as.data.frame.matrix(freq_obs)
freq_obs$sum = rowSums(freq_obs)
freq_obs = freq_obs[c(1,4)]

while (nrow(freq_obs)!=nrow(freq_sim)){
  freq_sim[nrow(freq_obs),] = NA
  rownames(freq_sim) = c(1:nrow(freq_obs))
}

#print
cols=palette()[1:nrow(unique_df)]

barplot(unique_df,beside = T,col = cols,ylim = c(0,1),,ylab = "proportion",xlab = "Rank node")

for (i in 1:ncol(unique_df)){
  #test per differenze
  freq = as.data.frame.matrix(freq)
  freq$sum = rowSums(freq)
  #are there differences between class length?
  test=prop.test(freq[1:nrow(freq),i], freq[1:nrow(freq),ncol(freq)])
  if (test$p.value<=0.05) {
    lab = "*"
    print(test)
  }
  if (test$p.value<=0.01){
    lab = "**"
    print(test)
  }
  if (test$p.value<=0.001){
    lab = "***"
    print(test)
  }
  if (test$p.value>0.05) {
    lab = NA
    print(test)
  }
  text(x = mean(br[,i]),y = mean(rel_freq[,i])+0.07,labels = lab, cex = 1.5)
}
legend("topright",legend = c("Observation", "576 simulations"), fill = c("grey",cols))
