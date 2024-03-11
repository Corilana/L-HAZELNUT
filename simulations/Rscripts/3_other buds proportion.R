#confronto tra dati simulati e dati reali:
#-distribuzione altre gemme
#2023
#francesca Grisafi
source("scripts/useful_functions.R")
library(plotrix);library(data.table)

# importa dataset REAL ----------------------------------------------------
real_shoot=data.frame(fread("data/real_buds_deruta.csv",stringsAsFactors = T))
head(real_shoot)
#la scala è a livello di GEMME 

# importa dataset SIMULATED come Deruta -----------------------------------
all_sim=data.frame(fread("data/parent_sampled_as_deruta.csv",stringsAsFactors = T))
head(all_sim)
#la scala è a livello di GEMME

# sistema dataset ---------------------------------------------------------
#prendi solo rami prolettici
real_shoot = droplevels(real_shoot[real_shoot$shoot_type == "PROLEPTIC",c("length", "shoot_ID","fate")])
head(real_shoot)

all_sim = droplevels(all_sim[all_sim$fate != "S",c("length_cm", "shoot_id","fate")])
head(all_sim)

#realizza un datet unificato con REAL e SIMULATED
names(all_sim) = names(real_shoot)
all_sim$sim = "Simulated"
real_shoot$sim = "Real"

unique_df = rbind(all_sim,real_shoot)
unique_df$sim = as.factor(unique_df$sim)

# Calcola frequenze blind nodes-------------------------------------------------------
freq = table(unique_df$sim,unique_df$fate)
print(freq)

#prop and test
rel_freq = prop.table(freq,1)
rel_freq = as.matrix(rel_freq)
cols=palette()[1:nrow(rel_freq)]

#BLIND NODES
blind = rel_freq[,1]
br = barplot(height = blind, ylim = c(0,1),xaxt = 'n', beside = T, col = c(cols),ylab = "proportion")
#test per differenze
freq = as.data.frame.matrix(freq)
freq$sum = rowSums(freq)
#are there differences between class length?
test=prop.test(freq[1:nrow(freq),1], freq[1:nrow(freq),ncol(freq)])
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
text(x = mean(br[,1]),y = mean(rel_freq[,1])+0.07,labels = lab, cex = 1.5)
legend("topright",legend = c("Observation", "576 simulations"), fill = c(cols))

#V and M
other = rel_freq[,2:3]
br = barplot(height = other, ylim = c(0,1), beside = T, col = c(cols),ylab = "proportion")
#test per differenze
freq = as.data.frame.matrix(freq)
#are there differences between class length?
for (j in c(1:2)) {
  lev = c(2,3)
  i = lev[j]
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
  text(x = mean(br[,j]),y = mean(rel_freq[,i])+0.07,labels = lab, cex = 1.5)
  
}
legend("topright",legend = c("Observation", "576 simulations"), fill = c(cols))
