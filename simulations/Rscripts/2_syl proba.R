#proporzione nodi con rami sillettici
#2023
#francesca Grisafi
source("scripts/useful_functions.R")

library(plotrix);library(data.table)

# importa dataset REAL ---------------------------------------------------------
real_shoot=data.frame(fread("data/real_buds_deruta.csv",stringsAsFactors = T))
head(real_shoot)
#la scala è a livello di GEMME dobbiamo portarla a livello NODO
real_shoot = unique(real_shoot[c("length", "shoot_ID","rank_node","shoot_type")])
real_shoot$shoot_type=factor(real_shoot$shoot_type, levels = c("PROLEPTIC","SYLLEPTIC")) 

# importa dataset SIMULATED come Deruta -----------------------------------
all_sim=data.frame(fread("data/parent_sampled_as_deruta.csv",stringsAsFactors = T))
head(all_sim)
#la scala è a livello di GEMME dobbiamo portarla a livello NODO
#inseriamo la tipologia di gergmolio da cui deriva la gemma
all_sim$shoot_type = NA
all_sim[all_sim$fate!="S","shoot_type"]="PROLEPTIC"
all_sim[all_sim$fate=="S","shoot_type"]="SYLLEPTIC"
all_sim$shoot_type=factor(all_sim$shoot_type, levels = c("PROLEPTIC","SYLLEPTIC"))
#unifichiamo per rango nodo
all_sim = unique(all_sim[c("length_cm", "shoot_id","rank","shoot_type")])
head(all_sim)

# calcolo frequenze -------------------------------------------------------
names(all_sim) = names(real_shoot)
all_sim$sim = "Simulated"
real_shoot$sim = "Real"

unique_df = rbind(all_sim,real_shoot)
unique_df$sim = as.factor(unique_df$sim)

n = length(all_sim$shoot_ID)
freq = table(unique_df$sim,unique_df$shoot_type)
print(freq)

#prop and test
rel_freq = prop.table(freq,1)
rel_freq = as.matrix(rel_freq)
cols=palette()[1:nrow(rel_freq)]

syl = rel_freq[,2]
br = barplot(height = syl, ylim = c(0,1),xaxt = 'n', beside = T, col = c(cols),ylab = "proportion")
#test per differenze
freq = as.data.frame.matrix(freq)
freq$sum = rowSums(freq)
#are there differences between class length?
test=prop.test(freq[1:nrow(freq),2], freq[1:nrow(freq),ncol(freq)])
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
text(x = mean(br[,1]),y = mean(rel_freq[,2])+0.17,labels = lab, cex = 1.5)
legend("topright",legend = c("Observation", "576 simulations"), fill = c(cols))



