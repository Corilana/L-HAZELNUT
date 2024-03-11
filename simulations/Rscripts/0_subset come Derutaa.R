#li subsetto in modeo da ottenere datase con solo 26 rami sh, 25me, 28lo, 25 vlo
#in modo che sia uguale a quello di Deruta
#dopodiche unisco in un unico file excell tutti i dataset ottenuti
#autore: FG

library(data.table)

#lista i file che avevano almeno 26 rami sh, 25me, 28lo, 25 vlo (SUCCESS)
ls=list.files("outputs/simulations/merged/")
succ_ls=grep("_success",ls,value = T)

#importa tutti i csv dei germogli parentali success 
results=data.frame(fread(paste0(getwd(),"/outputs/simulations/merged/", grep("parent", succ_ls, value = T))))
#Crea categorie di lunghezza
cats = c("Sh", "Me", "Lo", "VLo")
results$cat = factor(results$cat, levels = cats)
results$new_cat = factor(results$new_cat, levels = cats)

#importa tutti i csv dei gergmoli figli success
bud_seq=data.frame(fread(paste0(getwd(),"/outputs/simulations/merged/", grep("child", succ_ls, value = T))))
bud_seq$cat = factor(bud_seq$cat, levels = cats)
bud_seq$new_cat = factor(bud_seq$new_cat, levels = cats)

#create random sample "quantity" number for each class length and merge it into a new dataset
quantity=c(26,25,28,25)
nb_Sh= quantity[1]
nb_Me= quantity[2]
nb_Lo= quantity[3]
nb_VLo= quantity[4]
simulated_data=data.frame()
simulated_buds=data.frame()

nb_sim = unique(sort(results$simulation_nb))
for (s in nb_sim) {
  dataset = results[results$simulation_nb==s,]
  data = unique((dataset[,c("shoot_id", "cat") ]))
  b_data = droplevels(bud_seq[bud_seq$simulation_nb==s, ])
  id_sh=unique(data[data$cat=="Sh","shoot_id"])
  if (length(id_sh)>=nb_Sh){
    ics_sh=sample(id_sh,nb_Sh)
    id_Me=unique(data[data$cat=="Me","shoot_id"])
    if (length(id_Me)>=nb_Me){
      ics_me=sample(id_Me,nb_Me)
      id_Lo=unique(data[data$cat=="Lo","shoot_id"])
      if (length(id_Lo)>=nb_Lo){
        ics_lo=sample(id_Lo,nb_Lo)
        id_VLo=unique(data[data$cat=="VLo","shoot_id"])
        if (length(id_VLo)>=nb_VLo){
          ics_vlo=sample(id_VLo,nb_VLo)
          ics = append(ics_sh,ics_me)
          ics = append (ics,ics_lo)
          ics = append(ics,ics_vlo)
          new= droplevels(dataset[dataset$shoot_id%in%ics,])
          ics_bud = unique(sort(new$bud_id))
          new_bud = droplevels(b_data[b_data$shoot_id%in%ics_bud,])
          simulated_data=rbind(simulated_data,new)
          simulated_buds = rbind(simulated_buds,new_bud) 
        }
      }
    }
  }
}

# #salvo in un altro file csv
fwrite(simulated_data, "data/parent_sampled_as_deruta.csv",col.names = T)
fwrite(simulated_buds, "data/child_sampled_as_deruta.csv",col.names = T)

