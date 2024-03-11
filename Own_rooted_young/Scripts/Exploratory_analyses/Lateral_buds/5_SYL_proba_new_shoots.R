#AIM: is the bud sprouting in sylleptic shoots?
#data:deruta 2020
#PhD: Francesca Grisafi
source("Own_rooted_young/Scripts/Modify_dataset/import_dataset_OR.R")
#visualize plot

names(MV.bud.SYL)
MV.bud.SYL$nb_new_shoots=factor(MV.bud.SYL$nb_new_shoots)
plot(nb_new_shoots~parent_length_cm,MV.bud.SYL)
plot(nb_new_shoots~parent_rank_node,MV.bud.SYL)
plot(nb_new_shoots~abs_norm_median_distance,MV.bud.SYL)
plot(nb_new_shoots~abs_median_distance,MV.bud.SYL)
plot(nb_new_shoots~median_distance,MV.bud.SYL)
plot(nb_new_shoots~norm_median_distance,MV.bud.SYL)
plot(nb_new_shoots~fate,MV.bud.SYL)
plot(nb_new_shoots~m,MV.bud.SYL)
plot(nb_new_shoots~v,MV.bud.SYL)
#maybe the number of other buds or fate or rank