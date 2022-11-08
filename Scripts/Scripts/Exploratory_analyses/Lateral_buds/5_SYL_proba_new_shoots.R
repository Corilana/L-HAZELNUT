#AIM: is the bud sprouting in sylleptic shoots?
#data:deruta 2020
#PhD: Francesca Grisafi
source("Scripts/Modify_dataset/import_dataset.R")
#visualize plot
names(MV.bud.SYL)
MV.bud.SYL$presence_new_shoots=factor(MV.bud.SYL$presence_new_shoots)
plot(presence_new_shoots~parent_length_cm,MV.bud.SYL)
plot(presence_new_shoots~parent_rank_node,MV.bud.SYL)
plot(presence_new_shoots~normal_distance,MV.bud.SYL)
plot(presence_new_shoots~distance_abs,MV.bud.SYL)
plot(presence_new_shoots~median_distance,MV.bud.SYL)
plot(presence_new_shoots~median_distance_norm,MV.bud.SYL)
plot(presence_new_shoots~fate,MV.bud.SYL)
plot(presence_new_shoots~m,MV.bud.SYL)
plot(presence_new_shoots~v,MV.bud.SYL)
#maybe the number of other buds or fate or rank