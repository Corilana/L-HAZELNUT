#AIM: how many m,v,b are per node in proleptic shoots
#data:deruta 2020
#PhD: Francesca Grisafi
source("Own_rooted_young/Scripts/Modify_dataset/import_dataset_OR.R")

# AIM1: NB of observations (buds(mvb)) in proleptic 1yo shoots 
plot(met.proleptic$tot_buds_mvb~met.proleptic$length,pch=19)
plot(met.proleptic$tot_buds_mvb~met.proleptic$rank_node,pch=19)
plot(met.proleptic$tot_buds_mvb~met.proleptic$`Length(node)`,pch=19)
plot(met.proleptic$tot_buds_mvb~met.proleptic$abs_median_distance,pch=19)
plot(met.proleptic$tot_buds_mvb~met.proleptic$abs_norm_median_distance,pch=19)
plot(met.proleptic$tot_buds_mvb~met.proleptic$median_distance,pch=19)
plot(met.proleptic$tot_buds_mvb~met.proleptic$norm_median_distance,pch=19)
#seems not to be correlated

