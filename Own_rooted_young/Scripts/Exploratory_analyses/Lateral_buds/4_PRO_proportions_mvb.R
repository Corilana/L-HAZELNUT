#AIM: Proportion of buds (m+b+v) in proleptic 1yo shoots
#data:deruta 2020
#PhD: Francesca Grisafi
source("Own_rooted_young/Scripts/Modify_dataset/import_dataset_OR.R")

#visualize plot
plot(bud.proleptic$fate~bud.proleptic$length)
plot(bud.proleptic$fate~bud.proleptic$rank_node)
plot(bud.proleptic$fate~bud.proleptic$`Length(node)`)
#seems that rank influences the fate

