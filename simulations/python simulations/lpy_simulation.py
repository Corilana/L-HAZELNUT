import warnings
from openalea.lpy import *
import os
import time
from pandas import *

#funzione per far correre il modello su lpy
#output: elenco parametri delle gemme che si svilupperanno nel 2021 (children)
#output: elenco delle sequenze di gemme nei rami del 2020 (parent)
def simulate(seed = 0):
    warnings.simplefilter(action='ignore', category=FutureWarning)
    #seed counts the simulations
    os.chdir("C://Users//franc//OneDrive - Università Cattolica del Sacro Cuore//Ricerca//Progetti//L-HAZELNUT//lpy_models//Simulations")
    #faccio correre il modello su lpy
    l = Lsystem('L-HAZELNUT_simulations6.lpy', {'SEED':seed})
    lstring = l.derive()
    #sequenza gemme
    buds_list = l.prol_buds

    return buds_list

def nbsimulate(start_seed=0, nb=1):
    category = ["Sh", "Me", "Lo", "VLo"]
    quantity = [26, 25, 28, 25]
    # success= shoots that have at least 26sh shoots, 25me, 28lo, 25Vlo
    success = 0
    insuccess = 0

    for i in range(start_seed,start_seed+nb):
        # count simulations and class
        time_start = time.perf_counter()

        nb_class = 0
        buds_list = simulate(i)
        buds_list["simulation_nb"] = i

        par = buds_list.loc[buds_list.year==2020,]
        par_cat = par.loc[:,["shoot_id","cat"]]
        par_cat = par_cat.drop_duplicates()
        freq_class = array(par_cat['cat'].value_counts().reindex(["Sh", "Me", "Lo", "VLo"]))
        cat = DataFrame({'class_length': category, 'frequence': freq_class})
        cat['class_length'] = Categorical(cat['class_length'], categories=["Sh", "Me", "Lo", "VLo"],ordered=True)

        chi = buds_list.loc[buds_list.year==2021,]

        for j in range(len(category)):
            typo_class = category[j]
            freq_class = quantity[j]

            subset_df = cat.loc[cat['class_length'] == typo_class]
            categ = subset_df['class_length']
            freq_categ = int(subset_df['frequence'])

            if len(categ) != 0 and freq_categ >= freq_class:
                nb_class += 1
            else:
                nb_class += 0

        if nb_class == 4:
            path = "C://Users//franc//OneDrive - Università Cattolica del Sacro Cuore//Ricerca//Progetti//L-HAZELNUT//outputs//simulations//"
            success += 1
            file_name = path + 'parent_success_' + str(i).zfill(4) + '.csv'
            par.to_csv(file_name, mode="w", index=False, header=not os.path.exists(file_name))

            file_name = path + 'child_success_' + str(i).zfill(4) + '.csv'
            chi.to_csv(file_name, mode="w", index=False, header=not os.path.exists(file_name))

            time_elapsed = (time.perf_counter() - time_start)
            print("simulation nb:", i, ": success", "%5.1f secs " % (time_elapsed))

        else:
            path = "C://Users//franc//OneDrive - Università Cattolica del Sacro Cuore//Ricerca//Progetti//L-HAZELNUT//outputs//simulations//"
            insuccess += 1
            file_name = path + 'parent_insuccess_' + str(i).zfill(4) + '.csv'
            par.to_csv(file_name, mode="w", index=False, header=not os.path.exists(file_name))

            file_name = path + 'child_insuccess_' + str(i).zfill(4) + '.csv'
            chi.to_csv(file_name, mode="w", index=False, header=not os.path.exists(file_name))

            time_elapsed = (time.perf_counter() - time_start)
            print("simulation nb:", i, ": insuccess", "%5.1f secs " % (time_elapsed))

    print("nb of success is: ", success, "out of ", nb, "trials")
    print("nb of insuccess is: ", insuccess, "out of ", nb, "trials")