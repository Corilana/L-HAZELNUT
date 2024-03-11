import os
import pandas as pd
import time

#compute success and insuccess
file_path = "C:\\Users\\franc\\OneDrive - Università Cattolica del Sacro Cuore\\Ricerca\\Progetti\\L-HAZELNUT\\\outputs\\simulations"
all_files = os.listdir(file_path)
csv_files = list(filter(lambda f: f.endswith('.csv'), all_files))
child = list(filter(lambda f: f.startswith('child'), csv_files))
parent = list(filter(lambda f: f.startswith('parent'), csv_files))

specific_word = "_success"
ch_suc = [item for item in child if specific_word in item]
new_ch_succ = []
for line in ch_suc:
    time_start = time.perf_counter()
    a = pd.read_csv(os.path.join(file_path, line))
    new_ch_succ.append(a)
    time_elapsed = (time.perf_counter() - time_start)
    print("merging child success:", line, " : %6.4f secs " % (time_elapsed))
ch_succ = pd.concat(new_ch_succ)

par_suc = [item for item in parent if specific_word in item]
new_par_succ = []
for line in par_suc:
    time_start = time.perf_counter()
    a = pd.read_csv(os.path.join(file_path, line))
    new_par_succ.append(a)
    time_elapsed = (time.perf_counter() - time_start)
    print("merging parent success:", line, " : %6.4f secs " % (time_elapsed))
par_succ = pd.concat(new_par_succ)

specific_word = "_insuccess"
ch_ins = [item for item in child if specific_word in item]
new_ch_insucc = []
for line in ch_ins:
    time_start = time.perf_counter()
    a = pd.read_csv(os.path.join(file_path, line))
    new_ch_insucc.append(a)
    time_elapsed = (time.perf_counter() - time_start)
    print("merging child insuccess:", line, " : %6.4f secs " % (time_elapsed))
ch_insucc = pd.concat(new_ch_insucc)

par_ins = [item for item in parent if specific_word in item]
new_par_insucc = []
for line in par_ins:
    time_start = time.perf_counter()
    a = pd.read_csv(os.path.join(file_path, line))
    new_par_insucc.append(a)
    time_elapsed = (time.perf_counter() - time_start)
    print("merging parent insuccess:", line, " : %6.4f secs " % (time_elapsed))
par_insucc = pd.concat(new_par_insucc)

print("nb success is: ", len(ch_suc), "out of ", len(child), "simulations")

#merge all
ch_succ.to_csv(os.path.join(file_path, "merged","child_success.csv"))
ch_insucc.to_csv(os.path.join(file_path, "merged","child_insuccess.csv"))
par_succ.to_csv(os.path.join(file_path, "merged","parent_success.csv"))
par_insucc.to_csv(os.path.join(file_path, "merged","parent_insuccess.csv"))
