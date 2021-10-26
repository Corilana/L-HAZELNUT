# TODO: complete

# Import data 
import os

from CsvToSeq import csv_to_sequences
from openalea.sequence_analysis import sequences

print(os.getcwd())

# Adapt to reflect your own path, commenting the lines below but not deleting them.
virtual = True
if not(virtual):
    home_dir = "D:\\"
else:
    home_dir = "/media/sf_transfert"
base_path = os.path.join(home_dir, "devlp_shared", "HazelnutFSPM")
data_path = os.path.join(base_path, "Dataframes")
                         
seqfile = data_path + os.sep + "DATASET_multiplebuds.csv"

   
pylist = csv_to_sequences(seqfile)

# Variables: "length" (constant per sequence), 
# "shoot1yo" (constant per sequence), "a", "l", "m", "c"
pyseq = sequences.Sequences(pylist)    

