# Functions to open and convert data sets

import os
import pandas as pd

def csv_to_sequences(seqfile):
    """
    Conversion from csv file to list that can be transformed into 
    openalea.sequence_analysis.sequences.Sequences
    
    :Parameters:
        * `seqfile` (str): name and path of csv file to convert

    :Returns:
        python list l with the structure following structure: 
            l[s][i][v] where s in the index of the sequence, 
            i the index within the sequence (say, a metamer) and
            v is a variable (e.g., length, number of catkins, ...)
    """

    csvf = pd.read_csv(seqfile, sep="\t")

    # Replace decimal separator "," -> "."
    csvf['length1yo'] = csvf['length1yo'].str.replace('[A-Za-z]', '').str.replace(',', '.').astype(float)

    csvf_select = csvf[["length","shoot1yo","a","l","m","c"]]

    indices = set(csvf_select['shoot1yo']) # indices of sequences

    # Numbering classes of length     
    length_val = ['VLo', 'Me', 'Sh', 'Lo']
    assert(set(csvf_select['length']) == set(length_val))

    length_to_num = dict(zip(['Sh', 'Me', 'Lo', 'VLo'], range(4)))

    # Could somehow be improved not to produce warnings.
    for k in length_val:
        v = length_to_num[k]
        csvf_select['length'] = csvf_select['length'].str.replace(k, str(v))

    # Conversion of length classes from string to int
    csvf_select['length'] = csvf_select['length'].astype(int)

    pyseq = []
    for i in indices:
        pyseq += [csvf_select[csvf_select['shoot1yo'] == i].get_values().tolist()]
        
    return(pyseq)
    