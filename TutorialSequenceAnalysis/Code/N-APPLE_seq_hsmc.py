#########################################################################
#
#  N-APPLE shoot
#
#  VARIABLE 1 : treatment code (1: control, 2: treatment with 20 g N/tree dose, 3: treatment with 30 g N/tree dose),
#  VARIABLE 2 : cultivar (1: Rubinola, 2: Topaz, 3: Golden Delicious),
#  VARIABLE 3 : axillary shoot type (0: no shoot, 1: short shoot, 2: medium shoot, 3: long shoot, 4: sylleptic shoot),
#  VARIABLE 4 : lateral flowering (0: no lateral flowering, 1: LF in median zone, 2: LF in distal zone, 3: LF in median and distal zone = Long LF zone),
#  VARIABLE 5 : terminal flowering (0: no flower cluster, 1: presence of a flower cluster).
#
#  Data : Martin Meszaros, Evelyne Costes, Jean-Baptiste Durand
#
#########################################################################

from openalea.aml import *

# N-APPLE

import os
import numpy as np

from Utils import *
from amlseq2R import *

#os.getcwd() returns the current working directory of a process
print(os.getcwd())

# Adapt to reflect your own path, commenting the lines below but not deleting them.
virtual = True
if not(virtual):
    home_dir = "D:\\"
else:
    home_dir = "C:\\Users\\franc\\Desktop\\MtpData"
base_path = os.path.join(home_dir, "HazelnutFSPM", "TutorialSequenceAnalysis")
data_path = base_path

#legge la sequenza
seq1 = Sequences(data_path + os.sep + "multiseq_N_APPLE_unix.seq")

# Python sequence
from openalea.sequence_analysis import sequences
pyseq1 = sequences.Sequences(data_path + os.sep + "multiseq_N_APPLE_unix.seq")

# Remove treatment and cultivar variable since we do not want to model their stochastic behaviour
pyseq1 = pyseq1.select_variable([3,4,5], True) 

# To display data
Display(SelectVariable(ValueSelect(seq1, 2, 1), [1, 3, 4, 5]), ViewPoint="Data", Format="Line")

# To plot the probability of variable 3 (actually the first meaningful variable) over index
Plot(seq1, 'Intensity', 3)

#############################################################
# Important: press space bar to rotate view, 
# press q to quit or the program will stall
#############################################################

# axillary shoot type
# (0: no shoot, 1: short shoot, 2: medium shoot, 3: long shoot, 4: sylleptic shoot)
# Zone 0: mostly 0/4 (no shoot, sylleptic shoot)
# Zone 1: mostly 1 (short shoot)
# Zone 2: mostly 0, a few 1-3 (no shoot, short shoot - long shoot)
# Zone 3: everything but 4 (sylleptic), mostly 2 (medium shoots) maybe
# Zone 4: mostly 3 (short shoots)

Plot(seq1, 'Intensity', 4)
# lateral flowering
# (0: no lateral flowering, 1: LF in median zone, 2: LF in distal zone, 3: LF in median and distal zone = Long LF zone),
# Zone 0': mostly 0 (no shoot lateral flowering)
# 1-3 come gradually after that, 1 (LF in median zone) remains minoritary

Plot(seq1, 'Intensity', 5)
# terminal flowering 
# (0: no flower cluster, 1: presence of a flower cluster).
# Zone 0'': majority 0
# Zone 1'': majority 1

# Make one Sequences object per cultivar and discard cultivar variable
seq11 = SelectVariable(ValueSelect(seq1, 2, 1), [1, 3, 4, 5]) # 38 sequences
seq12 = SelectVariable(ValueSelect(seq1, 2, 2), [1, 3, 4, 5]) # 40 sequences
seq13 = SelectVariable(ValueSelect(seq1, 2, 3), [1, 3, 4, 5]) # 40 sequences

def seq_len(o):
    """
    Length of sequence
    """
    s = str(o)
    pc = s.find(":") # position of ":"
    ps = s.find("sequences")
    return(int(s[pc+1:ps-1]))
    
c_index = np.array([seq_len(seq11), seq_len(seq11) + seq_len(seq12), seq_len(seq11) + seq_len(seq12) + seq_len(seq13)])

Plot(seq11, 'Intensity', 2)
# Zones are roughly as in global. There seems to be less 1 (short S) in Zone 1 and more in Zone 3
# with less 2 (medium S) in Zone 3. Also more 3 (long S) in zone 4 and reduced or missing Zone 4? 
# Value 4 (sylleptic S) does not appear in Zone 0 anymore (nor anywhere else).
Plot(seq11, 'Intensity', 3)
# Similar to global. Value 2 (LF in distal zone) becomes more probable in zone 1'.
Plot(seq11, 'Intensity', 4)
# Similar to global. Value 1 (flower cluster) becomes more probable in zone 1'' while value 0 becomes less probable.

Plot(seq12, 'Intensity', 2)
# Roughly same pattern as in global. Zone 0 may contain less 4 (sylleptic S). Zone 3 seems reduced or missing.
Plot(seq12, 'Intensity', 3)
# 0 (no lateral Fl.) remains ultramajority almost everywhere but the very end of the sequence. 
# Slightly less 2 (LF in distal) and more 3 (LF in medial and distal) in Zone 1'.Plot(seq12, 'Intensity', 4)
Plot(seq12, 'Intensity', 4)
# Long-range alternation of 0 and 1

Plot(seq13, 'Intensity', 2)
# Much more 4 (sylleptics) than average in Zone 0. 
# Zone 3 has much more 2 (medium S) than average. 
# Zone 4 may not exist or has minoritary 4 (long S).
Plot(seq13, 'Intensity', 3)
# Value 0 (no lateral flowering) remains ultramajority almost everywhere but the very end of the sequence.
Plot(seq13, 'Intensity', 4)
# Value 0 (no fl. cluster) remains ultramajority almost everywhere but the very end of the sequence.

# Make one Sequences object per treatment x cultivar and discard treatment variable
seq111 = SelectVariable(ValueSelect(seq11, 1, 1), [2, 3, 4])
seq112 = SelectVariable(ValueSelect(seq11, 1, 2), [2, 3, 4])
seq113 = SelectVariable(ValueSelect(seq11, 1, 3), [2, 3, 4])
seq121 = SelectVariable(ValueSelect(seq12, 1, 1), [2, 3, 4])
seq122 = SelectVariable(ValueSelect(seq12, 1, 2), [2, 3, 4])
seq123 = SelectVariable(ValueSelect(seq12, 1, 3), [2, 3, 4])
seq131 = SelectVariable(ValueSelect(seq13, 1, 1), [2, 3, 4])
seq132 = SelectVariable(ValueSelect(seq13, 1, 2), [2, 3, 4])
seq133 = SelectVariable(ValueSelect(seq13, 1, 3), [2, 3, 4])
# Plot(seq111, "Intensity", 1)
# Plot(seq111, "Intensity", 2)

# Discard cultivar and treatment variables for HSMC analysis
seq1v = SelectVariable(seq1, [3, 4, 5])

# Histogram of seq lengths
Plot(ExtractHistogram(seq1, "Length"))


##########################################
# Estimate hidden semi-Markov chain model
##########################################

# trivariate hidden semi-Markov chain 
nb_states = 5 # number of states, you have to try several values and compare BIC / ICL values
hsmc1 = Estimate(seq1v, "HIDDEN_SEMI-MARKOV", "Ordinary", nb_states, "LeftRight")

# BIC  -18349.1, ICL  -18764.3

Plot(hsmc1, "Intensity", 1)
# Display(hsmc1)
 
output_model_file = "seq1v_" + str(nb_states) + "s_LR.hsmc"
Save(hsmc1, output_model_file)

Plot(hsmc1, "Observation", 1) # Use directly gnuplot on file 10.plot, 20.plot, 30.plot etc. if not working

seg = ExtractData(hsmc1) # Data and segmentation

seg_file = "seq1v_" + str(nb_states) + "s_LR_segm.seq"
from openalea.sequence_analysis import *
import openalea.aml
openalea.aml.Save(seg, seg_file, ViewPoint="Data")
segpy = Sequences(seg_file)
seg_pyseq, _ = ReadSequence(".", seg_file)
WriteRSequence(seg_pyseq, seg_file[0:-4] + ".csv", RestoredStatesHeader())
