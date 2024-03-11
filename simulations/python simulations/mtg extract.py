from openalea.lpy import *
from openalea.mtg.io import lpy2mtg, mtg2lpy, axialtree2mtg, mtg2axialtree
import os

os.chdir("C://Users//franc//OneDrive - Università Cattolica del Sacro Cuore//Ricerca//Progetti//L-HAZELNUT//lpy_models//Simulations")

l = Lsystem('L-HAZELNUT_simulations6.lpy', {'SEED': 0})#read lsystem
lstring = l.derive()#lstring
axialtree = l.iterate()#crea l'axial tree
scene = l.sceneInterpretation(axialtree)#scena

#salva la scena
#scene.save("C://Users//franc//OneDrive - Università Cattolica del Sacro Cuore//Ricerca//Progetti//L-HAZELNUT//lpy_models//Simulations//lhazelnut.obj")

#metti dei parametri
scales = {'ProlBud':2, 'SylBud':2,'Internode':1, 'Nut':3,'Catkin':3,'Leaf':2 }
mtg1 = axialtree2mtg(axialtree, scales, scene, None)
mtg2 = lpy2mtg(axialtree, l)

