# AppleCultivarsTreatments: environment

This directory contains the instructions that would allow for reproducing our results using a shared environment: singularity image, conda environment.

 * conda_base_no-builds.yml: conda environment with --no-build exports (platform independent?). Restricted to be used with R (excluding VPlants part)
 * conda_base.yml: specific file for conda environment (ubuntu 18.04.5). Restricted to be used with R (excluding VPlants part)
 * VPlants.def: singularity recipe

NB. To create a conda .yml from an existing environment, use *conda env export > conda_base.yml*
 
 
 To use the conda .yml files to create a conda environment, use *conda env create -f conda_base.yml* or *conda env create -f conda_base_no-builds.yml* 
 The first line of the .yml file sets the new environment's name.
 
## Remarks
* Pomegranate for HMMs requires joblib 0.17.0 (or lower).
