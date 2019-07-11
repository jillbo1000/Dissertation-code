These files did the gwas calculations on the CHPC.


f1.slurm: bash file to run the initial filter on the different GWAS datasets.

f1.conf: command lines referenced in f1.slurm to do filtering on the GWAS
datasets.

en.slurm: bash file to run elastic net, random forests, and trees on the 
gwas data.

en.conf: command lines referenced in en.slurm for elastic net, random 
forests, and tree calculations.

gwas_filter.R: runs the initial filter using the gwas3 package and outputs
a dataset with the p-values or distance correlations. 

en.R: runs the elastic net, tree, and random forest calculations. Returns a 
dataset that contains the SNPs that were selected for every option of 
elastic net, trees, and random forests along with the SNPs that are truly 
functional SNPs. 