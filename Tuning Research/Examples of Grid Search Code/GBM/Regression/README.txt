These are the files used to do the computations for the grid for the binary data
with GBM and to generate the plots. The files are: 

GBM_ab1.slurm: example bash file to do the runs on the CHPC. 

ab1.conf: command lines that were accessed by the slurm file. The numbers
and strings at the end of the command line are arguments that were fed into 
the R script. 

*.R (* represents name of dataset): R script for doing the grid calculations. 
It returns a portion of the data frame that has the information for the grid. 
The partial data frames are assembled into a single data frame at a later 
stage in the process. 

GBMreg-cvpred.R: contains function called by *.R to do compute the 
cross-validated MSE for the GBM models. It returns the overall CV MSE and 
the MSE for each fold of cross-validation. 

GBM_*.R (* represents name of dataset): each of these grabs the grid data 
sets that were computed on the CHPC, combines them into a single data frame, 
creates a list of smaller data frames that make plotting easier, and then 
plots the grid data. These scripts plot a portion of the grid, which was 
very large for GBM. 

get_data: used by the plotting functions to combine the many grid datasets into a 
single data frame and then return a list of dataframes that are used for plotting.