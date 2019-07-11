These files did the optimization calculations for each of the datasets. 
The file descriptions follow.

SVM_reg_opt1.slurm: example bash file that controls the computations done 
on the CHPC. 

opt1.conf: file that contains the command lines called in 
SVM_reg_opt1.slurm. The numbers and strings at the end of the R file are 
arguments that are passed to the R script. 

SVM_reg_*.R (* represents the optimization algorithm): these did 10 runs for 
each of the optimization algorithms. It outputs a dataset with 10 observations, 
one for every run. The MSE, runtime, and final values for the tuning
parameters are returned. 

SVM_opt_funs.R: contains the functions that bring in the data and the functions
that the optimization functions optimize. 

SVM_reg_opt_tables: reads in all of the datasets from the optimization tests
and creates graphs to assess optimization algorithm performance. 

Get_opt_data.R: script for retrieving the optimization datasets computed by the
CHPC

make_summary_table.R: Makes a summary table that can be used to plot the data. 

get_data.R: retrieves the grid data so that the minimum error rates and MSEs
can be used in the optimization graphs. 