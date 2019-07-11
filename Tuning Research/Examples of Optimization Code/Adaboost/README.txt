These files did the optimization calculations for each of the datasets. 
The file descriptions follow.

ADA_bin_opt1.slurm: example bash file that controls the computations done 
on the CHPC. 

myADAopt1.conf: file that contains the command lines called in 
ADA_bin_opt1.slurm. The numbers and strings at the end of the R file are 
arguments that are passed to the R script. 

ADA_bin_*.R (* represents the optimization algorithm): these did 10 runs for 
each of the optimization algorithms. It outputs a dataset with 10 observations, 
one for every run. The error rate, runtime, and final values for the tuning
parameters are returned. 

ADA_opt_funs.R: contains the functions that bring in the data and the functions
that the optimization functions optimize. 

ADA_bin_opt_graphs: reads in all of the datasets from the optimization tests
and creates graphs to assess optimization algorithm performance. 

Get_opt_data.R: script for retrieving the optimization datasets computed by the
CHPC

make_summary_table.R: Makes a summary table that can be used to plot the data. 

get_data.R: retrieves the grid data so that the minimum error rates and MSEs
can be used in the optimization graphs. 