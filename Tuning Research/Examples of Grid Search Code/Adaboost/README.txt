Each of the files in this folder were used to generate the large grids for 
adaboost. The files descriptions follow: 

ADA_bc.slurm: used to run the breast cancer grids on the CHPC. This is the bash 
file that controls the runs on slurm. 

myADA-bc.conf: command lines that were accessed by ADA_bc.slurm. The extra 
numbers at the end of the script were fed into R as arguments. 

ADAbin-cvpred.R: script that is called by the other scripts to run cross
validation on the adaboost models. It returns a list with the overall cross
validation error and the errors for each fold. 

ADA_binary.R: this are the R scripts that generated the data for
the adaboost grids. The data name is passed as an argument int the CHPC files. 
The script outputs a chunk of the grid data. All of these chunks can be 
assembled into a single dataset and analyzed as a group. 

ADA_binary_plots.R: grabs all of the grid data, assembles them into a single 
dataframe for each dataset, creates a list of dataframes that are helpful for 
graphing, and then graphs the grids using ggplot2. 

get_data.R: goes into the specified folder, grabs all of the data out, and 
combines it into a single dataframe. It then subsets the dataframe into 
several dataframes that are helpful for plotting and returns all of the dataframes
as a list. 


