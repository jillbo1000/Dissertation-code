These files did the optimization calculations for each of the datasets. 
The file descriptions follow.

eztune_*_*_performance2.R (first * is either bin or reg, second * is the
model type): These collect the results from the EZtune tests and 
create graphs and tables that can be used to compare the results for the 
different argument options. 

get_eztune_results.R: extracts the EZtune test results from the files 
generated on the CHPC. 

data_summary.R: contains functions used to summarize the EZtune results data
in preparation for plotting. 

eztune_table.R: makes a list that has two tables. The first table is the summary
of the results for all of the EZtune test runs. The second table is the same
table formatted for LaTeX. 

get_data.R: retrieves the grid data so that the best grid results can be used in 
the plots and tables for comparison. 

Get_opt_data.R: this was included because it is sourced in the 
eztune_*_*_performance2.R scripts, but it is not actually used anywhere. It 
retrieves data files from the optimization tests and combines them into a single
data frame. 