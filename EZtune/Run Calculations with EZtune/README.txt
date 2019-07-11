These files did the optimization calculations for each of the datasets. 
The file descriptions follow.

ez1-2.slurm: example bash script to do EZtune tests on the CHPC for eztune2.R.

ez1-2.conf: example command lines to do EZtune tests on the CHPC for eztune2.R.

ez3-1.slurm: example bash script to do EZtune tests on the CHPC for eztune3.R.

ez3-1.conf: example command lines to do EZtune tests on the CHPC for eztune3.R.

eztune2.R: does 10 tests on EZtune for each of the argument combinations
that are passed to the script. It is designed to be flexible. It can be used
for adaboost, svm, gbm, etc. so only on script is needed for all of the tests. 
It outputs a dataframe that has the results for all 10 runs for that set of 
arguments. 

eztune3.R: some of the datasets were too large to get all 10 runs in in less
than the time allotted by the CHPC. This script does one run at a time. 

get_data.R: retrieves the data and contains some functions that are helpful
for data manipulation in the main script. 