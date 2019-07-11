These files contain the scripts and CHPC files for creating the plots used 
to analyze the data. 

rhag2.slurm: bash file used to create the PDF version of the plots on the CHPC. 
The plots are so big it is nearly impossible to navigate the PDF so PNG versions
were created. 

rhag2.conf: The command lines for creating the PDF plots for the rhag data
on the CHPC. It uses the Plots_rhag_chpc.R script. 

rhag_png.slurm: bash file used to create the png version of the plots on the CHPC. 
The PDF plots were too big to analyze so these were made for ease of analysis. 

rhag_png.conf: The command lines for creating the png plots for the rhag data
on the CHPC. It uses the Plots_rhag_chpc_png.R script. 

Plots_rhag_chpc.R: script for creating PDF plots of elastic net, tree, and 
random forests reults. 

Plots_rhag_chpc_png.R: script for creating png plots of elastic net, tree, and 
random forests reults.

Note that similar files were used for the timema and mouse data. 