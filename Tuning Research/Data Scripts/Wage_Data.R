

wage <- read.table("/scratch/general/lustre/u6007925/Grid/Data/Wage.txt", header = TRUE)
wage <- wage[complete.cases(wage), ]

