# set working directory to source file

library(tidyverse)
library(gridExtra)

source("C:/Users/jflun/Dropbox/Dissertation/Tuning Research/Grid Search/Plot Scripts/get_data.R")


bcg <- datagridClass("C:/Users/jflun/Dropbox/Dissertation/Tuning Research/Grid Search/Grid Data/ADA/Breast Cancer", 
                     dataset = "Breast Cancer")
iog <- datagridClass("C:/Users/jflun/Dropbox/Dissertation/Tuning Research/Grid Search/Grid Data/ADA/Ionosphere", 
                     dataset = "Ionosphere")
pig <- datagridClass("C:/Users/jflun/Dropbox/Dissertation/Tuning Research/Grid Search/Grid Data/ADA/Pima", 
                     dataset = "Pima")
sog <- datagridClass("C:/Users/jflun/Dropbox/Dissertation/Tuning Research/Grid Search/Grid Data/ADA/Sonar", 
                     dataset = "Sonar")
lig <- datagridClass("C:/Users/jflun/Dropbox/Dissertation/Tuning Research/Grid Search/Grid Data/ADA/Mullein", 
                     dataset = "Lichen")
mug <- datagridClass("C:/Users/jflun/Dropbox/Dissertation/Tuning Research/Grid Search/Grid Data/ADA/Lichen", 
                     dataset = "Mullein")


#------------------------------------------------------------------------------
#                            Graphs for all data
#------------------------------------------------------------------------------

alldat <- rbind(bcg$datAll, iog$datAll, pig$datAll, sog$datAll, 
                lig$datAll, mug$datAll)

# pdf("../Grid Search Plots/ADA/ADA_Binary_Error_All.pdf", height = 9, width = 6.5)
bc <- alldat[alldat$cat == "Breast Cancer", ]
ggplot(bc, aes(x = Iter, y = Maxdepth)) +
  geom_tile(aes(fill = Error)) +
  theme_bw() +
  labs(x = "Iter", y = "Maxdepth") +
  ggtitle("All Errors for ADA Breast Cancer Data") +
  facet_wrap(~Nu, ncol = 3)
# dev.off()

pdf("../Grid Search Plots/SVM/Binary/SVM_Binary_UCL_All.pdf", height = 9, width = 6.5)
ggplot(alldat, aes(x = Cost, y = Gamma)) +
  geom_tile(aes(fill = ErrUCL)) +
  theme_bw() +
  labs(x = "Cost (2^x)", y = "Gamma (2^y)") +
  ggtitle("All Error UCL for SVM Binary Data") +
  facet_wrap(~cat, ncol = 2)
dev.off()

pdf("../Grid Search Plots/SVM/Binary/SVM_Binary_Time_All.pdf", height = 9, width = 6.5)
ggplot(alldat, aes(x = Cost, y = Gamma)) +
  geom_tile(aes(fill = Time)) +
  theme_bw() +
  labs(x = "Cost (2^x)", y = "Gamma (2^y)") +
  ggtitle("All Time SVM Binary Data") +
  facet_wrap(~cat, ncol = 2)
dev.off()


#------------------------------------------------------------------------------
#                     Graphs for best 20 percent 
#------------------------------------------------------------------------------

dat20A <- rbind(bcg$dat20A, iog$dat20A, pig$dat20A, sog$dat20A, 
                lig$dat20A, mug$dat20A)

dat20LCL <- rbind(bcg$dat20LCL, iog$dat20LCL, pig$dat20LCL, sog$dat20LCL, 
                lig$dat20LCL, mug$dat20LCL)

dat20Time <- rbind(bcg$dat20Time, iog$dat20Time, pig$dat20Time, sog$dat20Time, 
                lig$dat20Time, mug$dat20Time)

best20A <- rbind(bcg$top20acc, iog$top20acc, pig$top20acc, sog$top20acc, 
                 lig$top20acc, mug$top20acc)

best20LCL <- rbind(bcg$top20LCL, iog$top20LCL, pig$top20LCL, sog$top20LCL, 
                   lig$top20LCL, mug$top20LCL)

best20Time <- rbind(bcg$top20Time, iog$top20Time, pig$top20Time, sog$top20Time, 
                    lig$top20Time, mug$top20Time)

pdf("../Grid Search Plots/SVM/Binary/SVM_Binary_Error_Best.pdf", height = 9, width = 6.5)
ggplot(dat20A, aes(x = Cost, y = Gamma)) +
  geom_tile(aes(fill = Error)) +
  geom_point(data = best20A, aes(Cost, Gamma), 
             size = 2, color = "orange1") + 
  theme_bw() +
  labs(x = "Cost (2^x)", y = "Gamma (2^y)") +
  ggtitle("Best 20% Errors for SVM Binary Data with 20 Best Highlighted") +
  facet_wrap(~cat, ncol = 2)
dev.off()

pdf("../Grid Search Plots/SVM/Binary/SVM_Binary_UCL_Best.pdf", height = 9, width = 6.5)
ggplot(dat20LCL, aes(x = Cost, y = Gamma)) +
  geom_tile(aes(fill = AccLCL)) +
  geom_point(data = best20LCL, aes(Cost, Gamma), 
             size = 2, color = "orange1") + 
  theme_bw() +
  labs(x = "Cost (2^x)", y = "Gamma (2^y)") +
  ggtitle("Best 20% Error UCL for SVM Binary Data with 20 Best Highlighted") +
  facet_wrap(~cat, ncol = 2)
dev.off()

pdf("../Grid Search Plots/SVM/Binary/SVM_Binary_Time_Best.pdf", height = 9, width = 6.5)
ggplot(dat20Time, aes(x = Cost, y = Gamma)) +
  geom_tile(aes(fill = Time)) +
  geom_point(data = best20Time, aes(Cost, Gamma), 
             size = 2, color = "orange1") + 
  theme_bw() +
  labs(x = "Cost (2^x)", y = "Gamma (2^y)") +
  ggtitle("Best 20% Time SVM Binary Data with 20 Best Highlighted") +
  facet_wrap(~cat, ncol = 2)
dev.off()



