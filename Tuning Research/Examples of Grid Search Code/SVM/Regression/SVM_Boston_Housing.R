# set working directory to source file

library(tidyverse)
library(ggplot2)
library(gridExtra)

source("C:/Users/jflun/Dropbox/Dissertation/Tuning Research/Grid Search/Plot Scripts/get_data.R")

bhg <- datagridReg("C:/Users/jflun/Dropbox/Dissertation/Tuning Research/Grid Search/Grid Data/SVM/Regression/Boston Housing", 
                   dataset = "Boston Housing")

#------------------------------------------------------------------------------
#                            Graphs for Boston Housing data
#------------------------------------------------------------------------------

bh1.1 <- ggplot(bhg$datAll, aes(x = Cost, y = Gamma)) +
  geom_tile(aes(fill = MSE)) +
  theme_bw() +
  labs(x = "Cost (2^x)", y = "Gamma (2^y)") +
  ggtitle("All MSE for SVM Boston Housing Data") +
  facet_wrap(~Epsilon, ncol = 3)


bh1.2 <- ggplot(bhg$datAll, aes(x = Cost, y = Gamma)) +
  geom_tile(aes(fill = MSE_UCL)) +
  theme_bw() +
  labs(x = "Cost (2^x)", y = "Gamma (2^y)") +
  ggtitle("All UCL of MSEs for SVM Boston Housing Data") +
  facet_wrap(~Epsilon, ncol = 3)


bh1.3 <- ggplot(bhg$datAll, aes(x = Cost, y = Gamma)) +
  geom_tile(aes(fill = Time)) +
  theme_bw() +
  labs(x = "Cost (2^x)", y = "Gamma (2^y)") +
  ggtitle("All Times for SVM Boston Housing Data") +
  facet_wrap(~Epsilon, ncol = 3)



bh2.1 <- ggplot(bhg$dat20A, aes(x = Cost, y = Gamma)) +
  geom_tile(aes(fill = MSE)) +
  geom_point(data = bhg$top20acc, aes(x = Cost, y = Gamma), 
             size = 2, color = "orange1") + 
  theme_bw() +
  labs(x = "Cost (2^x)", y = "Gamma (2^y)") +
  ggtitle("Best 20% MSEs for SVM Boston Housing Data") +
  facet_wrap(~Epsilon, ncol = 3)


bh2.2 <- ggplot(bhg$dat20UCL, aes(x = Cost, y = Gamma)) +
  geom_tile(aes(fill = MSE_UCL)) +
  geom_point(data = bhg$top20UCL, aes(x = Cost, y = Gamma), 
             size = 2, color = "orange1") + 
  theme_bw() +
  labs(x = "Cost (2^x)", y = "Gamma (2^y)") +
  ggtitle("Best 20% UCLs of MSEs for SVM Boston Housing Data") +
  facet_wrap(~Epsilon, ncol = 3)


bh2.3 <- ggplot(bhg$dat20Time, aes(x = Cost, y = Gamma)) +
  geom_tile(aes(fill = Time)) +
  geom_point(data = bhg$top20Time, aes(x = Cost, y = Gamma), 
             size = 2, color = "orange1") + 
  theme_bw() +
  labs(x = "Cost (2^x)", y = "Gamma (2^y)") +
  ggtitle("Best 20% Times for SVM Boston Housing Data") +
  facet_wrap(~Epsilon, ncol = 3)



# pdf("../../Grid Search Plots/SVM/Regression/SVM_Boston_Housing.pdf", height = 9, width = 6.5)
# grid.arrange(bh1.1, bh2.1)
# grid.arrange(bh1.2, bh2.2)
# grid.arrange(bh1.3, bh2.3)
# dev.off()

pdf("../../Grid Search Plots/SVM/Regression/SVM_Boston_Housing.pdf", height = 9, width = 6.5)
bh1.1
bh1.2
bh1.3
bh2.1
bh2.2
bh2.3
dev.off()



