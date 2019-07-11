# set working directory to source file

library(tidyverse)
library(ggplot2)
library(gridExtra)

source("C:/Users/jflun/Dropbox/Dissertation/Tuning Research/Grid Search/Plot Scripts/get_data.R")

abg <- datagridReg("C:/Users/jflun/Dropbox/Dissertation/Tuning Research/Grid Search/Grid Data/SVM/Regression/Abalone", 
                   dataset = "Abalone")

#------------------------------------------------------------------------------
#                            Graphs for Abalone data
#------------------------------------------------------------------------------

ab1.1 <- ggplot(abg$datAll, aes(x = Cost, y = Gamma)) +
  geom_tile(aes(fill = MSE)) +
  theme_bw() +
  labs(x = "Cost (2^x)", y = "Gamma (2^y)") +
  ggtitle("All MSE for SVM Abalone Data") +
  facet_wrap(~Epsilon, ncol = 3)


ab1.2 <- ggplot(abg$datAll, aes(x = Cost, y = Gamma)) +
  geom_tile(aes(fill = MSE_UCL)) +
  theme_bw() +
  labs(x = "Cost (2^x)", y = "Gamma (2^y)") +
  ggtitle("All UCL of MSEs for SVM Abalone Data") +
  facet_wrap(~Epsilon, ncol = 3)


ab1.3 <- ggplot(abg$datAll, aes(x = Cost, y = Gamma)) +
  geom_tile(aes(fill = Time)) +
  theme_bw() +
  labs(x = "Cost (2^x)", y = "Gamma (2^y)") +
  ggtitle("All Times for SVM Abalone Data") +
  facet_wrap(~Epsilon, ncol = 3)



ab2.1 <- ggplot(abg$dat20A, aes(x = Cost, y = Gamma)) +
  geom_tile(aes(fill = MSE)) +
  geom_point(data = abg$top20acc, aes(x = Cost, y = Gamma), 
             size = 2, color = "orange1") + 
  theme_bw() +
  labs(x = "Cost (2^x)", y = "Gamma (2^y)") +
  ggtitle("Best 20% MSEs for SVM Abalone Data") +
  facet_wrap(~Epsilon, ncol = 3)


ab2.2 <- ggplot(abg$dat20UCL, aes(x = Cost, y = Gamma)) +
  geom_tile(aes(fill = MSE_UCL)) +
  geom_point(data = abg$top20UCL, aes(x = Cost, y = Gamma), 
             size = 2, color = "orange1") + 
  theme_bw() +
  labs(x = "Cost (2^x)", y = "Gamma (2^y)") +
  ggtitle("Best 20% UCLs of MSEs for SVM Abalone Data") +
  facet_wrap(~Epsilon, ncol = 3)


ab2.3 <- ggplot(abg$dat20Time, aes(x = Cost, y = Gamma)) +
  geom_tile(aes(fill = Time)) +
  geom_point(data = abg$top20Time, aes(x = Cost, y = Gamma), 
             size = 2, color = "orange1") + 
  theme_bw() +
  labs(x = "Cost (2^x)", y = "Gamma (2^y)") +
  ggtitle("Best 20% Times for SVM Abalone Data") +
  facet_wrap(~Epsilon, ncol = 3)



pdf("../../Grid Search Plots/SVM/Regression/SVM_Abalone.pdf", height = 9, width = 6.5)
# grid.arrange(ab1.1, ab1.2, ab1.3)
# grid.arrange(ab2.1, ab2.2, ab2.3)
ab1.1
ab2.1
ab1.2
ab2.2
ab1.3
ab2.3
dev.off()



