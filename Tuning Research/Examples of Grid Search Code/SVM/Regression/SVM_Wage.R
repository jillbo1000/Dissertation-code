# set working directory to source file

library(tidyverse)
library(ggplot2)
library(gridExtra)

source("C:/Users/jflun/Dropbox/Dissertation/Tuning Research/Grid Search/Plot Swaipts/get_data.R")

wag <- datagridReg("C:/Users/jflun/Dropbox/Dissertation/Tuning Research/Grid Search/Grid Data/SVM/Regression/Wage", 
                   dataset = "Wage")

#------------------------------------------------------------------------------
#                            Graphs for Wage data
#------------------------------------------------------------------------------

wa1.1 <- ggplot(wag$datAll, aes(x = Cost, y = Gamma)) +
  geom_tile(aes(fill = MSE)) +
  theme_bw() +
  labs(x = "Cost (2^x)", y = "Gamma (2^y)") +
  ggtitle("All MSE for SVM Wage Data") +
  facet_wrap(~Epsilon, ncol = 3)


wa1.2 <- ggplot(wag$datAll, aes(x = Cost, y = Gamma)) +
  geom_tile(aes(fill = MSE_UCL)) +
  theme_bw() +
  labs(x = "Cost (2^x)", y = "Gamma (2^y)") +
  ggtitle("All UCL of MSEs for SVM Wage Data") +
  facet_wrap(~Epsilon, ncol = 3)


wa1.3 <- ggplot(wag$datAll, aes(x = Cost, y = Gamma)) +
  geom_tile(aes(fill = Time)) +
  theme_bw() +
  labs(x = "Cost (2^x)", y = "Gamma (2^y)") +
  ggtitle("All Times for SVM Wage Data") +
  facet_wrap(~Epsilon, ncol = 3)



wa2.1 <- ggplot(wag$dat20A, aes(x = Cost, y = Gamma)) +
  geom_tile(aes(fill = MSE)) +
  geom_point(data = wag$top20acc, aes(x = Cost, y = Gamma), 
             size = 2, color = "orange1") + 
  theme_bw() +
  labs(x = "Cost (2^x)", y = "Gamma (2^y)") +
  ggtitle("Best 20% MSEs for SVM Wage Data") +
  facet_wrap(~Epsilon, ncol = 3)


wa2.2 <- ggplot(wag$dat20UCL, aes(x = Cost, y = Gamma)) +
  geom_tile(aes(fill = MSE_UCL)) +
  geom_point(data = wag$top20UCL, aes(x = Cost, y = Gamma), 
             size = 2, color = "orange1") + 
  theme_bw() +
  labs(x = "Cost (2^x)", y = "Gamma (2^y)") +
  ggtitle("Best 20% UCLs of MSEs for SVM Wage Data") +
  facet_wrap(~Epsilon, ncol = 3)


wa2.3 <- ggplot(wag$dat20Time, aes(x = Cost, y = Gamma)) +
  geom_tile(aes(fill = Time)) +
  geom_point(data = wag$top20Time, aes(x = Cost, y = Gamma), 
             size = 2, color = "orange1") + 
  theme_bw() +
  labs(x = "Cost (2^x)", y = "Gamma (2^y)") +
  ggtitle("Best 20% Times for SVM Wage Data") +
  facet_wrap(~Epsilon, ncol = 3)



# pdf("../../Grid Search Plots/SVM/Regression/SVM_Wage.pdf", height = 9, width = 6.5)
# grid.arrange(wa1.1, Wage.1)
# grid.arrange(wa1.2, Wage.2)
# grid.arrange(wa1.3, Wage.3)
# dev.off()

pdf("../../Grid Search Plots/SVM/Regression/SVM_Wage.pdf", height = 9, width = 6.5)
wa1.1
wa1.2
wa1.3
wa2.1
wa2.2
wa2.3
dev.off()




