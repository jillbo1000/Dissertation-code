# set working directory to source file

library(tidyverse)
library(ggplot2)
library(gridExtra)

source("C:/Users/jflun/Dropbox/Dissertation/Tuning Research/Grid Search/Plot Sunipts/get_data.R")

ung <- datagridReg("C:/Users/jflun/Dropbox/Dissertation/Tuning Research/Grid Search/Grid Data/SVM/Regression/Union", 
                   dataset = "Union")

#------------------------------------------------------------------------------
#                            Graphs for Union data
#------------------------------------------------------------------------------

un1.1 <- ggplot(ung$datAll, aes(x = Cost, y = Gamma)) +
  geom_tile(aes(fill = MSE)) +
  theme_bw() +
  labs(x = "Cost (2^x)", y = "Gamma (2^y)") +
  ggtitle("All MSE for SVM Union Data") +
  facet_wrap(~Epsilon, ncol = 3)


un1.2 <- ggplot(ung$datAll, aes(x = Cost, y = Gamma)) +
  geom_tile(aes(fill = MSE_UCL)) +
  theme_bw() +
  labs(x = "Cost (2^x)", y = "Gamma (2^y)") +
  ggtitle("All UCL of MSEs for SVM Union Data") +
  facet_wrap(~Epsilon, ncol = 3)


un1.3 <- ggplot(ung$datAll, aes(x = Cost, y = Gamma)) +
  geom_tile(aes(fill = Time)) +
  theme_bw() +
  labs(x = "Cost (2^x)", y = "Gamma (2^y)") +
  ggtitle("All Times for SVM Union Data") +
  facet_wrap(~Epsilon, ncol = 3)



un2.1 <- ggplot(ung$dat20A, aes(x = Cost, y = Gamma)) +
  geom_tile(aes(fill = MSE)) +
  geom_point(data = ung$top20acc, aes(x = Cost, y = Gamma), 
             size = 2, color = "orange1") + 
  theme_bw() +
  labs(x = "Cost (2^x)", y = "Gamma (2^y)") +
  ggtitle("Best 20% MSEs for SVM Union Data") +
  facet_wrap(~Epsilon, ncol = 3)


un2.2 <- ggplot(ung$dat20UCL, aes(x = Cost, y = Gamma)) +
  geom_tile(aes(fill = MSE_UCL)) +
  geom_point(data = ung$top20UCL, aes(x = Cost, y = Gamma), 
             size = 2, color = "orange1") + 
  theme_bw() +
  labs(x = "Cost (2^x)", y = "Gamma (2^y)") +
  ggtitle("Best 20% UCLs of MSEs for SVM Union Data") +
  facet_wrap(~Epsilon, ncol = 3)


un2.3 <- ggplot(ung$dat20Time, aes(x = Cost, y = Gamma)) +
  geom_tile(aes(fill = Time)) +
  geom_point(data = ung$top20Time, aes(x = Cost, y = Gamma), 
             size = 2, color = "orange1") + 
  theme_bw() +
  labs(x = "Cost (2^x)", y = "Gamma (2^y)") +
  ggtitle("Best 20% Times for SVM Union Data") +
  facet_wrap(~Epsilon, ncol = 3)



# pdf("../../Grid Search Plots/SVM/Regression/SVM_Union.pdf", height = 9, width = 6.5)
# grid.arrange(un1.1, Union.1)
# grid.arrange(un1.2, Union.2)
# grid.arrange(un1.3, Union.3)
# dev.off()

pdf("../../Grid Search Plots/SVM/Regression/SVM_Union.pdf", height = 9, width = 6.5)
un1.1
un1.2
un1.3
un2.1
un2.2
un2.3
dev.off()




