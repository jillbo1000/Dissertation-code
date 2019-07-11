# set working directory to source file

library(tidyverse)
library(ggplot2)
library(gridExtra)

source("C:/Users/jflun/Dropbox/Dissertation/Tuning Research/Grid Search/Plot Scripts/get_data.R")

ohg <- datagridReg("C:/Users/jflun/Dropbox/Dissertation/Tuning Research/Grid Search/Grid Data/SVM/Regression/Ohio Housing", 
                   dataset = "Ohio Housing")

#------------------------------------------------------------------------------
#                            Graphs for Ohio Housing data
#------------------------------------------------------------------------------

oh1.1 <- ggplot(ohg$datAll, aes(x = Cost, y = Gamma)) +
  geom_tile(aes(fill = MSE)) +
  theme_bw() +
  labs(x = "Cost (2^x)", y = "Gamma (2^y)") +
  ggtitle("All MSE for SVM Ohio Housing Data") +
  facet_wrap(~Epsilon, ncol = 3)


oh1.2 <- ggplot(ohg$datAll, aes(x = Cost, y = Gamma)) +
  geom_tile(aes(fill = MSE_UCL)) +
  theme_bw() +
  labs(x = "Cost (2^x)", y = "Gamma (2^y)") +
  ggtitle("All UCL of MSEs for SVM Ohio Housing Data") +
  facet_wrap(~Epsilon, ncol = 3)


oh1.3 <- ggplot(ohg$datAll, aes(x = Cost, y = Gamma)) +
  geom_tile(aes(fill = Time)) +
  theme_bw() +
  labs(x = "Cost (2^x)", y = "Gamma (2^y)") +
  ggtitle("All Times for SVM Ohio Housing Data") +
  facet_wrap(~Epsilon, ncol = 3)



oh2.1 <- ggplot(ohg$dat20A, aes(x = Cost, y = Gamma)) +
  geom_tile(aes(fill = MSE)) +
  geom_point(data = ohg$top20acc, aes(x = Cost, y = Gamma), 
             size = 2, color = "orange1") + 
  theme_bw() +
  labs(x = "Cost (2^x)", y = "Gamma (2^y)") +
  ggtitle("Best 20% MSEs for SVM Ohio Housing Data") +
  facet_wrap(~Epsilon, ncol = 3)


oh2.2 <- ggplot(ohg$dat20UCL, aes(x = Cost, y = Gamma)) +
  geom_tile(aes(fill = MSE_UCL)) +
  geom_point(data = ohg$top20UCL, aes(x = Cost, y = Gamma), 
             size = 2, color = "orange1") + 
  theme_bw() +
  labs(x = "Cost (2^x)", y = "Gamma (2^y)") +
  ggtitle("Best 20% UCLs of MSEs for SVM Ohio Housing Data") +
  facet_wrap(~Epsilon, ncol = 3)


oh2.3 <- ggplot(ohg$dat20Time, aes(x = Cost, y = Gamma)) +
  geom_tile(aes(fill = Time)) +
  geom_point(data = ohg$top20Time, aes(x = Cost, y = Gamma), 
             size = 2, color = "orange1") + 
  theme_bw() +
  labs(x = "Cost (2^x)", y = "Gamma (2^y)") +
  ggtitle("Best 20% Times for SVM Ohio Housing Data") +
  facet_wrap(~Epsilon, ncol = 3)



# pdf("../../Grid Search Plots/SVM/Regression/SVM_Ohio_Housing.pdf", height = 9, width = 6.5)
# grid.arrange(oh1.1, Ohio Housing.1)
# grid.arrange(oh1.2, Ohio Housing.2)
# grid.arrange(oh1.3, Ohio Housing.3)
# dev.off()

pdf("../../Grid Search Plots/SVM/Regression/SVM_Ohio_Housing.pdf", height = 9, width = 6.5)
oh1.1
oh1.2
oh1.3
oh2.1
oh2.2
oh2.3
dev.off()



