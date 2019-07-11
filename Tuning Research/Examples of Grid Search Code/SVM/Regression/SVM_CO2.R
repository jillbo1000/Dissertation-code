# set working directory to source file

library(tidyverse)
library(ggplot2)
library(gridExtra)

source("C:/Users/jflun/Dropbox/Dissertation/Tuning Research/Grid Search/Plot Scripts/get_data.R")

cog <- datagridReg("C:/Users/jflun/Dropbox/Dissertation/Tuning Research/Grid Search/Grid Data/SVM/Regression/CO2", 
                   dataset = "CO2")

#------------------------------------------------------------------------------
#                            Graphs for CO2 data
#------------------------------------------------------------------------------

co1.1 <- ggplot(cog$datAll, aes(x = Cost, y = Gamma)) +
  geom_tile(aes(fill = MSE)) +
  theme_bw() +
  labs(x = "Cost (2^x)", y = "Gamma (2^y)") +
  ggtitle("All MSE for SVM CO2 Data") +
  facet_wrap(~Epsilon, ncol = 3)


co1.2 <- ggplot(cog$datAll, aes(x = Cost, y = Gamma)) +
  geom_tile(aes(fill = MSE_UCL)) +
  theme_bw() +
  labs(x = "Cost (2^x)", y = "Gamma (2^y)") +
  ggtitle("All UCL of MSEs for SVM CO2 Data") +
  facet_wrap(~Epsilon, ncol = 3)


co1.3 <- ggplot(cog$datAll, aes(x = Cost, y = Gamma)) +
  geom_tile(aes(fill = Time)) +
  theme_bw() +
  labs(x = "Cost (2^x)", y = "Gamma (2^y)") +
  ggtitle("All Times for SVM CO2 Data") +
  facet_wrap(~Epsilon, ncol = 3)



co2.1 <- ggplot(cog$dat20A, aes(x = Cost, y = Gamma)) +
  geom_tile(aes(fill = MSE)) +
  geom_point(data = cog$top20acc, aes(x = Cost, y = Gamma), 
             size = 2, color = "orange1") + 
  theme_bw() +
  labs(x = "Cost (2^x)", y = "Gamma (2^y)") +
  ggtitle("Best 20% MSEs for SVM CO2 Data") +
  facet_wrap(~Epsilon, ncol = 3)


co2.2 <- ggplot(cog$dat20UCL, aes(x = Cost, y = Gamma)) +
  geom_tile(aes(fill = MSE_UCL)) +
  geom_point(data = cog$top20UCL, aes(x = Cost, y = Gamma), 
             size = 2, color = "orange1") + 
  theme_bw() +
  labs(x = "Cost (2^x)", y = "Gamma (2^y)") +
  ggtitle("Best 20% UCLs of MSEs for SVM CO2 Data") +
  facet_wrap(~Epsilon, ncol = 3)


co2.3 <- ggplot(cog$dat20Time, aes(x = Cost, y = Gamma)) +
  geom_tile(aes(fill = Time)) +
  geom_point(data = cog$top20Time, aes(x = Cost, y = Gamma), 
             size = 2, color = "orange1") + 
  theme_bw() +
  labs(x = "Cost (2^x)", y = "Gamma (2^y)") +
  ggtitle("Best 20% Times for SVM CO2 Data") +
  facet_wrap(~Epsilon, ncol = 3)



# pdf("../../Grid Search Plots/SVM/Regression/SVM_CO2.pdf", height = 9, width = 6.5)
# grid.arrange(co1.1, co2.1)
# grid.arrange(co1.2, co2.2)
# grid.arrange(co1.3, co2.3)
# dev.off()

pdf("../../Grid Search Plots/SVM/Regression/SVM_CO2.pdf", height = 9, width = 6.5)
co1.1
co1.2
co1.3
co2.1
co2.2
co2.3
dev.off()



