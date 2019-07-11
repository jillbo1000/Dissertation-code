# set working directory to source file

library(tidyverse)
library(ggplot2)
library(gridExtra)

source("C:/Users/jflun/Dropbox/Dissertation/Tuning Research/Grid Search/Plot Scripts/get_data.R")

crg <- datagridReg("C:/Users/jflun/Dropbox/Dissertation/Tuning Research/Grid Search/Grid Data/SVM/Regression/Crime", 
                   dataset = "Crime")

#------------------------------------------------------------------------------
#                            Graphs for Crime data
#------------------------------------------------------------------------------

cr1.1 <- ggplot(crg$datAll, aes(x = Cost, y = Gamma)) +
  geom_tile(aes(fill = MSE)) +
  theme_bw() +
  labs(x = "Cost (2^x)", y = "Gamma (2^y)") +
  ggtitle("All MSE for SVM Crime Data") +
  facet_wrap(~Epsilon, ncol = 3)


cr1.2 <- ggplot(crg$datAll, aes(x = Cost, y = Gamma)) +
  geom_tile(aes(fill = MSE_UCL)) +
  theme_bw() +
  labs(x = "Cost (2^x)", y = "Gamma (2^y)") +
  ggtitle("All UCL of MSEs for SVM Crime Data") +
  facet_wrap(~Epsilon, ncol = 3)


cr1.3 <- ggplot(crg$datAll, aes(x = Cost, y = Gamma)) +
  geom_tile(aes(fill = Time)) +
  theme_bw() +
  labs(x = "Cost (2^x)", y = "Gamma (2^y)") +
  ggtitle("All Times for SVM Crime Data") +
  facet_wrap(~Epsilon, ncol = 3)



cr2.1 <- ggplot(crg$dat20A, aes(x = Cost, y = Gamma)) +
  geom_tile(aes(fill = MSE)) +
  geom_point(data = crg$top20acc, aes(x = Cost, y = Gamma), 
             size = 2, color = "orange1") + 
  theme_bw() +
  labs(x = "Cost (2^x)", y = "Gamma (2^y)") +
  ggtitle("Best 20% MSEs for SVM Crime Data") +
  facet_wrap(~Epsilon, ncol = 3)


cr2.2 <- ggplot(crg$dat20UCL, aes(x = Cost, y = Gamma)) +
  geom_tile(aes(fill = MSE_UCL)) +
  geom_point(data = crg$top20UCL, aes(x = Cost, y = Gamma), 
             size = 2, color = "orange1") + 
  theme_bw() +
  labs(x = "Cost (2^x)", y = "Gamma (2^y)") +
  ggtitle("Best 20% UCLs of MSEs for SVM Crime Data") +
  facet_wrap(~Epsilon, ncol = 3)


cr2.3 <- ggplot(crg$dat20Time, aes(x = Cost, y = Gamma)) +
  geom_tile(aes(fill = Time)) +
  geom_point(data = crg$top20Time, aes(x = Cost, y = Gamma), 
             size = 2, color = "orange1") + 
  theme_bw() +
  labs(x = "Cost (2^x)", y = "Gamma (2^y)") +
  ggtitle("Best 20% Times for SVM Crime Data") +
  facet_wrap(~Epsilon, ncol = 3)



# pdf("../../Grid Search Plots/SVM/Regression/SVM_Crime.pdf", height = 9, width = 6.5)
# grid.arrange(cr1.1, Crime.1)
# grid.arrange(cr1.2, Crime.2)
# grid.arrange(cr1.3, Crime.3)
# dev.off()

pdf("../../Grid Search Plots/SVM/Regression/SVM_Crime.pdf", height = 9, width = 6.5)
cr1.1
cr1.2
cr1.3
cr2.1
cr2.2
cr2.3
dev.off()



