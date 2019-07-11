# set working directory to source file

library(tidyverse)
library(ggplot2)
library(gridExtra)

source("C:/Users/jflun/Dropbox/Dissertation/Tuning Research/Grid Search/Plot Scripts/get_data.R")

abg <- datagridReg("C:/Users/jflun/Dropbox/Dissertation/Tuning Research/Grid Search/Grid Data/GBM/Regression/Abalone", 
                   dataset = "Abalone")


#------------------------------------------------------------------------------
#
#                            Graphs for all data
#
#------------------------------------------------------------------------------

alldat <- abg$datAll

# alldat <- alldat[!is.na(alldat$MSE) & alldat$MSE < Inf & alldat$MSE_UCL < Inf, ]
nrow(alldat[alldat$MSE > 100000, ]) # $[1] 7040

alldat <- alldat[alldat$MSE < 100000, ]
summary(alldat)

alldat1 <- alldat[alldat$Shrinkage == -1 | alldat$Shrinkage == 0.1, ]
alldat2 <- alldat[alldat$Shrinkage == -2 | alldat$Shrinkage == 0.01, ]
alldat3 <- alldat[alldat$Shrinkage == -3 | alldat$Shrinkage == 0.001, ]

# Abalone MSEs for shrinkage = 10^(-1)
g1.1 <- ggplot(alldat1, aes(x = IntDepth, y = NumTrees)) +
  geom_tile(aes(fill = MSE)) +
  theme_bw() +
  labs(x = "interaction.depth", y = "Number of Trees") +
  ggtitle("All MSEs for Abalone Data, Shrinkage = 10^(-1)") +
  facet_wrap(~MinNode, ncol = 4) 

g2.1 <- ggplot(alldat1, aes(x = IntDepth, y = NumTrees)) +
  geom_tile(aes(fill = MSE_UCL)) +
  theme_bw() +
  labs(x = "interaction.depth", y = "n.trees") +
  ggtitle("All MSE UCLs for Abalone Data, Shrinkage = 10^(-1)") +
  facet_wrap(~MinNode, ncol = 4)

g3.1 <- ggplot(alldat1, aes(x = IntDepth, y = NumTrees)) +
  geom_tile(aes(fill = Time)) +
  theme_bw() +
  labs(x = "interaction.depth", y = "n.trees") +
  ggtitle("All Times for Abalone Data, Shrinkage = 10^(-1)") +
  facet_wrap(~MinNode, ncol = 4)

# Abalone MSEs for shrinkage = 10^(-2)

g1.2 <- ggplot(alldat2, aes(x = IntDepth, y = NumTrees)) +
  geom_tile(aes(fill = MSE)) +
  theme_bw() +
  labs(x = "interaction.depth", y = "Number of Trees") +
  ggtitle("All MSEs for Abalone Data, Shrinkage = 10^(-2)") +
  facet_wrap(~MinNode, ncol = 4) 

g2.2 <- ggplot(alldat2, aes(x = IntDepth, y = NumTrees)) +
  geom_tile(aes(fill = MSE_UCL)) +
  theme_bw() +
  labs(x = "interaction.depth", y = "n.trees") +
  ggtitle("All MSE UCLs for Abalone Data, Shrinkage = 10^(-2)") +
  facet_wrap(~MinNode, ncol = 4)

g3.2 <- ggplot(alldat2, aes(x = IntDepth, y = NumTrees)) +
  geom_tile(aes(fill = Time)) +
  theme_bw() +
  labs(x = "interaction.depth", y = "n.trees") +
  ggtitle("All Times for Abalone Data, Shrinkage = 10^(-2)") +
  facet_wrap(~MinNode, ncol = 4)

# Abalone MSEs for shrinkage = 10^(-2)

g1.3 <- ggplot(alldat3, aes(x = IntDepth, y = NumTrees)) +
  geom_tile(aes(fill = MSE)) +
  theme_bw() +
  labs(x = "interaction.depth", y = "Number of Trees") +
  ggtitle("All MSEs for Abalone Data, Shrinkage = 10^(-3)") +
  facet_wrap(~MinNode, ncol = 4) 

g2.3 <- ggplot(alldat3, aes(x = IntDepth, y = NumTrees)) +
  geom_tile(aes(fill = MSE_UCL)) +
  theme_bw() +
  labs(x = "interaction.depth", y = "n.trees") +
  ggtitle("All MSE UCLs for Abalone Data, Shrinkage = 10^(-3)") +
  facet_wrap(~MinNode, ncol = 4)

g3.3 <- ggplot(alldat3, aes(x = IntDepth, y = NumTrees)) +
  geom_tile(aes(fill = Time)) +
  theme_bw() +
  labs(x = "interaction.depth", y = "n.trees") +
  ggtitle("All Times for Abalone Data, Shrinkage = 10^(-3)") +
  facet_wrap(~MinNode, ncol = 4)


#------------------------------------------------------------------------------
#                     Graphs for best 20 percent
#------------------------------------------------------------------------------

dat20A <- abg$dat20A
dat20UCL <- abg$dat20UCL
dat20Time <- abg$dat20Time
dat1A <- abg$dat20A
dat1UCL <- abg$dat20UCL
dat1Time <- abg$dat20Time
best20A <- abg$top20acc
best20UCL <- abg$top20UCL
best20Time <- abg$top20Time

dat20A.1 <- dat20A[dat20A$Shrinkage == -1 | dat20A$Shrinkage == 0.1, ]
dat20A.2 <- dat20A[dat20A$Shrinkage == -2 | dat20A$Shrinkage == 0.01, ]
dat20A.3 <- dat20A[dat20A$Shrinkage == -3 | dat20A$Shrinkage == 0.001, ]

dat20UCL.1 <- dat20UCL[dat20UCL$Shrinkage == -1 | dat20UCL$Shrinkage == 0.1, ]
dat20UCL.2 <- dat20UCL[dat20UCL$Shrinkage == -2 | dat20UCL$Shrinkage == 0.01, ]
dat20UCL.3 <- dat20UCL[dat20UCL$Shrinkage == -3 | dat20UCL$Shrinkage == 0.001, ]

dat20Time.1 <- dat20Time[dat20Time$Shrinkage == -1 | dat20Time$Shrinkage == 0.1, ]
dat20Time.2 <- dat20Time[dat20Time$Shrinkage == -2 | dat20Time$Shrinkage == 0.01, ]
dat20Time.3 <- dat20Time[dat20Time$Shrinkage == -3 | dat20Time$Shrinkage == 0.001, ]

dat1A.1 <- dat1A[dat1A$Shrinkage == -1 | dat1A$Shrinkage == 0.1, ]
dat1A.2 <- dat1A[dat1A$Shrinkage == -2 | dat1A$Shrinkage == 0.01, ]
dat1A.3 <- dat1A[dat1A$Shrinkage == -3 | dat1A$Shrinkage == 0.001, ]

dat1UCL.1 <- dat1UCL[dat1UCL$Shrinkage == -1 | dat1UCL$Shrinkage == 0.1, ]
dat1UCL.2 <- dat1UCL[dat1UCL$Shrinkage == -2 | dat1UCL$Shrinkage == 0.01, ]
dat1UCL.3 <- dat1UCL[dat1UCL$Shrinkage == -3 | dat1UCL$Shrinkage == 0.001, ]

dat1Time.1 <- dat1Time[dat1Time$Shrinkage == -1 | dat1Time$Shrinkage == 0.1, ]
dat1Time.2 <- dat1Time[dat1Time$Shrinkage == -2 | dat1Time$Shrinkage == 0.01, ]
dat1Time.3 <- dat1Time[dat1Time$Shrinkage == -3 | dat1Time$Shrinkage == 0.001, ]

best20A.1 <- best20A[best20A$Shrinkage == -1 | best20A$Shrinkage == 0.1, ]
best20A.2 <- best20A[best20A$Shrinkage == -2 | best20A$Shrinkage == 0.01, ]
best20A.3 <- best20A[best20A$Shrinkage == -3 | best20A$Shrinkage == 0.001, ]

best20UCL.1 <- best20UCL[best20UCL$Shrinkage == -1 | best20UCL$Shrinkage == 0.1, ]
best20UCL.2 <- best20UCL[best20UCL$Shrinkage == -2 | best20UCL$Shrinkage == 0.01, ]
best20UCL.3 <- best20UCL[best20UCL$Shrinkage == -3 | best20UCL$Shrinkage == 0.001, ]

best20Time.1 <- best20Time[best20Time$Shrinkage == -1 | best20Time$Shrinkage == 0.1, ]
best20Time.2 <- best20Time[best20Time$Shrinkage == -2 | best20Time$Shrinkage == 0.01, ]
best20Time.3 <- best20Time[best20Time$Shrinkage == -3 | best20Time$Shrinkage == 0.001, ]

# Plots for shrinkage 10^(-1)

g4.1 <- ggplot(dat20A.1, aes(x = IntDepth, y = NumTrees)) +
  geom_tile(aes(fill = MSE)) +
  geom_point(data = best20A.1, aes(IntDepth, NumTrees),
             size = 2, color = "orange1") +
  theme_bw() +
  labs(x = "interaction.depth", y = "n.trees") +
  ggtitle("Best 20% MSEs for Abalone Data, Shrinkage = 10^(-1)") +
  facet_wrap(~MinNode, ncol = 4)

g5.1 <- ggplot(dat20UCL.1, aes(x = IntDepth, y = NumTrees)) +
  geom_tile(aes(fill = MSE_UCL)) +
  geom_point(data = best20UCL.1, aes(IntDepth, NumTrees),
             size = 2, color = "orange1") +
  theme_bw() +
  labs(x = "interaction.depth", y = "n.trees") +
  ggtitle("Best 20% UCLs for Abalone Data, Shrinkage = 10^(-1)") +
  facet_wrap(~MinNode, ncol = 4)

g6.1 <- ggplot(dat20Time.1, aes(x = IntDepth, y = NumTrees)) +
  geom_tile(aes(fill = Time)) +
  geom_point(data = best20Time.1, aes(IntDepth, NumTrees),
             size = 2, color = "orange1") +
  theme_bw() +
  labs(x = "interaction.depth", y = "n.trees") +
  ggtitle("Best 20% Times for Abalone Data, Shrinkage = 10^(-1)") +
  facet_wrap(~MinNode, ncol = 4)


# Plots for shrinkage 10^(-2)

g4.2 <- ggplot(dat20A.2, aes(x = IntDepth, y = NumTrees)) +
  geom_tile(aes(fill = MSE)) +
  geom_point(data = best20A.2, aes(IntDepth, NumTrees),
             size = 2, color = "orange1") +
  theme_bw() +
  labs(x = "interaction.depth", y = "n.trees") +
  ggtitle("Best 20% MSEs for Abalone Data, Shrinkage = 10^(-2)") +
  facet_wrap(~MinNode, ncol = 4)

g5.2 <- ggplot(dat20UCL.2, aes(x = IntDepth, y = NumTrees)) +
  geom_tile(aes(fill = MSE_UCL)) +
  geom_point(data = best20UCL.2, aes(IntDepth, NumTrees),
             size = 2, color = "orange1") +
  theme_bw() +
  labs(x = "interaction.depth", y = "n.trees") +
  ggtitle("Best 20% UCLs for Abalone Data, Shrinkage = 10^(-2)") +
  facet_wrap(~MinNode, ncol = 4)

g6.2 <- ggplot(dat20Time.2, aes(x = IntDepth, y = NumTrees)) +
  geom_tile(aes(fill = Time)) +
  geom_point(data = best20Time.2, aes(IntDepth, NumTrees),
             size = 2, color = "orange1") +
  theme_bw() +
  labs(x = "interaction.depth", y = "n.trees") +
  ggtitle("Best 20% Times for Abalone Data, Shrinkage = 10^(-2)") +
  facet_wrap(~MinNode, ncol = 4)


# Plots for shrinkage 10^(-3)

g4.3 <- ggplot(dat20A.3, aes(x = IntDepth, y = NumTrees)) +
  geom_tile(aes(fill = MSE)) +
  geom_point(data = best20A.3, aes(IntDepth, NumTrees),
             size = 2, color = "orange1") +
  theme_bw() +
  labs(x = "interaction.depth", y = "n.trees") +
  ggtitle("Best 20% MSEs for Abalone Data, Shrinkage = 10^(-3)") +
  facet_wrap(~MinNode, ncol = 4)

g5.3 <- ggplot(dat20UCL.3, aes(x = IntDepth, y = NumTrees)) +
  geom_tile(aes(fill = MSE_UCL)) +
  geom_point(data = best20UCL.3, aes(IntDepth, NumTrees),
             size = 2, color = "orange1") +
  theme_bw() +
  labs(x = "interaction.depth", y = "n.trees") +
  ggtitle("Best 20% UCLs for Abalone Data, Shrinkage = 10^(-3)") +
  facet_wrap(~MinNode, ncol = 4)

g6.3 <- ggplot(dat20Time.3, aes(x = IntDepth, y = NumTrees)) +
  geom_tile(aes(fill = Time)) +
  geom_point(data = best20Time.3, aes(IntDepth, NumTrees),
             size = 2, color = "orange1") +
  theme_bw() +
  labs(x = "interaction.depth", y = "n.trees") +
  ggtitle("Best 20% Times for Abalone Data, Shrinkage = 10^(-3)") +
  facet_wrap(~MinNode, ncol = 4)

tmp1 <- ggplot(alldat1, aes(x = IntDepth, y = NumTrees)) +
  geom_tile(aes(fill = MSE), fill ="white") +
  theme_bw() +
  labs(x = "interaction.depth", y = "Number of Trees") +
  ggtitle("Best 20% MSEs for Abalone Data, Shrinkage = 10^(-3)") +
  facet_wrap(~MinNode, ncol = 4) 

tmp2 <- ggplot(alldat1, aes(x = IntDepth, y = NumTrees)) +
  geom_tile(aes(fill = MSE), fill ="white") +
  theme_bw() +
  labs(x = "interaction.depth", y = "Number of Trees") +
  ggtitle("Best 20% UCLs for Abalone Data, Shrinkage = 10^(-3)") +
  facet_wrap(~MinNode, ncol = 4) 

tmp3 <- ggplot(alldat1, aes(x = IntDepth, y = NumTrees)) +
  geom_tile(aes(fill = MSE), fill ="white") +
  theme_bw() +
  labs(x = "interaction.depth", y = "Number of Trees") +
  ggtitle("Best 20% MSEs for Abalone Data, Shrinkage = 10^(-2)") +
  facet_wrap(~MinNode, ncol = 4) 

tmp4 <- ggplot(alldat1, aes(x = IntDepth, y = NumTrees)) +
  geom_tile(aes(fill = MSE), fill ="white") +
  theme_bw() +
  labs(x = "interaction.depth", y = "Number of Trees") +
  ggtitle("Best 20% UCLs for Abalone Data, Shrinkage = 10^(-2)") +
  facet_wrap(~MinNode, ncol = 4) 


pdf("../../Grid Search Plots/GBM/Regression/GBM_Abalone.pdf", height = 9, width = 6.5)
grid.arrange(g1.1, g1.2, g1.3, ncol = 1)
grid.arrange(g4.1, g4.2, g4.3, ncol = 1)
grid.arrange(g2.1, g2.2, g2.3, ncol = 1)
grid.arrange(g5.1, g5.2, g5.3, ncol = 1)
grid.arrange(g3.1, g3.2, g3.3, ncol = 1)
grid.arrange(g6.1, g6.2, g6.3, ncol = 1)
dev.off()


