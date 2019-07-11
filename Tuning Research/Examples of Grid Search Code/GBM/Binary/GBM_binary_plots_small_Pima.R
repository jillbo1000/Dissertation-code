# set working directory to source file

library(tidyverse)
library(gridExtra)

source("C:/Users/jflun/Dropbox/Dissertation/Tuning Research/Grid Search/Plot Scripts/get_data.R")


pig <- datagridClassGBM("C:/Users/jflun/Dropbox/Dissertation/Tuning Research/Grid Search/Grid Data/GBM/Binary/Pima Small",
                        dataset = "Pima")


#------------------------------------------------------------------------------
#
#                            Graphs for all data
#
#------------------------------------------------------------------------------

alldat <- pig$datAll

alldat1 <- alldat[alldat$Shrinkage == -1, ]
alldat2 <- alldat[alldat$Shrinkage == -2, ]
alldat3 <- alldat[alldat$Shrinkage == -3, ]

# Pima Errors for shrinkage = 10^(-1)
pi1.1 <- alldat1[alldat1$cat == "Pima", ]

g1.1 <- ggplot(pi1.1, aes(x = IntDepth, y = NumTrees)) +
  geom_tile(aes(fill = Error)) +
  theme_bw() +
  labs(x = "interaction.depth", y = "Number of Trees") +
  ggtitle("All Errors for Pima Data, Shrinkage = 10^(-1)") +
  facet_wrap(~MinNode, ncol = 4) 

g2.1 <- ggplot(pi1.1, aes(x = IntDepth, y = NumTrees)) +
  geom_tile(aes(fill = ErrUCL)) +
  theme_bw() +
  labs(x = "interaction.depth", y = "n.trees") +
  ggtitle("All Error UCLs for Pima Data, Shrinkage = 10^(-1)") +
  facet_wrap(~MinNode, ncol = 4)

g3.1 <- ggplot(pi1.1, aes(x = IntDepth, y = NumTrees)) +
  geom_tile(aes(fill = Time)) +
  theme_bw() +
  labs(x = "interaction.depth", y = "n.trees") +
  ggtitle("All Times for Pima Data, Shrinkage = 10^(-1)") +
  facet_wrap(~MinNode, ncol = 4)

# Pima Errors for shrinkage = 10^(-2)
pi1.2 <- alldat2[alldat2$cat == "Pima", ]

g1.2 <- ggplot(pi1.2, aes(x = IntDepth, y = NumTrees)) +
  geom_tile(aes(fill = Error)) +
  theme_bw() +
  labs(x = "interaction.depth", y = "Number of Trees") +
  ggtitle("All Errors for Pima Data, Shrinkage = 10^(-2)") +
  facet_wrap(~MinNode, ncol = 4) 

g2.2 <- ggplot(pi1.2, aes(x = IntDepth, y = NumTrees)) +
  geom_tile(aes(fill = ErrUCL)) +
  theme_bw() +
  labs(x = "interaction.depth", y = "n.trees") +
  ggtitle("All Error UCLs for Pima Data, Shrinkage = 10^(-2)") +
  facet_wrap(~MinNode, ncol = 4)

g3.2 <- ggplot(pi1.2, aes(x = IntDepth, y = NumTrees)) +
  geom_tile(aes(fill = Time)) +
  theme_bw() +
  labs(x = "interaction.depth", y = "n.trees") +
  ggtitle("All Times for Pima Data, Shrinkage = 10^(-2)") +
  facet_wrap(~MinNode, ncol = 4)

# Pima Errors for shrinkage = 10^(-2)
pi1.3 <- alldat3[alldat3$cat == "Pima", ]

g1.3 <- ggplot(pi1.3, aes(x = IntDepth, y = NumTrees)) +
  geom_tile(aes(fill = Error)) +
  theme_bw() +
  labs(x = "interaction.depth", y = "Number of Trees") +
  ggtitle("All Errors for Pima Data, Shrinkage = 10^(-3)") +
  facet_wrap(~MinNode, ncol = 4) 

g2.3 <- ggplot(pi1.3, aes(x = IntDepth, y = NumTrees)) +
  geom_tile(aes(fill = ErrUCL)) +
  theme_bw() +
  labs(x = "interaction.depth", y = "n.trees") +
  ggtitle("All Error UCLs for Pima Data, Shrinkage = 10^(-3)") +
  facet_wrap(~MinNode, ncol = 4)

g3.3 <- ggplot(pi1.3, aes(x = IntDepth, y = NumTrees)) +
  geom_tile(aes(fill = Time)) +
  theme_bw() +
  labs(x = "interaction.depth", y = "n.trees") +
  ggtitle("All Times for Pima Data, Shrinkage = 10^(-3)") +
  facet_wrap(~MinNode, ncol = 4)


#------------------------------------------------------------------------------
#                     Graphs for best 20 percent
#------------------------------------------------------------------------------

dat20A <- pig$dat20A
dat20LCL <- pig$dat20LCL
dat20Time <- pig$dat20Time
best20A <- pig$top20acc
best20LCL <- pig$top20LCL
best20Time <- pig$top20Time

dat20A.1 <- dat20A[dat20A$Shrinkage == -1, ]
dat20A.2 <- dat20A[dat20A$Shrinkage == -2, ]
dat20A.3 <- dat20A[dat20A$Shrinkage == -3, ]

dat20LCL.1 <- dat20LCL[dat20LCL$Shrinkage == -1, ]
dat20LCL.2 <- dat20LCL[dat20LCL$Shrinkage == -2, ]
dat20LCL.3 <- dat20LCL[dat20LCL$Shrinkage == -3, ]

dat20Time.1 <- dat20Time[dat20Time$Shrinkage == -1, ]
dat20Time.2 <- dat20Time[dat20Time$Shrinkage == -2, ]
dat20Time.3 <- dat20Time[dat20Time$Shrinkage == -3, ]

best20A.1 <- best20A[best20A$Shrinkage == -1, ]
best20A.2 <- best20A[best20A$Shrinkage == -2, ]
best20A.3 <- best20A[best20A$Shrinkage == -3, ]

best20LCL.1 <- best20LCL[best20LCL$Shrinkage == -1, ]
best20LCL.2 <- best20LCL[best20LCL$Shrinkage == -2, ]
best20LCL.3 <- best20LCL[best20LCL$Shrinkage == -3, ]

best20Time.1 <- best20Time[best20Time$Shrinkage == -1, ]
best20Time.2 <- best20Time[best20Time$Shrinkage == -2, ]
best20Time.3 <- best20Time[best20Time$Shrinkage == -3, ]

# Plots for shrinkage 10^(-1)

g4.1 <- ggplot(dat20A.1, aes(x = IntDepth, y = NumTrees)) +
  geom_tile(aes(fill = Error)) +
  geom_point(data = best20A.1, aes(IntDepth, NumTrees),
             size = 2, color = "orange1") +
  theme_bw() +
  labs(x = "interaction.depth", y = "n.trees") +
  ggtitle("Best 20% Errors for Pima Data, Shrinkage = 10^(-1)") +
  facet_wrap(~MinNode, ncol = 4)

g5.1 <- ggplot(dat20LCL.1, aes(x = IntDepth, y = NumTrees)) +
  geom_tile(aes(fill = ErrUCL)) +
  geom_point(data = best20LCL.1, aes(IntDepth, NumTrees),
             size = 2, color = "orange1") +
  theme_bw() +
  labs(x = "interaction.depth", y = "n.trees") +
  ggtitle("Best 20% UCLs for Pima Data, Shrinkage = 10^(-1)") +
  facet_wrap(~MinNode, ncol = 4)

g6.1 <- ggplot(dat20Time.1, aes(x = IntDepth, y = NumTrees)) +
  geom_tile(aes(fill = Time)) +
  geom_point(data = best20Time.1, aes(IntDepth, NumTrees),
             size = 2, color = "orange1") +
  theme_bw() +
  labs(x = "interaction.depth", y = "n.trees") +
  ggtitle("Best 20% Times for Pima Data, Shrinkage = 10^(-1)") +
  facet_wrap(~MinNode, ncol = 4)


# Plots for shrinkage 10^(-2)

g4.2 <- ggplot(dat20A.2, aes(x = IntDepth, y = NumTrees)) +
  geom_tile(aes(fill = Error)) +
  geom_point(data = best20A.2, aes(IntDepth, NumTrees),
             size = 2, color = "orange1") +
  theme_bw() +
  labs(x = "interaction.depth", y = "n.trees") +
  ggtitle("Best 20% Errors for Pima Data, Shrinkage = 10^(-2)") +
  facet_wrap(~MinNode, ncol = 4)

g5.2 <- ggplot(dat20LCL.2, aes(x = IntDepth, y = NumTrees)) +
  geom_tile(aes(fill = ErrUCL)) +
  geom_point(data = best20LCL.2, aes(IntDepth, NumTrees),
             size = 2, color = "orange1") +
  theme_bw() +
  labs(x = "interaction.depth", y = "n.trees") +
  ggtitle("Best 20% UCLs for Pima Data, Shrinkage = 10^(-2)") +
  facet_wrap(~MinNode, ncol = 4)

g6.2 <- ggplot(dat20Time.2, aes(x = IntDepth, y = NumTrees)) +
  geom_tile(aes(fill = Time)) +
  geom_point(data = best20Time.2, aes(IntDepth, NumTrees),
             size = 2, color = "orange1") +
  theme_bw() +
  labs(x = "interaction.depth", y = "n.trees") +
  ggtitle("Best 20% Times for Pima Data, Shrinkage = 10^(-2)") +
  facet_wrap(~MinNode, ncol = 4)


# Plots for shrinkage 10^(-3)

g4.3 <- ggplot(dat20A.3, aes(x = IntDepth, y = NumTrees)) +
  geom_tile(aes(fill = Error)) +
  geom_point(data = best20A.3, aes(IntDepth, NumTrees),
             size = 2, color = "orange1") +
  theme_bw() +
  labs(x = "interaction.depth", y = "n.trees") +
  ggtitle("Best 20% Errors for Pima Data, Shrinkage = 10^(-3)") +
  facet_wrap(~MinNode, ncol = 4)

g5.3 <- ggplot(dat20LCL.3, aes(x = IntDepth, y = NumTrees)) +
  geom_tile(aes(fill = ErrUCL)) +
  geom_point(data = best20LCL.3, aes(IntDepth, NumTrees),
             size = 2, color = "orange1") +
  theme_bw() +
  labs(x = "interaction.depth", y = "n.trees") +
  ggtitle("Best 20% UCLs for Pima Data, Shrinkage = 10^(-3)") +
  facet_wrap(~MinNode, ncol = 4)

g6.3 <- ggplot(dat20Time.3, aes(x = IntDepth, y = NumTrees)) +
  geom_tile(aes(fill = Time)) +
  geom_point(data = best20Time.3, aes(IntDepth, NumTrees),
             size = 2, color = "orange1") +
  theme_bw() +
  labs(x = "interaction.depth", y = "n.trees") +
  ggtitle("Best 20% Times for Pima Data, Shrinkage = 10^(-3)") +
  facet_wrap(~MinNode, ncol = 4)


pdf("../Grid Search Plots/GBM/Binary/GBM_Binary_Small_Pima.pdf", height = 9, width = 6.5)
grid.arrange(g1.1, g1.2, g1.3, ncol = 1)
grid.arrange(g4.1, g4.2, g4.3, ncol = 1)
grid.arrange(g2.1, g2.2, g2.3, ncol = 1)
grid.arrange(g5.1, g5.2, g5.3, ncol = 1)
grid.arrange(g3.1, g3.2, g3.3, ncol = 1)
grid.arrange(g6.1, g6.2, g6.3, ncol = 1)
dev.off()


