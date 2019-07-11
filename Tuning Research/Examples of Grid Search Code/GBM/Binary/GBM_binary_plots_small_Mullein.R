# set working directory to source file

mubrary(tidyverse)
mubrary(gridExtra)

source("C:/Users/jflun/Dropbox/Dissertation/Tuning Research/Grid Search/Plot Scripts/get_data.R")


mug <- datagridClassGBM("C:/Users/jflun/Dropbox/Dissertation/Tuning Research/Grid Search/Grid Data/GBM/Binary/Mullein Small",
                        dataset = "Mullein")


#------------------------------------------------------------------------------
#
#                            Graphs for all data
#
#------------------------------------------------------------------------------

alldat <- mug$datAll

alldat1 <- alldat[alldat$Shrinkage == -1, ]
alldat2 <- alldat[alldat$Shrinkage == -2, ]
alldat3 <- alldat[alldat$Shrinkage == -3, ]

# Mullein Errors for shrinkage = 10^(-1)
mu1.1 <- alldat1[alldat1$cat == "Mullein", ]

g1.1 <- ggplot(mu1.1, aes(x = IntDepth, y = NumTrees)) +
  geom_tile(aes(fill = Error)) +
  theme_bw() +
  labs(x = "interaction.depth", y = "Number of Trees") +
  ggtitle("All Errors for Mullein Data, Shrinkage = 10^(-1)") +
  facet_wrap(~MinNode, ncol = 4) 

g2.1 <- ggplot(mu1.1, aes(x = IntDepth, y = NumTrees)) +
  geom_tile(aes(fill = ErrUCL)) +
  theme_bw() +
  labs(x = "interaction.depth", y = "n.trees") +
  ggtitle("All Error UCLs for Mullein Data, Shrinkage = 10^(-1)") +
  facet_wrap(~MinNode, ncol = 4)

g3.1 <- ggplot(mu1.1, aes(x = IntDepth, y = NumTrees)) +
  geom_tile(aes(fill = Time)) +
  theme_bw() +
  labs(x = "interaction.depth", y = "n.trees") +
  ggtitle("All Times for Mullein Data, Shrinkage = 10^(-1)") +
  facet_wrap(~MinNode, ncol = 4)

# Mullein Errors for shrinkage = 10^(-2)
mu1.2 <- alldat2[alldat2$cat == "Mullein", ]

g1.2 <- ggplot(mu1.2, aes(x = IntDepth, y = NumTrees)) +
  geom_tile(aes(fill = Error)) +
  theme_bw() +
  labs(x = "interaction.depth", y = "Number of Trees") +
  ggtitle("All Errors for Mullein Data, Shrinkage = 10^(-2)") +
  facet_wrap(~MinNode, ncol = 4) 

g2.2 <- ggplot(mu1.2, aes(x = IntDepth, y = NumTrees)) +
  geom_tile(aes(fill = ErrUCL)) +
  theme_bw() +
  labs(x = "interaction.depth", y = "n.trees") +
  ggtitle("All Error UCLs for Mullein Data, Shrinkage = 10^(-2)") +
  facet_wrap(~MinNode, ncol = 4)

g3.2 <- ggplot(mu1.2, aes(x = IntDepth, y = NumTrees)) +
  geom_tile(aes(fill = Time)) +
  theme_bw() +
  labs(x = "interaction.depth", y = "n.trees") +
  ggtitle("All Times for Mullein Data, Shrinkage = 10^(-2)") +
  facet_wrap(~MinNode, ncol = 4)

# Mullein Errors for shrinkage = 10^(-2)
mu1.3 <- alldat3[alldat3$cat == "Mullein", ]

g1.3 <- ggplot(mu1.3, aes(x = IntDepth, y = NumTrees)) +
  geom_tile(aes(fill = Error)) +
  theme_bw() +
  labs(x = "interaction.depth", y = "Number of Trees") +
  ggtitle("All Errors for Mullein Data, Shrinkage = 10^(-3)") +
  facet_wrap(~MinNode, ncol = 4) 

g2.3 <- ggplot(mu1.3, aes(x = IntDepth, y = NumTrees)) +
  geom_tile(aes(fill = ErrUCL)) +
  theme_bw() +
  labs(x = "interaction.depth", y = "n.trees") +
  ggtitle("All Error UCLs for Mullein Data, Shrinkage = 10^(-3)") +
  facet_wrap(~MinNode, ncol = 4)

g3.3 <- ggplot(mu1.3, aes(x = IntDepth, y = NumTrees)) +
  geom_tile(aes(fill = Time)) +
  theme_bw() +
  labs(x = "interaction.depth", y = "n.trees") +
  ggtitle("All Times for Mullein Data, Shrinkage = 10^(-3)") +
  facet_wrap(~MinNode, ncol = 4)


#------------------------------------------------------------------------------
#                     Graphs for best 20 percent
#------------------------------------------------------------------------------

dat20A <- mug$dat20A
dat20LCL <- mug$dat20LCL
dat20Time <- mug$dat20Time
best20A <- mug$top20acc
best20LCL <- mug$top20LCL
best20Time <- mug$top20Time

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
  ggtitle("Best 20% Errors for Mullein Data, Shrinkage = 10^(-1)") +
  facet_wrap(~MinNode, ncol = 4)

g5.1 <- ggplot(dat20LCL.1, aes(x = IntDepth, y = NumTrees)) +
  geom_tile(aes(fill = ErrUCL)) +
  geom_point(data = best20LCL.1, aes(IntDepth, NumTrees),
             size = 2, color = "orange1") +
  theme_bw() +
  labs(x = "interaction.depth", y = "n.trees") +
  ggtitle("Best 20% UCLs for Mullein Data, Shrinkage = 10^(-1)") +
  facet_wrap(~MinNode, ncol = 4)

g6.1 <- ggplot(dat20Time.1, aes(x = IntDepth, y = NumTrees)) +
  geom_tile(aes(fill = Time)) +
  geom_point(data = best20Time.1, aes(IntDepth, NumTrees),
             size = 2, color = "orange1") +
  theme_bw() +
  labs(x = "interaction.depth", y = "n.trees") +
  ggtitle("Best 20% Times for Mullein Data, Shrinkage = 10^(-1)") +
  facet_wrap(~MinNode, ncol = 4)


# Plots for shrinkage 10^(-2)

g4.2 <- ggplot(dat20A.2, aes(x = IntDepth, y = NumTrees)) +
  geom_tile(aes(fill = Error)) +
  geom_point(data = best20A.2, aes(IntDepth, NumTrees),
             size = 2, color = "orange1") +
  theme_bw() +
  labs(x = "interaction.depth", y = "n.trees") +
  ggtitle("Best 20% Errors for Mullein Data, Shrinkage = 10^(-2)") +
  facet_wrap(~MinNode, ncol = 4)

g5.2 <- ggplot(dat20LCL.2, aes(x = IntDepth, y = NumTrees)) +
  geom_tile(aes(fill = ErrUCL)) +
  geom_point(data = best20LCL.2, aes(IntDepth, NumTrees),
             size = 2, color = "orange1") +
  theme_bw() +
  labs(x = "interaction.depth", y = "n.trees") +
  ggtitle("Best 20% UCLs for Mullein Data, Shrinkage = 10^(-2)") +
  facet_wrap(~MinNode, ncol = 4)

g6.2 <- ggplot(dat20Time.2, aes(x = IntDepth, y = NumTrees)) +
  geom_tile(aes(fill = Time)) +
  geom_point(data = best20Time.2, aes(IntDepth, NumTrees),
             size = 2, color = "orange1") +
  theme_bw() +
  labs(x = "interaction.depth", y = "n.trees") +
  ggtitle("Best 20% Times for Mullein Data, Shrinkage = 10^(-2)") +
  facet_wrap(~MinNode, ncol = 4)


# Plots for shrinkage 10^(-3)

g4.3 <- ggplot(dat20A.3, aes(x = IntDepth, y = NumTrees)) +
  geom_tile(aes(fill = Error)) +
  geom_point(data = best20A.3, aes(IntDepth, NumTrees),
             size = 2, color = "orange1") +
  theme_bw() +
  labs(x = "interaction.depth", y = "n.trees") +
  ggtitle("Best 20% Errors for Mullein Data, Shrinkage = 10^(-3)") +
  facet_wrap(~MinNode, ncol = 4)

g5.3 <- ggplot(dat20LCL.3, aes(x = IntDepth, y = NumTrees)) +
  geom_tile(aes(fill = ErrUCL)) +
  geom_point(data = best20LCL.3, aes(IntDepth, NumTrees),
             size = 2, color = "orange1") +
  theme_bw() +
  labs(x = "interaction.depth", y = "n.trees") +
  ggtitle("Best 20% UCLs for Mullein Data, Shrinkage = 10^(-3)") +
  facet_wrap(~MinNode, ncol = 4)

g6.3 <- ggplot(dat20Time.3, aes(x = IntDepth, y = NumTrees)) +
  geom_tile(aes(fill = Time)) +
  geom_point(data = best20Time.3, aes(IntDepth, NumTrees),
             size = 2, color = "orange1") +
  theme_bw() +
  labs(x = "interaction.depth", y = "n.trees") +
  ggtitle("Best 20% Times for Mullein Data, Shrinkage = 10^(-3)") +
  facet_wrap(~MinNode, ncol = 4)

tmp1 <- ggplot(mu1.1, aes(x = IntDepth, y = NumTrees)) +
  geom_tile(aes(fill = Error), fill ="white") +
  theme_bw() +
  labs(x = "interaction.depth", y = "Number of Trees") +
  ggtitle("Best 20% Errors for Mullein Data, Shrinkage = 10^(-3)") +
  facet_wrap(~MinNode, ncol = 4) 

tmp2 <- ggplot(mu1.1, aes(x = IntDepth, y = NumTrees)) +
  geom_tile(aes(fill = Error), fill ="white") +
  theme_bw() +
  labs(x = "interaction.depth", y = "Number of Trees") +
  ggtitle("Best 20% UCLs for Mullein Data, Shrinkage = 10^(-3)") +
  facet_wrap(~MinNode, ncol = 4) 


pdf("../Grid Search Plots/GBM/Binary/GBM_Binary_Small_Mullein.pdf", height = 9, width = 6.5)
grid.arrange(g1.1, g1.2, g1.3, ncol = 1)
# g4.3 does not have any data
grid.arrange(g4.1, g4.2, tmp1, ncol = 1)
grid.arrange(g2.1, g2.2, g2.3, ncol = 1)
# g5.3 does not have any data
grid.arrange(g5.1, g5.2, tmp2, ncol = 1)
grid.arrange(g3.1, g3.2, g3.3, ncol = 1)
grid.arrange(g6.1, g6.2, g6.3, ncol = 1)
dev.off()


