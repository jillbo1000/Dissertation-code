# set working directory to source file

library(ggplot2)
library(gridExtra)

source("C:/Users/jflun/Dropbox/Dissertation/Tuning Research/Grid Search/Plot Scripts/get_data.R")

bcg <- datagridClass("C:/Users/jflun/Dropbox/Dissertation/Tuning Research/Grid Search/Grid Data/SVM/Binary/Breast Cancer", 
                     dataset = "Breast Cancer")
iog <- datagridClass("C:/Users/jflun/Dropbox/Dissertation/Tuning Research/Grid Search/Grid Data/SVM/Binary/Ionosphere", 
                     dataset = "Ionosphere")
pig <- datagridClass("C:/Users/jflun/Dropbox/Dissertation/Tuning Research/Grid Search/Grid Data/SVM/Binary/Pima", 
                     dataset = "Pima")
sog <- datagridClass("C:/Users/jflun/Dropbox/Dissertation/Tuning Research/Grid Search/Grid Data/SVM/Binary/Sonar", 
                     dataset = "Sonar")
lig <- datagridClass("C:/Users/jflun/Dropbox/Dissertation/Tuning Research/Grid Search/Grid Data/SVM/Binary/Lichen", 
                     dataset = "Lichen")
mug <- datagridClass("C:/Users/jflun/Dropbox/Dissertation/Tuning Research/Grid Search/Grid Data/SVM/Binary/Mullein", 
                     dataset = "Mullein")


#------------------------------------------------------------------------------
#                            Graphs for all data
#------------------------------------------------------------------------------

alldat <- rbind(bcg$datAll, iog$datAll, pig$datAll, sog$datAll, 
                lig$datAll, mug$datAll)

sA1 <- ggplot(alldat, aes(x = Cost, y = Gamma)) +
  geom_tile(aes(fill = Error)) +
  theme_bw() +
  labs(x = "Cost (2^x)", y = "Gamma (2^y)") +
  ggtitle("All Errors for SVM Binary Data") +
  facet_wrap(~cat, ncol = 2)


sA2 <- ggplot(alldat, aes(x = Cost, y = Gamma)) +
  geom_tile(aes(fill = ErrUCL)) +
  theme_bw() +
  labs(x = "Cost (2^x)", y = "Gamma (2^y)") +
  ggtitle("All Error UCL for SVM Binary Data") +
  facet_wrap(~cat, ncol = 2)


sA3 <- ggplot(alldat, aes(x = Cost, y = Gamma)) +
  geom_tile(aes(fill = Time)) +
  theme_bw() +
  labs(x = "Cost (2^x)", y = "Gamma (2^y)") +
  ggtitle("All Time SVM Binary Data") +
  facet_wrap(~cat, ncol = 2)


#------------------------------------------------------------------------------
#                     Graphs for best 20 percent 
#------------------------------------------------------------------------------

dat20A <- rbind(bcg$dat20A, iog$dat20A, pig$dat20A, sog$dat20A, 
                lig$dat20A, mug$dat20A)

dat20LCL <- rbind(bcg$dat20LCL, iog$dat20LCL, pig$dat20LCL, sog$dat20LCL, 
                lig$dat20LCL, mug$dat20LCL)

dat20Time <- rbind(bcg$dat20Time, iog$dat20Time, pig$dat20Time, sog$dat20Time, 
                lig$dat20Time, mug$dat20Time)

dat10A <- rbind(bcg$dat10A, iog$dat10A, pig$dat10A, sog$dat10A, 
                lig$dat10A, mug$dat10A)

dat10LCL <- rbind(bcg$dat10LCL, iog$dat10LCL, pig$dat10LCL, sog$dat10LCL, 
                  lig$dat10LCL, mug$dat10LCL)

dat10Time <- rbind(bcg$dat10Time, iog$dat10Time, pig$dat10Time, sog$dat10Time, 
                   lig$dat10Time, mug$dat10Time)

dat5A <- rbind(bcg$dat5A, iog$dat5A, pig$dat5A, sog$dat5A, 
                lig$dat5A, mug$dat5A)

dat5LCL <- rbind(bcg$dat5LCL, iog$dat5LCL, pig$dat5LCL, sog$dat5LCL, 
                  lig$dat5LCL, mug$dat5LCL)

dat5Time <- rbind(bcg$dat5Time, iog$dat5Time, pig$dat5Time, sog$dat5Time, 
                   lig$dat5Time, mug$dat5Time)

best20A <- rbind(bcg$top20acc, iog$top20acc, pig$top20acc, sog$top20acc, 
                 lig$top20acc, mug$top20acc)

best20LCL <- rbind(bcg$top20LCL, iog$top20LCL, pig$top20LCL, sog$top20LCL, 
                   lig$top20LCL, mug$top20LCL)

best20Time <- rbind(bcg$top20Time, iog$top20Time, pig$top20Time, sog$top20Time, 
                    lig$top20Time, mug$top20Time)

s20.Err <- ggplot(dat20A, aes(x = Cost, y = Gamma)) +
  geom_tile(aes(fill = Error)) +
  geom_point(data = best20A, aes(Cost, Gamma), 
             size = 2, color = "orange1") + 
  theme_bw() +
  labs(x = "Cost (2^x)", y = "Gamma (2^y)") +
  ggtitle("Best 20% Errors for SVM Binary Data with 20 Best Highlighted") +
  facet_wrap(~cat, ncol = 2)


s20.UCL <- ggplot(dat20LCL, aes(x = Cost, y = Gamma)) +
  geom_tile(aes(fill = ErrUCL)) +
  geom_point(data = best20LCL, aes(Cost, Gamma), 
             size = 2, color = "orange1") + 
  theme_bw() +
  labs(x = "Cost (2^x)", y = "Gamma (2^y)") +
  ggtitle("Best 20% Error UCL for SVM Binary Data with 20 Best Highlighted") +
  facet_wrap(~cat, ncol = 2)


s20.Time <- ggplot(dat20Time, aes(x = Cost, y = Gamma)) +
  geom_tile(aes(fill = Time)) +
  geom_point(data = best20Time, aes(Cost, Gamma), 
             size = 2, color = "orange1") + 
  theme_bw() +
  labs(x = "Cost (2^x)", y = "Gamma (2^y)") +
  ggtitle("Best 20% Time SVM Binary Data with 20 Best Highlighted") +
  facet_wrap(~cat, ncol = 2)

s10.Err <- ggplot(dat10A, aes(x = Cost, y = Gamma)) +
  geom_tile(aes(fill = Error)) +
  geom_point(data = best20A, aes(Cost, Gamma), 
             size = 2, color = "orange1") + 
  theme_bw() +
  labs(x = "Cost (2^x)", y = "Gamma (2^y)") +
  ggtitle("Best 10% Errors for SVM Binary Data with 20 Best Highlighted") +
  facet_wrap(~cat, ncol = 2)


s10.UCL <- ggplot(dat10LCL, aes(x = Cost, y = Gamma)) +
  geom_tile(aes(fill = ErrUCL)) +
  geom_point(data = best20LCL, aes(Cost, Gamma), 
             size = 2, color = "orange1") + 
  theme_bw() +
  labs(x = "Cost (2^x)", y = "Gamma (2^y)") +
  ggtitle("Best 10% Error UCL for SVM Binary Data with 20 Best Highlighted") +
  facet_wrap(~cat, ncol = 2)


s10.Time <- ggplot(dat10Time, aes(x = Cost, y = Gamma)) +
  geom_tile(aes(fill = Time)) +
  geom_point(data = best20Time, aes(Cost, Gamma), 
             size = 2, color = "orange1") + 
  theme_bw() +
  labs(x = "Cost (2^x)", y = "Gamma (2^y)") +
  ggtitle("Best 10% Time SVM Binary Data with 20 Best Highlighted") +
  facet_wrap(~cat, ncol = 2)


s5.Err <- ggplot(dat5A, aes(x = Cost, y = Gamma)) +
  geom_tile(aes(fill = Error)) +
  geom_point(data = best20A, aes(Cost, Gamma), 
             size = 2, color = "orange1") + 
  theme_bw() +
  labs(x = "Cost (2^x)", y = "Gamma (2^y)") +
  ggtitle("Best 5% Errors for SVM Binary Data with 20 Best Highlighted") +
  facet_wrap(~cat, ncol = 2)


s5.UCL <- ggplot(dat5LCL, aes(x = Cost, y = Gamma)) +
  geom_tile(aes(fill = ErrUCL)) +
  geom_point(data = best20LCL, aes(Cost, Gamma), 
             size = 2, color = "orange1") + 
  theme_bw() +
  labs(x = "Cost (2^x)", y = "Gamma (2^y)") +
  ggtitle("Best 5% Error UCL for SVM Binary Data with 20 Best Highlighted") +
  facet_wrap(~cat, ncol = 2)


s5.Time <- ggplot(dat5Time, aes(x = Cost, y = Gamma)) +
  geom_tile(aes(fill = Time)) +
  geom_point(data = best20Time, aes(Cost, Gamma), 
             size = 2, color = "orange1") + 
  theme_bw() +
  labs(x = "Cost (2^x)", y = "Gamma (2^y)") +
  ggtitle("Best 5% Time SVM Binary Data with 20 Best Highlighted") +
  facet_wrap(~cat, ncol = 2)

# Better Time Plots
sT.bc <- ggplot(bcg$datAll, aes(x = Cost, y = Gamma)) +
  geom_tile(aes(fill = Time)) +
  geom_point(data = bcg$top20Time, aes(Cost, Gamma), 
             size = 1, color = "orange1") + 
  theme_bw() +
  labs(x = "Cost (2^x)", y = "Gamma (2^y)") +
  ggtitle("Breast Cancer Time Surface") +
  facet_wrap(~cat, ncol = 2)

sT.io <- ggplot(iog$datAll, aes(x = Cost, y = Gamma)) +
  geom_tile(aes(fill = Time)) +
  geom_point(data = iog$top20Time, aes(Cost, Gamma), 
             size = 1, color = "orange1") + 
  theme_bw() +
  labs(x = "Cost (2^x)", y = "Gamma (2^y)") +
  ggtitle("Ionosphere Time Surface") +
  facet_wrap(~cat, ncol = 2)

sT.li <- ggplot(lig$datAll, aes(x = Cost, y = Gamma)) +
  geom_tile(aes(fill = Time)) +
  geom_point(data = lig$top20Time, aes(Cost, Gamma), 
             size = 1, color = "orange1") + 
  theme_bw() +
  labs(x = "Cost (2^x)", y = "Gamma (2^y)") +
  ggtitle("Lichen Time Surface") +
  facet_wrap(~cat, ncol = 2)

sT.mu <- ggplot(mug$datAll, aes(x = Cost, y = Gamma)) +
  geom_tile(aes(fill = Time)) +
  geom_point(data = mug$top20Time, aes(Cost, Gamma), 
             size = 1, color = "orange1") + 
  theme_bw() +
  labs(x = "Cost (2^x)", y = "Gamma (2^y)") +
  ggtitle("Mullein Time Surface") +
  facet_wrap(~cat, ncol = 2)

sT.pi <- ggplot(pig$datAll, aes(x = Cost, y = Gamma)) +
  geom_tile(aes(fill = Time)) +
  geom_point(data = pig$top20Time, aes(Cost, Gamma), 
             size = 1, color = "orange1") + 
  theme_bw() +
  labs(x = "Cost (2^x)", y = "Gamma (2^y)") +
  ggtitle("Pima Time Surface") +
  facet_wrap(~cat, ncol = 2)

sT.so <- ggplot(sog$datAll, aes(x = Cost, y = Gamma)) +
  geom_tile(aes(fill = Time)) +
  geom_point(data = sog$top20Time, aes(Cost, Gamma), 
             size = 1, color = "orange1") + 
  theme_bw() +
  labs(x = "Cost (2^x)", y = "Gamma (2^y)") +
  ggtitle("Sonar Time Surface") +
  facet_wrap(~cat, ncol = 2)



pdf("../Grid Search Plots/SVM/Binary/SVM_Binary_Results.pdf", height = 9, width = 6.5)
sA1
s20.Err
s10.Err
s5.Err
sA2
s20.UCL
s10.UCL
s5.UCL
grid.arrange(sT.bc, sT.io, sT.li, sT.mu, sT.pi, sT.so, ncol = 2)
s20.Time
s10.Time
s5.Time
dev.off()


#------------------------------------------------------------------------------
#                            Plots for Clemson Talk
#------------------------------------------------------------------------------

pdf("C:/Users/jflun/Dropbox/Job Search/Clemson/error_surface.pdf", height = 6, width = 9)
cA1 <- ggplot(alldat, aes(x = Cost, y = Gamma)) +
  geom_tile(aes(fill = Error)) +
  theme_bw() +
  labs(x = "Cost (2^x)", y = "Gamma (2^y)") +
  ggtitle("All Errors for SVM Binary Data") +
  facet_wrap(~cat, ncol = 3)
cA1
dev.off()

pdf("C:/Users/jflun/Dropbox/Job Search/Clemson/error20_surface.pdf", height = 6, width = 9)
c20.Err <- ggplot(dat20A, aes(x = Cost, y = Gamma)) +
  geom_tile(aes(fill = Error)) +
  geom_point(data = best20A, aes(Cost, Gamma), 
             size = 2, color = "orange1") + 
  theme_bw() +
  labs(x = "Cost (2^x)", y = "Gamma (2^y)") +
  ggtitle("Best 20% Errors for SVM Binary Data with 20 Best Highlighted") +
  facet_wrap(~cat, ncol = 3)
c20.Err
dev.off()

pdf("C:/Users/jflun/Dropbox/Job Search/Clemson/time_surface.pdf", height = 6, width = 9)
grid.arrange(sT.bc, sT.io, sT.li, sT.mu, sT.pi, sT.so, ncol = 3)
dev.off()





