# set working directory to source file

# File grabs all of the data for each dataset and then creates a summary 
# table for each dataset. Tables can be used to make graphs of time/error
# comparisons

library(ggplot2)
library(gridExtra)
library(RColorBrewer)

source("Get_opt_data.R")
source("make_summary_table.R")
source("C:/Users/jflun/Dropbox/Dissertation/Tuning Research/Grid Search/Plot Scripts/get_data.R")

# Retrieve data

ab.dat <- get_opt_data("C:/Users/jflun/Dropbox/Dissertation/Tuning Research/Optimization/Optimization Tests/GBM/Regression/Data/Abalone/", 
                       "Abalone")
bh.dat <- get_opt_data("C:/Users/jflun/Dropbox/Dissertation/Tuning Research/Optimization/Optimization Tests/GBM/Regression/Data/Boston Housing/", 
                       "Boston Housing")
co.dat <- get_opt_data("C:/Users/jflun/Dropbox/Dissertation/Tuning Research/Optimization/Optimization Tests/GBM/Regression/Data/CO2/", 
                       "CO2")
cr.dat <- get_opt_data("C:/Users/jflun/Dropbox/Dissertation/Tuning Research/Optimization/Optimization Tests/GBM/Regression/Data/Crime/",
                       "Crime")
oh.dat <- get_opt_data("C:/Users/jflun/Dropbox/Dissertation/Tuning Research/Optimization/Optimization Tests/GBM/Regression/Data/Ohio Housing/", 
                       "Ohio Housing")
un.dat <- get_opt_data("C:/Users/jflun/Dropbox/Dissertation/Tuning Research/Optimization/Optimization Tests/GBM/Regression/Data/Union/", 
                       "Union")
wa.dat <- get_opt_data("C:/Users/jflun/Dropbox/Dissertation/Tuning Research/Optimization/Optimization Tests/GBM/Regression/Data/Wage/", 
                       "Wage")


# Bring in grid data to get best errors

abg <- datagridReg("C:/Users/jflun/Dropbox/Dissertation/Tuning Research/Grid Search/Grid Data/GBM/Regression/Abalone", 
                     dataset = "Abalone")
bhg <- datagridReg("C:/Users/jflun/Dropbox/Dissertation/Tuning Research/Grid Search/Grid Data/GBM/Regression/Boston Housing", 
                     dataset = "Boston Housing")
cog <- datagridReg("C:/Users/jflun/Dropbox/Dissertation/Tuning Research/Grid Search/Grid Data/GBM/Regression/CO2", 
                     dataset = "CO2")
crg <- datagridReg("C:/Users/jflun/Dropbox/Dissertation/Tuning Research/Grid Search/Grid Data/GBM/Regression/Crime", 
                     dataset = "Crime")
ohg <- datagridReg("C:/Users/jflun/Dropbox/Dissertation/Tuning Research/Grid Search/Grid Data/GBM/Regression/Ohio Housing", 
                     dataset = "Ohio Housing")
ung <- datagridReg("C:/Users/jflun/Dropbox/Dissertation/Tuning Research/Grid Search/Grid Data/GBM/Regression/Union", 
                     dataset = "Union")
wag <- datagridReg("C:/Users/jflun/Dropbox/Dissertation/Tuning Research/Grid Search/Grid Data/GBM/Regression/Wage", 
                     dataset = "Wage")

best_results <- cbind.data.frame(rep("", 7), rep(0, 7), rep(0, 7))
colnames(best_results) <- c("Data", "MSE_UCL", "TimeUCL")
best_results$Data <- as.character(best_results$Data)

datasets <- list(abg$datAll, bhg$datAll, cog$datAll, crg$datAll, ohg$datAll, ung$datAll, wag$datAll)
for(i in 1:7) {
  best_results[i, 1] <- datasets[[i]]$cat[1]
  best_results[i, 2] <- min(datasets[[i]]$MSE)
  best_results[i, 3] <- min(datasets[[i]]$Time)
}



# Make tables

ab.tab <- make_summary_opt_table2(ab.dat)
bh.tab <- make_summary_opt_table2(bh.dat)
co.tab <- make_summary_opt_table2(co.dat)
cr.tab <- make_summary_opt_table2(cr.dat)
oh.tab <- make_summary_opt_table2(oh.dat)
un.tab <- make_summary_opt_table2(un.dat)
wa.tab <- make_summary_opt_table2(wa.dat)

allsum <- rbind(ab.tab, bh.tab, co.tab, cr.tab, oh.tab, un.tab, wa.tab)
allsum$Lims <- ifelse(grepl("1", allsum$Optimizer), "3-fold CV", "10-fold CV")
allsum$Optim <- gsub(".1", "", allsum$Optimizer)
allsum$Optim <- gsub(".2", "", allsum$Optim)

datasets <- unique(allsum$Dataset)

summary(allsum[allsum$Mean_MSE < 1e+20, ])
allsum[allsum$Mean_MSE > 1e+20, ]
summary(allsum[allsum$Mean_MSE > 1e+20, ])

allsum <- allsum[allsum$Mean_MSE < 1e+20, ]


# Plots of results

ggplot(allsum, aes(x = Optim, y = Time_UCL, group = factor(Dataset))) +
  geom_path(aes(color = factor(Dataset)), lwd = 1.2) +
  facet_wrap(~Lims, ncol = 2) +
  scale_color_brewer(palette = "Paired") +
  ggtitle("Time UCL in Seconds") +
  theme_bw() +
  theme(legend.title=element_blank(),
        axis.title.x = element_blank(), 
        axis.text.x = element_text(angle = 60, hjust = 1))

allsum10 <- allsum[allsum$Lims == "10-fold CV", ]
allsum3 <- allsum[allsum$Lims != "3-fold CV", ]

ggplot(allsum, aes(x = Optim, y = MSE_UCL, group = factor(Lims))) +
  geom_path(aes(color = factor(Lims)), lwd = 1.2) +
  facet_wrap(~Dataset, ncol = 1) +
  scale_color_brewer(palette = "Paired") +
  ggtitle("MSE UCL in Seconds") +
  theme_bw() +
  theme(legend.title=element_blank(),
        axis.title.x = element_blank(), 
        axis.text.x = element_text(angle = 60, hjust = 1))

error_plots <- vector("list", length(unique(allsum$Dataset)))
time_plots <- vector("list", length(unique(allsum$Dataset)))
cols <- brewer.pal(7, "Set3")

for(i in 1:length(unique(allsum$Dataset))) {
  tmp <- allsum[allsum$Dataset == datasets[i], ]
  tmp_limit <- best_results[best_results$Data == datasets[i], 2]
  error_plots[[i]] <- ggplot(tmp, aes(x = Optim, y = MSE_UCL, group = factor(Lims))) +
    geom_path(aes(color = factor(Lims)), lwd = 1.2) +
    facet_wrap(~Dataset, ncol = 1) +
    geom_hline(yintercept = tmp_limit) +
    scale_color_brewer(palette = "Paired") +
    labs(title = "MSE", y = "MSE UCL") +
    theme_bw() +
    theme(legend.title=element_blank(),
          axis.title.x = element_blank(), 
          axis.text.x = element_text(angle = 60, hjust = 1), 
          strip.background = element_rect(fill = cols[i]), 
          strip.text = element_text(colour = "black"))
  
  time_plots[[i]] <- ggplot(tmp, aes(x = Optim, y = Time_UCL, group = factor(Lims))) +
    geom_path(aes(color = factor(Lims)), lwd = 1.2) +
    facet_wrap(~Dataset, ncol = 1) +
    scale_color_brewer(palette = "Paired") +
    labs(title = "Time", y = "Time UCL (Seconds)") +
    theme_bw() +
    theme(legend.title=element_blank(),
          axis.title.x = element_blank(), 
          axis.text.x = element_text(angle = 60, hjust = 1), 
          strip.background =element_rect(fill = cols[i]), 
          strip.text = element_text(colour = "black"), 
          legend.position = "bottom")
}

pdf("GBM_reg_time_opt.pdf", height = 11, width = 13)
g1 <- ggplotGrob(error_plots[[1]])
id.legend <- grep("guide", g1$layout$name)
legend <- g1[["grobs"]][[id.legend]]
lwidth <- sum(legend$width)

grid.arrange(error_plots[[1]] + theme(legend.position="none"), 
             error_plots[[2]] + theme(legend.position="none"), 
             error_plots[[3]] + theme(legend.position="none"), 
             error_plots[[4]] + theme(legend.position="none"), 
             time_plots[[1]] + theme(legend.position="none"), 
             time_plots[[2]] + theme(legend.position="none"), 
             time_plots[[3]] + theme(legend.position="none"), 
             time_plots[[4]] + theme(legend.position="none"), 
             error_plots[[5]] + theme(legend.position="none"), 
             error_plots[[6]] + theme(legend.position="none"), 
             error_plots[[7]] + theme(legend.position="none"), 
             time_plots[[5]] + theme(legend.position="none"), 
             time_plots[[6]] + theme(legend.position="none"), 
             time_plots[[7]] + theme(legend.position="none"), 
             legend,
             layout_matrix = rbind(c(1,2,3,4), 
                                   c(1,2,3,4),
                                   c(1,2,3,4), 
                                   c(1,2,3,4),
                                   c(5,6,7,8), 
                                   c(5,6,7,8), 
                                   c(5,6,7,8), 
                                   c(5,6,7,8), 
                                   c(9,10,11,15), 
                                   c(9,10,11,15), 
                                   c(9,10,11,15), 
                                   c(9,10,11,15), 
                                   c(12,13,14,15), 
                                   c(12,13,14,15), 
                                   c(12,13,14,15), 
                                   c(12,13,14,15)))
dev.off()

#-------------------------------------------------------------------------
#
#                         Parallel Coordinate Plot
#
#-------------------------------------------------------------------------

# Transform data by subtracing the best error obtained on the grid, then
# dividing by the max value. The same is done for time but without the shift. 

head(ab.dat)
ab.dat$MSEStd <- (ab.dat$MSE - best_results[1, 2]) / max(ab.dat$MSE - best_results[1, 2])
ab.dat$TimeStd <- ab.dat$Time / max(ab.dat$Time)
summary(ab.dat)

bh.dat$MSEStd <- (bh.dat$MSE - best_results[2, 2]) / max(bh.dat$MSE - best_results[2, 2])
bh.dat$TimeStd <- bh.dat$Time / max(bh.dat$Time)
summary(bh.dat)

co.dat$MSEStd <- (co.dat$MSE - best_results[3, 2]) / max(co.dat$MSE - best_results[3, 2])
co.dat$TimeStd <- co.dat$Time / max(co.dat$Time)
summary(co.dat)

cr.dat$MSEStd <- (cr.dat$MSE - best_results[4, 2]) / max(cr.dat$MSE - best_results[4, 2])
cr.dat$TimeStd <- cr.dat$Time / max(cr.dat$Time)
summary(cr.dat)

oh.dat$MSEStd <- (oh.dat$MSE - best_results[5, 2]) / max(oh.dat$MSE - best_results[5, 2])
oh.dat$TimeStd <- oh.dat$Time / max(oh.dat$Time)
summary(oh.dat)

un.dat$MSEStd <- (un.dat$MSE - best_results[6, 2]) / max(un.dat$MSE - best_results[6, 2])
un.dat$TimeStd <- un.dat$Time / max(un.dat$Time)
summary(un.dat)

wa.dat <- wa.dat[wa.dat$MSE < 1e+200, ]
wa.dat$MSEStd <- (wa.dat$MSE - best_results[6, 2]) / max(wa.dat$MSE - best_results[6, 2])
wa.dat$TimeStd <- wa.dat$Time / max(wa.dat$Time)
summary(wa.dat)

dat_pcp <- rbind(ab.dat, bh.dat, co.dat, cr.dat, oh.dat, un.dat, wa.dat)
dat_pcp2 <- dat_pcp[grepl(".2", dat_pcp$Optimizer), ]
dat_pcp2$Optimizer <- gsub(".2", "", dat_pcp2$Optimizer)
# head(cbind(dat_pcp$Optimizer, as.character(grepl(".2", dat_pcp$Optimizer))))

pcp_sum <- group_by(dat_pcp2, Optimizer, cat) %>%
  summarize(Mean_Time = mean(Time, na.rm = TRUE), 
            SD_Time = sd(Time, na.rm = TRUE), 
            Time_UCL = mean(Time, na.rm = TRUE) + 1.96 * sd(Time, na.rm = TRUE), 
            Mean_MSE = mean(MSE, na.rm = TRUE), 
            SD_MSE = sd(MSE, na.rm = TRUE), 
            MSE_UCL = mean(MSE, na.rm = TRUE) + 1.96 * sd(MSE, na.rm = TRUE))

pcp_sum$MSEStd <- 0
pcp_sum$TimeStd <- 0
for(i in 1:7) {
  tmp <- pcp_sum[pcp_sum$cat == best_results$Data[i], ]
  pcp_sum$MSEStd <- ifelse(pcp_sum$cat == best_results$Data[i], 
                           (pcp_sum$MSE_UCL - best_results[i, 2]) / max(tmp$MSE_UCL - best_results[i, 2]), 
                           pcp_sum$MSEStd)
  pcp_sum$TimeStd <- ifelse(pcp_sum$cat == best_results$Data[i], 
                            (pcp_sum$Time_UCL) / max(tmp$Time_UCL), 
                            pcp_sum$TimeStd)
}

class(pcp_sum) <- "data.frame"

# Add in some values for the missing Mullein data

cols <- brewer.pal(10, "Paired")[c(1:4, 7:10)]
# cols <- brewer.pal(8, "Dark2")[c(1:5, 8)]

g1 <- ggplot(pcp_sum, aes(x = Optimizer, y = MSEStd, group = factor(cat))) +
  geom_path(aes(color = factor(cat)), lwd = 1.2) +
  scale_color_manual("", values = cols) +
  labs(title ="Standardized MSEs", x = "", y = "Standardized MSE") +
  theme_bw()

g2 <- ggplot(pcp_sum, aes(x = Optimizer, y = TimeStd, group = factor(cat))) +
  geom_path(aes(color = factor(cat)), lwd = 1.2) +
  scale_color_manual("", values = cols) +
  labs(title ="Standardized Computation Time", x = "", y = "Standardized Time") +
  theme_bw()


g5 <- ggplotGrob(g1)
id.legend <- grep("guide", g5$layout$name)
legend <- g5[["grobs"]][[id.legend]]
lwidth <- sum(legend$width)


grid.arrange(g1, g2, ncol = 1)

pdf("GBM_reg_time_opt_pcp.pdf", height = 5.5, width = 8)
grid.arrange(g1 + theme(legend.position="none"), 
             g2 + theme(legend.position="none"), 
             legend,
             layout_matrix = rbind(c(1,1,1,1,3), 
                                   c(2,2,2,2,3))
)
dev.off()



