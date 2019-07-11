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

bc.dat <- get_opt_data("C:/Users/jflun/Dropbox/Dissertation/Tuning Research/Optimization/Optimization Tests/SVM/Binary/Data/Breast Cancer/", 
                       "Breast Cancer")
io.dat <- get_opt_data("C:/Users/jflun/Dropbox/Dissertation/Tuning Research/Optimization/Optimization Tests/SVM/Binary/Data/Ionosphere/", 
                       "Ionosphere")
li.dat <- get_opt_data("C:/Users/jflun/Dropbox/Dissertation/Tuning Research/Optimization/Optimization Tests/SVM/Binary/Data/Lichen/", 
                       "Lichen")
mu.dat <- get_opt_data("C:/Users/jflun/Dropbox/Dissertation/Tuning Research/Optimization/Optimization Tests/SVM/Binary/Data/Mullein/",
                       "Mullein")
pi.dat <- get_opt_data("C:/Users/jflun/Dropbox/Dissertation/Tuning Research/Optimization/Optimization Tests/SVM/Binary/Data/Pima/", 
                       "Pima")
so.dat <- get_opt_data("C:/Users/jflun/Dropbox/Dissertation/Tuning Research/Optimization/Optimization Tests/SVM/Binary/Data/Sonar/", 
                       "Sonar")


# Bring in grid data to get best errors

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

best_results <- cbind.data.frame(rep("", 6), rep(0, 6), rep(0, 6))
colnames(best_results) <- c("Data", "ErrorUCL", "TimeUCL")
best_results$Data <- as.character(best_results$Data)

datasets <- list(bcg$datAll, iog$datAll, lig$datAll, mug$datAll, pig$datAll, sog$datAll)
for(i in 1:6) {
  best_results[i, 1] <- datasets[[i]]$cat[1]
  best_results[i, 2] <- min(datasets[[i]]$Error)
  best_results[i, 3] <- min(datasets[[i]]$Time)
}



# Make tables

bc.tab <- make_summary_opt_table(bc.dat)
io.tab <- make_summary_opt_table(io.dat)
li.tab <- make_summary_opt_table(li.dat)
mu.tab <- make_summary_opt_table(mu.dat)
pi.tab <- make_summary_opt_table(pi.dat)
so.tab <- make_summary_opt_table(so.dat)

allsum <- rbind(bc.tab, io.tab, li.tab, mu.tab, pi.tab, so.tab)
allsum$Lims <- ifelse(grepl("1", allsum$Optimizer), "Big Gamma Region", "Small Gamma Region")
allsum$Optim <- gsub(".1", "", allsum$Optimizer)
allsum$Optim <- gsub(".2", "", allsum$Optim)

datasets <- unique(allsum$Dataset)

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

allsumB <- allsum[allsum$Lims == "Big Gamma Region", ]
allsumS <- allsum[allsum$Lims != "Big Gamma Region", ]

ggplot(allsum, aes(x = Optim, y = Error_UCL, group = factor(Lims))) +
  geom_path(aes(color = factor(Lims)), lwd = 1.2) +
  facet_wrap(~Dataset, ncol = 1) +
  scale_color_brewer(palette = "Paired") +
  ggtitle("Error UCL in Seconds") +
  theme_bw() +
  theme(legend.title=element_blank(),
        axis.title.x = element_blank(), 
        axis.text.x = element_text(angle = 60, hjust = 1))

error_plots <- vector("list", length(unique(allsum$Dataset)))
time_plots <- vector("list", length(unique(allsum$Dataset)))
cols <- brewer.pal(6, "Set3")

for(i in 1:length(unique(allsum$Dataset))) {
  tmp <- allsum[allsum$Dataset == datasets[i], ]
  tmp_limit <- best_results[best_results$Data == datasets[i], 2]
  error_plots[[i]] <- ggplot(tmp, aes(x = Optim, y = Error_UCL, group = factor(Lims))) +
    geom_path(aes(color = factor(Lims)), lwd = 1.2) +
    facet_wrap(~Dataset, ncol = 1) +
    geom_hline(yintercept = tmp_limit) +
    scale_color_brewer(palette = "Paired") +
    labs(title = "Error", y = "Error UCL") +
    theme_bw() +
    theme(legend.title=element_blank(),
          axis.title.x = element_blank(), 
          axis.text.x = element_text(angle = 60, hjust = 1), 
          strip.background = element_rect(fill = cols[i]), 
          strip.text = element_text(colour = "black"), 
          legend.position = "bottom")
  
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

pdf("time_opt.pdf", height = 12, width = 12)
g1 <- ggplotGrob(error_plots[[1]])
id.legend <- grep("guide", g1$layout$name)
legend <- g1[["grobs"]][[id.legend]]
lwidth <- sum(legend$width)

grid.arrange(error_plots[[1]] + theme(legend.position="none"), 
             error_plots[[2]] + theme(legend.position="none"), 
             error_plots[[3]] + theme(legend.position="none"), 
             time_plots[[1]] + theme(legend.position="none"), 
             time_plots[[2]] + theme(legend.position="none"), 
             time_plots[[3]] + theme(legend.position="none"), 
             error_plots[[4]] + theme(legend.position="none"), 
             error_plots[[5]] + theme(legend.position="none"), 
             error_plots[[6]] + theme(legend.position="none"), 
             time_plots[[4]] + theme(legend.position="none"), 
             time_plots[[5]] + theme(legend.position="none"), 
             time_plots[[6]] + theme(legend.position="none"), 
             legend,
             layout_matrix = rbind(c(1,2,3), 
                                   c(1,2,3),
                                   c(1,2,3), 
                                   c(1,2,3),
                                   c(4,5,6), 
                                   c(4,5,6), 
                                   c(4,5,6), 
                                   c(4,5,6), 
                                   c(7,8,9), 
                                   c(7,8,9), 
                                   c(7,8,9), 
                                   c(7,8,9), 
                                   c(10,11,12), 
                                   c(10,11,12), 
                                   c(10,11,12), 
                                   c(10,11,12), 
                                   c(13,13,13)))
dev.off()

#-------------------------------------------------------------------------
#
#                         Parallel Coordinate Plot
#
#-------------------------------------------------------------------------

# Transform data by subtracing the best error obtained on the grid, then
# dividing by the max value. The same is done for time but without the shift. 

head(bc.dat)
bc.dat$ErrStd <- (bc.dat$Error - best_results[1, 2]) / max(bc.dat$Error - best_results[1, 2])
bc.dat$TimeStd <- bc.dat$Time / max(bc.dat$Time)
summary(bc.dat)

io.dat$ErrStd <- (io.dat$Error - best_results[2, 2]) / max(io.dat$Error - best_results[2, 2])
io.dat$TimeStd <- io.dat$Time / max(io.dat$Time)
summary(io.dat)

li.dat$ErrStd <- (li.dat$Error - best_results[3, 2]) / max(li.dat$Error - best_results[3, 2])
li.dat$TimeStd <- li.dat$Time / max(li.dat$Time)
summary(li.dat)

mu.dat$ErrStd <- (mu.dat$Error - best_results[4, 2]) / max(mu.dat$Error - best_results[4, 2])
mu.dat$TimeStd <- mu.dat$Time / max(mu.dat$Time)
summary(mu.dat)

pi.dat$ErrStd <- (pi.dat$Error - best_results[5, 2]) / max(pi.dat$Error - best_results[5, 2])
pi.dat$TimeStd <- pi.dat$Time / max(pi.dat$Time)
summary(pi.dat)

so.dat$ErrStd <- (so.dat$Error - best_results[6, 2]) / max(so.dat$Error - best_results[6, 2])
so.dat$TimeStd <- so.dat$Time / max(so.dat$Time)
summary(so.dat)

dat_pcp <- rbind(bc.dat, io.dat, li.dat, mu.dat, pi.dat, so.dat)
dat_pcp2 <- dat_pcp[grepl(".2", dat_pcp$Optimizer), ]
dat_pcp2$Optimizer <- gsub(".2", "", dat_pcp2$Optimizer)
# head(cbind(dat_pcp$Optimizer, as.character(grepl(".2", dat_pcp$Optimizer))))

pcp_sum <- group_by(dat_pcp2, Optimizer, cat) %>%
  summarize(Mean_Time = mean(Time, na.rm = TRUE), 
            SD_Time = sd(Time, na.rm = TRUE), 
            Time_UCL = mean(Time, na.rm = TRUE) + 1.96 * sd(Time, na.rm = TRUE), 
            Mean_Error = mean(Error, na.rm = TRUE), 
            SD_Error = sd(Error, na.rm = TRUE), 
            Error_UCL = mean(Error, na.rm = TRUE) + 1.96 * sd(Error, na.rm = TRUE))

pcp_sum$ErrStd <- 0
pcp_sum$TimeStd <- 0
for(i in 1:6) {
  tmp <- pcp_sum[pcp_sum$cat == best_results$Data[i], ]
  pcp_sum$ErrStd <- ifelse(pcp_sum$cat == best_results$Data[i], 
                           (pcp_sum$Error_UCL - best_results[i, 2]) / max(tmp$Error_UCL - best_results[i, 2]), 
                           pcp_sum$ErrStd)
  pcp_sum$TimeStd <- ifelse(pcp_sum$cat == best_results$Data[i], 
                            (pcp_sum$Time_UCL) / max(tmp$Time_UCL), 
                            pcp_sum$TimeStd)
}

class(pcp_sum) <- "data.frame"

# Add in some values for the missing Mullein data

mullein_tmp <- as.data.frame(cbind(c("ga", "hjkb", "lbfgsb3", "two"), "Mullein", 
                                   matrix(1, ncol = 8, nrow = 4)))
colnames(mullein_tmp) <- colnames(pcp_sum)

for(i in 1:10) {
  if(i < 3) {
    mullein_tmp[, i] <- as.character(mullein_tmp[, i])
  } 
  class(mullein_tmp[, i]) <- class(pcp_sum[, i])
}

mullein_tmp$ErrStd <- NA

pcp_sum2 <- rbind(pcp_sum, mullein_tmp)
pcp_sum2 <- pcp_sum2[order(pcp_sum2$Optimizer), ]

cols <- brewer.pal(8, "Paired")[c(1:4, 7:8)]
# cols <- brewer.pal(8, "Dark2")[c(1:5, 8)]

g1 <- ggplot(pcp_sum, aes(x = Optimizer, y = ErrStd, group = factor(cat))) +
  geom_path(aes(color = factor(cat)), lwd = 1.2) +
  scale_color_manual("", values = cols) +
  labs(title ="Standardized Errors", x = "", y = "Standardized Error") +
  theme_bw()

g2 <- ggplot(pcp_sum2, aes(x = Optimizer, y = TimeStd, group = factor(cat))) +
  geom_path(aes(color = factor(cat)), lwd = 1.2) +
  scale_color_manual("", values = cols) +
  labs(title ="Standardized Computation Time", x = "", y = "Standardized Time") +
  theme_bw()


g5 <- ggplotGrob(g1)
id.legend <- grep("guide", g5$layout$name)
legend <- g5[["grobs"]][[id.legend]]
lwidth <- sum(legend$width)


grid.arrange(g1, g2, ncol = 1)

pdf("time_opt_pcp.pdf", height = 5.5, width = 8)
grid.arrange(g1 + theme(legend.position="none"), 
             g2 + theme(legend.position="none"), 
             legend,
             layout_matrix = rbind(c(1,1,1,1,3), 
                                   c(2,2,2,2,3))
)
dev.off()



