# set working directory to source file

# File grabs all of the data for each dataset and then creates a summary 
# table for each dataset. Tables can be used to make graphs of time/error
# comparisons

library(ggplot2)
library(gridExtra)
library(RColorBrewer)

source("Get_eztune_results.R")
source("data_summary.R")
source("eztune_table.R")
source("C:/Users/jflun/Dropbox/Dissertation/Tuning Research/Optimization/Optimization Tests/Data Summary Scripts/Get_opt_data.R")
source("C:/Users/jflun/Dropbox/Dissertation/Tuning Research/Grid Search/Plot Scripts/get_data.R")

data_sets <- c("Abalone", "BostonHousing", "CO2", "Crime", "OhioHousing", "Union", "Wage")

ab <- get_eztune_results("../Abalone2", "Abalone", "svm")
bh <- get_eztune_results("../BostonHousing2", "Boston Housing", "svm")
co2 <- get_eztune_results("../CO22", "CO2", "svm")
cr <- get_eztune_results("../Crime2", "Crime", "svm")
oh <- get_eztune_results("../OhioHousing2", "Ohio Housing", "svm")
un <- get_eztune_results("../Union2", "Union", "svm")
wa <- get_eztune_results("../Wage2", "Wage", "svm")

# Bring in grid data to get best errors
best_mse <- read.csv("best_mse.csv")

# Add variables for standardized mses
ab.sum <- make_summary_eztune_table2(ab)
# ab.sum$perf_std <- ab.sum$mean_perf_cv - best_mse[1, 2] 
# ab.sum$perf_std <- ab.sum$perf_std / max(ab.sum$perf_std)
ab.sum$perf_std <- ab.sum$mean_perf_cv - min(ab.sum$mean_perf_cv)
ab.sum$perf_std <- ab.sum$perf_std / max(ab.sum$perf_std)
ab.sum$time_std <- ab.sum$mean_time - min(ab.sum$mean_time)
ab.sum$time_std <- ab.sum$time_std/ max(ab.sum$time_std)

bh.sum <- make_summary_eztune_table2(bh)
# bh.sum$perf_std <- bh.sum$mean_perf_cv - best_mse[2, 2] 
# bh.sum$perf_std <- bh.sum$perf_std / max(bh.sum$perf_std)
bh.sum$perf_std <- bh.sum$mean_perf_cv - min(bh.sum$mean_perf_cv)
bh.sum$perf_std <- bh.sum$perf_std / max(bh.sum$perf_std)
bh.sum$time_std <- bh.sum$mean_time - min(bh.sum$mean_time)
bh.sum$time_std <- bh.sum$time_std/ max(bh.sum$time_std)

co2.sum <- make_summary_eztune_table2(co2)
# co2.sum$perf_std <- co2.sum$mean_perf_cv - best_mse[3, 2] 
# co2.sum$perf_std <- co2.sum$perf_std / max(co2.sum$perf_std)
co2.sum$perf_std <- co2.sum$mean_perf_cv - min(co2.sum$mean_perf_cv)
co2.sum$perf_std <- co2.sum$perf_std / max(co2.sum$perf_std)
co2.sum$time_std <- co2.sum$mean_time - min(co2.sum$mean_time)
co2.sum$time_std <- co2.sum$time_std/ max(co2.sum$time_std)

cr.sum <- make_summary_eztune_table2(cr)
# cr.sum$perf_std <- cr.sum$mean_perf_cv - best_mse[4, 2] 
# cr.sum$perf_std <- cr.sum$perf_std / max(cr.sum$perf_std)
cr.sum$perf_std <- cr.sum$mean_perf_cv - min(cr.sum$mean_perf_cv)
cr.sum$perf_std <- cr.sum$perf_std / max(cr.sum$perf_std)
cr.sum$time_std <- cr.sum$mean_time - min(cr.sum$mean_time)
cr.sum$time_std <- cr.sum$time_std/ max(cr.sum$time_std)

oh.sum <- make_summary_eztune_table2(oh)
# oh.sum$perf_std <- oh.sum$mean_perf_cv - best_mse[5, 2] 
# oh.sum$perf_std <- oh.sum$perf_std / max(oh.sum$perf_std)
oh.sum$perf_std <- oh.sum$mean_perf_cv - min(oh.sum$mean_perf_cv)
oh.sum$perf_std <- oh.sum$perf_std / max(oh.sum$perf_std)
oh.sum$time_std <- oh.sum$mean_time - min(oh.sum$mean_time)
oh.sum$time_std <- oh.sum$time_std/ max(oh.sum$time_std)

un.sum <- make_summary_eztune_table2(un)
# un.sum$perf_std <- un.sum$mean_perf_cv - best_mse[6, 2] 
# un.sum$perf_std <- un.sum$perf_std / max(un.sum$perf_std)
un.sum$perf_std <- un.sum$mean_perf_cv - min(un.sum$mean_perf_cv)
un.sum$perf_std <- un.sum$perf_std / max(un.sum$perf_std)
un.sum$time_std <- un.sum$mean_time - min(un.sum$mean_time)
un.sum$time_std <- un.sum$time_std/ max(un.sum$time_std)

wa.sum <- make_summary_eztune_table2(wa)
# wa.sum$perf_std <- wa.sum$mean_perf_cv - best_mse[7, 2] 
# wa.sum$perf_std <- wa.sum$perf_std / max(wa.sum$perf_std)
wa.sum$perf_std <- wa.sum$mean_perf_cv - min(wa.sum$mean_perf_cv)
wa.sum$perf_std <- wa.sum$perf_std / max(wa.sum$perf_std)
wa.sum$time_std <- wa.sum$mean_time - min(wa.sum$mean_time)
wa.sum$time_std <- wa.sum$time_std/ max(wa.sum$time_std)

std_dat <- rbind(ab.sum, bh.sum, co2.sum, cr.sum, oh.sum, un.sum, wa.sum)
dim(std_dat)
summary(std_dat)
unique(std_dat$Dataset)
std_dat

# Set factor levels to make for better plotting
unique(std_dat$type)
type <- c("Resub", "CV = 10", "CV = 3", "Fast = TRUE", "Fast = 0.25", 
         "Fast = 0.5", "Fast = 0.75", "Fast = 0.9", "Fast = 100", 
         "Fast = 200", "Fast = 300", "Fast = 400")  
std_dat$type <- factor(std_dat$type, levels = type)
std_dat <- std_dat[order(std_dat$type), ]
std_dat$Optim <- ifelse(std_dat$optimizer == "hjn", "Hooke-Jeeves", "Genetic Algorithm")

cols <- brewer.pal(10, "Paired")[c(1:4, 7:10)]

g1 <- ggplot(std_dat, aes(x = type, y = perf_std, group = factor(Dataset))) +
  geom_path(aes(color = factor(Dataset)), lwd = 1.2) +
  scale_color_manual("", values = cols) +
  labs(title ="Support Vector Regression", x = "", y = "Standardized MSE") +
  facet_wrap(~Optim, ncol = 1) +
  theme_bw() +
  theme(axis.title.x=element_blank(),
        axis.text.x=element_blank(),
        axis.ticks.x=element_blank())

g2 <- ggplot(std_dat, aes(x = type, y = time_std, group = factor(Dataset))) +
  geom_path(aes(color = factor(Dataset)), lwd = 1.2) +
  scale_color_manual("", values = cols) +
  labs(x = "", y = "Standardized Time") +
  facet_wrap(~Optim, ncol = 1) +
  theme_bw() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1, vjust = 1))

g3 <- ggplotGrob(g1)
id.legend <- grep("guide", g3$layout$name)
legend <- g3[["grobs"]][[id.legend]]
lwidth <- sum(legend$width)

pdf("../Images/svm_reg_eztune.pdf", width = 6, height = 7.5)
grid.arrange(g1 + theme(legend.position="none"),
             legend,
             g2 + theme(legend.position="none"),
             # ncol = 2,
             widths = c(4, 1.3),
             # heights = unit(c(4, 5), c("cm", "cm"))
             heights = c(4.4, 5)
)
dev.off()

#------------------------------------------------------------------------------
#                             Make Table
#------------------------------------------------------------------------------

svm_reg_results <- results_table(dat = std_dat, best = best_mse, 
                                 datasets = data_sets, model = "SVM")

write.table(svm_reg_results[[2]], "../Tables/svm_reg_results.txt", row.names = FALSE, col.names = FALSE, quote = FALSE)

