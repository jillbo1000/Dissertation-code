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

data_sets <- c("Breast Cancer", "Ionosphere", "Lichen", "Mullein", "Pima", "Sonar")

bc <- get_eztune_results("../BreastCancer2", "Breast Cancer", "svm")
io <- get_eztune_results("../Ionosphere2", "Ionosphere", "svm")
li <- get_eztune_results("../Lichen2", "Lichen", "svm")
mu <- get_eztune_results("../Mullein2", "Mullein", "svm")
pi <- get_eztune_results("../Pima2", "Pima", "svm")
so <- get_eztune_results("../Sonar2", "Sonar", "svm")

io <- io[io$fast != 400, ]
so <- so[so$fast != 300, ]

# Bring in grid data to get best errors
best_err <- read.csv("best_err.csv")

# Add variables for standardized mses
bc.sum <- make_summary_eztune_table2(bc)
bc.sum$mean_perf <- 1 - bc.sum$mean_perf
bc.sum$mean_perf_cv <- 1 - bc.sum$mean_perf_cv
bc.sum$perf_std <- bc.sum$mean_perf_cv - min(bc.sum$mean_perf_cv)
bc.sum$perf_std <- bc.sum$perf_std / max(bc.sum$perf_std)
bc.sum$time_std <- bc.sum$mean_time - min(bc.sum$mean_time)
bc.sum$time_std <- bc.sum$time_std/ max(bc.sum$time_std)

io.sum <- make_summary_eztune_table2(io)
io.sum$mean_perf <- 1 - io.sum$mean_perf
io.sum$mean_perf_cv <- 1 - io.sum$mean_perf_cv
io.sum$perf_std <- io.sum$mean_perf_cv - min(io.sum$mean_perf_cv)
io.sum$perf_std <- io.sum$perf_std / max(io.sum$perf_std)
io.sum$time_std <- io.sum$mean_time - min(io.sum$mean_time)
io.sum$time_std <- io.sum$time_std/ max(io.sum$time_std)

li.sum <- make_summary_eztune_table2(li)
li.sum$mean_perf <- 1 - li.sum$mean_perf
li.sum$mean_perf_cv <- 1 - li.sum$mean_perf_cv
li.sum$perf_std <- li.sum$mean_perf_cv - min(li.sum$mean_perf_cv)
li.sum$perf_std <- li.sum$perf_std / max(li.sum$perf_std)
li.sum$time_std <- li.sum$mean_time - min(li.sum$mean_time)
li.sum$time_std <- li.sum$time_std/ max(li.sum$time_std)

mu.sum <- make_summary_eztune_table2(mu)
mu.sum$mean_perf <- 1 - mu.sum$mean_perf
mu.sum$mean_perf_cv <- 1 - mu.sum$mean_perf_cv
mu.sum$perf_std <- mu.sum$mean_perf_cv - min(mu.sum$mean_perf_cv)
mu.sum$perf_std <- mu.sum$perf_std / max(mu.sum$perf_std)
mu.sum$time_std <- mu.sum$mean_time - min(mu.sum$mean_time)
mu.sum$time_std <- mu.sum$time_std/ max(mu.sum$time_std)

pi.sum <- make_summary_eztune_table2(pi)
pi.sum$mean_perf <- 1 - pi.sum$mean_perf
pi.sum$mean_perf_cv <- 1 - pi.sum$mean_perf_cv
pi.sum$perf_std <- pi.sum$mean_perf_cv - min(pi.sum$mean_perf_cv)
pi.sum$perf_std <- pi.sum$perf_std / max(pi.sum$perf_std)
pi.sum$time_std <- pi.sum$mean_time - min(pi.sum$mean_time)
pi.sum$time_std <- pi.sum$time_std/ max(pi.sum$time_std)

so.sum <- make_summary_eztune_table2(so)
so.sum$mean_perf <- 1 - so.sum$mean_perf
so.sum$mean_perf_cv <- 1 - so.sum$mean_perf_cv
so.sum$perf_std <- so.sum$mean_perf_cv - min(so.sum$mean_perf_cv)
so.sum$perf_std <- so.sum$perf_std / max(so.sum$perf_std)
so.sum$time_std <- so.sum$mean_time - min(so.sum$mean_time)
so.sum$time_std <- so.sum$time_std/ max(so.sum$time_std)

std_dat <- rbind(bc.sum, io.sum, li.sum, mu.sum, pi.sum, so.sum)
dim(std_dat)
summary(std_dat)
unique(std_dat$Dataset)
std_dat

# Note that with binary eztune the performance measure is accuracy
# instead of error. This converts it so that the figures and tables
# are consistent throughout the document. 

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
  labs(title ="Support Vector Machine", x = "", y = "Standardized Error Rate") +
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

pdf("../Images/svm_bin_eztune.pdf", width = 6, height = 7.5)
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

svm_bin_results <- results_table(dat = std_dat, best = best_err, 
                                 datasets = data_sets, model = "SVM")

write.table(svm_bin_results[[2]], "../Tables/svm_bin_results.txt", row.names = FALSE, col.names = FALSE, quote = FALSE)

