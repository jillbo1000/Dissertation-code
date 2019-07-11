library("R.utils")
library(gwas3)
library(stringr)

num1 <- cmdArgs()
num1
dat <- as.character(num1[[1]])
dat
her <- as.integer(num1[[2]])
her
num <- as.integer(num1[[3]])
num
filt_num1 <- as.integer(num1[[4]])
filt_num1
filt_num2 <- as.integer(num1[[5]])
filt_num2
rand <- as.integer(num1[[6]])
rand

set.seed(rand)

# dat <- "rhag"
# her <- 1
# num <- 5
# filt_num1 <- 50
# filt_num2 <- 100
# rand <- 3428390

g <- paste("./", dat, "/", dat, "_geno.csv", sep = "")
geno <- read.csv(g)

pheno_folder <- paste("./", dat, "/", dat, "_h0", her, "_", num, "/", sep = "")
sfile <- paste(pheno_folder, dat, "_snps_h0", her, "_", num, ".csv", sep = "")
snps <- read.csv(sfile)
pfile <- paste(pheno_folder, dat, "_pheno_h0", her, "_", num, ".csv", sep = "")
pheno <- read.csv(pfile)
ffile <- paste(pheno_folder, dat, "_filter_h0", her, "_", num, ".csv", sep = "")
filt <- read.csv(ffile)

pheno_names <- c("ca", "cb", "cc", "da", "db", "dc")
pheno_names <- paste("_", pheno_names, sep = "")

alpha <- paste("en", c(0, 0.005, 0.01, 0.1, 0.25, 0.5, 0.75, 0.9, 10), sep = "")
alpha <- gsub("0\\.", "", alpha)


for(i in 1:ncol(pheno)) {
  # Extract filters for first phenotype
  filt_ph <- filt[, !is.na(str_extract(colnames(filt), pheno_names[i]))]
  
  for(j in 1:ncol(filt_ph)) {
    print(j)
    # Get the name for the filter/pheno combination
    parts <- unlist(strsplit(colnames(filt_ph)[j], "_"))
    win <- as.integer(substring(parts[4], 2))
    if("fdr" %in% parts) parts[1] <- "lm_fdr"
    
    nm <- paste(dat, parts[2], parts[3], parts[1], parts[4], sep = "_")
    nm_en <- paste(nm, alpha, sep = "_")
    nm_tree <- paste(nm_en, "tree", sep = "_")
    nm_rf <- paste(nm_en, "rf", sep = "_")
    nm_rfimp <- paste(nm_en, "rfimp", sep = "_")
    
    # Extract data from filter
    tmp1 <- filter_subset(geno, filt_ph[, j], type = parts[1], threshold = filt_num1, window = win)
    print("tmp1")
    tmp2 <- filter_subset(geno, filt_ph[, j], type = parts[1], threshold = filt_num2, window = win)
    print("tmp2")
    
    en1 <- elasticnet(tmp1, pheno[, i])
    en2 <- elasticnet(tmp2, pheno[, i])
    
    tree1 <- snp_tree(en1, pheno[, i])
    tree2 <- snp_tree(en2, pheno[, i])
    
    rf1 <- snp_rf(en1, pheno[, i])
    rf2 <- snp_rf(en2, pheno[, i])
    
    en1_dat <- matrix(NA, ncol = length(alpha), nrow = ncol(tmp1))
    tree1_dat <- matrix(NA, ncol = length(alpha), nrow = ncol(tmp1))
    rf1_dat <- matrix(NA, ncol = length(alpha), nrow = ncol(tmp1))
    rfimp1_dat <- matrix(NA, ncol = length(alpha), nrow = ncol(tmp1))
    
    en2_dat <- matrix(NA, ncol = length(alpha), nrow = ncol(tmp2))
    tree2_dat <- matrix(NA, ncol = length(alpha), nrow = ncol(tmp2))
    rf2_dat <- matrix(NA, ncol = length(alpha), nrow = ncol(tmp2))
    rfimp2_dat <- matrix(NA, ncol = length(alpha), nrow = ncol(tmp2))
    
    colnames(en1_dat) <- colnames(en2_dat) <- nm_en
    colnames(tree1_dat) <- colnames(tree2_dat) <- nm_tree
    colnames(rf1_dat) <- colnames(rf2_dat) <- nm_rf
    colnames(rfimp1_dat) <- colnames(rfimp2_dat) <- nm_rfimp
    
    for(k in 1:length(alpha)) {
      
      try(en1_dat[1:ncol(en1[[k]]$data), k] <- colnames(en1[[k]]$data))
      try(en2_dat[1:ncol(en2[[k]]$data), k] <- colnames(en2[[k]]$data))
      
      try(tree1_dat[1:length(tree1[[k]]$snps), k] <- tree1[[k]]$snps)
      try(tree2_dat[1:length(tree2[[k]]$snps), k] <- tree2[[k]]$snps)
      
      try(rf1_dat[1:length(rf1[[k]]$vi), k] <- row.names(rf1[[k]]$vi))
      try(rf2_dat[1:length(rf2[[k]]$vi), k] <- row.names(rf2[[k]]$vi))
      
      try(rfimp1_dat[1:length(rf1[[k]]$vi), k] <- rf1[[k]]$vi)
      try(rfimp2_dat[1:length(rf2[[k]]$vi), k] <- rf2[[k]]$vi)
      
    }
    
    f_snps1 <- rep(NA, nrow(en1_dat))
    f_snps1[1:length(snps[, i])] <- snps[, i]
    f_snps2 <- rep(NA, nrow(en2_dat))
    f_snps2[1:length(snps[, i])] <- snps[, i]
    
    dat1 <- cbind(f_snps1, en1_dat, tree1_dat, rf1_dat, rfimp1_dat)
    dat2 <- cbind(f_snps2, en2_dat, tree2_dat, rf2_dat, rfimp2_dat)
    
    colnames(dat1)[1] <- colnames(dat2)[1] <- "snps"
    
    ph_name1 <- paste(pheno_folder, "results/", nm, pheno_names[i], "_1.csv", sep = "")
    write.csv(dat1, ph_name1, row.names = FALSE)
    
    ph_name2 <- paste(pheno_folder, "results/", nm, pheno_names[i], "_2.csv", sep = "")
    write.csv(dat2, ph_name2, row.names = FALSE)
    
    
  }
}



