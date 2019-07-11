library("R.utils")
library(gwas3)

num <- cmdArgs()
num
data_name <- as.character(num[[1]])
data_name
her <- as.integer(num[[2]])
her
snps <- as.integer(num[[3]])
snps
type <- as.character(num[[4]])
type
win <- as.integer(num[[5]])
win
fdr <- as.logical(num[[6]])
fdr

# data_name <- "rhag"
# data_name
# her <- as.integer(3)
# her
# snps <- as.integer(10)
# snps
# type <- "dc"
# type
# win <- as.integer(1)
# win
# fdr <- as.logical(1)
# fdr


g <- paste(data_name, "/", data_name, "_geno.csv", sep = "")
g
geno <- read.csv(g)

# geno <- geno[, 1:1000]

d <- paste(data_name, "/", data_name, "_h0", her, "_", snps, "/", sep = "")

# Read in phenotypes
ph <- paste(data_name, "_pheno_h0", her, "_", snps, ".csv", sep = "")
pheno <- read.csv(paste(d, ph, sep = ""))

filt <- NULL

for(i in 1:6) {
  tmp <- snp_filter(geno, pheno[, i], type = type, window = win, fdr = fdr)
  filt <- cbind(filt, tmp)
}

id <- c("ca", "cb", "cc", "da", "db", "dc")
if(fdr == TRUE) id <- paste("fdr_", id, sep = "")

nms <- paste(type, "_h0", her, "_", snps, "_w", win, "_", id, sep = "")
colnames(filt) <- nms

fout <- paste(d, data_name, "_filter_", type, "_h0", her, "_", snps, "_w", win, sep = "")
if(fdr == TRUE) fout <- paste(fout, "_fdr", sep = "")
fout <- paste(fout, ".csv", sep = "")

write.csv(filt, fout, row.names = FALSE, quote = FALSE)
