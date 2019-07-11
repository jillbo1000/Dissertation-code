#!/usr/bin/env Rscript
library("R.utils")
library(gwas3)
library(scales)
library(RColorBrewer)

num = cmdArgs()
i <- as.integer(num[[1]])
j <- as.integer(num[[2]])

geno <- read.csv("../rhag/rhag_geno.csv")
head(geno[, 1:10])
# colnames(geno) <- paste("rs", 1:ncol(geno), sep = "")# Set working directory to source file

# i <- j <- k <- l <- m <- n <- p <- 1

# Bring in data using a loop
set <- "rhag"
h <- c(1, 3, 8)
size <- c(5, 10, 50)
filt <- c("dc", "lm", "lm_fdr")
win <- c(1, 10)
datnum <- c("ca", "cb", "cc", "da", "db", "dc")
datnum2 <- c("Continuous 1", "Continuous 2", "Continuous 3", "Discrete 1", "Discrete 2", "Discrete 3")
filtnum <- c(1, 2)
filtnum2 <- c("Small filter", "Large filter")
en <- paste("en", c(0, "005", "01", 1, 25, 5, 75, 9, 10), sep = "")
en2 <- c(0, 0.005, 0.01, 0.1, 0.25, 0.5, 0.75, 0.9, 1)

cols <- brewer.pal(4, "Set1")

# Begin plot
d1 <- paste("../", set, "/", set, "_h0", h[i], "_", size[j], 
            "/", sep = "")
f1 <- paste(set, "_results_plots_h0", h[i], "_", size[j], 
            ".pdf", sep = "")
pdf(paste(d1, f1, sep = ""), height = 10, width = 12)
for(k in length(filt):1) {
  for(l in 1:length(win)) {
    for(m in 1:length(filtnum)) {
      for(n in 1:length(datnum)) {
        
        d <- paste("../", set, "/", set, "_h0", h[i], "_", size[j], 
                   "/results/", sep = "")
        f <- paste(set, "_h0", h[i], "_", size[j], "_", filt[k], "_w", win[l], "_", 
                   datnum[n], "_", filtnum[m], ".csv", sep = "")
        
        deffects <- paste("../", set, "/", set, "_h0", h[i], "_", size[j], 
                          "/", sep = "")
        feffects <- paste(set, "_effect_h0", h[i], "_", size[j], ".csv", sep = "")
        
        ffilt <- paste(set, "_filter_h0", h[i], "_", size[j], ".csv", sep = "")
        
        res <- read.csv(paste(d, f, sep = ""))
        effects <- read.csv(paste(deffects, feffects, sep = ""))
        pvals <- read.csv(paste(deffects, ffilt, sep = ""))
        
        snps <- res$snps[!is.na(res$snps)]
        
        eff <- effects[, n]
        
        
        if(k == 1) pvals <- pvals[, 1:12]
        if(k == 3) pvals <- pvals[, 13:24]
        if(k == 2) pvals <- pvals[, 25:36]
        
        # pvals <- pvals[, grepl(paste(filt[k], "_", sep = ""), colnames(pvals))]
        pvals <- pvals[, grepl(paste("_", datnum[n], sep = ""), colnames(pvals))]
        pvals <- pvals[, grepl(paste("w", win[l], "_", sep = ""), colnames(pvals))]
        
        pvals <- ifelse(pvals < 1.0e-10, 1.0e-10, pvals) 
        pvals <- ifelse(pvals > 1, NA, pvals) 
        if(filt[k] != "dc") pvals <- -log10(pvals)
        pvals[is.na(pvals)] <- 0
        
        tree <- res[, grepl("tree", colnames(res))]
        enres <- res[, 2:10]
        rfimp <- res[, grepl("rfimp", colnames(res))]
        rf <- res[, grepl("rf", colnames(res))]
        rf <- rf[, !grepl("rfimp", colnames(rf))]
        
        plotrange <- -max(pvals) * 0.6
        enheight <- -max(pvals) * 0.2
        trheight <- -max(pvals) * 0.35
        rfheight <- -max(pvals) * 0.5
        buffer <- -ncol(geno) / 20
        
        ttl <- paste("R. pomonella, Heritability = 0.", h[i], ", ", size[j], 
                     " Effective SNPs, ", "\n Filter = ", filt[k], ", ",
                     "Window = ", win[l], ", ", filtnum2[m], ", Data = ", datnum2[n], sep = "")
        
        par(mfrow=c(3,3),oma = c(0, 0, 4, 0))
        for(p in 1:length(en)) {
          tr <- tree[, p]
          tr <- as.numeric(gsub("snp_rs", "", as.character(tr[!is.na(tr)])))
          
          enp <- enres[, p]
          enp <- as.numeric(gsub("rs", "", as.character(enp[!is.na(enp)])))
          
          rfp <- rf[, p]
          rfp <- as.numeric(gsub("rs", "", as.character(rfp[!is.na(rfp)])))
          
          rfip <- rfimp[, p]
          rfip <- rfip[!is.na(rfip)]
          
          if(length(enp) > 0) {
            plot(1:ncol(geno), pvals, pch = 16, cex = 0.5, 
                 col = alpha("gray40", 0.6), xlab = "", 
                 ylab = "Filter Value", xlim = c(buffer, ncol(geno)), 
                 ylim = c(plotrange, max(pvals)), 
                 main = paste("alpha = ", en2[p], sep = ""))
            abline(h = 0)
            abline(v = 0)
            text(buffer, enheight, labels = "EN", adj = 0.5, 
                 offset = 0, cex = 0.8)
            text(buffer, trheight, labels = "TR", adj = 0.5, 
                 offset = 0, cex = 0.8)
            text(buffer, rfheight, labels = "RF", adj = 0.5, 
                 offset = 0, cex = 0.8)
            for(z in 1:length(snps)) {
              ht <- max(pvals) * abs(eff[z]) / max(abs(eff))
              lines(c(snps[z], snps[z]), c(plotrange, ht), col = cols[1])
            }
            points(enp, rep(enheight, length(enp)), pch = 16, 
                   cex = 0.7, col = alpha(cols[4], 0.9))
            if(length(tr) > 0) {
              points(tr, rep(trheight, length(tr)), pch = 16, cex = 1, 
                     col = cols[2])
            }
            symbols(rfp, rep(rfheight, length(rfp)), 
                    circles = (rfip - min(rfip)) / max(rfip - min(rfip)), 
                    inches = 0.05, add = TRUE, bg = cols[3], fg = "black")
            
          } else {
            plot(1, type="n", axes=F, xlab="", ylab="", 
                 main = paste("alpha = ", en2[p], sep = ""))
          }
          
        }
        # plot(1, type="n", axes=F, xlab="", ylab="")
        mtext(ttl, outer = TRUE, cex = 1.5)
        print(ttl)
        
      }
    }
  }
}
dev.off()

