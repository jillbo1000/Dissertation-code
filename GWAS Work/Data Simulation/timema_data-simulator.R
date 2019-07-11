library(gwas3)

timema <- read.csv("C:/Users/jflun/Dropbox/Dissertation/LASSO_EN_Project/Data/timema/timema_geno.csv", 
                 header = FALSE)
head(timema[, 1:10])
dim(timema)
snps <- paste("rs", 1:ncol(timema), sep = "")
colnames(timema) <- snps

#------------------------------------------------------------------------------
#                         5 important SNPs with h = 0.3
#------------------------------------------------------------------------------

set.seed(4589846)
timema_5_h0.3_cont_a <- gwas_sim(timema, num_snps = 5, heretability = 0.3)
timema_5_h0.3_cont_b <- gwas_sim(timema, num_snps = 5, heretability = 0.3)
timema_5_h0.3_cont_c <- gwas_sim(timema, num_snps = 5, heretability = 0.3)

timema_5_h0.3_dis_a <- gwas_sim(timema, num_snps = 5, heretability = 0.3)
timema_5_h0.3_dis_b <- gwas_sim(timema, num_snps = 5, heretability = 0.3)
timema_5_h0.3_dis_c <- gwas_sim(timema, num_snps = 5, heretability = 0.3)

timema_5_h0.3_dis_a$phenotype <- ifelse(timema_5_h0.3_dis_a$phenotype > median(timema_5_h0.3_dis_a$phenotype), 
                                       0, 1)
timema_5_h0.3_dis_b$phenotype <- ifelse(timema_5_h0.3_dis_b$phenotype > median(timema_5_h0.3_dis_b$phenotype), 
                                      0, 1)
timema_5_h0.3_dis_c$phenotype <- ifelse(timema_5_h0.3_dis_c$phenotype > median(timema_5_h0.3_dis_c$phenotype), 
                                       0, 1)

pheno_5_h0.3 <- cbind(timema_5_h0.3_cont_a$phenotype, 
                      timema_5_h0.3_cont_b$phenotype, 
                      timema_5_h0.3_cont_c$phenotype, 
                      timema_5_h0.3_dis_a$phenotype, 
                      timema_5_h0.3_dis_b$phenotype, 
                      timema_5_h0.3_dis_c$phenotype)

snps_5_h0.3 <- cbind(timema_5_h0.3_cont_a$functional_snps, 
                     timema_5_h0.3_cont_b$functional_snps, 
                     timema_5_h0.3_cont_c$functional_snps, 
                     timema_5_h0.3_dis_a$functional_snps, 
                     timema_5_h0.3_dis_b$functional_snps, 
                     timema_5_h0.3_dis_c$functional_snps)

effect_5_h0.3 <- cbind(timema_5_h0.3_cont_a$effect, 
                       timema_5_h0.3_cont_b$effect, 
                       timema_5_h0.3_cont_c$effect, 
                       timema_5_h0.3_dis_a$effect, 
                       timema_5_h0.3_dis_b$effect, 
                       timema_5_h0.3_dis_c$effect)

her_5_h0.3 <- cbind(timema_5_h0.3_cont_a$estimated_heretability, 
                    timema_5_h0.3_cont_b$estimated_heretability, 
                    timema_5_h0.3_cont_c$estimated_heretability, 
                    timema_5_h0.3_dis_a$estimated_heretability, 
                    timema_5_h0.3_dis_b$estimated_heretability, 
                    timema_5_h0.3_dis_c$estimated_heretability)

colnames(pheno_5_h0.3) <- c("timema_5_0.3_cont_a", "timema_5_0.3_cont_b", "timema_5_0.3_cont_c", 
                            "timema_5_0.3_dis_a", "timema_5_0.3_dis_b", "timema_5_0.3_dis_c")
colnames(snps_5_h0.3) <- colnames(pheno_5_h0.3) 
colnames(effect_5_h0.3) <- colnames(pheno_5_h0.3) 
colnames(her_5_h0.3) <- colnames(pheno_5_h0.3)

head(pheno_5_h0.3)
dim(pheno_5_h0.3)
head(snps_5_h0.3)
dim(snps_5_h0.3)
head(effect_5_h0.3)
dim(effect_5_h0.3)
her_5_h0.3

write.csv(pheno_5_h0.3, "timema_pheno_h03_5.csv", row.names = FALSE)
write.csv(snps_5_h0.3, "timema_snps_h03_5.csv", row.names = FALSE)
write.csv(effect_5_h0.3, "timema_effect_h03_5.csv", row.names = FALSE)
write.csv(her_5_h0.3, "timema_her_h03_5.csv", row.names = FALSE)


#------------------------------------------------------------------------------
#                         5 important SNPs with h = 0.1
#------------------------------------------------------------------------------

set.seed(6548984)
timema_5_h0.1_cont_a <- gwas_sim(timema, num_snps = 5, heretability = 0.1)
timema_5_h0.1_cont_b <- gwas_sim(timema, num_snps = 5, heretability = 0.1)
timema_5_h0.1_cont_c <- gwas_sim(timema, num_snps = 5, heretability = 0.1)

timema_5_h0.1_dis_a <- gwas_sim(timema, num_snps = 5, heretability = 0.1)
timema_5_h0.1_dis_b <- gwas_sim(timema, num_snps = 5, heretability = 0.1)
timema_5_h0.1_dis_c <- gwas_sim(timema, num_snps = 5, heretability = 0.1)

timema_5_h0.1_dis_a$phenotype <- ifelse(timema_5_h0.1_dis_a$phenotype > median(timema_5_h0.1_dis_a$phenotype), 
                                       0, 1)
timema_5_h0.1_dis_b$phenotype <- ifelse(timema_5_h0.1_dis_b$phenotype > median(timema_5_h0.1_dis_b$phenotype), 
                                       0, 1)
timema_5_h0.1_dis_c$phenotype <- ifelse(timema_5_h0.1_dis_c$phenotype > median(timema_5_h0.1_dis_c$phenotype), 
                                       0, 1)

pheno_5_h0.1 <- cbind(timema_5_h0.1_cont_a$phenotype, 
                      timema_5_h0.1_cont_b$phenotype, 
                      timema_5_h0.1_cont_c$phenotype, 
                      timema_5_h0.1_dis_a$phenotype, 
                      timema_5_h0.1_dis_b$phenotype, 
                      timema_5_h0.1_dis_c$phenotype)

snps_5_h0.1 <- cbind(timema_5_h0.1_cont_a$functional_snps, 
                     timema_5_h0.1_cont_b$functional_snps, 
                     timema_5_h0.1_cont_c$functional_snps, 
                     timema_5_h0.1_dis_a$functional_snps, 
                     timema_5_h0.1_dis_b$functional_snps, 
                     timema_5_h0.1_dis_c$functional_snps)

effect_5_h0.1 <- cbind(timema_5_h0.1_cont_a$effect, 
                       timema_5_h0.1_cont_b$effect, 
                       timema_5_h0.1_cont_c$effect, 
                       timema_5_h0.1_dis_a$effect, 
                       timema_5_h0.1_dis_b$effect, 
                       timema_5_h0.1_dis_c$effect)

her_5_h0.1 <- cbind(timema_5_h0.1_cont_a$estimated_heretability, 
                    timema_5_h0.1_cont_b$estimated_heretability, 
                    timema_5_h0.1_cont_c$estimated_heretability, 
                    timema_5_h0.1_dis_a$estimated_heretability, 
                    timema_5_h0.1_dis_b$estimated_heretability, 
                    timema_5_h0.1_dis_c$estimated_heretability)

colnames(pheno_5_h0.1) <- c("timema_5_0.1_cont_a", "timema_5_0.1_cont_b", "timema_5_0.1_cont_c", 
                            "timema_5_0.1_dis_a", "timema_5_0.1_dis_b", "timema_5_0.1_dis_c")
colnames(snps_5_h0.1) <- colnames(pheno_5_h0.1) 
colnames(effect_5_h0.1) <- colnames(pheno_5_h0.1) 
colnames(her_5_h0.1) <- colnames(pheno_5_h0.1)

head(pheno_5_h0.1)
dim(pheno_5_h0.1)
head(snps_5_h0.1)
dim(snps_5_h0.1)
head(effect_5_h0.1)
dim(effect_5_h0.1)
her_5_h0.1

write.csv(pheno_5_h0.1, "timema_pheno_h01_5.csv", row.names = FALSE)
write.csv(snps_5_h0.1, "timema_snps_h01_5.csv", row.names = FALSE)
write.csv(effect_5_h0.1, "timema_effect_h01_5.csv", row.names = FALSE)
write.csv(her_5_h0.1, "timema_her_h01_5.csv", row.names = FALSE)



#------------------------------------------------------------------------------
#                         10 important SNPs with h = 0.3
#------------------------------------------------------------------------------

set.seed(65484545)
timema_10_h0.3_cont_a <- gwas_sim(timema, num_snps = 10, heretability = 0.3)
timema_10_h0.3_cont_b <- gwas_sim(timema, num_snps = 10, heretability = 0.3)
timema_10_h0.3_cont_c <- gwas_sim(timema, num_snps = 10, heretability = 0.3)

timema_10_h0.3_dis_a <- gwas_sim(timema, num_snps = 10, heretability = 0.3)
timema_10_h0.3_dis_b <- gwas_sim(timema, num_snps = 10, heretability = 0.3)
timema_10_h0.3_dis_c <- gwas_sim(timema, num_snps = 10, heretability = 0.3)

timema_10_h0.3_dis_a$phenotype <- ifelse(timema_10_h0.3_dis_a$phenotype > median(timema_10_h0.3_dis_a$phenotype), 
                                       0, 1)
timema_10_h0.3_dis_b$phenotype <- ifelse(timema_10_h0.3_dis_b$phenotype > median(timema_10_h0.3_dis_b$phenotype), 
                                       0, 1)
timema_10_h0.3_dis_c$phenotype <- ifelse(timema_10_h0.3_dis_c$phenotype > median(timema_10_h0.3_dis_c$phenotype), 
                                       0, 1)

pheno_10_h0.3 <- cbind(timema_10_h0.3_cont_a$phenotype, 
                      timema_10_h0.3_cont_b$phenotype, 
                      timema_10_h0.3_cont_c$phenotype, 
                      timema_10_h0.3_dis_a$phenotype, 
                      timema_10_h0.3_dis_b$phenotype, 
                      timema_10_h0.3_dis_c$phenotype)

snps_10_h0.3 <- cbind(timema_10_h0.3_cont_a$functional_snps, 
                     timema_10_h0.3_cont_b$functional_snps, 
                     timema_10_h0.3_cont_c$functional_snps, 
                     timema_10_h0.3_dis_a$functional_snps, 
                     timema_10_h0.3_dis_b$functional_snps, 
                     timema_10_h0.3_dis_c$functional_snps)

effect_10_h0.3 <- cbind(timema_10_h0.3_cont_a$effect, 
                       timema_10_h0.3_cont_b$effect, 
                       timema_10_h0.3_cont_c$effect, 
                       timema_10_h0.3_dis_a$effect, 
                       timema_10_h0.3_dis_b$effect, 
                       timema_10_h0.3_dis_c$effect)

her_10_h0.3 <- cbind(timema_10_h0.3_cont_a$estimated_heretability, 
                    timema_10_h0.3_cont_b$estimated_heretability, 
                    timema_10_h0.3_cont_c$estimated_heretability, 
                    timema_10_h0.3_dis_a$estimated_heretability, 
                    timema_10_h0.3_dis_b$estimated_heretability, 
                    timema_10_h0.3_dis_c$estimated_heretability)

colnames(pheno_10_h0.3) <- c("timema_10_0.3_cont_a", "timema_10_0.3_cont_b", "timema_10_0.3_cont_c", 
                            "timema_10_0.3_dis_a", "timema_10_0.3_dis_b", "timema_10_0.3_dis_c")
colnames(snps_10_h0.3) <- colnames(pheno_10_h0.3) 
colnames(effect_10_h0.3) <- colnames(pheno_10_h0.3) 
colnames(her_10_h0.3) <- colnames(pheno_10_h0.3)

head(pheno_10_h0.3)
dim(pheno_10_h0.3)
head(snps_10_h0.3)
dim(snps_10_h0.3)
head(effect_10_h0.3)
dim(effect_10_h0.3)
her_10_h0.3

write.csv(pheno_10_h0.3, "timema_pheno_h03_10.csv", row.names = FALSE)
write.csv(snps_10_h0.3, "timema_snps_h03_10.csv", row.names = FALSE)
write.csv(effect_10_h0.3, "timema_effect_h03_10.csv", row.names = FALSE)
write.csv(her_10_h0.3, "timema_her_h03_10.csv", row.names = FALSE)


#------------------------------------------------------------------------------
#                         10 important SNPs with h = 0.1
#------------------------------------------------------------------------------

set.seed(549879849)
timema_10_h0.1_cont_a <- gwas_sim(timema, num_snps = 10, heretability = 0.1)
timema_10_h0.1_cont_b <- gwas_sim(timema, num_snps = 10, heretability = 0.1)
timema_10_h0.1_cont_c <- gwas_sim(timema, num_snps = 10, heretability = 0.1)

timema_10_h0.1_dis_a <- gwas_sim(timema, num_snps = 10, heretability = 0.1)
timema_10_h0.1_dis_b <- gwas_sim(timema, num_snps = 10, heretability = 0.1)
timema_10_h0.1_dis_c <- gwas_sim(timema, num_snps = 10, heretability = 0.1)

timema_10_h0.1_dis_a$phenotype <- ifelse(timema_10_h0.1_dis_a$phenotype > median(timema_10_h0.1_dis_a$phenotype), 
                                       0, 1)
timema_10_h0.1_dis_b$phenotype <- ifelse(timema_10_h0.1_dis_b$phenotype > median(timema_10_h0.1_dis_b$phenotype), 
                                       0, 1)
timema_10_h0.1_dis_c$phenotype <- ifelse(timema_10_h0.1_dis_c$phenotype > median(timema_10_h0.1_dis_c$phenotype), 
                                       0, 1)

pheno_10_h0.1 <- cbind(timema_10_h0.1_cont_a$phenotype, 
                      timema_10_h0.1_cont_b$phenotype, 
                      timema_10_h0.1_cont_c$phenotype, 
                      timema_10_h0.1_dis_a$phenotype, 
                      timema_10_h0.1_dis_b$phenotype, 
                      timema_10_h0.1_dis_c$phenotype)

snps_10_h0.1 <- cbind(timema_10_h0.1_cont_a$functional_snps, 
                     timema_10_h0.1_cont_b$functional_snps, 
                     timema_10_h0.1_cont_c$functional_snps, 
                     timema_10_h0.1_dis_a$functional_snps, 
                     timema_10_h0.1_dis_b$functional_snps, 
                     timema_10_h0.1_dis_c$functional_snps)

effect_10_h0.1 <- cbind(timema_10_h0.1_cont_a$effect, 
                       timema_10_h0.1_cont_b$effect, 
                       timema_10_h0.1_cont_c$effect, 
                       timema_10_h0.1_dis_a$effect, 
                       timema_10_h0.1_dis_b$effect, 
                       timema_10_h0.1_dis_c$effect)

her_10_h0.1 <- cbind(timema_10_h0.1_cont_a$estimated_heretability, 
                    timema_10_h0.1_cont_b$estimated_heretability, 
                    timema_10_h0.1_cont_c$estimated_heretability, 
                    timema_10_h0.1_dis_a$estimated_heretability, 
                    timema_10_h0.1_dis_b$estimated_heretability, 
                    timema_10_h0.1_dis_c$estimated_heretability)

colnames(pheno_10_h0.1) <- c("timema_10_0.1_cont_a", "timema_10_0.1_cont_b", "timema_10_0.1_cont_c", 
                            "timema_10_0.1_dis_a", "timema_10_0.1_dis_b", "timema_10_0.1_dis_c")
colnames(snps_10_h0.1) <- colnames(pheno_10_h0.1) 
colnames(effect_10_h0.1) <- colnames(pheno_10_h0.1) 
colnames(her_10_h0.1) <- colnames(pheno_10_h0.1)

head(pheno_10_h0.1)
dim(pheno_10_h0.1)
head(snps_10_h0.1)
dim(snps_10_h0.1)
head(effect_10_h0.1)
dim(effect_10_h0.1)
her_10_h0.1

write.csv(pheno_10_h0.1, "timema_pheno_h01_10.csv", row.names = FALSE)
write.csv(snps_10_h0.1, "timema_snps_h01_10.csv", row.names = FALSE)
write.csv(effect_10_h0.1, "timema_effect_h01_10.csv", row.names = FALSE)
write.csv(her_10_h0.1, "timema_her_h01_10.csv", row.names = FALSE)


#------------------------------------------------------------------------------
#                         50 important SNPs with h = 0.3
#------------------------------------------------------------------------------

set.seed(35489458)
timema_50_h0.3_cont_a <- gwas_sim(timema, num_snps = 50, heretability = 0.3)
timema_50_h0.3_cont_b <- gwas_sim(timema, num_snps = 50, heretability = 0.3)
timema_50_h0.3_cont_c <- gwas_sim(timema, num_snps = 50, heretability = 0.3)

timema_50_h0.3_dis_a <- gwas_sim(timema, num_snps = 50, heretability = 0.3)
timema_50_h0.3_dis_b <- gwas_sim(timema, num_snps = 50, heretability = 0.3)
timema_50_h0.3_dis_c <- gwas_sim(timema, num_snps = 50, heretability = 0.3)

timema_50_h0.3_dis_a$phenotype <- ifelse(timema_50_h0.3_dis_a$phenotype > median(timema_50_h0.3_dis_a$phenotype), 
                                        0, 1)
timema_50_h0.3_dis_b$phenotype <- ifelse(timema_50_h0.3_dis_b$phenotype > median(timema_50_h0.3_dis_b$phenotype), 
                                        0, 1)
timema_50_h0.3_dis_c$phenotype <- ifelse(timema_50_h0.3_dis_c$phenotype > median(timema_50_h0.3_dis_c$phenotype), 
                                        0, 1)

pheno_50_h0.3 <- cbind(timema_50_h0.3_cont_a$phenotype, 
                       timema_50_h0.3_cont_b$phenotype, 
                       timema_50_h0.3_cont_c$phenotype, 
                       timema_50_h0.3_dis_a$phenotype, 
                       timema_50_h0.3_dis_b$phenotype, 
                       timema_50_h0.3_dis_c$phenotype)

snps_50_h0.3 <- cbind(timema_50_h0.3_cont_a$functional_snps, 
                      timema_50_h0.3_cont_b$functional_snps, 
                      timema_50_h0.3_cont_c$functional_snps, 
                      timema_50_h0.3_dis_a$functional_snps, 
                      timema_50_h0.3_dis_b$functional_snps, 
                      timema_50_h0.3_dis_c$functional_snps)

effect_50_h0.3 <- cbind(timema_50_h0.3_cont_a$effect, 
                        timema_50_h0.3_cont_b$effect, 
                        timema_50_h0.3_cont_c$effect, 
                        timema_50_h0.3_dis_a$effect, 
                        timema_50_h0.3_dis_b$effect, 
                        timema_50_h0.3_dis_c$effect)

her_50_h0.3 <- cbind(timema_50_h0.3_cont_a$estimated_heretability, 
                     timema_50_h0.3_cont_b$estimated_heretability, 
                     timema_50_h0.3_cont_c$estimated_heretability, 
                     timema_50_h0.3_dis_a$estimated_heretability, 
                     timema_50_h0.3_dis_b$estimated_heretability, 
                     timema_50_h0.3_dis_c$estimated_heretability)

colnames(pheno_50_h0.3) <- c("timema_50_0.3_cont_a", "timema_50_0.3_cont_b", "timema_50_0.3_cont_c", 
                             "timema_50_0.3_dis_a", "timema_50_0.3_dis_b", "timema_50_0.3_dis_c")
colnames(snps_50_h0.3) <- colnames(pheno_50_h0.3) 
colnames(effect_50_h0.3) <- colnames(pheno_50_h0.3) 
colnames(her_50_h0.3) <- colnames(pheno_50_h0.3)

head(pheno_50_h0.3)
dim(pheno_50_h0.3)
head(snps_50_h0.3)
dim(snps_50_h0.3)
head(effect_50_h0.3)
dim(effect_50_h0.3)
her_50_h0.3

write.csv(pheno_50_h0.3, "timema_pheno_h03_50.csv", row.names = FALSE)
write.csv(snps_50_h0.3, "timema_snps_h03_50.csv", row.names = FALSE)
write.csv(effect_50_h0.3, "timema_effect_h03_50.csv", row.names = FALSE)
write.csv(her_50_h0.3, "timema_her_h03_50.csv", row.names = FALSE)


#------------------------------------------------------------------------------
#                         50 important SNPs with h = 0.1
#------------------------------------------------------------------------------

set.seed(489845654)
timema_50_h0.1_cont_a <- gwas_sim(timema, num_snps = 50, heretability = 0.1)
timema_50_h0.1_cont_b <- gwas_sim(timema, num_snps = 50, heretability = 0.1)
timema_50_h0.1_cont_c <- gwas_sim(timema, num_snps = 50, heretability = 0.1)

timema_50_h0.1_dis_a <- gwas_sim(timema, num_snps = 50, heretability = 0.1)
timema_50_h0.1_dis_b <- gwas_sim(timema, num_snps = 50, heretability = 0.1)
timema_50_h0.1_dis_c <- gwas_sim(timema, num_snps = 50, heretability = 0.1)

timema_50_h0.1_dis_a$phenotype <- ifelse(timema_50_h0.1_dis_a$phenotype > median(timema_50_h0.1_dis_a$phenotype), 
                                        0, 1)
timema_50_h0.1_dis_b$phenotype <- ifelse(timema_50_h0.1_dis_b$phenotype > median(timema_50_h0.1_dis_b$phenotype), 
                                        0, 1)
timema_50_h0.1_dis_c$phenotype <- ifelse(timema_50_h0.1_dis_c$phenotype > median(timema_50_h0.1_dis_c$phenotype), 
                                        0, 1)

pheno_50_h0.1 <- cbind(timema_50_h0.1_cont_a$phenotype, 
                       timema_50_h0.1_cont_b$phenotype, 
                       timema_50_h0.1_cont_c$phenotype, 
                       timema_50_h0.1_dis_a$phenotype, 
                       timema_50_h0.1_dis_b$phenotype, 
                       timema_50_h0.1_dis_c$phenotype)

snps_50_h0.1 <- cbind(timema_50_h0.1_cont_a$functional_snps, 
                      timema_50_h0.1_cont_b$functional_snps, 
                      timema_50_h0.1_cont_c$functional_snps, 
                      timema_50_h0.1_dis_a$functional_snps, 
                      timema_50_h0.1_dis_b$functional_snps, 
                      timema_50_h0.1_dis_c$functional_snps)

effect_50_h0.1 <- cbind(timema_50_h0.1_cont_a$effect, 
                        timema_50_h0.1_cont_b$effect, 
                        timema_50_h0.1_cont_c$effect, 
                        timema_50_h0.1_dis_a$effect, 
                        timema_50_h0.1_dis_b$effect, 
                        timema_50_h0.1_dis_c$effect)

her_50_h0.1 <- cbind(timema_50_h0.1_cont_a$estimated_heretability, 
                     timema_50_h0.1_cont_b$estimated_heretability, 
                     timema_50_h0.1_cont_c$estimated_heretability, 
                     timema_50_h0.1_dis_a$estimated_heretability, 
                     timema_50_h0.1_dis_b$estimated_heretability, 
                     timema_50_h0.1_dis_c$estimated_heretability)

colnames(pheno_50_h0.1) <- c("timema_50_0.1_cont_a", "timema_50_0.1_cont_b", "timema_50_0.1_cont_c", 
                             "timema_50_0.1_dis_a", "timema_50_0.1_dis_b", "timema_50_0.1_dis_c")
colnames(snps_50_h0.1) <- colnames(pheno_50_h0.1) 
colnames(effect_50_h0.1) <- colnames(pheno_50_h0.1) 
colnames(her_50_h0.1) <- colnames(pheno_50_h0.1)

head(pheno_50_h0.1)
dim(pheno_50_h0.1)
head(snps_50_h0.1)
dim(snps_50_h0.1)
head(effect_50_h0.1)
dim(effect_50_h0.1)
her_50_h0.1

write.csv(pheno_50_h0.1, "timema_pheno_h01_50.csv", row.names = FALSE)
write.csv(snps_50_h0.1, "timema_snps_h01_50.csv", row.names = FALSE)
write.csv(effect_50_h0.1, "timema_effect_h01_50.csv", row.names = FALSE)
write.csv(her_50_h0.1, "timema_her_h01_50.csv", row.names = FALSE)

