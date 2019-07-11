library(gwas3)

timema <- read.csv("C:/Users/jflun/Dropbox/Dissertation/LASSO_EN_Project/Data/timema/timema_geno.csv")
head(timema[, 1:10])

#------------------------------------------------------------------------------
#                         5 important SNPs with h = 0.8
#------------------------------------------------------------------------------

set.seed(234534242)
timema_5_h0.8_cont_a <- gwas_sim(timema, num_snps = 5, heretability = 0.8)
timema_5_h0.8_cont_b <- gwas_sim(timema, num_snps = 5, heretability = 0.8)
timema_5_h0.8_cont_c <- gwas_sim(timema, num_snps = 5, heretability = 0.8)

timema_5_h0.8_dis_a <- gwas_sim(timema, num_snps = 5, heretability = 0.8)
timema_5_h0.8_dis_b <- gwas_sim(timema, num_snps = 5, heretability = 0.8)
timema_5_h0.8_dis_c <- gwas_sim(timema, num_snps = 5, heretability = 0.8)

timema_5_h0.8_dis_a$phenotype <- ifelse(timema_5_h0.8_dis_a$phenotype > median(timema_5_h0.8_dis_a$phenotype), 
                                       0, 1)
timema_5_h0.8_dis_b$phenotype <- ifelse(timema_5_h0.8_dis_b$phenotype > median(timema_5_h0.8_dis_b$phenotype), 
                                       0, 1)
timema_5_h0.8_dis_c$phenotype <- ifelse(timema_5_h0.8_dis_c$phenotype > median(timema_5_h0.8_dis_c$phenotype), 
                                       0, 1)

pheno_5_h0.8 <- cbind(timema_5_h0.8_cont_a$phenotype, 
                      timema_5_h0.8_cont_b$phenotype, 
                      timema_5_h0.8_cont_c$phenotype, 
                      timema_5_h0.8_dis_a$phenotype, 
                      timema_5_h0.8_dis_b$phenotype, 
                      timema_5_h0.8_dis_c$phenotype)

snps_5_h0.8 <- cbind(timema_5_h0.8_cont_a$functional_snps, 
                     timema_5_h0.8_cont_b$functional_snps, 
                     timema_5_h0.8_cont_c$functional_snps, 
                     timema_5_h0.8_dis_a$functional_snps, 
                     timema_5_h0.8_dis_b$functional_snps, 
                     timema_5_h0.8_dis_c$functional_snps)

effect_5_h0.8 <- cbind(timema_5_h0.8_cont_a$effect, 
                       timema_5_h0.8_cont_b$effect, 
                       timema_5_h0.8_cont_c$effect, 
                       timema_5_h0.8_dis_a$effect, 
                       timema_5_h0.8_dis_b$effect, 
                       timema_5_h0.8_dis_c$effect)

her_5_h0.8 <- cbind(timema_5_h0.8_cont_a$estimated_heretability, 
                    timema_5_h0.8_cont_b$estimated_heretability, 
                    timema_5_h0.8_cont_c$estimated_heretability, 
                    timema_5_h0.8_dis_a$estimated_heretability, 
                    timema_5_h0.8_dis_b$estimated_heretability, 
                    timema_5_h0.8_dis_c$estimated_heretability)

colnames(pheno_5_h0.8) <- c("timema_5_0.8_cont_a", "timema_5_0.8_cont_b", "timema_5_0.8_cont_c", 
                            "timema_5_0.8_dis_a", "timema_5_0.8_dis_b", "timema_5_0.8_dis_c")
colnames(snps_5_h0.8) <- colnames(pheno_5_h0.8) 
colnames(effect_5_h0.8) <- colnames(pheno_5_h0.8) 
colnames(her_5_h0.8) <- colnames(pheno_5_h0.8)

head(pheno_5_h0.8)
dim(pheno_5_h0.8)
head(snps_5_h0.8)
dim(snps_5_h0.8)
head(effect_5_h0.8)
dim(effect_5_h0.8)
her_5_h0.8

write.csv(pheno_5_h0.8, "timema_pheno_h08_5.csv", row.names = FALSE)
write.csv(snps_5_h0.8, "timema_snps_h08_5.csv", row.names = FALSE)
write.csv(effect_5_h0.8, "timema_effect_h08_5.csv", row.names = FALSE)
write.csv(her_5_h0.8, "timema_her_h08_5.csv", row.names = FALSE)





#------------------------------------------------------------------------------
#                         10 important SNPs with h = 0.8
#------------------------------------------------------------------------------

set.seed(23454543)
timema_10_h0.8_cont_a <- gwas_sim(timema, num_snps = 10, heretability = 0.8)
timema_10_h0.8_cont_b <- gwas_sim(timema, num_snps = 10, heretability = 0.8)
timema_10_h0.8_cont_c <- gwas_sim(timema, num_snps = 10, heretability = 0.8)

timema_10_h0.8_dis_a <- gwas_sim(timema, num_snps = 10, heretability = 0.8)
timema_10_h0.8_dis_b <- gwas_sim(timema, num_snps = 10, heretability = 0.8)
timema_10_h0.8_dis_c <- gwas_sim(timema, num_snps = 10, heretability = 0.8)

timema_10_h0.8_dis_a$phenotype <- ifelse(timema_10_h0.8_dis_a$phenotype > median(timema_10_h0.8_dis_a$phenotype), 
                                       0, 1)
timema_10_h0.8_dis_b$phenotype <- ifelse(timema_10_h0.8_dis_b$phenotype > median(timema_10_h0.8_dis_b$phenotype), 
                                       0, 1)
timema_10_h0.8_dis_c$phenotype <- ifelse(timema_10_h0.8_dis_c$phenotype > median(timema_10_h0.8_dis_c$phenotype), 
                                       0, 1)

pheno_10_h0.8 <- cbind(timema_10_h0.8_cont_a$phenotype, 
                      timema_10_h0.8_cont_b$phenotype, 
                      timema_10_h0.8_cont_c$phenotype, 
                      timema_10_h0.8_dis_a$phenotype, 
                      timema_10_h0.8_dis_b$phenotype, 
                      timema_10_h0.8_dis_c$phenotype)

snps_10_h0.8 <- cbind(timema_10_h0.8_cont_a$functional_snps, 
                     timema_10_h0.8_cont_b$functional_snps, 
                     timema_10_h0.8_cont_c$functional_snps, 
                     timema_10_h0.8_dis_a$functional_snps, 
                     timema_10_h0.8_dis_b$functional_snps, 
                     timema_10_h0.8_dis_c$functional_snps)

effect_10_h0.8 <- cbind(timema_10_h0.8_cont_a$effect, 
                       timema_10_h0.8_cont_b$effect, 
                       timema_10_h0.8_cont_c$effect, 
                       timema_10_h0.8_dis_a$effect, 
                       timema_10_h0.8_dis_b$effect, 
                       timema_10_h0.8_dis_c$effect)

her_10_h0.8 <- cbind(timema_10_h0.8_cont_a$estimated_heretability, 
                    timema_10_h0.8_cont_b$estimated_heretability, 
                    timema_10_h0.8_cont_c$estimated_heretability, 
                    timema_10_h0.8_dis_a$estimated_heretability, 
                    timema_10_h0.8_dis_b$estimated_heretability, 
                    timema_10_h0.8_dis_c$estimated_heretability)

colnames(pheno_10_h0.8) <- c("timema_10_0.8_cont_a", "timema_10_0.8_cont_b", "timema_10_0.8_cont_c", 
                            "timema_10_0.8_dis_a", "timema_10_0.8_dis_b", "timema_10_0.8_dis_c")
colnames(snps_10_h0.8) <- colnames(pheno_10_h0.8) 
colnames(effect_10_h0.8) <- colnames(pheno_10_h0.8) 
colnames(her_10_h0.8) <- colnames(pheno_10_h0.8)

head(pheno_10_h0.8)
dim(pheno_10_h0.8)
head(snps_10_h0.8)
dim(snps_10_h0.8)
head(effect_10_h0.8)
dim(effect_10_h0.8)
her_10_h0.8

write.csv(pheno_10_h0.8, "timema_pheno_h08_10.csv", row.names = FALSE)
write.csv(snps_10_h0.8, "timema_snps_h08_10.csv", row.names = FALSE)
write.csv(effect_10_h0.8, "timema_effect_h08_10.csv", row.names = FALSE)
write.csv(her_10_h0.8, "timema_her_h08_10.csv", row.names = FALSE)



#------------------------------------------------------------------------------
#                         50 important SNPs with h = 0.8
#------------------------------------------------------------------------------

set.seed(76575434)
timema_50_h0.8_cont_a <- gwas_sim(timema, num_snps = 50, heretability = 0.8)
timema_50_h0.8_cont_b <- gwas_sim(timema, num_snps = 50, heretability = 0.8)
timema_50_h0.8_cont_c <- gwas_sim(timema, num_snps = 50, heretability = 0.8)

timema_50_h0.8_dis_a <- gwas_sim(timema, num_snps = 50, heretability = 0.8)
timema_50_h0.8_dis_b <- gwas_sim(timema, num_snps = 50, heretability = 0.8)
timema_50_h0.8_dis_c <- gwas_sim(timema, num_snps = 50, heretability = 0.8)

timema_50_h0.8_dis_a$phenotype <- ifelse(timema_50_h0.8_dis_a$phenotype > median(timema_50_h0.8_dis_a$phenotype), 
                                        0, 1)
timema_50_h0.8_dis_b$phenotype <- ifelse(timema_50_h0.8_dis_b$phenotype > median(timema_50_h0.8_dis_b$phenotype), 
                                        0, 1)
timema_50_h0.8_dis_c$phenotype <- ifelse(timema_50_h0.8_dis_c$phenotype > median(timema_50_h0.8_dis_c$phenotype), 
                                        0, 1)

pheno_50_h0.8 <- cbind(timema_50_h0.8_cont_a$phenotype, 
                       timema_50_h0.8_cont_b$phenotype, 
                       timema_50_h0.8_cont_c$phenotype, 
                       timema_50_h0.8_dis_a$phenotype, 
                       timema_50_h0.8_dis_b$phenotype, 
                       timema_50_h0.8_dis_c$phenotype)

snps_50_h0.8 <- cbind(timema_50_h0.8_cont_a$functional_snps, 
                      timema_50_h0.8_cont_b$functional_snps, 
                      timema_50_h0.8_cont_c$functional_snps, 
                      timema_50_h0.8_dis_a$functional_snps, 
                      timema_50_h0.8_dis_b$functional_snps, 
                      timema_50_h0.8_dis_c$functional_snps)

effect_50_h0.8 <- cbind(timema_50_h0.8_cont_a$effect, 
                        timema_50_h0.8_cont_b$effect, 
                        timema_50_h0.8_cont_c$effect, 
                        timema_50_h0.8_dis_a$effect, 
                        timema_50_h0.8_dis_b$effect, 
                        timema_50_h0.8_dis_c$effect)

her_50_h0.8 <- cbind(timema_50_h0.8_cont_a$estimated_heretability, 
                     timema_50_h0.8_cont_b$estimated_heretability, 
                     timema_50_h0.8_cont_c$estimated_heretability, 
                     timema_50_h0.8_dis_a$estimated_heretability, 
                     timema_50_h0.8_dis_b$estimated_heretability, 
                     timema_50_h0.8_dis_c$estimated_heretability)

colnames(pheno_50_h0.8) <- c("timema_50_0.8_cont_a", "timema_50_0.8_cont_b", "timema_50_0.8_cont_c", 
                             "timema_50_0.8_dis_a", "timema_50_0.8_dis_b", "timema_50_0.8_dis_c")
colnames(snps_50_h0.8) <- colnames(pheno_50_h0.8) 
colnames(effect_50_h0.8) <- colnames(pheno_50_h0.8) 
colnames(her_50_h0.8) <- colnames(pheno_50_h0.8)

head(pheno_50_h0.8)
dim(pheno_50_h0.8)
head(snps_50_h0.8)
dim(snps_50_h0.8)
head(effect_50_h0.8)
dim(effect_50_h0.8)
her_50_h0.8

write.csv(pheno_50_h0.8, "timema_pheno_h08_50.csv", row.names = FALSE)
write.csv(snps_50_h0.8, "timema_snps_h08_50.csv", row.names = FALSE)
write.csv(effect_50_h0.8, "timema_effect_h08_50.csv", row.names = FALSE)
write.csv(her_50_h0.8, "timema_her_h08_50.csv", row.names = FALSE)

