library(gwas3)

rhag <- read.csv("C:/Users/jflun/Dropbox/Dissertation/LASSO_EN_Project/Data/rhag/rhag_geno.csv")
head(rhag[, 1:10])

#------------------------------------------------------------------------------
#                         5 important SNPs with h = 0.8
#------------------------------------------------------------------------------

set.seed(3425342)
rhag_5_h0.8_cont_a <- gwas_sim(rhag, num_snps = 5, heretability = 0.8)
rhag_5_h0.8_cont_b <- gwas_sim(rhag, num_snps = 5, heretability = 0.8)
rhag_5_h0.8_cont_c <- gwas_sim(rhag, num_snps = 5, heretability = 0.8)

rhag_5_h0.8_dis_a <- gwas_sim(rhag, num_snps = 5, heretability = 0.8)
rhag_5_h0.8_dis_b <- gwas_sim(rhag, num_snps = 5, heretability = 0.8)
rhag_5_h0.8_dis_c <- gwas_sim(rhag, num_snps = 5, heretability = 0.8)

rhag_5_h0.8_dis_a$phenotype <- ifelse(rhag_5_h0.8_dis_a$phenotype > median(rhag_5_h0.8_dis_a$phenotype), 
                                       0, 1)
rhag_5_h0.8_dis_b$phenotype <- ifelse(rhag_5_h0.8_dis_b$phenotype > median(rhag_5_h0.8_dis_b$phenotype), 
                                       0, 1)
rhag_5_h0.8_dis_c$phenotype <- ifelse(rhag_5_h0.8_dis_c$phenotype > median(rhag_5_h0.8_dis_c$phenotype), 
                                       0, 1)

pheno_5_h0.8 <- cbind(rhag_5_h0.8_cont_a$phenotype, 
                      rhag_5_h0.8_cont_b$phenotype, 
                      rhag_5_h0.8_cont_c$phenotype, 
                      rhag_5_h0.8_dis_a$phenotype, 
                      rhag_5_h0.8_dis_b$phenotype, 
                      rhag_5_h0.8_dis_c$phenotype)

snps_5_h0.8 <- cbind(rhag_5_h0.8_cont_a$functional_snps, 
                     rhag_5_h0.8_cont_b$functional_snps, 
                     rhag_5_h0.8_cont_c$functional_snps, 
                     rhag_5_h0.8_dis_a$functional_snps, 
                     rhag_5_h0.8_dis_b$functional_snps, 
                     rhag_5_h0.8_dis_c$functional_snps)

effect_5_h0.8 <- cbind(rhag_5_h0.8_cont_a$effect, 
                       rhag_5_h0.8_cont_b$effect, 
                       rhag_5_h0.8_cont_c$effect, 
                       rhag_5_h0.8_dis_a$effect, 
                       rhag_5_h0.8_dis_b$effect, 
                       rhag_5_h0.8_dis_c$effect)

her_5_h0.8 <- cbind(rhag_5_h0.8_cont_a$estimated_heretability, 
                    rhag_5_h0.8_cont_b$estimated_heretability, 
                    rhag_5_h0.8_cont_c$estimated_heretability, 
                    rhag_5_h0.8_dis_a$estimated_heretability, 
                    rhag_5_h0.8_dis_b$estimated_heretability, 
                    rhag_5_h0.8_dis_c$estimated_heretability)

colnames(pheno_5_h0.8) <- c("rhag_5_0.8_cont_a", "rhag_5_0.8_cont_b", "rhag_5_0.8_cont_c", 
                            "rhag_5_0.8_dis_a", "rhag_5_0.8_dis_b", "rhag_5_0.8_dis_c")
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

write.csv(pheno_5_h0.8, "rhag_pheno_h08_5.csv", row.names = FALSE)
write.csv(snps_5_h0.8, "rhag_snps_h08_5.csv", row.names = FALSE)
write.csv(effect_5_h0.8, "rhag_effect_h08_5.csv", row.names = FALSE)
write.csv(her_5_h0.8, "rhag_her_h08_5.csv", row.names = FALSE)





#------------------------------------------------------------------------------
#                         10 important SNPs with h = 0.8
#------------------------------------------------------------------------------

set.seed(44534543)
rhag_10_h0.8_cont_a <- gwas_sim(rhag, num_snps = 10, heretability = 0.8)
rhag_10_h0.8_cont_b <- gwas_sim(rhag, num_snps = 10, heretability = 0.8)
rhag_10_h0.8_cont_c <- gwas_sim(rhag, num_snps = 10, heretability = 0.8)

rhag_10_h0.8_dis_a <- gwas_sim(rhag, num_snps = 10, heretability = 0.8)
rhag_10_h0.8_dis_b <- gwas_sim(rhag, num_snps = 10, heretability = 0.8)
rhag_10_h0.8_dis_c <- gwas_sim(rhag, num_snps = 10, heretability = 0.8)

rhag_10_h0.8_dis_a$phenotype <- ifelse(rhag_10_h0.8_dis_a$phenotype > median(rhag_10_h0.8_dis_a$phenotype), 
                                       0, 1)
rhag_10_h0.8_dis_b$phenotype <- ifelse(rhag_10_h0.8_dis_b$phenotype > median(rhag_10_h0.8_dis_b$phenotype), 
                                       0, 1)
rhag_10_h0.8_dis_c$phenotype <- ifelse(rhag_10_h0.8_dis_c$phenotype > median(rhag_10_h0.8_dis_c$phenotype), 
                                       0, 1)

pheno_10_h0.8 <- cbind(rhag_10_h0.8_cont_a$phenotype, 
                      rhag_10_h0.8_cont_b$phenotype, 
                      rhag_10_h0.8_cont_c$phenotype, 
                      rhag_10_h0.8_dis_a$phenotype, 
                      rhag_10_h0.8_dis_b$phenotype, 
                      rhag_10_h0.8_dis_c$phenotype)

snps_10_h0.8 <- cbind(rhag_10_h0.8_cont_a$functional_snps, 
                     rhag_10_h0.8_cont_b$functional_snps, 
                     rhag_10_h0.8_cont_c$functional_snps, 
                     rhag_10_h0.8_dis_a$functional_snps, 
                     rhag_10_h0.8_dis_b$functional_snps, 
                     rhag_10_h0.8_dis_c$functional_snps)

effect_10_h0.8 <- cbind(rhag_10_h0.8_cont_a$effect, 
                       rhag_10_h0.8_cont_b$effect, 
                       rhag_10_h0.8_cont_c$effect, 
                       rhag_10_h0.8_dis_a$effect, 
                       rhag_10_h0.8_dis_b$effect, 
                       rhag_10_h0.8_dis_c$effect)

her_10_h0.8 <- cbind(rhag_10_h0.8_cont_a$estimated_heretability, 
                    rhag_10_h0.8_cont_b$estimated_heretability, 
                    rhag_10_h0.8_cont_c$estimated_heretability, 
                    rhag_10_h0.8_dis_a$estimated_heretability, 
                    rhag_10_h0.8_dis_b$estimated_heretability, 
                    rhag_10_h0.8_dis_c$estimated_heretability)

colnames(pheno_10_h0.8) <- c("rhag_10_0.8_cont_a", "rhag_10_0.8_cont_b", "rhag_10_0.8_cont_c", 
                            "rhag_10_0.8_dis_a", "rhag_10_0.8_dis_b", "rhag_10_0.8_dis_c")
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

write.csv(pheno_10_h0.8, "rhag_pheno_h08_10.csv", row.names = FALSE)
write.csv(snps_10_h0.8, "rhag_snps_h08_10.csv", row.names = FALSE)
write.csv(effect_10_h0.8, "rhag_effect_h08_10.csv", row.names = FALSE)
write.csv(her_10_h0.8, "rhag_her_h08_10.csv", row.names = FALSE)



#------------------------------------------------------------------------------
#                         50 important SNPs with h = 0.8
#------------------------------------------------------------------------------

set.seed(82345434)
rhag_50_h0.8_cont_a <- gwas_sim(rhag, num_snps = 50, heretability = 0.8)
rhag_50_h0.8_cont_b <- gwas_sim(rhag, num_snps = 50, heretability = 0.8)
rhag_50_h0.8_cont_c <- gwas_sim(rhag, num_snps = 50, heretability = 0.8)

rhag_50_h0.8_dis_a <- gwas_sim(rhag, num_snps = 50, heretability = 0.8)
rhag_50_h0.8_dis_b <- gwas_sim(rhag, num_snps = 50, heretability = 0.8)
rhag_50_h0.8_dis_c <- gwas_sim(rhag, num_snps = 50, heretability = 0.8)

rhag_50_h0.8_dis_a$phenotype <- ifelse(rhag_50_h0.8_dis_a$phenotype > median(rhag_50_h0.8_dis_a$phenotype), 
                                        0, 1)
rhag_50_h0.8_dis_b$phenotype <- ifelse(rhag_50_h0.8_dis_b$phenotype > median(rhag_50_h0.8_dis_b$phenotype), 
                                        0, 1)
rhag_50_h0.8_dis_c$phenotype <- ifelse(rhag_50_h0.8_dis_c$phenotype > median(rhag_50_h0.8_dis_c$phenotype), 
                                        0, 1)

pheno_50_h0.8 <- cbind(rhag_50_h0.8_cont_a$phenotype, 
                       rhag_50_h0.8_cont_b$phenotype, 
                       rhag_50_h0.8_cont_c$phenotype, 
                       rhag_50_h0.8_dis_a$phenotype, 
                       rhag_50_h0.8_dis_b$phenotype, 
                       rhag_50_h0.8_dis_c$phenotype)

snps_50_h0.8 <- cbind(rhag_50_h0.8_cont_a$functional_snps, 
                      rhag_50_h0.8_cont_b$functional_snps, 
                      rhag_50_h0.8_cont_c$functional_snps, 
                      rhag_50_h0.8_dis_a$functional_snps, 
                      rhag_50_h0.8_dis_b$functional_snps, 
                      rhag_50_h0.8_dis_c$functional_snps)

effect_50_h0.8 <- cbind(rhag_50_h0.8_cont_a$effect, 
                        rhag_50_h0.8_cont_b$effect, 
                        rhag_50_h0.8_cont_c$effect, 
                        rhag_50_h0.8_dis_a$effect, 
                        rhag_50_h0.8_dis_b$effect, 
                        rhag_50_h0.8_dis_c$effect)

her_50_h0.8 <- cbind(rhag_50_h0.8_cont_a$estimated_heretability, 
                     rhag_50_h0.8_cont_b$estimated_heretability, 
                     rhag_50_h0.8_cont_c$estimated_heretability, 
                     rhag_50_h0.8_dis_a$estimated_heretability, 
                     rhag_50_h0.8_dis_b$estimated_heretability, 
                     rhag_50_h0.8_dis_c$estimated_heretability)

colnames(pheno_50_h0.8) <- c("rhag_50_0.8_cont_a", "rhag_50_0.8_cont_b", "rhag_50_0.8_cont_c", 
                             "rhag_50_0.8_dis_a", "rhag_50_0.8_dis_b", "rhag_50_0.8_dis_c")
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

write.csv(pheno_50_h0.8, "rhag_pheno_h08_50.csv", row.names = FALSE)
write.csv(snps_50_h0.8, "rhag_snps_h08_50.csv", row.names = FALSE)
write.csv(effect_50_h0.8, "rhag_effect_h08_50.csv", row.names = FALSE)
write.csv(her_50_h0.8, "rhag_her_h08_50.csv", row.names = FALSE)

