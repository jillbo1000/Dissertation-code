library(gwas3)

mouse <- read.csv("C:/Users/jflun/Dropbox/Dissertation/LASSO_EN_Project/Data/mouse/mouse_geno.csv")
head(mouse[, 1:10])

#------------------------------------------------------------------------------
#                         5 important SNPs with h = 0.8
#------------------------------------------------------------------------------

set.seed(342543234)
mouse_5_h0.8_cont_a <- gwas_sim(mouse, num_snps = 5, heretability = 0.8)
mouse_5_h0.8_cont_b <- gwas_sim(mouse, num_snps = 5, heretability = 0.8)
mouse_5_h0.8_cont_c <- gwas_sim(mouse, num_snps = 5, heretability = 0.8)

mouse_5_h0.8_dis_a <- gwas_sim(mouse, num_snps = 5, heretability = 0.8)
mouse_5_h0.8_dis_b <- gwas_sim(mouse, num_snps = 5, heretability = 0.8)
mouse_5_h0.8_dis_c <- gwas_sim(mouse, num_snps = 5, heretability = 0.8)

mouse_5_h0.8_dis_a$phenotype <- ifelse(mouse_5_h0.8_dis_a$phenotype > median(mouse_5_h0.8_dis_a$phenotype), 
                                       0, 1)
mouse_5_h0.8_dis_b$phenotype <- ifelse(mouse_5_h0.8_dis_b$phenotype > median(mouse_5_h0.8_dis_b$phenotype), 
                                       0, 1)
mouse_5_h0.8_dis_c$phenotype <- ifelse(mouse_5_h0.8_dis_c$phenotype > median(mouse_5_h0.8_dis_c$phenotype), 
                                       0, 1)

pheno_5_h0.8 <- cbind(mouse_5_h0.8_cont_a$phenotype, 
                      mouse_5_h0.8_cont_b$phenotype, 
                      mouse_5_h0.8_cont_c$phenotype, 
                      mouse_5_h0.8_dis_a$phenotype, 
                      mouse_5_h0.8_dis_b$phenotype, 
                      mouse_5_h0.8_dis_c$phenotype)

snps_5_h0.8 <- cbind(mouse_5_h0.8_cont_a$functional_snps, 
                     mouse_5_h0.8_cont_b$functional_snps, 
                     mouse_5_h0.8_cont_c$functional_snps, 
                     mouse_5_h0.8_dis_a$functional_snps, 
                     mouse_5_h0.8_dis_b$functional_snps, 
                     mouse_5_h0.8_dis_c$functional_snps)

effect_5_h0.8 <- cbind(mouse_5_h0.8_cont_a$effect, 
                       mouse_5_h0.8_cont_b$effect, 
                       mouse_5_h0.8_cont_c$effect, 
                       mouse_5_h0.8_dis_a$effect, 
                       mouse_5_h0.8_dis_b$effect, 
                       mouse_5_h0.8_dis_c$effect)

her_5_h0.8 <- cbind(mouse_5_h0.8_cont_a$estimated_heretability, 
                    mouse_5_h0.8_cont_b$estimated_heretability, 
                    mouse_5_h0.8_cont_c$estimated_heretability, 
                    mouse_5_h0.8_dis_a$estimated_heretability, 
                    mouse_5_h0.8_dis_b$estimated_heretability, 
                    mouse_5_h0.8_dis_c$estimated_heretability)

colnames(pheno_5_h0.8) <- c("mouse_5_0.8_cont_a", "mouse_5_0.8_cont_b", "mouse_5_0.8_cont_c", 
                            "mouse_5_0.8_dis_a", "mouse_5_0.8_dis_b", "mouse_5_0.8_dis_c")
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

write.csv(pheno_5_h0.8, "mouse_pheno_h08_5.csv", row.names = FALSE)
write.csv(snps_5_h0.8, "mouse_snps_h08_5.csv", row.names = FALSE)
write.csv(effect_5_h0.8, "mouse_effect_h08_5.csv", row.names = FALSE)
write.csv(her_5_h0.8, "mouse_her_h08_5.csv", row.names = FALSE)





#------------------------------------------------------------------------------
#                         10 important SNPs with h = 0.8
#------------------------------------------------------------------------------

set.seed(44534543)
mouse_10_h0.8_cont_a <- gwas_sim(mouse, num_snps = 10, heretability = 0.8)
mouse_10_h0.8_cont_b <- gwas_sim(mouse, num_snps = 10, heretability = 0.8)
mouse_10_h0.8_cont_c <- gwas_sim(mouse, num_snps = 10, heretability = 0.8)

mouse_10_h0.8_dis_a <- gwas_sim(mouse, num_snps = 10, heretability = 0.8)
mouse_10_h0.8_dis_b <- gwas_sim(mouse, num_snps = 10, heretability = 0.8)
mouse_10_h0.8_dis_c <- gwas_sim(mouse, num_snps = 10, heretability = 0.8)

mouse_10_h0.8_dis_a$phenotype <- ifelse(mouse_10_h0.8_dis_a$phenotype > median(mouse_10_h0.8_dis_a$phenotype), 
                                       0, 1)
mouse_10_h0.8_dis_b$phenotype <- ifelse(mouse_10_h0.8_dis_b$phenotype > median(mouse_10_h0.8_dis_b$phenotype), 
                                       0, 1)
mouse_10_h0.8_dis_c$phenotype <- ifelse(mouse_10_h0.8_dis_c$phenotype > median(mouse_10_h0.8_dis_c$phenotype), 
                                       0, 1)

pheno_10_h0.8 <- cbind(mouse_10_h0.8_cont_a$phenotype, 
                      mouse_10_h0.8_cont_b$phenotype, 
                      mouse_10_h0.8_cont_c$phenotype, 
                      mouse_10_h0.8_dis_a$phenotype, 
                      mouse_10_h0.8_dis_b$phenotype, 
                      mouse_10_h0.8_dis_c$phenotype)

snps_10_h0.8 <- cbind(mouse_10_h0.8_cont_a$functional_snps, 
                     mouse_10_h0.8_cont_b$functional_snps, 
                     mouse_10_h0.8_cont_c$functional_snps, 
                     mouse_10_h0.8_dis_a$functional_snps, 
                     mouse_10_h0.8_dis_b$functional_snps, 
                     mouse_10_h0.8_dis_c$functional_snps)

effect_10_h0.8 <- cbind(mouse_10_h0.8_cont_a$effect, 
                       mouse_10_h0.8_cont_b$effect, 
                       mouse_10_h0.8_cont_c$effect, 
                       mouse_10_h0.8_dis_a$effect, 
                       mouse_10_h0.8_dis_b$effect, 
                       mouse_10_h0.8_dis_c$effect)

her_10_h0.8 <- cbind(mouse_10_h0.8_cont_a$estimated_heretability, 
                    mouse_10_h0.8_cont_b$estimated_heretability, 
                    mouse_10_h0.8_cont_c$estimated_heretability, 
                    mouse_10_h0.8_dis_a$estimated_heretability, 
                    mouse_10_h0.8_dis_b$estimated_heretability, 
                    mouse_10_h0.8_dis_c$estimated_heretability)

colnames(pheno_10_h0.8) <- c("mouse_10_0.8_cont_a", "mouse_10_0.8_cont_b", "mouse_10_0.8_cont_c", 
                            "mouse_10_0.8_dis_a", "mouse_10_0.8_dis_b", "mouse_10_0.8_dis_c")
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

write.csv(pheno_10_h0.8, "mouse_pheno_h08_10.csv", row.names = FALSE)
write.csv(snps_10_h0.8, "mouse_snps_h08_10.csv", row.names = FALSE)
write.csv(effect_10_h0.8, "mouse_effect_h08_10.csv", row.names = FALSE)
write.csv(her_10_h0.8, "mouse_her_h08_10.csv", row.names = FALSE)



#------------------------------------------------------------------------------
#                         50 important SNPs with h = 0.8
#------------------------------------------------------------------------------

set.seed(82345434)
mouse_50_h0.8_cont_a <- gwas_sim(mouse, num_snps = 50, heretability = 0.8)
mouse_50_h0.8_cont_b <- gwas_sim(mouse, num_snps = 50, heretability = 0.8)
mouse_50_h0.8_cont_c <- gwas_sim(mouse, num_snps = 50, heretability = 0.8)

mouse_50_h0.8_dis_a <- gwas_sim(mouse, num_snps = 50, heretability = 0.8)
mouse_50_h0.8_dis_b <- gwas_sim(mouse, num_snps = 50, heretability = 0.8)
mouse_50_h0.8_dis_c <- gwas_sim(mouse, num_snps = 50, heretability = 0.8)

mouse_50_h0.8_dis_a$phenotype <- ifelse(mouse_50_h0.8_dis_a$phenotype > median(mouse_50_h0.8_dis_a$phenotype), 
                                        0, 1)
mouse_50_h0.8_dis_b$phenotype <- ifelse(mouse_50_h0.8_dis_b$phenotype > median(mouse_50_h0.8_dis_b$phenotype), 
                                        0, 1)
mouse_50_h0.8_dis_c$phenotype <- ifelse(mouse_50_h0.8_dis_c$phenotype > median(mouse_50_h0.8_dis_c$phenotype), 
                                        0, 1)

pheno_50_h0.8 <- cbind(mouse_50_h0.8_cont_a$phenotype, 
                       mouse_50_h0.8_cont_b$phenotype, 
                       mouse_50_h0.8_cont_c$phenotype, 
                       mouse_50_h0.8_dis_a$phenotype, 
                       mouse_50_h0.8_dis_b$phenotype, 
                       mouse_50_h0.8_dis_c$phenotype)

snps_50_h0.8 <- cbind(mouse_50_h0.8_cont_a$functional_snps, 
                      mouse_50_h0.8_cont_b$functional_snps, 
                      mouse_50_h0.8_cont_c$functional_snps, 
                      mouse_50_h0.8_dis_a$functional_snps, 
                      mouse_50_h0.8_dis_b$functional_snps, 
                      mouse_50_h0.8_dis_c$functional_snps)

effect_50_h0.8 <- cbind(mouse_50_h0.8_cont_a$effect, 
                        mouse_50_h0.8_cont_b$effect, 
                        mouse_50_h0.8_cont_c$effect, 
                        mouse_50_h0.8_dis_a$effect, 
                        mouse_50_h0.8_dis_b$effect, 
                        mouse_50_h0.8_dis_c$effect)

her_50_h0.8 <- cbind(mouse_50_h0.8_cont_a$estimated_heretability, 
                     mouse_50_h0.8_cont_b$estimated_heretability, 
                     mouse_50_h0.8_cont_c$estimated_heretability, 
                     mouse_50_h0.8_dis_a$estimated_heretability, 
                     mouse_50_h0.8_dis_b$estimated_heretability, 
                     mouse_50_h0.8_dis_c$estimated_heretability)

colnames(pheno_50_h0.8) <- c("mouse_50_0.8_cont_a", "mouse_50_0.8_cont_b", "mouse_50_0.8_cont_c", 
                             "mouse_50_0.8_dis_a", "mouse_50_0.8_dis_b", "mouse_50_0.8_dis_c")
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

write.csv(pheno_50_h0.8, "mouse_pheno_h08_50.csv", row.names = FALSE)
write.csv(snps_50_h0.8, "mouse_snps_h08_50.csv", row.names = FALSE)
write.csv(effect_50_h0.8, "mouse_effect_h08_50.csv", row.names = FALSE)
write.csv(her_50_h0.8, "mouse_her_h08_50.csv", row.names = FALSE)

