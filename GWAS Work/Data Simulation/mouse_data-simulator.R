library(gwas3)

mouse <- read.csv("C:/Users/jflun/Dropbox/Dissertation/LASSO_EN_Project/Data/mouse/mouse_geno.csv")

#------------------------------------------------------------------------------
#                         5 important SNPs with h = 0.3
#------------------------------------------------------------------------------

set.seed(3242839)
mouse_5_h0.3_cont_a <- gwas_sim(mouse, num_snps = 5, heretability = 0.3)
mouse_5_h0.3_cont_b <- gwas_sim(mouse, num_snps = 5, heretability = 0.3)
mouse_5_h0.3_cont_c <- gwas_sim(mouse, num_snps = 5, heretability = 0.3)

mouse_5_h0.3_dis_a <- gwas_sim(mouse, num_snps = 5, heretability = 0.3)
mouse_5_h0.3_dis_b <- gwas_sim(mouse, num_snps = 5, heretability = 0.3)
mouse_5_h0.3_dis_c <- gwas_sim(mouse, num_snps = 5, heretability = 0.3)

mouse_5_h0.3_dis_a$phenotype <- ifelse(mouse_5_h0.3_dis_a$phenotype > median(mouse_5_h0.3_dis_a$phenotype), 
                                       0, 1)
mouse_5_h0.3_dis_b$phenotype <- ifelse(mouse_5_h0.3_dis_b$phenotype > median(mouse_5_h0.3_dis_b$phenotype), 
                                      0, 1)
mouse_5_h0.3_dis_c$phenotype <- ifelse(mouse_5_h0.3_dis_c$phenotype > median(mouse_5_h0.3_dis_c$phenotype), 
                                       0, 1)

pheno_5_h0.3 <- cbind(mouse_5_h0.3_cont_a$phenotype, 
                      mouse_5_h0.3_cont_b$phenotype, 
                      mouse_5_h0.3_cont_c$phenotype, 
                      mouse_5_h0.3_dis_a$phenotype, 
                      mouse_5_h0.3_dis_b$phenotype, 
                      mouse_5_h0.3_dis_c$phenotype)

snps_5_h0.3 <- cbind(mouse_5_h0.3_cont_a$functional_snps, 
                     mouse_5_h0.3_cont_b$functional_snps, 
                     mouse_5_h0.3_cont_c$functional_snps, 
                     mouse_5_h0.3_dis_a$functional_snps, 
                     mouse_5_h0.3_dis_b$functional_snps, 
                     mouse_5_h0.3_dis_c$functional_snps)

effect_5_h0.3 <- cbind(mouse_5_h0.3_cont_a$effect, 
                       mouse_5_h0.3_cont_b$effect, 
                       mouse_5_h0.3_cont_c$effect, 
                       mouse_5_h0.3_dis_a$effect, 
                       mouse_5_h0.3_dis_b$effect, 
                       mouse_5_h0.3_dis_c$effect)

her_5_h0.3 <- cbind(mouse_5_h0.3_cont_a$estimated_heretability, 
                    mouse_5_h0.3_cont_b$estimated_heretability, 
                    mouse_5_h0.3_cont_c$estimated_heretability, 
                    mouse_5_h0.3_dis_a$estimated_heretability, 
                    mouse_5_h0.3_dis_b$estimated_heretability, 
                    mouse_5_h0.3_dis_c$estimated_heretability)

colnames(pheno_5_h0.3) <- c("mouse_5_0.3_cont_a", "mouse_5_0.3_cont_b", "mouse_5_0.3_cont_c", 
                            "mouse_5_0.3_dis_a", "mouse_5_0.3_dis_b", "mouse_5_0.3_dis_c")
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

write.csv(pheno_5_h0.3, "mouse_pheno_h03_5.csv", row.names = FALSE)
write.csv(snps_5_h0.3, "mouse_snps_h03_5.csv", row.names = FALSE)
write.csv(effect_5_h0.3, "mouse_effect_h03_5.csv", row.names = FALSE)
write.csv(her_5_h0.3, "mouse_her_h03_5.csv", row.names = FALSE)


#------------------------------------------------------------------------------
#                         5 important SNPs with h = 0.1
#------------------------------------------------------------------------------

set.seed(3437289)
mouse_5_h0.1_cont_a <- gwas_sim(mouse, num_snps = 5, heretability = 0.1)
mouse_5_h0.1_cont_b <- gwas_sim(mouse, num_snps = 5, heretability = 0.1)
mouse_5_h0.1_cont_c <- gwas_sim(mouse, num_snps = 5, heretability = 0.1)

mouse_5_h0.1_dis_a <- gwas_sim(mouse, num_snps = 5, heretability = 0.1)
mouse_5_h0.1_dis_b <- gwas_sim(mouse, num_snps = 5, heretability = 0.1)
mouse_5_h0.1_dis_c <- gwas_sim(mouse, num_snps = 5, heretability = 0.1)

mouse_5_h0.1_dis_a$phenotype <- ifelse(mouse_5_h0.1_dis_a$phenotype > median(mouse_5_h0.1_dis_a$phenotype), 
                                       0, 1)
mouse_5_h0.1_dis_b$phenotype <- ifelse(mouse_5_h0.1_dis_b$phenotype > median(mouse_5_h0.1_dis_b$phenotype), 
                                       0, 1)
mouse_5_h0.1_dis_c$phenotype <- ifelse(mouse_5_h0.1_dis_c$phenotype > median(mouse_5_h0.1_dis_c$phenotype), 
                                       0, 1)

pheno_5_h0.1 <- cbind(mouse_5_h0.1_cont_a$phenotype, 
                      mouse_5_h0.1_cont_b$phenotype, 
                      mouse_5_h0.1_cont_c$phenotype, 
                      mouse_5_h0.1_dis_a$phenotype, 
                      mouse_5_h0.1_dis_b$phenotype, 
                      mouse_5_h0.1_dis_c$phenotype)

snps_5_h0.1 <- cbind(mouse_5_h0.1_cont_a$functional_snps, 
                     mouse_5_h0.1_cont_b$functional_snps, 
                     mouse_5_h0.1_cont_c$functional_snps, 
                     mouse_5_h0.1_dis_a$functional_snps, 
                     mouse_5_h0.1_dis_b$functional_snps, 
                     mouse_5_h0.1_dis_c$functional_snps)

effect_5_h0.1 <- cbind(mouse_5_h0.1_cont_a$effect, 
                       mouse_5_h0.1_cont_b$effect, 
                       mouse_5_h0.1_cont_c$effect, 
                       mouse_5_h0.1_dis_a$effect, 
                       mouse_5_h0.1_dis_b$effect, 
                       mouse_5_h0.1_dis_c$effect)

her_5_h0.1 <- cbind(mouse_5_h0.1_cont_a$estimated_heretability, 
                    mouse_5_h0.1_cont_b$estimated_heretability, 
                    mouse_5_h0.1_cont_c$estimated_heretability, 
                    mouse_5_h0.1_dis_a$estimated_heretability, 
                    mouse_5_h0.1_dis_b$estimated_heretability, 
                    mouse_5_h0.1_dis_c$estimated_heretability)

colnames(pheno_5_h0.1) <- c("mouse_5_0.1_cont_a", "mouse_5_0.1_cont_b", "mouse_5_0.1_cont_c", 
                            "mouse_5_0.1_dis_a", "mouse_5_0.1_dis_b", "mouse_5_0.1_dis_c")
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

write.csv(pheno_5_h0.1, "mouse_pheno_h01_5.csv", row.names = FALSE)
write.csv(snps_5_h0.1, "mouse_snps_h01_5.csv", row.names = FALSE)
write.csv(effect_5_h0.1, "mouse_effect_h01_5.csv", row.names = FALSE)
write.csv(her_5_h0.1, "mouse_her_h01_5.csv", row.names = FALSE)



#------------------------------------------------------------------------------
#                         10 important SNPs with h = 0.3
#------------------------------------------------------------------------------

set.seed(4358479)
mouse_10_h0.3_cont_a <- gwas_sim(mouse, num_snps = 10, heretability = 0.3)
mouse_10_h0.3_cont_b <- gwas_sim(mouse, num_snps = 10, heretability = 0.3)
mouse_10_h0.3_cont_c <- gwas_sim(mouse, num_snps = 10, heretability = 0.3)

mouse_10_h0.3_dis_a <- gwas_sim(mouse, num_snps = 10, heretability = 0.3)
mouse_10_h0.3_dis_b <- gwas_sim(mouse, num_snps = 10, heretability = 0.3)
mouse_10_h0.3_dis_c <- gwas_sim(mouse, num_snps = 10, heretability = 0.3)

mouse_10_h0.3_dis_a$phenotype <- ifelse(mouse_10_h0.3_dis_a$phenotype > median(mouse_10_h0.3_dis_a$phenotype), 
                                       0, 1)
mouse_10_h0.3_dis_b$phenotype <- ifelse(mouse_10_h0.3_dis_b$phenotype > median(mouse_10_h0.3_dis_b$phenotype), 
                                       0, 1)
mouse_10_h0.3_dis_c$phenotype <- ifelse(mouse_10_h0.3_dis_c$phenotype > median(mouse_10_h0.3_dis_c$phenotype), 
                                       0, 1)

pheno_10_h0.3 <- cbind(mouse_10_h0.3_cont_a$phenotype, 
                      mouse_10_h0.3_cont_b$phenotype, 
                      mouse_10_h0.3_cont_c$phenotype, 
                      mouse_10_h0.3_dis_a$phenotype, 
                      mouse_10_h0.3_dis_b$phenotype, 
                      mouse_10_h0.3_dis_c$phenotype)

snps_10_h0.3 <- cbind(mouse_10_h0.3_cont_a$functional_snps, 
                     mouse_10_h0.3_cont_b$functional_snps, 
                     mouse_10_h0.3_cont_c$functional_snps, 
                     mouse_10_h0.3_dis_a$functional_snps, 
                     mouse_10_h0.3_dis_b$functional_snps, 
                     mouse_10_h0.3_dis_c$functional_snps)

effect_10_h0.3 <- cbind(mouse_10_h0.3_cont_a$effect, 
                       mouse_10_h0.3_cont_b$effect, 
                       mouse_10_h0.3_cont_c$effect, 
                       mouse_10_h0.3_dis_a$effect, 
                       mouse_10_h0.3_dis_b$effect, 
                       mouse_10_h0.3_dis_c$effect)

her_10_h0.3 <- cbind(mouse_10_h0.3_cont_a$estimated_heretability, 
                    mouse_10_h0.3_cont_b$estimated_heretability, 
                    mouse_10_h0.3_cont_c$estimated_heretability, 
                    mouse_10_h0.3_dis_a$estimated_heretability, 
                    mouse_10_h0.3_dis_b$estimated_heretability, 
                    mouse_10_h0.3_dis_c$estimated_heretability)

colnames(pheno_10_h0.3) <- c("mouse_10_0.3_cont_a", "mouse_10_0.3_cont_b", "mouse_10_0.3_cont_c", 
                            "mouse_10_0.3_dis_a", "mouse_10_0.3_dis_b", "mouse_10_0.3_dis_c")
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

write.csv(pheno_10_h0.3, "mouse_pheno_h03_10.csv", row.names = FALSE)
write.csv(snps_10_h0.3, "mouse_snps_h03_10.csv", row.names = FALSE)
write.csv(effect_10_h0.3, "mouse_effect_h03_10.csv", row.names = FALSE)
write.csv(her_10_h0.3, "mouse_her_h03_10.csv", row.names = FALSE)


#------------------------------------------------------------------------------
#                         10 important SNPs with h = 0.1
#------------------------------------------------------------------------------

set.seed(8324783)
mouse_10_h0.1_cont_a <- gwas_sim(mouse, num_snps = 10, heretability = 0.1)
mouse_10_h0.1_cont_b <- gwas_sim(mouse, num_snps = 10, heretability = 0.1)
mouse_10_h0.1_cont_c <- gwas_sim(mouse, num_snps = 10, heretability = 0.1)

mouse_10_h0.1_dis_a <- gwas_sim(mouse, num_snps = 10, heretability = 0.1)
mouse_10_h0.1_dis_b <- gwas_sim(mouse, num_snps = 10, heretability = 0.1)
mouse_10_h0.1_dis_c <- gwas_sim(mouse, num_snps = 10, heretability = 0.1)

mouse_10_h0.1_dis_a$phenotype <- ifelse(mouse_10_h0.1_dis_a$phenotype > median(mouse_10_h0.1_dis_a$phenotype), 
                                       0, 1)
mouse_10_h0.1_dis_b$phenotype <- ifelse(mouse_10_h0.1_dis_b$phenotype > median(mouse_10_h0.1_dis_b$phenotype), 
                                       0, 1)
mouse_10_h0.1_dis_c$phenotype <- ifelse(mouse_10_h0.1_dis_c$phenotype > median(mouse_10_h0.1_dis_c$phenotype), 
                                       0, 1)

pheno_10_h0.1 <- cbind(mouse_10_h0.1_cont_a$phenotype, 
                      mouse_10_h0.1_cont_b$phenotype, 
                      mouse_10_h0.1_cont_c$phenotype, 
                      mouse_10_h0.1_dis_a$phenotype, 
                      mouse_10_h0.1_dis_b$phenotype, 
                      mouse_10_h0.1_dis_c$phenotype)

snps_10_h0.1 <- cbind(mouse_10_h0.1_cont_a$functional_snps, 
                     mouse_10_h0.1_cont_b$functional_snps, 
                     mouse_10_h0.1_cont_c$functional_snps, 
                     mouse_10_h0.1_dis_a$functional_snps, 
                     mouse_10_h0.1_dis_b$functional_snps, 
                     mouse_10_h0.1_dis_c$functional_snps)

effect_10_h0.1 <- cbind(mouse_10_h0.1_cont_a$effect, 
                       mouse_10_h0.1_cont_b$effect, 
                       mouse_10_h0.1_cont_c$effect, 
                       mouse_10_h0.1_dis_a$effect, 
                       mouse_10_h0.1_dis_b$effect, 
                       mouse_10_h0.1_dis_c$effect)

her_10_h0.1 <- cbind(mouse_10_h0.1_cont_a$estimated_heretability, 
                    mouse_10_h0.1_cont_b$estimated_heretability, 
                    mouse_10_h0.1_cont_c$estimated_heretability, 
                    mouse_10_h0.1_dis_a$estimated_heretability, 
                    mouse_10_h0.1_dis_b$estimated_heretability, 
                    mouse_10_h0.1_dis_c$estimated_heretability)

colnames(pheno_10_h0.1) <- c("mouse_10_0.1_cont_a", "mouse_10_0.1_cont_b", "mouse_10_0.1_cont_c", 
                            "mouse_10_0.1_dis_a", "mouse_10_0.1_dis_b", "mouse_10_0.1_dis_c")
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

write.csv(pheno_10_h0.1, "mouse_pheno_h01_10.csv", row.names = FALSE)
write.csv(snps_10_h0.1, "mouse_snps_h01_10.csv", row.names = FALSE)
write.csv(effect_10_h0.1, "mouse_effect_h01_10.csv", row.names = FALSE)
write.csv(her_10_h0.1, "mouse_her_h01_10.csv", row.names = FALSE)


#------------------------------------------------------------------------------
#                         50 important SNPs with h = 0.3
#------------------------------------------------------------------------------

set.seed(89430985)
mouse_50_h0.3_cont_a <- gwas_sim(mouse, num_snps = 50, heretability = 0.3)
mouse_50_h0.3_cont_b <- gwas_sim(mouse, num_snps = 50, heretability = 0.3)
mouse_50_h0.3_cont_c <- gwas_sim(mouse, num_snps = 50, heretability = 0.3)

mouse_50_h0.3_dis_a <- gwas_sim(mouse, num_snps = 50, heretability = 0.3)
mouse_50_h0.3_dis_b <- gwas_sim(mouse, num_snps = 50, heretability = 0.3)
mouse_50_h0.3_dis_c <- gwas_sim(mouse, num_snps = 50, heretability = 0.3)

mouse_50_h0.3_dis_a$phenotype <- ifelse(mouse_50_h0.3_dis_a$phenotype > median(mouse_50_h0.3_dis_a$phenotype), 
                                        0, 1)
mouse_50_h0.3_dis_b$phenotype <- ifelse(mouse_50_h0.3_dis_b$phenotype > median(mouse_50_h0.3_dis_b$phenotype), 
                                        0, 1)
mouse_50_h0.3_dis_c$phenotype <- ifelse(mouse_50_h0.3_dis_c$phenotype > median(mouse_50_h0.3_dis_c$phenotype), 
                                        0, 1)

pheno_50_h0.3 <- cbind(mouse_50_h0.3_cont_a$phenotype, 
                       mouse_50_h0.3_cont_b$phenotype, 
                       mouse_50_h0.3_cont_c$phenotype, 
                       mouse_50_h0.3_dis_a$phenotype, 
                       mouse_50_h0.3_dis_b$phenotype, 
                       mouse_50_h0.3_dis_c$phenotype)

snps_50_h0.3 <- cbind(mouse_50_h0.3_cont_a$functional_snps, 
                      mouse_50_h0.3_cont_b$functional_snps, 
                      mouse_50_h0.3_cont_c$functional_snps, 
                      mouse_50_h0.3_dis_a$functional_snps, 
                      mouse_50_h0.3_dis_b$functional_snps, 
                      mouse_50_h0.3_dis_c$functional_snps)

effect_50_h0.3 <- cbind(mouse_50_h0.3_cont_a$effect, 
                        mouse_50_h0.3_cont_b$effect, 
                        mouse_50_h0.3_cont_c$effect, 
                        mouse_50_h0.3_dis_a$effect, 
                        mouse_50_h0.3_dis_b$effect, 
                        mouse_50_h0.3_dis_c$effect)

her_50_h0.3 <- cbind(mouse_50_h0.3_cont_a$estimated_heretability, 
                     mouse_50_h0.3_cont_b$estimated_heretability, 
                     mouse_50_h0.3_cont_c$estimated_heretability, 
                     mouse_50_h0.3_dis_a$estimated_heretability, 
                     mouse_50_h0.3_dis_b$estimated_heretability, 
                     mouse_50_h0.3_dis_c$estimated_heretability)

colnames(pheno_50_h0.3) <- c("mouse_50_0.3_cont_a", "mouse_50_0.3_cont_b", "mouse_50_0.3_cont_c", 
                             "mouse_50_0.3_dis_a", "mouse_50_0.3_dis_b", "mouse_50_0.3_dis_c")
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

write.csv(pheno_50_h0.3, "mouse_pheno_h03_50.csv", row.names = FALSE)
write.csv(snps_50_h0.3, "mouse_snps_h03_50.csv", row.names = FALSE)
write.csv(effect_50_h0.3, "mouse_effect_h03_50.csv", row.names = FALSE)
write.csv(her_50_h0.3, "mouse_her_h03_50.csv", row.names = FALSE)


#------------------------------------------------------------------------------
#                         50 important SNPs with h = 0.1
#------------------------------------------------------------------------------

set.seed(4783945)
mouse_50_h0.1_cont_a <- gwas_sim(mouse, num_snps = 50, heretability = 0.1)
mouse_50_h0.1_cont_b <- gwas_sim(mouse, num_snps = 50, heretability = 0.1)
mouse_50_h0.1_cont_c <- gwas_sim(mouse, num_snps = 50, heretability = 0.1)

mouse_50_h0.1_dis_a <- gwas_sim(mouse, num_snps = 50, heretability = 0.1)
mouse_50_h0.1_dis_b <- gwas_sim(mouse, num_snps = 50, heretability = 0.1)
mouse_50_h0.1_dis_c <- gwas_sim(mouse, num_snps = 50, heretability = 0.1)

mouse_50_h0.1_dis_a$phenotype <- ifelse(mouse_50_h0.1_dis_a$phenotype > median(mouse_50_h0.1_dis_a$phenotype), 
                                        0, 1)
mouse_50_h0.1_dis_b$phenotype <- ifelse(mouse_50_h0.1_dis_b$phenotype > median(mouse_50_h0.1_dis_b$phenotype), 
                                        0, 1)
mouse_50_h0.1_dis_c$phenotype <- ifelse(mouse_50_h0.1_dis_c$phenotype > median(mouse_50_h0.1_dis_c$phenotype), 
                                        0, 1)

pheno_50_h0.1 <- cbind(mouse_50_h0.1_cont_a$phenotype, 
                       mouse_50_h0.1_cont_b$phenotype, 
                       mouse_50_h0.1_cont_c$phenotype, 
                       mouse_50_h0.1_dis_a$phenotype, 
                       mouse_50_h0.1_dis_b$phenotype, 
                       mouse_50_h0.1_dis_c$phenotype)

snps_50_h0.1 <- cbind(mouse_50_h0.1_cont_a$functional_snps, 
                      mouse_50_h0.1_cont_b$functional_snps, 
                      mouse_50_h0.1_cont_c$functional_snps, 
                      mouse_50_h0.1_dis_a$functional_snps, 
                      mouse_50_h0.1_dis_b$functional_snps, 
                      mouse_50_h0.1_dis_c$functional_snps)

effect_50_h0.1 <- cbind(mouse_50_h0.1_cont_a$effect, 
                        mouse_50_h0.1_cont_b$effect, 
                        mouse_50_h0.1_cont_c$effect, 
                        mouse_50_h0.1_dis_a$effect, 
                        mouse_50_h0.1_dis_b$effect, 
                        mouse_50_h0.1_dis_c$effect)

her_50_h0.1 <- cbind(mouse_50_h0.1_cont_a$estimated_heretability, 
                     mouse_50_h0.1_cont_b$estimated_heretability, 
                     mouse_50_h0.1_cont_c$estimated_heretability, 
                     mouse_50_h0.1_dis_a$estimated_heretability, 
                     mouse_50_h0.1_dis_b$estimated_heretability, 
                     mouse_50_h0.1_dis_c$estimated_heretability)

colnames(pheno_50_h0.1) <- c("mouse_50_0.1_cont_a", "mouse_50_0.1_cont_b", "mouse_50_0.1_cont_c", 
                             "mouse_50_0.1_dis_a", "mouse_50_0.1_dis_b", "mouse_50_0.1_dis_c")
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

write.csv(pheno_50_h0.1, "mouse_pheno_h01_50.csv", row.names = FALSE)
write.csv(snps_50_h0.1, "mouse_snps_h01_50.csv", row.names = FALSE)
write.csv(effect_50_h0.1, "mouse_effect_h01_50.csv", row.names = FALSE)
write.csv(her_50_h0.1, "mouse_her_h01_50.csv", row.names = FALSE)

