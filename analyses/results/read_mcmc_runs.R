# Set working directory containing the mcmc files
# I changed this for each of the models I used.
setwd("~/Box Sync/my_R_packages/senegalHIVmodel/analyses/scripts/Models/BySubtype/C_m2/Preliminary_results/withZmatrix/PARALLEL")
fileNames <- Sys.glob("*.rds")

library(BayesianTools)
library(senegalHIVmodel)

# reads the MCMC runs that were saved as RDS files
read_mcmc_rds(fileNames)

################################################################################
# All subtypes: Model 2
#PARALLEL: Longer runs 10,000 iterations/chain
m2.1 <- createMcmcSamplerList(list(r1, r2, r3, r4, r6, r7, r9, r10))
m2.2 <- createMcmcSamplerList(list(r11, r12, r13, r14, r16, r17, r18, r19))

save(m2.1, m2.2, file="Model2.rda")


# All subtypes: Model 3
#PARALLEL: Longer runs 10,000 iterations/chain
m3.1 <- createMcmcSamplerList(list(r1, r2, r4, r6, r7, r8, r9))
m3.2 <- createMcmcSamplerList(list(r10, r15, r18, r19, r21, r22, r25, r28))

save(m3.1, m3.2, file="Model3.rda")


# All subtypes: Model 4
#PARALLEL: Longer runs 10,000 iterations/chain
m4.1 <- createMcmcSamplerList(list(r1, r2, r4, r6, r7, r8, r9, r12))
m4.2 <- createMcmcSamplerList(list(r14, r15, r21, r22, r23, r25, r26, r28, r29))

save(m4.1, m4.2, file="Model4.rda")


# All subtypes: Model 5 (prevalence)
#PARALLEL: Longer runs 10,000 iterations/chain
m5.1 <- createMcmcSamplerList(list(r1, r2, r3, r4, r5, r6, r7, r8, r9, r11, r13,
                                   r14, r16, r17, r18))
m5.2 <- createMcmcSamplerList(list(r19, r20, r21, r22, r23, r25, r26, r27, r28,
                                   r29, r30, r31, r32, r33, r34, r35))

save(m5.1, m5.2, file="Model5.rda")

# All subtypes: Model 6 (prevalence)
#PARALLEL: Longer runs 12,000 iterations/chain
m6.1 <- createMcmcSamplerList(list(r1, r2, r3, r4, r6, r7))
#PARALLEL: Longer runs 10,000 iterations/chain
m6.2 <- createMcmcSamplerList(list(r11, r12, r15, r16, r20, r24, r25))

save(m6.1, m6.2, file="Model6.rda")


# All subtypes: Model 7 (prevalence)
#PARALLEL: Longer runs 10,000 iterations/chain
m7.1 <- createMcmcSamplerList(list(r1, r2, r4, r5, r8, r9))
m7.2 <- createMcmcSamplerList(list(r11, r12, r13, r14, r15, r16, r17, r18, r21))

save(m7.1, m7.2, file="Model7.rda")

################################################################################
# Subtype C: Model 1
#PARALLEL: Longer runs 12,000 iterations/chain
C_m1.1 <- createMcmcSamplerList(list(r1, r2, r3, r4, r5, r8, r9, r10))
C_m1.2 <- createMcmcSamplerList(list(r11, r13, r15, r17, r18, r19, r22, r23))

save(C_m1.1, C_m1.2, file="C_m1.rda")

# Subtype C: Model 2
#PARALLEL: Longer runs 15,000 iterations/chain
C_m2.1 <- createMcmcSamplerList(list(r1, r2, r6, r7))
C_m2.2 <- createMcmcSamplerList(list(r13, r20, r21, r22))

C_m2.3 <- createMcmcSamplerList(list(r1, r2, r6, r7, r13, r20, r21, r22))
C_m2.4 <- createMcmcSamplerList(list(r29, r30, r34, r36, r37, r38, r40))

save(C_m2.1, C_m2.2, file="C_m2.rda")
save(C_m2.3, C_m2.4, file="C_m2_longerRuns.rda")

# Subtype C: Model 3 (prevalence)
#PARALLEL: Longer runs 10,000 iterations/chain
C_m3.1 <- createMcmcSamplerList(list(r2, r3, r6, r9, r15, r17, r20))
#PARALLEL: Longer runs 12,000 iterations/chain saved @ more_analysis
C_m3.2 <- createMcmcSamplerList(list(r2, r4, r5, r8, r9, r10))

save(C_m3.1, file="C_m3.rda")
save(C_m3.2, file="C_m3.2.rda")


# Subtype C: Model 4 (prevalence)
#PARALLEL: Longer runs 12,000 iterations/chain
C_m4.1 <- createMcmcSamplerList(list(r3, r4, r5, r6, r7, r8, r9, r11, r12, r13,
                                     r14, r15, r18, r21, r22))
C_m4.2 <- createMcmcSamplerList(list(r25, r27, r29, r31, r33, r34, r35, r36,
                                     r37, r39, r40, r41, r43, r46, r47))

save(C_m4.1, C_m4.2, file="C_m4.rda")


################################################################################
# Subtype 02_AG: Model 1
#PARALLEL: Longer runs 10,000 iterations/chain
AG_m1.1 <- createMcmcSamplerList(list(r1, r2, r3, r5, r7, r8, r9, r10, r11, r13,
                                      r14, r15, r16, r17, r18))
AG_m1.2 <- createMcmcSamplerList(list(r21, r23, r24, r25, r26, r29, r30, r31,
                                      r32, r34, r37, r39, r40, r41, r42, r43))

save(AG_m1.1, AG_m1.2, file="AG_m1.rda")


# Subtype 02_AG: Model 2
#PARALLEL: Longer runs 10,000 iterations/chain
AG_m2.1 <- createMcmcSamplerList(list(r1, r2, r4, r5, r7, r9, r10, r11, r19))
AG_m2.2 <- createMcmcSamplerList(list(r24, r25, r27, r29, r30, r31, r32, r34,
                                      r38, r39))

save(AG_m2.1, AG_m2.2, file="AG_m2.rda")


# Subtype 02_AG: Model 3 (prevalence)
#PARALLEL: Longer runs 12,000 iterations/chain
AG_m3.1 <- createMcmcSamplerList(list(r1, r2, r3, r4))
#PARALLEL: Longer runs 10,000 iterations/chain
AG_m3.2 <- createMcmcSamplerList(list(r5, r6, r7, r8, r9, r10, r11, r12, r13,
                                      r14, r15))

save(AG_m3.1, AG_m3.2, file="AG_m3.rda")


# Subtype 02_AG: Model 4 (prevalence)
#PARALLEL: Longer runs 8,800 iterations/chain
AG_m4.1 <- createMcmcSamplerList(list(r1, r2, r3, r4, r5, r6, r7, r8, r9, r10,
                                      r11, r12, r14, r15, r16, r17))
AG_m4.2 <- createMcmcSamplerList(list(r18, r19, r20, r21, r22, r24, r25, r26,
                                      r27, r28, r29, r30, r31, r32, r33, r34))

save(AG_m4.1, AG_m4.2, file="AG_m4.rda")
