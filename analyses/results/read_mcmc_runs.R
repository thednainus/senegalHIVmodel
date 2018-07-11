setwd("~/Box Sync/my_R_packages/senegalHIVmodel/analyses/scripts/MaleX/bySubtype/C_m3/Preliminary_analysis/z-matrix")
fileNames <- Sys.glob("*.rds")


read_mcmc_rds(fileNames)

library(BayesianTools)

C_m3.m <- createMcmcSamplerList(list(r1,r2,r3,r4,r5,r6,r7,r8,r9,r10,
                                     r11,r12,r14,r15,r17))

C_m4.m <- createMcmcSamplerList(list(r1,r2,r3,r4,r5,r6,r7,r8,r9,r10,
                                     r11,r12,r13,r14,r15,r16,r17,r18))
