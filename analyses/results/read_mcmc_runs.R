setwd("~/Box Sync/my_R_packages/senegalHIVmodel/analyses/scripts/Models/BySubtype/02_AG_m4/Preliminary_results/z-matrix/newZmatrix")
fileNames <- Sys.glob("*.rds")


read_mcmc_rds(fileNames)

library(BayesianTools)

#Good!
C_m1.m <- createMcmcSamplerList(list(r1,r2,r3,r4,r5,r6,r7,r8,r9,r10,r11,r12,
                                     r13,r15,r16,r17,r20,r22,
                                     r24))
#Not that good!
C_m2.m <- createMcmcSamplerList(list(r3,r5,r8,r9,r10,r12,
                                     r14,r16,r17,r18,r22,
                                     r23))

#good!
C_m3.m <- createMcmcSamplerList(list(r2,r3,r6,r7,r8,r9,r10,
                                     r14,r15,r19,r20,r22,
                                     r23,r24))


#good!
C_m4.m <- createMcmcSamplerList(list(r1,r2,r3,r4,r5,r6,r7,r8,r9,r10,
                                     r11,r12,r13,r14,r15,r18,
                                     r19,r20,r21,r22,r23))

#Okish
m2 <- createMcmcSamplerList(list(r1,r3,r4,r5,r6,r8))
#not good
m3 <- createMcmcSamplerList(list(r1,r2,r3,r4,r5,r6,r7,r8,r9,r12,r13,
                                 r16,r17,r18))
#good
m4 <- createMcmcSamplerList(list(r1,r3,r4,r5,r6,r8,r9))
#good
m5 <- createMcmcSamplerList(list(r1,r2,r3,r4,r5,r6,r7,r8,r9,r10))
#Okish
m6 <- createMcmcSamplerList(list(r1,r3,r5,r6,r8,r9,r10,r12,r13,
                                 r16,r17,r18))
#not that good
m7 <- createMcmcSamplerList(list(r1,r5,r6,r7,r8,r11,r12))

#good
AG_m3 <- createMcmcSamplerList(list(r3,r4,r5,r6,r8,r9,r10,r11,r13,r14))

#good 8,000 iter
AG_m4.1 <- createMcmcSamplerList(list(r1,r2,r3))

#good 6,000 iter
AG_m4.2 <- createMcmcSamplerList(list(r4,r5,r6,r7,r8,r10,r11))

