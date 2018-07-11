# 09 July 2018
# After checking if all HIV sequences were corecttly subtyped, I found that
# sequence FM210705 is from subtype G NOT subtype C
# Stpes to be completed:
# 1. Remove sequence FM210705 from the raxml phylogenetic tree
# 2. Check the src sequence associated to FM210705 and check its subtype (2nd
# best hit was a sequence also from Senegal, so additional G sequence was
# introduced in the phylogenetic tree)

library(ape)

#GTR+G
setwd("~/Box Sync/Senegal/HIV_myDesktop/data/C/additional_data/raxml-ng/no_duplicated/CGR/by_codon/C_CGR_GTR+G/")
C.GTR_G <- read.tree("ML_0_GTR+G.raxml.bestTree")
C.GTR_G_new <- drop.tip(C.GTR_G, "FM210705.C.SN.2004")
write.tree(C.GTR_G_new, "optimize_branch/ML_0_GTR+G.raxml_mod.bestTree")

#GTR+Gp12+3
setwd("~/Box Sync/Senegal/HIV_myDesktop/data/C/additional_data/raxml-ng/no_duplicated/CGR/by_codon/C_CGR_GTR+Gp")
C.GTR_Gp <- read.tree("ML_0_GTR+Gp.raxml.bestTree")
C.GTR_Gp_new <- drop.tip(C.GTR_Gp, "FM210705.C.SN.2004")
write.tree(C.GTR_Gp_new, "optimize_branch/ML_0_GTR+Gp.raxml_mod.bestTree")

#GTR+I+G
setwd("~/Box Sync/Senegal/HIV_myDesktop/data/C/additional_data/raxml-ng/no_duplicated/CGR/by_codon/C_CGR_GTR_I_G/")
C.GTR_G_I <- read.tree("ML_0_GTR+I+G.raxml.bestTree")
C.GTR_G_I_new <- drop.tip(C.GTR_G_I, "FM210705.C.SN.2004")
write.tree(C.GTR_G_I_new, "optimize_branch/ML_0_GTR+I+G.raxml_mod.bestTree")

#GTR+R
setwd("~/Box Sync/Senegal/HIV_myDesktop/data/C/additional_data/raxml-ng/no_duplicated/CGR/by_codon/C_CGR_GTR+R/")
C.GTR_R <- read.tree("ML_0_GTR+R.raxml.bestTree")
C.GTR_R_new <- drop.tip(C.GTR_R, "FM210705.C.SN.2004")
write.tree(C.GTR_R_new, "optimize_branch/ML_0_GTR+R.raxml_mod.bestTree")
