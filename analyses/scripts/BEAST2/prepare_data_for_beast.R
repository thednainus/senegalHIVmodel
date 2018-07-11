# Scipts that reads a fasta file and change the sequence names according to
# information on csv file

library(ape)

# Reading metadata for Senegal only samples and close global reference (CGR) samples
SN.data <- read.csv(system.file("data/HIV_subtypes_summary_SENEGAL_noDups.csv", package = "senegalHIVmodel"))
CGR.data <-  read.csv(system.file("data/HIV_subtypes_summary_CGR.csv", package = "senegalHIVmodel"))

# organize metadata in 2 columns.
# the first column is the sequence names
# the second colum is the state (gpf, gpm, msm, or src) of each sequences
all_data <- organize_metadata(CGR.data, SN.data)

all_data_bySubtype <- organize_metadata_bySubtype(SN.data, CGR.data, code = 1)

##### subtype B ########
B.SN.df <- all_data_bySubtype$B_all_data[[1]]
B.missing_sample <- all_data_bySubtype$B_all_data[[2]]

B.CGR.df <- all_data_bySubtype$B_all_CGR[[1]]
B.CGR.missing_sample <- all_data_bySubtype$B_all_CGR[[2]]

B.info <- beast_phydyn(B.missing_sample, B.CGR.missing_sample, B.SN.df, B.CGR.df, all_data)


##### subtype C ########
C.SN.df <- all_data_bySubtype$C_all_data[[1]]
C.missing_sample <- all_data_bySubtype$C_all_data[[2]]

C.CGR.df <- all_data_bySubtype$C_all_CGR[[1]]
C.CGR.missing_sample <- all_data_bySubtype$C_all_CGR[[2]]

C.info <- beast_phydyn(C.missing_sample, C.CGR.missing_sample, C.SN.df, C.CGR.df, all_data)

##### subtype 02_AG ########
AG.SN.df <- all_data_bySubtype$AG_all_data[[1]]
AG.missing_sample <- all_data_bySubtype$AG_all_data[[2]]

AG.CGR.df <- all_data_bySubtype$AG_all_CGR[[1]]
AG.CGR.missing_sample <- all_data_bySubtype$AG_all_CGR[[2]]

AG.info <- beast_phydyn(AG.missing_sample, AG.CGR.missing_sample, AG.SN.df, AG.CGR.df, all_data)

##### merge all info data #####

all_info <- rbind(B.info, C.info, AG.info)

###############################################################################
# Read fasta alignment for subtype B
B.seq <- read.FASTA(system.file("data/alignments/by_codon/masked_B_plus_additional_seq_CGR_noDups_ali.fasta", package = "senegalHIVmodel"))

B.seq_mod <- B.seq
# Replaces the tip names in the sequence file to the names
# containing ages as decimal values
names(B.seq_mod) <- B.info$beast_tip[match(names(B.seq), B.info$tip)]

names(B.seq_mod) <- B.info$phydyn_tip[match(names(B.seq) , B.info$tip)]


# write the modified version of the sequence file to another fasta file
# only the tip names were modified to be easier for BEAST to read
# the dates on the tips
#write.dna(x = B.seq_mod, file = "inst/data/alignments/B_SN_CGR.fasta",
#          format = "fasta")

#write.dna(x = B.seq_mod, file = "inst/data/alignments/B_SN_CGRphydyn.fasta",
#                    format = "fasta")

###############################################################################
# Read fasta alignment for subtype C
C.seq <- read.FASTA(system.file("data/alignments/by_codon/masked_C_plus_additional_seq_andCGR_noDups_ali_mod.fasta", package = "senegalHIVmodel"))

C.seq_mod <- C.seq
# Replaces the tip names in the sequence file to the names
# containing ages as decimal values
names(C.seq_mod) <- C.info$beast_tip[match(names(C.seq), C.info$tip)]

names(C.seq_mod) <- C.info$phydyn_tip[match(names(C.seq) , C.info$tip)]


# write the modified version of the sequence file to another fasta file
# only the tip names were modified to be easier for BEAST to read
# the dates on the tips
#write.dna(x = C.seq_mod, file = "inst/data/alignments/C_SN_CGR.fasta",
#          format = "fasta")

#write.dna(x = C.seq_mod, file = "inst/data/alignments/C_SN_CGRphydyn.fasta",
#                    format = "fasta")


###############################################################################
# Read fasta alignment for subtype 02_AG
AG.seq <- read.FASTA(system.file("data/alignments/by_codon/masked_02_AG_plus_additional_seq_noDups_CGR_ali_mod.fasta", package = "senegalHIVmodel"))

AG.seq_mod <- AG.seq
# Replaces the tip names in the sequence file to the names
# containing ages as decimal values
names(AG.seq_mod) <- AG.info$beast_tip[match(names(AG.seq), AG.info$tip)]
names(AG.seq_mod) <- AG.info$phydyn_tip[match(names(AG.seq) , AG.info$tip)]


# write the modified version of the sequence file to another fasta file
# only the tip names were modified to be easier for BEAST to read
# the dates on the tips
#write.dna(x = AG.seq_mod, file = "inst/data/alignments/AG_SN_CGR.fasta",
#          format = "fasta")

#write.dna(x = AG.seq_mod, file = "inst/data/alignments/AG_SN_CGRphydyn.fasta",
#                    format = "fasta")







# do the same type of name manipulation with the datedtree estimated for
# subtype B
#B.dtr <- readRDS(system.file("data/trees_by_subtype/dtr.B.CGR.GTR_Gp12+3_byCodon.RDS",
#                             package = "senegalHIVmodel"))

#B.dtr_mod <- B.dtr

#B.dtr_mod$tip.label <- B.df$phydyn_tip[match(B.dtr_mod$tip.label, B.df$tip)]
#B.dtr_mod2 <- drop.tip(B.dtr_mod, "KU685093.B.SN_2010.625_NA")
# Dated tree estimated with treedater
#write.tree(B.dtr_mod2, file = "inst/data/trees_by_subtype/dtr.B.CGR.GTR_Gp12+3_byCodon.tre")


# subtype 02_AG
AG.dtr <- readRDS(system.file("data/trees_by_subtype/dtr.AG.CGR.GTR_Gp12+3_byCodon.RDS",
                             package = "senegalHIVmodel"))
AG.dtr_mod <- AG.dtr

AG.dtr_mod$tip.label <- AG.info$phydyn_tip[match(AG.dtr_mod$tip.label, AG.info$tip)]

# get the tips that is NA (does not have a compartment associated to it)
tip2isNA <- function(tip)
{
  if ( ! grepl('SN' , tip )) return(FALSE)
  ano <- strsplit( tip, '\\_')[[1]][4]
  print(ano)
  if(ano == "NA") return(TRUE)
  return(FALSE)
}

tipisNA <- sapply( AG.dtr_mod$tip.label, tip2isNA )

AG.dtr_mod2 <- drop.tip(AG.dtr_mod, AG.dtr_mod$tip.label[tipisNA] )

# Dated tree estimated with treedater
write.tree(AG.dtr_mod2, file = "inst/data/trees_by_subtype/dtr.AG.CGR.GTR_Gp12+3_byCodon.tre")


# subtype C
C.dtr <- readRDS(system.file("data/trees_by_subtype/dtr.C.CGR.GTR_Gp12+3_byCodon.RDS",
                              package = "senegalHIVmodel"))
C.dtr_mod <- C.dtr

C.dtr_mod$tip.label <- C.info$phydyn_tip[match(C.dtr_mod$tip.label, C.info$tip)]
names(C.dtr_mod$sts) <- C.dtr_mod$tip.label
# get the tips that is NA (does not have a compartment associated to it)
tip2isNA <- function(tip)
{
  if ( ! grepl('SN' , tip )) return(FALSE)
  ano <- strsplit( tip, '\\_')[[1]][3]
  print(ano)
  if(ano == "NA") return(TRUE)
  return(FALSE)
}

tipisNA <- sapply( C.dtr_mod$tip.label, tip2isNA )

C.dtr_mod2 <- drop.tip(C.dtr_mod, C.dtr_mod$tip.label[tipisNA] )
names(C.dtr_mod2$sts) <- C.dtr_mod2$tip.label

# Dated tree estimated with treedater
write.tree(C.dtr_mod2,
           file = "~/Box Sync/my_R_packages/senegalHIVmodel/inst/data/trees_by_subtype/dtr.C.CGR.GTR_Gp12+3_byCodon.tre")



