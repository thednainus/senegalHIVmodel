# script to blast the Senegal HIV DNA sequences to generate source sequences
# "Source" represents sequences from other countries, and their inclusion in the alignment will allow the model to account
# for importation of lineages to Senegal.
# CHANGE DIRECTORY AND FILE NAME ACCORDINGLY - I SHOULD HAVE ADDED A VARIABLE TO HANDLE THAT

from Bio import SeqIO
from miscellaneous import Misc
import os


if __name__ == "__main__":

    # CHANGE DIRECTORY/FILENAME HERE
    # fasta_output will be the filename to save the fasta sequences for best match sequences
    fasta_output = "/Users/user/Box Sync/HIV_myDesktop/data/C/additional_data/blast/no_duplicates/HIV.C.CGR.noDups.fasta"

    # CHANGE FILENAME HERE
    # gb_output will be the filename to save the genbank files for best match sequences
    gb_output = "/Users/user/Box Sync/HIV_myDesktop/data/C/additional_data/blast/no_duplicates/HIV.C.CGR.noDups.gb"

    # CHANGE FILENAME HERE
    # output name to save the source metadata info
    source_metadata = "/Users/user/Box Sync/HIV_myDesktop/data/C/additional_data/blast/no_duplicates/CGR.C.noDups.csv" #output name to save the source metadata info

    misc = Misc(fasta_output, gb_output, source_metadata)

    # CHANGE DIRECTORY/FILE NAME HERE
    dirname="/Users/user/Box Sync/HIV_myDesktop/data/C/additional_data/blast/no_duplicates"

    os.chdir(dirname)

    # CHANGE FILE NAME HERE!! xml file containing the blast results
    # Command used for blasting sequences:
    # blastn - db nt - query sequences.fasta - max_target_seqs 1 - max_hsps 5 - word_size 28 - evalue 0.001 - outfmt 5 - remote - out outfile_name.xml
    # where sequence.fasta is the fasta file for Senegal sequences
    # and outfile_name.xml is the name of the output file to save the results of blast
    filename="sn_lanl2016_prAln3_C_noDups_noOutgroup_megablast.xml"

    results = open(filename)

    # Gets the GenBank gi number list (https://www.ncbi.nlm.nih.gov/genbank/sequenceids/)
    gis_str = misc.check_blast_records(results)
    results.close()
    print gis_str

    # test if file containing the GenBank records exists (.gb file). If file exists, it opens the GenBank records from this file
    # instead of downloading it from GenBank
    gb_file = gb_output
    if os.path.isfile(gb_file):
        gb_records = SeqIO.parse(open(gb_file, "r"), "genbank")
        source = misc.get_source_genbank(gb_records)

    else:
        misc.download_genbank(gis_str)
        gb_records = SeqIO.parse(open(gb_file, "r"), "genbank")
        source = misc.get_source_genbank(gb_records)

    print source




