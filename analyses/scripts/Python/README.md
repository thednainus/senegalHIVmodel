In our analyses we added other DNA sequences not from Senegal that we referred them to as "source". The inclusion of "source" sequences in the DNA sequence alignment will allow the model to account for importation of lineages to Senegal.

To get these "source" sequences we used *blastn* using the the [BLAST command line applications](https://www.ncbi.nlm.nih.gov/books/NBK279690/). 

The *blastn* command we used to get such sequences were:
```bash
blastn -db nt -query filename.fasta -max_target_seqs 1 -max_hsps 5 -word_size 28 -evalue 0.001 -outfmt 5 -remote -out outfile_name.xml

# where filename.fasta is the name of the fasta file containing the sequences you would like to blast
# and outfile_name.xml is the name of the output file to save the blast results.
```

---
These scripts require [Python](https://www.python.org/) version 2.7.
Before running the Python scrips you will need to download [Biopython](https://biopython.org/wiki/Download).
