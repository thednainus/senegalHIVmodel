DNA sequence alignments used to generate maximum likelihood trees. Maximum likelihood trees were generated using the program [RAxML-NG](https://github.com/amkozlov/raxml-ng) version 0.5.1.

Basic command used to generate maximum likelihood trees using RAxML-NG:

'''bash
raxml-ng --search --msa alignment.fasta -model GTR+G --tree pars{1} --threads 2 --seed 125433

# alignment.fasta is a DNA sequence alignment
'''

Basic command used to generate bootstrapped maximum likelihood trees using RAxML-NG:

'''bash
raxml-ng --bootstrap --msa alignment.fasta -model GTR+G --bs-trees 1 --threads 2 --seed 543278

# alignment.fasta is a DNA sequence alignment
'''

We independently reconstructed phylogenetic trees for each subtype and using 4 different DNA substitution models: 

(1) GTR + Gamma (General Time Reversible plus gamma distribution with 4 categories); 
(2) GTR + I + Gamma (where I is the proportion of invariable sites); 
(3) GTR + R (where R is the FreeRate model \citep{soubrier_influence_2012}); and 
(4) GTR + Gamma and using 2 partitions for the data, one partition for first and second codon positions, and another partition for the third codon position. We also used the parsimony-based randomized stepwise addition trees as starting trees to search for the best ML tree.


Note that we implemented all maximum likelihood analyses using the computing resources of the [Open Science Grid (OSG)](https://opensciencegrid.org/). To estimate the ML tree, we run 20 jobs per alignment per DNA substitution model in parallel. We choose the tree with the highest likelihood as the best ML tree. Similarly, to calculate branch support for each tree, we run 1,000 independent bootstrapped trees per alignment in parallel which were merged into a single file to calculate the bootstrap support for each branch of the best ML tree.
