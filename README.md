# kmerizer

Given an input file in FASTA format (set "test.fa"):

 >HWI-ST885:65:C07WUACXX:8:1101:1330:2049 1:Y:0:ACAGTG
 TTCTTTCTCTTCCTCCGGCTACTTAGATGTTTCAGTTCACCGGGTTCCCCTCCA
 TACCCTATGGATTCAGA
 >HWI-ST885:65:C07WUACXX:8:1101:1267:2120 1:N:0:ACAGTG
 ATTCTCTGCGGCCTCTTTCGAGGCTCCCCTTTTTCCGAAGTTACGGGGTCAACT
 TGCCGAGTTCCTTAACAACCCTTCTCCCGTTGGCCTTAGGA

Create two files in "out" directory:

* ".kmers" file of k-sized "mers" (contiguous letters)

* ".locs" file read id and the number of k-mers in that read

The ".kmers" file is used with Jellyfish to query a suffix array.
The output of that is the k-mer and the number of times it was seen.
Then we'll use the ".locs" file to find match those lines with the 
read to calculate the mode.
