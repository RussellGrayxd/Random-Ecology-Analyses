##############################################################
################### DNA Sequencing and Tree Building #########
##############################################################


setwd("C:/Users/Russe/Desktop/Ecology in R course/Lesson 7 Phylogenetics")

# install necessary packages
install.packages("haplotypes")
install.packages("adegenet")
remotes::install_github("fmichonneau/phyloch")

# open them in the library
library(haplotypes)
library(adegenet)
library(ape)
library(phyloch)


# read in the genome data, you will get a warning 
# since its in txt and not fasta, but it reads in properly
dna <- fasta2DNAbin("Amanita_genomes.txt")


# now we will find the genetic distances for individual pairs
# make a tree using the distances
D <- dist.dna(dna, model = "K80")
length(D) #number of pairwise distances, computed as n(n-1)/2
# histogram of sequences
hist(D, col="royalblue", nclass=14,
     main="Distribution of pairwise genetic distances",
     xlab="Number of differing nucleotides")


# first, we will convert the matrix to a data frame
temp <- as.data.frame(as.matrix(D))
# darker shades mean a larger distance 
# if you use color as well, but this is more intensive for 
# r graphics to process
# however, this won't work for our data
table.paint(temp, cleg=0, clabel.row=.5, clabel.col=.5) 



# this will create your "phylo" object
# for tree plotting
# note: if you're using nj and you have NA values in the
# distance matrix, use njs() instead.
# if you get errors about agglomeration criterion, change
# the fs= argument value
tre <- njs(D, fs=2)
class(tre) #all trees created using {ape} package will be of class phylo

plot(tre)

tre <- ladderize(tre)
tre # tells us what the tree will look like but doesn't show the actual construction