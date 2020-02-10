library(paleobioDB)
library(paleotree)
library(Claddis)
library(strap)

dat <- pbdb_taxa (name=c("Colubridae", "Boidae", "Elapidae"), vocab="pbdb", 
                  show=c("attr", "app", "size", "nav", "phylo", "ident"), rel="children")
dat1 <- pbdb_occurrences(limit="all", vocab="pbdb",
                        base_name=c("Colubridae", "Boidae", "Elapidae"), show=c("phylo", "ident", "coords"))
#########################################

X11(width=13, height=7.8)
par(mfrow=c(1,2))
#get the taxon tree: Linnean method
ColubTree <- makePBDBtaxonTree(dat, "genus", method = "Linnean")
plot(ColubTree,cex = 0.4)
nodelabels(ColubTree$node.label,cex = 0.5)

#get the taxon tree: parentChild method
ColubTree <- makePBDBtaxonTree(dat, "genus", method = "parentChild", solveMissing = "mergeRoots")
plot(ColubTree,cex = 0.4)
nodelabels(ColubTree$node.label,cex = 0.5)

#get time data from occurrences
ColubOccGenus <- taxonSortPBDBocc(dat1,rank = "genus",onlyFormal = FALSE)
ColubTimeGenus <- occData2timeList(occList = ColubOccGenus)


#let's time-scale the parentChild tree with paleotree
# use minimum branch length for visualization
# and nonstoch.bin so we plot maximal ranges
timeTree <- bin_timePaleoPhy(ColubTree,timeList = ColubTimeGenus,
                             nonstoch.bin = TRUE,type = "mbl",vartime = 3)

library(strap)
#make pretty plot with library strap
X11(width=13, height=7.8)
geoscalePhylo(timeTree, ages = timeTree$ranges.used, cex.ts=0.9, cex.age = 0.8, 
              cex.tip = 0.8, erotate = 1, quat.rm = T, boxes = "Epoch", units = c("Period", "Epoch"))
nodelabels(text = c("Colubridae","Elapidae","Boidae"), cex = 1)

