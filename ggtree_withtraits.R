
remotes::install_github("GuangchuangYu/treeio")
install.packages("BiocManager")
BiocManager::install("ggtree")


library("ape")
library("taxize")
library("rentrez")
library("phytools")
library("select")
library("treeio")
library("ggtree")
library("data.tree")
library("tidytree")
library("ggplot2")


#This page is super helpful
#https://yulab-smu.github.io/treedata-book/chapter2.html

#############################################################
setwd("C:/Users/Russell/Desktop/Catherina project/Tree files")

#import the tree file
trees <- read.newick(file = "amph_shl_new_Consensus_7238.tre")

#import trait data file 
dat<-read.csv("biodatawithenvironmentaldata.csv")

#plot the full tree at the risk of crashing R :)
trees %>% 
  ggtree() + 
  geom_tiplab() +
  theme_tree2()

#check names to use in sample subset
head(trees$tip.label)

#make a subset of the tree to make sure its not a cluster fuck
treesub<-tree_subset(trees, "Rhinatrema_bivittatum", levels_back = 5)
treesub %>% 
  ggtree(aes(color = group)) + 
  geom_tiplab() +
  theme_tree2() + 
  scale_color_manual(values = c(`1` = "red", `0` = "black")) +
  xlim(0, 200)


#break the tree into its basal parts
tibtree <- as_tibble(trees)

#remove that stupid "_" from between the genera and specific epithet
tibtree$label <- sub("_", " ", tibtree$label)

#keep only the body size data we want in the "dat" data frame
dat <- select(dat, binomial, Body_size_mm)
#remove duplicates
dat <-unique(dat)
#change column names for jonining
colnames(dat)[colnames(dat)=="Body_size_mm"] <- "trait"
colnames(dat)[colnames(dat)=="binomial"] <- "label"
dat$label<-as.character(dat$label)

#convert to tibble 
tibdat<-tibble(label=paste0(dat$label),
               trait=dat$trait)
#add traits to the tree tibble we just made and merge using scientific name
tib_tree_new  <- full_join(tibtree, tibdat, by = 'label')
#reclassify the tibble columns again
tib_tree_new$parent<-as.integer(tib_tree_new$parent)
tib_tree_new$node<-as.integer(tib_tree_new$node)
tib_tree_new <- tib_tree_new[c("parent","node","branch.length","label","trait")]
#make sure everything is good for conversion
tib_tree_new
class(tib_tree_new)

#remove duplicates
tib_tree_newND<-unique(tib_tree_new)

#convert into phylo object with trait data as a feature
newtree<-as.treedata(tib_tree_newND)

#subset the tree data (ignore the warning, nobody cares)
treesub_new<-tree_subset(newtree, "Pseudophryne occidentalis", levels_back = 5)

#plot the tree with trait values as colors on the branches
ggtree(treesub_new, aes(color=trait),
       ladderize = FALSE, continuous = TRUE, size=1) +
  scale_color_gradientn(colours=c("red", 'orange', 'green', 'cyan', 'blue')) +
  geom_tiplab(hjust = -.1, size=2, color = "black") + 
  theme(legend.position = c(0.80, .85))+
  xlim(0,120) +
  labs(color = "Body Size (mm)")


#here's a fancy thing!?!? Overlapping text though
ggtree(treesub_new, aes(color=trait), 
       continuous = TRUE, yscale = "trait") + 
  scale_color_viridis_c() + 
  theme_minimal()+
  geom_tiplab(hjust = -.1, size=2) +
  xlim(0,120) 
