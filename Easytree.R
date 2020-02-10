########################################
library("metacoder")
library("rotl")
library("vegan")
library("dplyr")
library("taxize")
library("rgbif")
library("phytools")
########################################
#Use the name_suggest function to download records for many species based on higher taxonomic ranks
fam <- name_suggest(q = "Family Name", rank = "family")
occ_search(taxonKey = fam$key, return = "meta")$count
#this pulls your data from GBIF; change the limit to whatever you think is appropriate
dat <- occ_search(taxonKey = fam$key,return = "data", limit = 1000) #we limit the number of records to 10000, to restrict download time, remember to change this, if you want to download your own big dataset

#put taxonomic data into vectors
sp<-data.frame(table(dat$species))
ge<-data.frame(table(dat$genus))
fa<-data.frame(table(dat$family))
#collect single species names into a factor list
sp_list<-sp$Var1
#create a new dataframe with taxanomic info
taxa<-data.frame(table(dat$species, dat$genus, dat$family, dat$order, dat$class))
taxa<-select(taxa, -Freq)

#################################################################################
#set you NCBI API key
set_entrez_key("1a1d18fdeda88ca710de50e2fc5300b67709")
Sys.getenv("ENTREZ_KEY")

#obtain ncbi data on your species list
taxise_sp <- classification(sp_list, db = "ncbi")

#prepare your species list for a tree plot
tax_tree <- class2tree(taxise_sp)

#plot your tree
X11(width=8, height=20)
plot(tax_tree)
