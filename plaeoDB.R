library(paleobioDB)

dat <- pbdb_occurrences(limit="all", vocab="pbdb",
      base_name=c("Ophidia"), show=c("phylo", "ident", "coords"))

table(dat$taxon_name)
list(dat$taxon_name)

X11(width=13, height=7.8)
pbdb_map(dat)

X11(width=13, height=7.8)
pbdb_map_occur(dat)


################################################
X11(width=13, height=7.8)
par(mfrow=c(2,1))
pbdb_map_richness(dat,res=8,rank="species")
mtext(expression(paste(bold("A"))),side=3, line=0.2, adj=0, outer=F)
pbdb_map_richness(dat,res=8,rank="genus")
mtext(expression(paste(bold("B"))),side=3, line=0.2, adj=0, outer=F)


################################################
X11(width=13, height=7)
par(mfrow=c(1,2))
pbdb_richness(dat, rank="species", res=40, temporal_extent = c(0,250))
mtext(expression(paste(bold("A"))),side=3, line=0.2, adj=0, outer=F)
pbdb_richness(dat, rank="genus", res=40, temporal_extent = c(0,250))
mtext(expression(paste(bold("B"))),side=3, line=0.2, adj=0, outer=F)

################################################
  
X11(width=13, height=7.8)
par(mfrow=c(2,2))
pbdb_orig_ext(dat, rank = "species", temporal_extent = c(0,200), orig_ext=c(1), res = 10) 
mtext(expression(paste(bold("A"))),side=3, line=0.2, adj=0, outer=F)
pbdb_orig_ext(dat, rank = "species", temporal_extent = c(0,200), orig_ext=c(2), colour = "red",res = 10)
mtext(expression(paste(bold("B"))),side=3, line=0.2, adj=0, outer=F) 
pbdb_orig_ext(dat, rank = "genus", temporal_extent = c(0,200), orig_ext=c(1),res = 10)
mtext(expression(paste(bold("C"))),side=3, line=0.2, adj=0, outer=F) 
pbdb_orig_ext(dat, rank = "genus", temporal_extent = c(0,200), orig_ext=c(2), colour = "red",res = 10)
mtext(expression(paste(bold("D"))),side=3, line=0.2, adj=0, outer=F)

