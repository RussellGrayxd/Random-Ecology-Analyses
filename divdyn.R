# DL URL
# https://paleobiodb.org/data1.2/occs/list.csv?&base_name=Ophidia&interval=Ediacaran,Holocene&show=class,classext,genus,subgenus,abund,coll,coords,loc,paleoloc,strat,stratext,lith,%20env,ref,crmod,%20timebins

library(paleobioDB)

dat <- pbdb_occurrences(limit="all", vocab="pbdb",
                        base_name=c("Serpentes"), show=c("phylo", "ident", "coords", "attr", "time", "abund", "crmod", "stratext", "lithext"))

#compile class and genus into a column
dat$clgen <- paste(dat$class, dat$genus)


library(divDyn)
data(keys)
data(bins)
data(stages)

# stratigraphi resolution
# 10my bin resolution
	
	binMin <- categorize(dat[ ,"early_interval"], keys$binInt) 
	binMax <- categorize(dat[ ,"late_interval"], keys$binInt)
	
	binMin <- as.numeric(binMin) 
	binMax <- as.numeric(binMax)

	dat$bin <- rep(NA, nrow(dat))

	binCondition <- c(
		# the early and late interval fields indicate the same bin
		which(binMax==binMin),
		# or the late_interval field is empty
		which(binMax==-1))
	
	# in these entries, use the bin indicated by the early_interval
	dat$bin[binCondition] <- binMin[binCondition]
	
	sampBin <- binstat(dat, tax="clgen", bin="bin", coll="collection_no", duplicates=FALSE)
	
	# the plot
	tsplot(stages, boxes="sys", shading="sys", xlim=60:95, ylim=c(0,400),
	ylab="Number of entries", xlab="Age (Ma)")
	# occurrences
	lines(bins$mid, sampBin$occs, lwd=2)
	# collections
	lines(bins$mid, sampBin$colls, lwd=2, col="blue")
	# legend
	legend("topright", bg="white", legend=c("occurrences", "collections"),
	col=c("black", "blue"), lwd=2, inset=c(0.15,0.01), cex=1)

# stage resolution
	# the 'stg' entries (lookup)
	stgMin <- categorize(dat[ ,"early_interval"], keys$stgInt)
	stgMax <- categorize(dat[ ,"late_interval"], keys$stgInt)
	# convert to numeric
	stgMin <- as.numeric(stgMin)
	stgMax <- as.numeric(stgMax)
	
	#empty container
	dat$stg <- rep(NA, nrow(dat))
	# select entries, where
	stgCondition <- c(
	# the early and late interval fields indicate the same stg
	which(stgMax==stgMin),
	# or the late_intervar field is empty
	which(stgMax==-1))
	# in these entries, use the stg indicated by the early_interval
	dat$stg[stgCondition] <- stgMin[stgCondition]

	# we do not need to bother doing anything with early-paleozoic
	sampStg <- binstat(dat, tax="clgen", bin="stg", coll="collection_no", duplicates=FALSE)
	
	# the plot
	tsplot(stages, boxes="sys", shading="sys", xlim=60:95, ylim=c(0,400),
	ylab="Number of entries", xlab="Age (Ma)")
	# occurrences
	lines(stages$mid, sampStg$occs, lwd=2)
	# collections
	lines(stages$mid, sampStg$colls, lwd=2, col="blue")
	# legend
	legend("top", bg="white", legend=c("occurrences", "collections"),
	col=c("black", "blue"), lwd=2, inset=c(0.15,0.01), cex=1)
	
# basic function run
	stageDD<- divDyn(dat, tax="clgen", bin="stg")
	binDD<- divDyn(dat, tax="clgen", bin="bin")

	x11(width=18, height=6)
	par(mfrow=c(1,3))
	# stages
	tsplot(stages, boxes="sys", shading="sys", xlim=60:95, ylim=c(0,50),
		ylab="Richness at the stage-level", xlab="Age (Ma)")

	lines(stages$mid[1:max(stageDD$bin, na.rm=T)], stageDD$divRT)
	lines(stages$mid[1:max(stageDD$bin, na.rm=T)], stageDD$divSIB, col="red")

	# bisn
	tsplot(stages, boxes="sys", shading="sys", xlim=60:95, ylim=c(0,80),
		ylab="Richness on the 10my bin-level", xlab="Age (Ma)")

	lines(bins$mid[1:max(binDD$bin, na.rm=T)], binDD$divRT)
	lines(bins$mid[1:max(binDD$bin, na.rm=T)], binDD$divSIB, col="red")

	legend("top", bg="white", legend=c("RT", "SIB"),
		col=c("black", "red"), lwd=2, inset=c(0.15,0.01), cex=1)



# just as an example: the basic subsampling run - the number of records is hopelesslylow
	# classical rarefaction
	crDD<- subsample(dat, tax="clgen", bin="bin", q=10)

	# dashed lines on panel 2
	lines(bins$mid[1:max(crDD$bin, na.rm=T)], crDD$divSIB, col="red", lty=2)

# mean ages (possible, but not recommended)
	dat$meanMA<- apply(dat[,c("max_ma", "min_ma")], 1,mean)
	bin15<- divDyn(dat, tax="clgen", bin="meanMA", ages=TRUE, breaks=seq(300, 0, -15))
	
	tsplot(stages, boxes="sys", shading="sys", xlim=60:95, ylim=c(0,80),
		ylab="Richness with mean occ. binned to 15my intervals", xlab="Age (Ma)")

	lines(bin15$bin, bin15$divRT)
	lines(bin15$bin, bin15$divSIB, col="red")