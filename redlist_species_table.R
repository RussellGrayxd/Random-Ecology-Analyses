library(rredlist)

key = "your IUCN redlist API key goes here"

sp_list <- # your list of species goes here
  
  
  # run an iterative function through  each species record
  measures <- lapply(sp_list, function(x) {
    x %>%
      rl_history(key = key)
  })


# plyr's rbind.fill version:
rbind.fill.plyr <- function(x) {
  rbind.fill(lapply(x,function(y){as.data.frame(t(y),stringsAsFactors=FALSE)}))
}

rbind.named.fill <- function(x) {
  nam <- sapply(x, names)
  unam <- unique(unlist(nam))
  len <- sapply(x, length)
  out <- vector("list", length(len))
  for (i in seq_along(len)) {
    out[[i]] <- unname(x[[i]])[match(unam, nam[[i]])]
  }
  setNames(as.data.frame(do.call(rbind, out), stringsAsFactors=FALSE), unam)
}

# put the output into a dataframe and save it
results <- rbind.fill.plyr(measures)
results.rm <- lapply(results, function(x) x[lengths(x) > 0])
results.df <- rbind.fill.plyr(t(results.rm))
df <- rbind.fill.plyr(results.df)
colnames(df) <- c("a","b")
df$a <- unlist(df$a)


# unlist the remaining data
df.new <- df %>% unnest(b) %>% 
  group_by(a)
colnames(df.new) <- c("Species", "Year,", "Code", "Category")

write.csv(df.new, "redlist_table.csv")