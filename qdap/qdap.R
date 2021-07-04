install.packages("qdap")
library(qdap)
?qdap
phrase_net(sent_detect(x), r=.5)

library(igraph)
plot(phrase_net(sent_detect(x), r=.5), edge.curved = FALSE)

## Declaration of Independence Example
y <- readLines("http://www.constitution.org/usdeclar.txt")
y <- paste(y[grep("When, in the", y):length(y)], collapse=" ")
phrase_net(sent_detect(y), r=.7)


## Multiple grouping variables
z <- lapply(split(raj.act.1$dialogue, raj.act.1$person), paste, collapse = " ")
par(mfrow=c(2, 5), mai = c(.05, 0.15, 0.15, 0.15))
lapply(seq_along(z), function(i) {
  x <- try(phrase_net(sent_detect(z[i]), r=.6))
  if (!inherits(x, "try-error")) {
    print(x)
    box()
    mtext(names(z)[i])
  }
}) 


lapply(seq_along(z), function(i) {
  x <- try(phrase_net(sent_detect(z[i]), r=.6))
  if (!inherits(x, "try-error")) {
    dev.new()
    print(x)
    mtext(names(z)[i], padj=-1, cex=1.7, col="red")
  }
}) 

## End(Not run)






## Not run: 
posdat <- pos(DATA$state)
ltruncdf(posdat, 7, 4)
## str(posdat)
names(posdat)
posdat$text           #original text

## Methods
preprocessed(posdat)  #words replaced with parts of speech
counts(posdat)        #frequency of parts of speech by row
proportions(posdat)   #proportion of parts of speech by row

## Methods Plotting
plot(preprocessed(posdat))
plot(counts(posdat))
plot(proportions(posdat))
plot(posdat)

out1 <- pos(DATA$state, parallel = TRUE) # not always useful
ltruncdf(out1, 7, 4)

#use pos_tags to interpret part of speech tags used by pos & pos_by
pos_tags()[1:10, ]
pos_tags("matrix")[1:10, ]
pos_tags("dataframe")[1:10, ]
pos_tags("df")[1:10, ]
ltruncdf(pos_tags("all"), 3)

posbydat <- with(DATA, pos_by(state, sex))
names(posbydat)

## Methods
scores(posbydat)   
preprocessed(posbydat)
counts(posbydat)     
proportions(posbydat)   

## Methods Plotting
plot(preprocessed(posbydat))
plot(counts(posbydat))
plot(proportions(posbydat))
plot(posbydat)

ltruncdf(posbydat, 7, 4)
truncdf(posbydat$pos.by.prop, 4)

POSby <- with(DATA, pos_by(state, list(adult, sex)))
plot(POSby, values = TRUE, digits = 2)
#or more quickly - reuse the output from before
out2 <- with(DATA, pos_by(posbydat, list(adult, sex)))

## Definite/Indefinite Noun 
## 2 approached compared...
## The later is more efficient but less accurate

## ------------------------##
## Part off speech tagging ##
## ------------------------##
pos_after <- function(text.var, words, pos){
  
  posses <- strsplit(as.character(text.var[["POStagged"]][["POStagged"]]), "\\s+")
  namespos <- lapply(posses, function(x) {
    y <- unlist(strsplit(x, "/"))
    setNames(y[c(TRUE, FALSE)], y[c(FALSE, TRUE)])
  })
  
  lapply(namespos, function(x, thewords = words, thepos = pos){
    locs <- which(x %in% thewords)
    locs <- locs[!is.na(locs)]
    
    if (identical(unclass(locs), integer(0))) return(NA_character_)
    
    nounlocs <- which(names(x) %in% thepos)
    
    unname(x[unique(sapply(locs, function(x){ 
      min(nounlocs[nounlocs - x > 0])
    }))])
  })  
}

out2 <- setNames(lapply(list(a=c("a", "an"), the="the"), function(x) {
  o <- pos_after(rajPOS, x, c("NN", "NNS", "NNP", "NNPS"))
  m <- stats::setNames(data.frame(sort(table(unlist(o))), 
                                  stringsAsFactors = FALSE), c("word", "freq"))
  m[m$freq> 3, ]
}), c("a", "the"))


dat2 <- setNames(Reduce(function(x, y) {
  merge(x, y, by = "word", all = TRUE)}, out2), c("Word", "A", "THE"))

dat2 <- reshape2::melt(dat2, id="Word", variable.name="Article", value.name="freq")

dat2 <- dat2[order(dat2$freq, dat2$Word), ]

ord2 <- aggregate(freq ~ Word, dat2, sum)

dat2$Word <- factor(dat2$Word, levels=ord2[order(ord2[[2]]), 1])
rownames(dat2) <- NULL
library(tidyverse)
ggplot(dat2, aes(x=freq, y=Word)) +
  geom_point()+ facet_grid(~Article) +
  ggtitle("Part Of Speech Parsing Approach")

dev.new()

## --------------------##
## Regular Expressions ##
## --------------------##

library(qdapRegex);library(ggplot2);library(reshape2)

out <- setNames(lapply(c("@after_a", "@after_the"), function(x) {
  o <- rm_default(stringi:::stri_trans_tolower(raj$dialogue),
                  pattern = x, extract=TRUE)
  m <- stats::setNames(data.frame(sort(table(unlist(o))), 
                                  stringsAsFactors = FALSE), c("word", "freq"))
  m[m$freq> 3, ]
}), c("a", "the"))

dat <- setNames(Reduce(function(x, y) {
  merge(x, y, by = "word", all = TRUE)}, out), c("Word", "A", "THE"))

dat <- reshape2::melt(dat, id="Word", variable.name="Article", value.name="freq")

dat <- dat[order(dat$freq, dat$Word), ]

ord <- aggregate(freq ~ Word, dat, sum)

dat$Word <- factor(dat$Word, levels=ord[order(ord[[2]]), 1])
rownames(dat) <- NULL
ggplot(dat, aes(x=freq, y=Word)) + 
  geom_point()+ facet_grid(~Article) + 
  ggtitle("Regex Approach")

## End(Not run)
