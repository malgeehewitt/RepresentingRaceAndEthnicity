cleanText<-function(raw.text, wordlist){
  clean.text<-tolower(raw.text)
  clean.text<-unlist(strsplit(clean.text, ""))
  clean.text<-clean.text[which(clean.text %in% c(LETTERS, letters, " "))]
  clean.text<-paste(clean.text, collapse="")
  clean.text<-unlist(strsplit(clean.text, " "))
  word.find<-which(clean.text %in% wordlist)
  if(length(word.find>0)){
    clean.text<-clean.text[word.find]
    clean.text<-paste(clean.text, collapse=" ")
    return(clean.text)
  } 
}

#date range is a two member vector of start date, end date
buildFreqTable<-function(meta.table, source.folder, date.range, wordlist){
  all.dates<-seq(date.range[1], date.range[2], by=1)
  sub.table<-meta.table[which(meta.table$Date %in% all.dates),]
  all.filenames<-paste(source.folder, sub.table$Filename, sep="/")
  all.texts<-lapply(all.filenames, function(x) scan(x, what='character', sep="\n", quiet=T))
  all.texts<-lapply(all.texts, function(x) paste(x, collapse=" "))
  all.texts<-lapply(all.texts, function(x) cleanText(x, wordlist))
  all.texts<-unlist(all.texts)
  library(tm)
  all.texts<-Corpus(VectorSource(all.texts))
  all.text.dtm<-DocumentTermMatrix(all.texts, control=list(wordLengths=c(1,Inf)))
  all.text.dtm<-as.matrix(all.text.dtm)
  return(all.text.dtm)
}

vectorRepresentations<-function(raw.dtm){
  tdm<-t(raw.dtm)
  raw.pca<-prcomp(tdm)
  summary.stats<-summary(raw.pca)
  summary.stats<-summary.stats$importance
  pos<-1
  sum<-0
  while(sum<.80){
    sum<-sum+summary.stats[2,pos]
    pos<-pos+1
  }
  print(pos)
  vector.representation<-raw.pca$x[,1:pos]
  return(vector.representation)
}