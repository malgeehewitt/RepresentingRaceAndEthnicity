#bind table x to table y, adding values that exist in both
tableBind<-function(x,y){
  if(is.na(y[1])){
    return(x)
  }
  if(is.na(x[1])){
    x<-y
    return(y)
  }
  x[which(names(x) %in% names(y))]<-x[which(names(x) %in% names(y))]+y[which(names(y) %in% names(x))]
  x<-c(x,y[which(!names(y) %in% names(x))])
  x<-x[order(names(x))]
  return(x)
}

cleanReturn<-function(collocate.table){
  bad.cols<-which(is.na(collocate.table))
  if(length(bad.cols)>0){
    collocate.table<-collocate.table[-bad.cols]
  }
  return(collocate.table)
}

#add elements from a table of collcoates to a pre-populated list
tablePrePopFill<-function(x,y){
  if(is.na(y[1])){
    return(x)
  }
  matches<-which(names(x) %in% names(y))
  if(length(matches>0)){
    x[matches]<-x[matches]+y[which(names(y) %in% names(x))]
  }
  return(x)
}

#strip all non-letters from text
textClean<-function(text){
  text<-tolower(text)
  text<-unlist(strsplit(text, ''))
  text<-text[which(text %in% c(letters, ' '))]
  text<-paste(text, collapse='')
  return(text)
}

#in a given text, join all ngram targets using sep.char
joinTargets<-function(target.split, text.split, sep.char="_"){
  target.split<-unlist(strsplit(target.split, ' '))
  if(length(target.split)==1){
    return(text.split)
  }
  join.gram<-paste(target.split, collapse=sep.char)
  potential.joins<-which(text.split==target.split[1])
  if(length(potential.joins)==0){
    return(text.split)
  }
  if((potential.joins[length(potential.joins)]+length(target.split))>length(text.split)){
    potential.joins<-potential.joins[-length(potential.joins)]
  }
  for(i in 1:((length(target.split))-1)){
    if(length(potential.joins)==0){
      return(text.split)
    }
    rank.check<-potential.joins+i
    potential.joins<-potential.joins[which(text.split[rank.check]==target.split[i+1])]
  }
  text.split[potential.joins]<-join.gram
  rank.seq<-seq(1,(length(target.split)-1), by=1)
  remove.chars<-lapply(rank.seq, function(x) potential.joins+x)
  remove.chars<-unlist(remove.chars)
  text.split<-text.split[-remove.chars]
  return(text.split)
}

joinNgrams<-function(target, sep.char="_"){
  target<-unlist(strsplit(target, " "))
  if(length(target>1)){
    target<-paste(target, collapse=sep.char)
  }
  return(target)
}

#test a 2x2 contingency table for significance with a fisher's exact test
sigTest<-function(obs,exp,nobs, nexp){
  contingency.table<-matrix(c(obs,exp,nobs,nexp), ncol=2, byrow=T)
  word.p<-tryCatch(fisher.test(contingency.table, alternative="g")$p.value, error=function(cond){return(1)})
  return(word.p)
}

#given a table of collocates and a table of word frequencies, prepare a list of 2X2 contingency tables 
#to pass to sigTest
#wordMDCollocates<-function(raw.collocates, all.freq, alpha){
wordMDCollocates<-function(target.term, raw.collocates, all.freq, alpha){
  #target.term<-names(raw.collocates)
  na.index<-which(is.na(raw.collocates))
  if(length(na.index)>0){
    raw.collocates<-raw.collocates[-na.index]
  }
  print(target.term)
  raw.collocates<-raw.collocates[order(names(raw.collocates))]
  total.collocates<-sum(raw.collocates)
  all.freq<-all.freq[which(names(all.freq) %in% names(raw.collocates))]
  all.freq<-all.freq[order(names(all.freq))]
  exp.collocates<-all.freq*total.collocates
  raw.collocates<-raw.collocates[which(names(raw.collocates) %in% names(exp.collocates))]
  greater.index<-which(raw.collocates>exp.collocates)
  raw.collocates<-raw.collocates[greater.index]
  exp.collocates<-exp.collocates[greater.index]
  obs.non.collocates<-total.collocates-raw.collocates
  exp.non.collocates<-total.collocates-exp.collocates
  p.values<-unlist(mapply(function(w,x,y,z) sigTest(w,x,y,z), raw.collocates, exp.collocates, obs.non.collocates, exp.non.collocates))
  num.sig<-which(p.values<=alpha)
  if(length(num.sig)==0){
    print(paste("No Significant Collocates of", target.term, sep=" "))
    return(NA)
  }
  sig.collocates<-raw.collocates[num.sig]
  if(class(sig.collocates)=="table"){
    table.names<-names(sig.collocates)
    sig.collocates<-as.integer(sig.collocates)
    names(sig.collocates)<-table.names
  }
  exp.collocates<-exp.collocates[num.sig]
  obs.exp<-sig.collocates/exp.collocates
  obs.adj<-sig.collocates/total.collocates
  target.df<-data.frame(names(sig.collocates), sig.collocates, obs.adj, obs.exp, p.values[num.sig], rep(target.term, length(sig.collocates)), stringsAsFactors=F)
  colnames(target.df)<-c("Term", "NObs", "NObsScale", "Obs_Exp", "PValues", "Target")
  target.df<-target.df[order(target.df$Obs_Exp, decreasing=T),]
  return(target.df)
}

#given a target term, a horizon and a text, gather all collocats from the text and return a table of the collocates
gatherCollocatesText<-function(target.term, horizon, text.split, stop.words, limiter=NULL){
  text.split<-joinTargets(target.term, text.split)
  target.term<-joinNgrams(target.term)
  #print(target.term)
  target.match<-which(text.split==target.term)
  #print(length(target.match))
  if(length(target.match)==0){
    return(NA)
  }
  windows.open<-target.match-horizon
  windows.close<-target.match+horizon
  open.error<-which(windows.open<1)
  if(length(open.error)>0){
    windows.open[open.error]<-1
  }
  close.error<-which(windows.close>length(text.split))
  if(length(close.error)>0){
    windows.close[close.error]<-length(text.split)
  }
  col.term.index<-unlist(mapply(function(x,y) seq(x,y,by=1), windows.open, windows.close))
  all.collocates<-text.split[col.term.index]
  collocate.table<-table(all.collocates)
  number.hits<-length(target.match)
  if(collocate.table[which(names(collocate.table)==target.term)]==number.hits){
    collocate.table<-collocate.table[-which(names(collocate.table)==target.term)]
  } else {
    collocate.table[which(names(collocate.table)==target.term)]<-collocate.table[which(names(collocate.table)==target.term)]-number.hits
  }
  if(!is.null(stop.words)){
    collocate.table<-collocate.table[-which(names(collocate.table) %in% stop.words)]
  }
  if(!is.null(limiter)){
    limited.matches<-which(names(collocate.table) %in% limiter)
    if(length(limited.matches>0)){
      collocate.table<-collocate.table[limited.matches]
      return(collocate.table)
    } else {
      return(NA)
    }
  } else {
    return(collocate.table)
  }
}

#identify all duplicate collocates and assign the collocate to the target with the highest obs over expected
collapseColTable<-function(full.table){
  possible.targets<-rep(NA, nrow(full.table))
  unique.targets<-unique(full.table$Term)
  for(i in 1:length(unique.targets)){
    collocate.index<-which(full.table$Term==unique.targets[i])
    if(length(collocate.index==1)){
      possible.targets[collocate.index]<-full.table$Target[collocate.index]
    } else {
      all.groups<-full.table$Target[collocate.index]
      all.groups<-paste(all.groups, collapse="|")
      possible.targets[which(full.table$Obs_Exp[collocate.index]==min(full.table$Obs_Exp[collocate.index]))]<-all.groups
    }
  }
  full.table$PotentialGroups<-possible.targets
  remove.index<-which(is.na(full.table$PotentialGroups))
  if(length(remove.index>0)){
    full.table<-full.table[-remove.index,]
  }
  return(full.table)
}

#wrapping function for getting all collocates and creating output tables
#meta.table requires a column with "Filename"
#if date.filter is not NULL, requires a meta.table with "Date" column, date.filter takes a min and max date
#stopwords requires a vector of stop words
#bind.groups will bind together the first column of the target table with groups from the second column

getMDWCollocates<-function(target.list, 
                           horizon, 
                           source.folder, 
                           #meta.table, 
                           output.folder, 
                           alpha=0.05, 
                           date.filter=NULL, 
                           collapseTable=F, 
                           dictionary=NULL,
                           col.targets=NULL,
                           stop.words=NULL,
                           bind.groups=F,
                           min.obs=5,
                           min.texts=NULL){
  #code to parallelize processes
  library(parallel)
  #ncore<-detectCores()-2
  #clust.proc<-makeCluster(ncore, type="FORK")
  #if there is a date filter vector, print it
  if(!is.null(date.filter)){
    print(date.filter)
  }
  #if bind.groups is true, collocates will be assigned to targets as well as second order groups
  #in that case, the groups are passed as the second column of the target matrix - here they are split into separate variables
  if(bind.groups){
    target.groups<-target.list[,2]
    target.list<-target.list[,1]
  }
  #if there is a date filter, subset the metadata matrix to only include those dates
  if(!is.null(date.filter)){
    date.range<-seq(date.filter[1], date.filter[2], by=1)
    meta.table<-meta.table[which(meta.table$Date %in% date.range),]
    #if sample.texts=T, sample the number of texts from each date range equal to the number of texts in the smallest date range
    if(!is.null(min.texts)){
      meta.table<-meta.table[sample(nrow(meta.table), min.texts),]
    }
  }
  
  #get all files from the source folder
  files<-list.files(source.folder, full.names=T)
  files<-files[6000:6060]
  
  #subset the list of files to only include those in the metadata file (requires a $Filename column)
  #meta.table<-meta.table[which(meta.table$Filename %in% files),]
  #append directories to filenames
  #files<-paste(source.folder, meta.table$Filename, sep="/")
  #create an empty list (NA) for each target term
  target.collocates<-lapply(target.list, function(x) list())
  #name the list for the targets
  names(target.collocates)<-target.list
  #initialize Text Count
  text.count<-list()
  print(paste("Number of Texts:", as.character(length(files)),sep=' '))
  print("Gathering Collocates")
  #return(target.collocates)
  #for each text
  for(i in 1:length(files)){
    #scan the text
    #print(files[i])
    curr.text<-scan(files[i], what='character', sep="\n", quiet = T)
    #print the number of the texts remaining
    remaining<-length(files)-i
    if((remaining%%20)==0){
      print(remaining)
    }
    #remove newlines
    curr.text<-paste(curr.text, collapse=" ")
    #clean text
    curr.text<-textClean(curr.text)
    #split text into vector
    curr.text<-unlist(strsplit(curr.text, " "))
    #strip any non-dictionary words
    if(!is.null(dictionary)){
      curr.text<-curr.text[which(curr.text %in% dictionary)]
    }
    #old code
    #target.list<-lapply(target.list, function(x) joinNgrams(x))
    #print(target.list)
    
    #parallel apply the function to create a table of collocates for each target
    #collocate.tables<-parLapply(clust.proc, target.list, function(x) gatherCollocatesText(x, horizon, curr.text, stop.words))
    collocate.tables<-lapply(target.list, function(x) gatherCollocatesText(x, horizon, curr.text, stop.words))
    
    #stopCluster(clust.proc)
    #return(collocate.tables)
    #print("Binding Cols")
    #target.collocates<-mapply(function(x,y) c(x,y), target.collocates, collocate.tables)
    for(i in 1:length(target.collocates)){
      target.collocates[[i]]<-c(target.collocates[[i]], list(collocate.tables[[i]]))
    }
    #print("Binding Words")
    text.count<-c(text.count, list(table(curr.text)))
    
    #depreciated for tapply method
    #for each collocate table in the list, bind it into the master list of collocate tables (1 for each target)
    #target.collocates<-mapply(function(x,y) tableBind(x,y), target.collocates, collocate.tables)
    #bind the running count of each word in the corpus as a whole (to get final frequencies)
    #text.count<-tableBind(text.count, table(curr.text))
    #clean up the workspace
    remove(collocate.tables)
    gc(verbose=F)
  }
  print("Binding Collocates")
  target.collocates<-lapply(target.collocates, function(x) unlist(x))
  target.collocates<-lapply(target.collocates, function(x) cleanReturn(x))
  target.col.lengths<-lapply(target.collocates, function(x) length(x))
  zero.cols<-which(target.col.lengths==0)
  if(length(zero.cols)>0){
    target.collocates<-target.collocates[-zero.cols]
  }
  target.collocates<-lapply(target.collocates, function(x) tapply(x, names(x), sum))
  text.count<-unlist(text.count)
  text.count<-tapply(text.count, names(text.count), sum)
  
  #check and see which targets have no collocates
  nocol.index<-which(is.na(target.collocates))
  if(length(nocol.index>0)){
    #if there are any, identify them
    nocol.words<-names(target.collocates[nocol.index])
    #if there are more than one, paste them into a string
    if(nocol.words>1){
      nocol.words<-paste(nocol.words, collapse=", ")
    }
    #display the targets with no collocates
    print(paste("No collocates found for:", nocol.words, sep=" "))
    #remove targets from list of collocate vectors for significance assessment
    target.collocates<-target.collocates[-nocol.index]
    #remove targets with no collocates from the list of targets and list of groups
    target.list<-target.list[-nocol.index]
    target.groups<-target.groups[-nocol.index]
  }
  print("Calculating Collocates")
  #calculate frequencies for all of the words in the dictionary
  all.freqs<-text.count/sum(text.count)
  #calculate which collocates are significant for which targets - can this be paralellized? Why is target.list there?
  #ptm<-proc.time()
  #names(target.collocates)<-target.list
  #all.col.tables<-parLapply(clust.proc, target.collocates, function(x) wordMDCollocates(x, all.freqs, alpha))
  all.col.tables<-mapply(function(x,y) wordMDCollocates(x,y, all.freqs, alpha), target.list, target.collocates, SIMPLIFY=F)
  #print(proc.time()-ptm)
  #return(all.col.tables)
  #retrieve how many targets have no significant collocates
  non.sig.index<-which(is.na(all.col.tables))
  if(length(non.sig.index)>0){
    #if there are some without significant collocates, remove them
    all.col.tables<-all.col.tables[-non.sig.index]
    #if no targets have significant collocates, report them
    if(length(all.col.tables)==0){
      print("No Significant Collocates Found")
      return(NA)
    }
  }
  #stop the cluster
  #stopCluster(clust.proc)
  #final.col.table<-NULL
  #print(lapply(all.col.tabls function(x) dim(x)))
  #for(i in 1:length(all.col.tables)){
  #  final.col.table<-rbind(final.col.table, all.col.tables[[i]])
  #}
  #bind all of the tables created by the significance assessment by rows
  final.col.table<-do.call("rbind", all.col.tables)
  return(final.col.table)
  #add column names to the bound data frame
  colnames(final.col.table)<-c("Term", "NObs", "NObsScale", "Obs_Exp", "PValues", "Target")
  #assess which collocate frequencies fall below the threshold
  threshold.index<-which(final.col.table$NObs<min.obs)
  if(length(threshold.index)>0){
    #if there are any, remove them
    final.col.table<-final.col.table[-which(final.col.table$NObs<min.obs),]
  }
  #if removing duplicate collocates, pass table to function collapseColTable
  if(collapseTable){
    final.col.table<-collapseColTable(final.col.table)
  }
  #if there are group designations for the targets, bind these as a new column after matching them to the 'target' column
  if(bind.groups){
    group.col<-rep(NA,nrow(final.col.table))
    for(i in 1:length(target.list)){
      group.col[which(final.col.table$Target==target.list[i])]<-target.groups[i]
    }
    final.col.table$Group<-group.col
  }
  #remove blank spaces
  empty.terms<-which(final.col.table$Term=="")
  if(length(empty.terms)>0){
    final.col.table<-final.col.table[-empty.terms, ]
  }
  #if(!is.null(col.targets)){
  #  final.col.table<-final.col.table[which(final.col.table[,1] %in% col.targets)]
  #}
  if(is.null(date.filter[1])){
    write.csv(final.col.table, paste(output.folder, "AllCollocateTable_Declin.csv", sep="/"), row.names=F)
  }
  return(final.col.table)
}

#wrapping function for getting all collocates and creating output tables
#meta.table requires a column with "Filename"
#if date.filter is not NULL, requires a meta.table with "Date" column, date.filter takes a min and max date
#same as the above function, but streamlined for large lists of known collocates
getMDWCollocatesKnown<-function(target.list, 
                           horizon, 
                           source.folder, 
                           meta.table, 
                           output.folder, 
                           alpha=0.05, 
                           date.filter=NULL, 
                           collapseTable=F, 
                           dictionary=NULL,
                           collocate.limiter="targets",
                           stop.words=NULL,
                           bind.groups=F,
                           min.obs=5){
  #code to parallelize processes
  #library(parallel)
  #ncore<-detectCores()-2
  #clust.proc<-makeCluster(ncore, type="FORK")
  
  #if there is a date filter vector, print it
  if(!is.null(date.filter)){
    print(date.filter)
  }
  #if bind.groups is true, collocates will be assigned to targets as well as second order groups
  #in that case, the groups are passed as the second column of the target matrix - here they are split into separate variables
  if(bind.groups){
    target.groups<-target.list[,2]
    target.list<-target.list[,1]
  }
  #if collocate.limiter is 'targets' feed it the vector of targets
  if(collocate.limiter[1]=="targets"){
    collocate.limiter<-target.list
  }
  
  #if there is a date filter, subset the metadata matrix to only include those dates
  if(!is.null(date.filter)){
    date.range<-seq(date.filter[1], date.filter[2], by=1)
    meta.table<-meta.table[which(meta.table$Date %in% date.range),]
  }
  #get all files from the source folder
  files<-list.files(source.folder)
  #subset the list of files to only include those in the metadata file (requires a $Filename column)
  meta.table<-meta.table[which(meta.table$Filename %in% files),]
  #append directories to filenames
  files<-paste(source.folder, meta.table$Filename, sep="/")
  
  #create a table for the limiters, filled with 0s
  collocate.slots<-rep(0, length(collocate.limiter))
  names(collocate.slots)<-collocate.limiter
  
  #create a list of empty tables for each target term
  target.collocates<-as.list(rep(list(collocate.slots), length(target.list)))
  
  #name the list for the targets
  names(target.collocates)<-target.list
  #initialize Text Count
  text.count<-NA
  print(paste("Number of Texts:", as.character(length(files)),sep=' '))
  print("Gathering Collocates")
  #for each text
  for(i in 1:length(files)){
    ptm<-proc.time()
    #scan the text
    curr.text<-scan(files[i], what='character', sep="\n", quiet = T)
    #print the number of the text
    print(i)
    #remove newlines
    curr.text<-paste(curr.text, collapse=" ")
    #clean text
    curr.text<-textClean(curr.text)
    #split text into vector
    curr.text<-unlist(strsplit(curr.text, " "))
    #strip any non-dictionary words
    if(!is.null(dictionary)){
      curr.text<-curr.text[which(curr.text %in% dictionary)]
    }
    #old code
    #target.list<-lapply(target.list, function(x) joinNgrams(x))
    #print(target.list)
    print("Getting Cols")
    
    #parallel apply the function to create a table of collocates for each target (depreciated)
    #collocate.tables<-parLapply(clust.proc, target.list, function(x) gatherCollocatesText(x, horizon, curr.text, stop.words))
    collocate.tables<-lapply(target.list, function(x) gatherCollocatesText(x, horizon, curr.text, stop.words, collocate.limiter))
    
    non_empty.cols<-which(!is.na(collocate.tables))
    if(length(non_empty.cols)>0){
      print("Binding Cols")
      #for each collocate table in the list, bind it into the master list of collocate tables (1 for each target)
      target.collocates<-mapply(function(x,y) tablePrePopFill(x,y), target.collocates[non_empty.cols], collocate.tables[non_empty.cols], SIMPLIFY=F)
    }
    #bind the running count of each word in the corpus as a whole (to get final frequencies)
    print("Binding Totals")
    text.count<-tableBind(text.count, table(curr.text))
    #clean up the workspace
    remove(collocate.tables)
    gc(verbose=F)
    print(proc.time()-ptm)
  }
  #check and see which targets have no collocates
  nocol.index<-which(is.na(target.collocates))
  if(length(nocol.index>0)){
    #if there are any, identify them
    nocol.words<-names(target.collocates[nocol.index])
    #if there are more than one, paste them into a string
    if(nocol.words>1){
      nocol.words<-paste(nocol.words, collapse=", ")
    }
    #display the targets with no collocates
    print(paste("No collocates found for:", nocol.words, sep=" "))
    #remove targets from list of collocate vectors for significance assessment
    target.collocates<-target.collocates[-nocol.index]
    #remove targets with no collocates from the list of targets and list of groups
    target.list<-target.list[-nocol.index]
    target.groups<-target.groups[-nocol.index]
  }
  print("Calculating Collocates")
  #calculate frequencies for all of the words in the dictionary
  all.freqs<-text.count/sum(text.count)
  #calculate which collocates are significant for which targets - can this be paralellized? Why is target.list there?
  

  #ptm<-proc.time()
  #names(target.collocates)<-target.list
  #all.col.tables<-parLapply(clust.proc, target.collocates, function(x) wordMDCollocates(x, all.freqs, alpha))
  all.col.tables<-mapply(function(x,y) wordMDCollocates(x,y, all.freqs, alpha), target.list, target.collocates)
  #print(proc.time()-ptm)
  
  #retrieve how many targets have no significant collocates
  non.sig.index<-which(is.na(all.col.tables))
  if(length(non.sig.index)>0){
    #if there are some without significant collocates, remove them
    all.col.tables<-all.col.tables[-non.sig.index]
    #if no targets have significant collocates, report them
    if(length(all.col.tables)==0){
      print("No Significant Collocates Found")
      return(NA)
    }
  }
  #stop the cluster
  #stopCluster(clust.proc)
  
  #final.col.table<-NULL
  #print(lapply(all.col.tabls function(x) dim(x)))
  #for(i in 1:length(all.col.tables)){
  #  final.col.table<-rbind(final.col.table, all.col.tables[[i]])
  #}
  
  #bind all of the tables created by the significance assessment by rows
  final.col.table<-do.call("rbind", all.col.tables)
  #add column names to the bound data frame
  colnames(final.col.table)<-c("Term", "NObs", "NObsScale", "Obs_Exp", "PValues", "Target")
  #assess which collocate frequencies fall below the threshold
  threshold.index<-which(final.col.table$NObs<min.obs)
  if(length(threshold.index)>0){
    #if there are any, remove them
    final.col.table<-final.col.table[-which(final.col.table$NObs<min.obs),]
  }
  #if removing duplicate collocates, pass table to function collapseColTable
  if(collapseTable){
    final.col.table<-collapseColTable(final.col.table)
  }
  #if there are group designations for the targets, bind these as a new column after matching them to the 'target' column
  if(bind.groups){
    group.col<-rep(NA,nrow(final.col.table))
    for(i in 1:length(target.list)){
      group.col[which(final.col.table$Target==target.list[i])]<-target.groups[i]
    }
    final.col.table$Group<-group.col
  }
  #remove blank spaces
  empty.terms<-which(final.col.table$Term=="")
  if(length(empty.terms)>0){
    final.col.table<-final.col.table[-empty.terms, ]
  }
  #if(!is.null(col.targets)){
  #  final.col.table<-final.col.table[which(final.col.table[,1] %in% col.targets)]
  #}
  if(is.null(date.filter[1])){
    write.csv(final.col.table, paste(output.folder, "AllCollocateTable_Declin_test.csv", sep="/"), row.names=F)
    print("written")
  }
  return(final.col.table)
}

#given a vector of dates for date.filter, createst sequences of dates between each element
#and the date before the next one
dateCollocates<-function(target.list, 
                         horizon, 
                         source.folder, 
                         meta.table, 
                         output.folder, 
                         alpha=0.05, 
                         date.filter=NULL, 
                         collapseTable=F, 
                         dictionary=NULL,
                         col.targets=NULL,
                         stop.words=NULL,
                         bind.groups=F,
                         min.obs=5,
                         sample.texts=F,
                         file.name.append){
  start.dates<-date.filter
  end.dates<-date.filter[2:length(date.filter)]
  end.dates<-end.dates-1
  end.dates<-c(end.dates, max(meta.table$Date))
  date.categories<-mapply(function(x,y) paste(as.character(x),as.character(y),sep="_"), start.dates, end.dates)
  date.pairs<-mapply(function(x,y) c(x,y), start.dates, end.dates, SIMPLIFY = F)
  print(date.pairs)
  if(sample.texts){
    date.sizes<-lapply(date.pairs, function(x) length(which(meta.table$Date %in% seq(x[1], x[2], by=1))))
    min.texts<-min(unlist(date.sizes))
    print(min.texts)
  } else {
    min.texts<-NULL
  }
  collocate.tables<-lapply(date.pairs, function(x) getMDWCollocates(target.list, horizon, source.folder, meta.table, output.folder,alpha, x, collapseTable, dictionary, col.targets, stop.words, bind.groups, min.obs, min.texts))
  full.collocate.table<-NULL
  for(i in 1:length(collocate.tables)){
    curr.collocate.table<-collocate.tables[[i]]
    if(!is.na(curr.collocate.table)){
      curr.collocate.table$Period<-date.categories[i]
      full.collocate.table<-rbind(full.collocate.table, curr.collocate.table)
    }
  }
  outfile.name<-paste("FinalDateCollocateTable_", file.name.append, ".csv", sep="")
  write.csv(full.collocate.table, paste(output.folder, outfile.name, sep="/"), row.names=F)
  return(full.collocate.table)
}

stickyCols<-function(collocate.table){
  unique.cols<-as.character(unique(collocate.table$Term))
  all.periods<-as.character(unique(collocate.table$Period))
  sticky.table<-matrix(rep(NA, (length(unique.cols)*length(all.periods))), ncol=length(all.periods))
  for(i in 1:length(unique.cols)){
    sub.table<-collocate.table[which(collocate.table$Term==unique.cols[i]),]
    for(j in 1:length(all.periods)){
      period.group<-sub.table$Group[which(sub.table$Period==all.periods[j])]
      period.group<-unique(period.group)
      if(length(period.group)>1){
        period.group<-paste(period.group, collapse="; ")
      }
      if(length(period.group)==0){
        period.group="None"
      }
      sticky.table[i,j]<-period.group
    }
  }
  rownames(sticky.table)<-unique.cols
  colnames(sticky.table)<-all.periods
  change.groups<-apply(sticky.table, 1, function(x) checkChange(x))
  change.groups<-unlist(change.groups)
  sticky.table<-sticky.table[which(change.groups>1),]
  change.groups<-change.groups[-which(change.groups>1)]
  return(sticky.table)
}

checkChange<-function(table.row){
  unique.row<-unique(table.row)
  none.index<-which(unique.row=="None")
  if(length(none.index)>0){
    unique.row<-unique.row[-none.index]
  }
  changes<-length(unique.row)
  return(changes)
}

gatherFreqText<-function(target.term, curr.text){
  n.hits<-length(which(curr.text==target.term))
  return(n.hits)
}

getFreqTable<-function(target.list, 
                       source.folder, 
                       meta.table, 
                       output.folder, 
                       date.filter=NULL,
                       scaling.factor=1,
                       bind.groups=F){
  if(!is.null(date.filter)){
    print(date.filter)
  }
  if(bind.groups){
    target.groups<-target.list[,2]
    target.list<-target.list[,1]
  }
  if(!is.null(date.filter)){
    date.range<-seq(date.filter[1], date.filter[2], by=1)
    meta.table<-meta.table[which(meta.table$Date %in% date.range),]
  }
  files<-list.files(source.folder)
  meta.table<-meta.table[which(meta.table$Filename %in% files),]
  files<-paste(source.folder, meta.table$Filename, sep="/")
  target.freqs<-as.list(rep(NA, length(target.list)))
  names(target.freqs)<-target.list
  word.count<-0
  print(paste("Number of Texts:", as.character(length(files)),sep=' '))
  print("Gathering Freq")
  freqs.sum<-rep(0, length(target.list))
  for(i in 1:length(files)){
    curr.text<-scan(files[i], what='character', sep="\n", quiet = T)
    print(i)
    curr.text<-paste(curr.text, collapse=" ")
    curr.text<-textClean(curr.text)
    curr.text<-unlist(strsplit(curr.text, " "))
    word.count<-word.count+length(curr.text)
    freqs<-lapply(target.list, function(x) gatherFreqText(x, curr.text))
    freqs<-unlist(freqs)
    freqs.sum<-freqs.sum+freqs
    gc(verbose=F)
  }
  print(word.count)
  raw.freqs<-freqs.sum
  scaled.freqs<-(raw.freqs/word.count)*scaling.factor
  freq.table<-data.frame(target.list, raw.freqs, scaled.freqs, stringsAsFactors=F)
  colnames(freq.table)<-c("Term", "RawFreq", "ScaledFreq")
  if(bind.groups){
    group.col<-rep(NA,nrow(freq.table))
    for(i in 1:length(target.list)){
      group.col[which(freq.table$Term==target.list[i])]<-target.groups[i]
    }
    freq.table$Group<-group.col
  }
  if(is.null(date.filter[1])){
    write.csv(final.col.table, paste(output.folder, "AllFreqTable.csv", sep="/"), row.names=F)
  }
  return(freq.table)
}

#given a vector of dates for date.filter, createst sequences of dates between each element
#and the date before the next one
dateFreq<-function(target.list, 
                   source.folder, 
                   meta.table, 
                   output.folder, 
                   date.filter,
                   scaling.factor=1,
                   bind.groups=F){
  start.dates<-date.filter
  end.dates<-date.filter[2:length(date.filter)]
  end.dates<-end.dates-1
  end.dates<-c(end.dates, max(meta.table$Date))
  date.categories<-mapply(function(x,y) paste(as.character(x),as.character(y),sep="_"), start.dates, end.dates)
  date.pairs<-mapply(function(x,y) c(x,y), start.dates, end.dates, SIMPLIFY = F)
  freq.tables<-lapply(date.pairs, function(x) getFreqTable(target.list, source.folder, meta.table, output.folder, x, scaling.factor, bind.groups))
  full.freq.table<-NULL
  for(i in 1:length(freq.tables)){
    curr.freq.table<-freq.tables[[i]]
    if(!is.na(curr.freq.table)){
      curr.freq.table$Period<-date.categories[i]
      full.freq.table<-rbind(full.freq.table, curr.freq.table)
    }
  }
  write.csv(full.freq.table, paste(output.folder, "RealOrigCorpusDatefreqTable.csv", sep="/"), row.names=F)
  return(full.freq.table)
}

#takes a metadata table and runs get collocate MDWs on a text by text basis, sending one text to 
#getMDWCollocates at a time
singleTextCollocates<-function(target.list, 
                               horizon, 
                               source.folder, 
                               meta.table, 
                               output.folder, 
                               alpha=0.05,
                               dictionary=NULL,
                               stop.words=NULL,
                               bind.groups=F,
                               min.obs=5){
  all.collocates<-data.frame()
  for(i in 1:nrow(meta.table)){
    curr.text<-meta.table[i,]
    curr.cols<-getMDWCollocates(target.list, horizon, source.folder, curr.text, output.folder, date.filter=c(1,5000), dictionary=dictionary, stop.words=stop.words, bind.groups=bind.groups, min.obs=min.obs)
    all.collocates<-rbind(all.collocates, curr.cols)
  }
  write.csv(all.collocates, file=paste(output.folder, "AllCols.csv",sep="/"), row.names=F)
  return(all.collocates)
}

censorCol<-function(censor.term){
  censor.term<-unlist(strsplit(censor.term, ''))
  censor.term[which(censor.term %in% c('a', 'e', 'i', 'o', 'u', 'y'))]<-'*'
  censor.term<-paste(censor.term, collapse='')
  return(censor.term)
}

censorCols<-function(col.table, slur.list, target.col){
  slur.index<-which(col.table[,target.col] %in% slur.list)
  replace.terms<-lapply(col.table[slur.index, target.col], function(x) censorCol(x))
  col.table[slur.index,target.col]<-unlist(replace.terms)
  return(col.table)
}

groupBind<-function(col.table, target.table){
  unique.targets<-unique(col.table$Target)
  groups<-rep(NA, nrow(col.table))
  for(i in 1:length(unique.targets)){
    target.index<-which(col.table$Target==unique.targets[i])
    groups[target.index]<-target.table[which(target.table[,1]==unique.targets[i]),2]
  }
  col.table$Group<-groups
  return(col.table)
}
