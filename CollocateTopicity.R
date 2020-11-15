externalTopicity<-function(target.cols, other.cols){
  target.cols<-unlist(target.cols)
  target.cols<-unique(target.cols)
  other.cols<-unlist(other.cols)
  other.cols<-unique(unlist(other.cols))
  duplicate.cols<-which(target.cols %in% other.cols)
  percent.duplicated<-length(duplicate.cols)/length(target.cols)
  return(percent.duplicated)
}

internalTopicity<-function(group.target.lists){
  total.targets<-length(group.target.lists)
  collapsed.cols<-unlist(group.target.lists)
  duplicated.cols<-unique(collapsed.cols[which(duplicated(collapsed.cols))])
  unique.cols<-collapsed.cols[-which(collapsed.cols %in% duplicated.cols)]
  internal.topicity<-(length(unique.cols))/length(collapsed.cols)
  return(internal.topicity)
}

collocateTopicity<-function(period.table, period){
  curr.period<-period
  print(curr.period)
  unique.targets<-unique(period.table$Target)
  unique.groups<-unique(period.table$Group)
  group.targets<-lapply(unique.groups, function(x) period.table$Target[which(period.table$Group==x)])
  target.lists<-lapply(unique.targets, function(x) period.table$Term[which(period.table$Target==x)])
  group.lists<-lapply(unique.groups, function(x) period.table$Term[which(period.table$Group==x)])
  total.group.cols<-lapply(group.lists, function(x) length(x))
  total.group.targets<-lapply(unique.groups, function(x) period.table$Target[which(period.table$Group==x)])
  total.group.targets<-lapply(total.group.targets, function(x) length(unique(x)))
  group.lists<-lapply(group.lists, function(x) unique(x))
  group.external.topicity<-lapply(unique.groups, function(x) externalTopicity(group.lists[which(unique.groups==x)], (group.lists[-which(unique.groups==x)])))
  group.target.cols<-lapply(group.targets, function(x) period.table$Term[which(period.table$Target %in% x)])
  internal.topicity<-lapply(group.target.cols, function(x) internalTopicity(x))
  period.table<-data.frame(unique.groups, unlist(group.external.topicity), unlist(internal.topicity), rep(curr.period, length(unique.groups)), unlist(total.group.cols), unlist(total.group.targets), stringsAsFactors=F)
  colnames(period.table)<-c("Group", "ExternalTopicity", "InternalTopicity", "Period", "TotalCollocates", "TotalTargets")
  return(period.table)
}

allTopicity<-function(full.date.table){
  library(dplyr)
  unique.dates<-unique(full.date.table$Period)
  period.tables<-lapply(unique.dates, function(x) full.date.table[which(full.date.table$Period==x),])
  #lapply(period.tables, function(x) print(dim(period.tables)))
  #print(length(period.tables))
  #print(unique.dates)
  return.tables<-mapply(function(x,y) collocateTopicity(x,y), period.tables, unique.dates, SIMPLIFY=F)
  final.table<-bind_rows(return.tables)
  return(final.table)
}