getWordsEmbeddings<-function(text.vector, term.vector, dimensions){
  glove.table<-gloveModelVec(text.vector, term_min=3L, g.dimension=dimensions, source.type="text")
  glove.table<-cleanModel(glove.table, dictionary=term.vector)
  return(glove.table)
}

getTexts<-function(meta.table, text.dir, date.seq){
  sub.table<-meta.table[which(meta.table$Date %in% date.seq),]
  filenames<-sub.table$Filename
  filenames<-paste(text.dir, filenames, sep="/")
  text.files<-lapply(filenames, function(x) scan(x, what='character', sep="\n", quiet = T))
  text.files<-lapply(text.files, function(x) paste(x, collapse=" "))
  text.files<-unlist(text.files)
  return(text.files)
}

dateSampleGlove<-function(meta.table, text.dir, date.begins, n.years, term.vector, dimensions=50, output.dir){
  source("/Users/malgeehe/Dropbox/Text Mining/WordVectors/GloveFunctions.R")
  table.names<-unlist(lapply(date.begins, function(x) paste(as.character(x), as.character(x+n.years), sep="_")))
  print(table.names)
  date.seq<-mapply(function(x,y) seq(x, (x+y), by=1), date.begins, n.years, SIMPLIFY = F)
  print("Getting Texts")
  text.seq<-lapply(date.seq, function(x) getTexts(meta.table, text.dir, x))
  print("Creating Models")
  glove.models<-lapply(text.seq, function(x) getWordsEmbeddings(x, term.vector, dimensions))
  print("Writing Models")
  mapply(function(x,y) write.csv(x, paste(output.dir, "/", y, ".txt", sep='')), glove.models, table.names)
  return(glove.models)
}

accessionText<-function(single.text, vocabulary){
  empty.vector<-rep(0, length(vocabulary))
  names(empty.vector)<-vocabulary
  single.text<-unlist(strsplit(single.text, ''))
  single.text<-tolower(single.text)
  single.text<-single.text[which(single.text %in% c(letters, " "))]
  single.text<-paste(single.text, collapse='')
  single.text<-unlist(strsplit(single.text, " "))
  text.length<-length(single.text)
  single.text<-single.text[which(single.text %in% vocabulary)]
  if(length(single.text)>0){
    single.table<-table(single.text)
    for(i in 1:length(single.table)){
      empty.vector[which(names(empty.vector) == names(single.table[i]))]<-single.table[i]
    }
  }
  empty.vector<-(empty.vector/text.length)*100000
  return(empty.vector)
}

buildDTM<-function(sub.corpus, term.vector){
  text.freqs<-lapply(sub.corpus, function(x) accessionText(x, term.vector))
  text.table<-do.call("rbind", text.freqs)
  return(text.table)
}

dateSampleDTMs<-function(meta.table, text.dir, date.begins, n.years, term.vector, output.dir){
  source("/Users/malgeehe/Dropbox/Text Mining/WordVectors/GloveFunctions.R")
  table.names<-unlist(lapply(date.begins, function(x) paste(as.character(x), as.character(x+n.years), sep="-")))
  print(table.names)
  date.seq<-mapply(function(x,y) seq(x, (x+y), by=1), date.begins, n.years, SIMPLIFY = F)
  print("Getting Texts")
  text.seq<-lapply(date.seq, function(x) getTexts(meta.table, text.dir, x))
  print("Creating DTMs")
  dtm.tables<-lapply(text.seq, function(x) buildDTM(x, term.vector))
  print("Writing Models")
  mapply(function(x,y) write.csv(x, paste(output.dir, "/", y, ".csv", sep='')), dtm.tables, table.names)
  return(dtm.tables)
}

meanDist<-function(sub.matrix){
  require(text2vec)
  sim.matrix<-sim2(sub.matrix, method="cosine")
  mean.dist<-mean(sim.matrix)
  return(mean.dist)
}

#note-collocate.table is any dataframe with one column called "Term" and one called "Group"
periodDist<-function(sub.matrix, collocate.table, period){
  unique.groups<-unique(collocate.table$Group)
  group.cols<-lapply(unique.groups, function(x) collocate.table[which(collocate.table$Group == x),1])
  group.lengths<-unlist(lapply(group.cols, function(x) length(x)))
  bad.groups<-which(group.lengths<2)
  if(length(bad.groups)>0){
    group.cols<-group.cols[-bad.groups]
    unique.groups<-unique.groups[-bad.groups]
  }
  group.matricies<-lapply(group.cols, function(x) sub.matrix[which(rownames(sub.matrix) %in% x),])
  group.means<-unlist(lapply(group.matricies, function(x) meanDist(x)))
  means.table<-data.frame(rep(period, length(group.means)), unique.groups, group.means, stringsAsFactors=F)
  colnames(means.table)<-c("Period", "Group", "Mean")
  print(dim(means.table))
  return(means.table)
}

meanGroupDist<-function(matrix.list, collocate.table){
  periods<-names(matrix.list)
  collocate.subs<-lapply(unique(collocate.table$Period), function(x) collocate.table[which(collocate.table$Period==x),])
  period.dists<-mapply(function(x,y,z) periodDist(x, y, z), matrix.list, collocate.subs, periods, SIMPLIFY = F)
  period.dists<-do.call("rbind", period.dists)
  return(period.dists)
}

#functions for finding the number of actual groups members in each period that cluster together in k-means
groupAgreement<-function(period.table, k.table, original.group){
  #extract the members of the group from the period collocate table
  sub.table<-period.table[which(period.table$Group==original.group),]
  #find the k groups that the members of the original group were assigned to - return as a vector
  k.groups<-k.table[which(k.table[,1] %in% sub.table$Term),2]
  #sort and count the different k group memberships of the original group
  k.groups.unique<-sort(table(k.groups), decreasing=T)

  #need the percentage of each cluster made up of members of other groups
  all.k.table<-table(k.table[,2])
  k.groups.unique<-k.groups.unique[order(names(k.groups.unique))]
  #calculate the percentage of the group in each cluster
  k.groups.perc<-k.groups.unique/(sum(k.groups.unique))*100
  all.k.table<-all.k.table[order(names(all.k.table))]
  all.k.table<-all.k.table[which(names(all.k.table) %in% names(k.groups.unique))]
  k.groups.remainder<-all.k.table-k.groups.unique
  k.groups.perc.other<-k.groups.remainder/all.k.table
  k.groups.perc.adj<-k.groups.perc-k.groups.perc.other
  k.groups.perc.adj<-sort(k.groups.perc.adj, decreasing=T)
  
  #extract a vector of which k groups the terms were assigned to
  assigned.group<-names(k.groups.perc.adj)[1]
  #calculate the number of k.groups
  n.k.groups<-length(k.groups.unique)
  assigned.score<-k.groups.perc.adj[1]
  #return the number of k.groups, and the percentage of the terms in the largest k group
  return.vector<-c(n.k.groups, assigned.score)
  return(return.vector)
}

#alternate function that returns not a single result, but a table of all cluster assignments per group
groupAgreementTable<-function(period.table, k.table, original.group){
  #extract the members of the group from the period collocate table
  sub.table<-period.table[which(period.table$Group==original.group),]
  group.clusters<-k.table[which(k.table[,1] %in% sub.table$Term),2]
  group.clusters<-table(group.clusters)
  all.clusters.sub<-k.table[which(k.table[,2] %in% names(group.clusters)),2]
  all.clusters.table<-table(all.clusters.sub)
  group.clusters<-group.clusters[order(names(group.clusters))]
  all.clusters.table<-all.clusters.table[order(names(all.clusters.table))]
  all.clusters.remainder<-all.clusters.table-group.clusters
  all.clusters.names<-names(group.clusters)
  group.totals<-rep(sum(group.clusters), length(group.clusters))
  group.percent<-(group.clusters/group.totals)*100
  remainder.percent<-(all.clusters.remainder/all.clusters.table)*100
  new.stat<-group.percent-remainder.percent
  zero.index<-which(remainder.percent==0)
  if(length(zero.index)>0){
    remainder.percent[zero.index]<-1
  }
  new.stat.adj<-group.percent/remainder.percent
  stat.matrix<-cbind(group.clusters, all.clusters.table, all.clusters.remainder, group.totals, group.percent, remainder.percent, new.stat, new.stat.adj)
  group.id<-rep(original.group, length(group.clusters))
  cluster.table<-data.frame(group.id, all.clusters.names, stat.matrix)
  colnames(cluster.table)<-c("Group", "Cluster", "Group_members_in_cluster", "Cluster_Size_Total", "Cluster_remainder", "Group_Size", "Percent_Group_in_Cluster", "Percent_Cluster_Not_in_Group", "Difference_Cluster_Perc_Remainder_Perc", "Cluster_Perc_Scaled_by_Remainder_Perc")
  cluster.table<-cluster.table[order(cluster.table$Difference_Cluster_Perc_Remainder_Perc, decreasing = T),]
  cluster.table.seq<-seq(1,nrow(cluster.table), by=1)
  cluster.table$Rank_Subtraction_Stat<-cluster.table.seq
  cluster.table<-cluster.table[order(cluster.table$Cluster_Perc_Scaled_by_Remainder_Perc, decreasing=T),]
  cluster.table$Rank_Scaled_Stat<-cluster.table.seq
  return(cluster.table)
}

#note - as opposed to the above table, this table returns all of the k memberships for each group - these can be graphed over the periods
kSums<-function(period.table, k.table, original.group){
  #extract the members of the group from the period collocate table
  sub.table<-period.table[which(period.table$Group==original.group),]
  #find the k groups that the members of the original group were assigned to - return as a vector
  k.groups<-k.table[which(k.table[,1] %in% sub.table$Term),2]
  #create an unsorted table of counts for the number of terms in each k group
  k.counts<-table(k.groups)
  #turn the raw counts into percentages
  k.counts<-k.counts/sum(k.counts)
  #bind the percentage in each k group to the group names and the names of the original group
  k.return<-data.frame(names(k.counts), as.vector(k.counts), rep(original.group, length(k.counts)), stringsAsFactors = F)
  #return the bound table
  colnames(k.return)<-c("K_Group", "Percent_Membership", "Original_Group")
  return(k.return)
}

periodGroups<-function(period.matrix, period.collocate.table, period.name, plot.dir, k.words=NULL, output.plots=T){
  #extra code to ensure that all calculations are done on tables with the top 2000 words in each period
  if(!is.null(k.words)){
    #reduce the TDM matrix to just the collocates
    period.matrix<-period.matrix[which(rownames(period.matrix) %in% period.collocate.table$Term),]
    #sum the word frequencies across the rows
    word.frequencies<-rowSums(period.matrix)
    #sort the matrix into decreasing frequencies
    period.matrix<-period.matrix[order(word.frequencies, decreasing=T),]
    #take only to the top k.words rows from the matrix
    period.matrix<-period.matrix[1:k.words,]
    #reduce the collocate table to only the remaining rows of the DTM
    period.collocate.table<-period.collocate.table[which(period.collocate.table$Term %in% rownames(period.matrix)),]
    #print the size of the remaining matrix (should be equal to k)
    print(dim(period.matrix))
  }
  #print the name of the period
  print(period.name)
  #print step 1 name
  print("Getting Stats")
  #get all terms from the TDM
  all.terms<-rownames(period.matrix)
  #get the unique groups from the collocate table
  unique.groups<-unique(period.collocate.table$Group)
  #set the number of groups to the length of the unique.groups table
  ngroups<-length(unique.groups)
  #order the period collocate table in order of highest Ob/Exp to lowest (assures that the duplicate collocates will be
  #associated with the Target/Group they have the highest significance for)
  period.collocate.table<-period.collocate.table[order(period.collocate.table$Obs_Exp, decreasing=T),]
  
  #control of whether to do the kmeans calcualtions on the raw data or pcs - to switch change the value of do.pca
  do.pca<-F
  if(do.pca){
    #to remove confounding factors associated with hyperdimensionality, retrieve the n number of principle components
    #of each term equal to the number of pcs that cross the 0.7 percent threshold (Jolif, 1971)
    period.pca<-prcomp(period.matrix)
    period.eigs<-period.pca$sdev^2
    perc.var<-period.eigs/sum(period.eigs)
    n.pcs<-1
    cum.var<-0
    while(cum.var<0.7){
      cum.var<-cum.var+perc.var[n.pcs]
      n.pcs<-n.pcs+1
    }
    print(paste("Modeling on", as.character(n.pcs), "PCs", sep=" "))
    period.matrix.pcs<-period.pca$x[,1:n.pcs]
    rownames(period.matrix.pcs)<-rownames(period.matrix)
    
    #calculate a kmeans cluster on the TDM using the same number of groups as the collocates
    k.cluster<-kmeans(period.matrix.pcs, ngroups, iter.max=50)
  } else {
    k.cluster<-kmeans(period.matrix, ngroups, iter.max=50)
  }
  #extract the class assignments
  k.classes<-k.cluster$cluster
  #create a data.frame with the terms and classes
  k.table<-data.frame(rownames(period.matrix), k.classes, stringsAsFactors=F)
  #insertion of new code the returns the full period kmeans clustering rather than just the top stat
  #can run the old code by simply changing old.code to T
  old.code<-F
    if(old.code){
    #use the function groupAgreement to return, for each group, a vector with [1]the number of k groups; [2] the percentage of the group's terms in the largest kgroup
    k.group.agree<-lapply(unique.groups, function(x) groupAgreement(period.collocate.table, k.table, x))
    #bind the returned vectors into a table with two columns ([,1] the number of k groups and [,2] the percentage of group terms in the largest k group)
    k.group.agree<-do.call("rbind", k.group.agree)
    #bind the group names to the table
    k.group.agree<-data.frame(k.group.agree, unique.groups)
    #bind the name of the period to the table (since this function works on one period only, these can later be bound into a table with all periods)
    k.group.agree$Period<-rep(period.name, nrow(k.group.agree))
    #assign column names to the table
    colnames(k.group.agree)<-c("Num_K_Groups", "PercentAgreement", "Group", "Period")
    #use the function kSums to return, for each group a table with 3 columns - [,1] the name of the kgroup, [,2] the number of terms in the kgroup, [,3] the group name
    k.sum.table<-lapply(unique.groups, function(x) kSums(period.collocate.table, k.table, x))
    #bind the tables for each group together
    k.sum.table<-do.call("rbind", k.sum.table)
    #add the period to the table
    k.sum.table$Period<-rep(period.name, nrow(k.sum.table))
    #create a list to return - [[1]] the table with the total number of groups and the percentage of terms in the largest k group for each Group in each Period
    #[[2]] the membership in each k group for each term in each period
    return.list<-list(k.group.agree, k.sum.table)
  } else { 
    #alternate function for returning table
    cluster.agree.table<-lapply(unique.groups, function(x) groupAgreementTable(period.collocate.table, k.table, x))
    cluster.agree.table<-do.call("rbind", cluster.agree.table)
    cluster.agree.table$Period<-rep(period.name, nrow(cluster.agree.table))
    return.list<-cluster.agree.table
  }
  
  
  if(output.plots){
    #print the current step
    print("Outputting Plot")
    #remove duplicated terms from the collocate table (since they are in order by Obs/Exp, the highest Obs/Exp duplicate is kept)
    col.table.cut<-period.collocate.table[-which(duplicated(period.collocate.table$Term)),]
    #extract the rows from the period.matrix representing the collocates in the cut table
    period.matrix.cut<-period.matrix[which(rownames(period.matrix) %in% col.table.cut$Term),]
    #ensure no extra collocates by extracting just the collocates in the period DTM
    col.table.cut<-col.table.cut[which(col.table.cut$Term %in% rownames(period.matrix.cut)),]
    #sort the collcoate table by term name
    col.table.cut<-col.table.cut[order(col.table.cut$Term),]
    #order the period matrix by the rownames (putting it in the same order as the collocate table)
    period.matrix.cut<-period.matrix.cut[order(rownames(period.matrix.cut)),]
    #load the Rtsne library
    library(Rtsne)
    #load the ggplot2 library
    library(ggplot2)
    #create rTSNE model using the reduced/ordered period TDM
    reduced.period.matrix.tsne<-Rtsne(period.matrix.cut, check_duplicates=F)
    #extract the coordinates from the tSNE model
    reduced.period.matrix.tsne<-reduced.period.matrix.tsne$Y
    #bind coordinates to the group label and term names into a single data frame
    plot.table.tsne<-data.frame(reduced.period.matrix.tsne, col.table.cut$Group, rownames(period.matrix.cut))
    #names the columns of the tSNE table
    colnames(plot.table.tsne)<-c("Tsne1", "Tsne2", "Group", "Term")
    #create the tSNE plot from the table
    period.plot.tsne<-ggplot(plot.table.tsne, aes(x=Tsne1, Tsne2, color=Group, label=Term))+geom_point(size=3)+geom_text(size=0.8, color="black")+scale_color_manual(values=c("goldenrod", "darkmagenta", "lawngreen", "hotpink", "chartreuse", "cyan", "dodgerblue", "firebrick", "darkolivegreen1", "navy", "sienna1", "mediumpurple1", "red", "orange"))
    #create the filename for that period by attaching it to the "Scatterplot_Tsne" folder
    plot.file.tsne<-paste(plot.dir, "/Scatterplot_Tsne/", period.name, "_tsne.pdf", sep="")
    #create a pdf object with that filename
    pdf(plot.file.tsne, height=20, width=20)
    #plot the tSNE model in the .pdf
    print(period.plot.tsne)
    #close the file
    dev.off()
    plot.file.tsne.table<-paste(plot.dir, "/Scatterplot_Tsne/", period.name, "_tsne.csv", sep="")
    write.csv(plot.table.tsne, file=plot.file.tsne.table, row.names=F)
    #create a PCA object from the reduced TDM
    reduced.period.matrix<-prcomp(period.matrix.cut)
    #extract the first two principal components from the model object
    reduced.period.matrix<-reduced.period.matrix$x[,1:2]
    #create a dataframe of information to plot by binding the PCs to the Group names and Term names
    plot.table.pca<-data.frame(reduced.period.matrix, col.table.cut$Group, rownames(period.matrix.cut))
    #name the columns of the PCA table
    colnames(plot.table.pca)<-c("PC1", "PC2", "Group", "Term")
    #create a ggplot of the PCA
    period.plot.pca<-ggplot(plot.table.pca, aes(x=PC1, PC2, color=Group, label=Term))+geom_point(size=3)+geom_text(size=0.8, color="black")+scale_color_manual(values=c("goldenrod", "darkmagenta", "lawngreen", "hotpink", "chartreuse", "cyan", "dodgerblue", "firebrick", "darkolivegreen1", "navy", "sienna1", "mediumpurple1", "red", "orange"))
    #create the PCA filename
    plot.file.pca<-paste(plot.dir, "/Scatterplot_PCA/", period.name, "_pca.pdf", sep="")
    #create a pdf object with that filename
    pdf(plot.file.pca, height=20, width=20)
    #print the PCA plot to that file
    print(period.plot.pca)
    #close .pdf file
    dev.off()
    plot.file.pca.table<-paste(plot.dir, "/Scatterplot_PCA/", period.name, "_pca.csv", sep="")
    write.csv(plot.table.pca, file=plot.file.pca.table, row.names=F)
  }
  #return the list with the 2 tables for that period
  return(return.list)
}

#plot the change in k group assignment over periods for a single group
plotKGroups<-function(assignment.table, plot.dir){
  #load ggplot2 library
  library(ggplot2)
  #extract the name of the current group from the table
  group.name<-assignment.table$Original_Group
  #remove any forward slashes from the group names (as they'll be used as filenames)
  group.name<-gsub("/", "_", group.name)
  #create a filename for the current plot of kgroup distribution over time
  filename<-paste(plot.dir, "/", group.name, "_KGroups.pdf", sep="")
  #create a plot of kgroup distribution over time
  group.plot<-ggplot(assignment.table, aes(x=Period, y=Percent_Membership, fill=K_Group))+geom_bar(stat="identity")
  #create a pdf object
  pdf(filename, height=10, width=15)
  #print the distribution plot to the pdf file
  print(group.plot)
  #close the file
  dev.off()
}

#wrapping function to run periodGroups on a list of date limited tdms (matrices)
kMeansStickiness<-function(matrix.list, collocate.table, plot.dir, k.words=NULL, output.plots=T){
  #extract the names of the matrix period list to serve as the period names these should be the same as collocate.table$Period
  period.names<-names(matrix.list)
  #divide up the collocate table into a list of collocates per period
  period.col.tables<-lapply(period.names, function(x) collocate.table[which(collocate.table$Period==x),])
  #run periodGroups on each member of the matrix/collocate list - returns a list of tables
  results.list<-mapply(function(x,y,z,a) periodGroups(x, y, z, plot.dir, output.plots=output.plots), matrix.list, period.col.tables, period.names, SIMPLIFY = F)
  #old.code returns the two tables of a single stat per group per period, and the list of kgroups per period
  #new code returns both as one table - to switch between them, change the value of old.code
  old.code<-F
  if(old.code){
    #extract every k.group.agree table (the table with the number of k groups and percentage of terms in the largest k group per actual group)
    agreements<-lapply(results.list, function(x) x[[1]])
    #bind the k.group.agree tables into a single table
    agreements<-do.call("rbind", agreements)
    #extract every k.sum.table (the table with the size of each k group each actual group is distributed among)
    assignments<-lapply(results.list, function(x) x[[2]])
    #bind the.k.sum.tables into a single table
    assignments<-do.call("rbind", assignments)
    #extract a list of the unique groups in the assignments
    unique.groups<-unique(assignments$Original_Group)
    #for each unique group, extract the assignment table (k.sum.tables) for that group (the original return was a list of periods, this creates a list of groups across periods)
    assignment.group<-lapply(unique.groups, function(x) assignments[which(assignments$Original_Group==x),])
    #use plotKGroups function to plot the change in k group makeup over the periods for each group
    lapply(assignment.group, function(x) plotKGroups(x, plot.dir))
    #return the agreement table (the table with the number of kgroups and percent of largest k group)
    return(agreements)
  } else {
    agreements<-do.call("rbind", results.list)
    return(agreements)
  }
}  

#need a wrapper to 1, create the matricies, 2, reduce the collocate table to shared collocates, 3, write the k tables, 4 sample the columns in the TDM
#changelog: function checks if source.folder is a list - if so, it assumes the tdm tables have already been made and skips the import step
stickiness<-function(source.corpus, meta.table, collocate.table, output.dir, k.words=NULL, shared.words=T, sample.texts=T, output.plots=T){
  print(output.dir)
  #if shared.words flag is true
  if(shared.words){
    #use findTermsinAll function to return a vector of terms shared in all periods
    shared.terms<-findTermsinAll(collocate.table)
    #remove all terms not shared in all periods from the table
    collocate.table<-collocate.table[which(collocate.table$Term %in% shared.terms),]
    #set the dictionary equal to the shared terms (for reducing the texts)
    dict<-shared.terms
  } else {
    dict<-unique(collocate.table$Term)
  }
  #find the unique period labels
  periods<-unique(collocate.table$Period)
  #split the period labels by the underscore to get start and end dates for each period
  period.boundaries<-lapply(periods, function(x) unlist(strsplit(x, "_")))
  #for each period create a vector of all years in that period
  period.years<-lapply(period.boundaries, function(x) seq(as.numeric(x[1]), as.numeric(x[2]), by=1))
  #load the tm library
  library(tm)
  print("Making Tables")
  #use the makeTDM function to create a list of tdms - one for each period
  if(length(source.corpus)>length(period.years)){
    #old function that reimported the corpus every time
    #tdm.list<-lapply(period.years, function(x) makeTDM(meta.table, source.folder, x, dict))
    #new function that uses an existing corpus to make period tdm slices
    print("Making TDM tables")
    tdm.list<-lapply(period.years, function(x) bulkTDM(meta.table, source.corpus, x, dict))
  } else {
    tdm.list<-source.corpus
  }
  #if the sample text flag is true
  if(sample.texts){
    #find the number of texts in each period tdm
    text.sizes<-lapply(tdm.list, function(x) ncol(x))
    #find the lowest number of texts in a period
    max.texts<-min(unlist(text.sizes))
    print(paste("Sampling:", as.character(max.texts), "texts", sep=" "))
    #sample that number of texts from each tdm
    tdm.list<-lapply(tdm.list, function(x) x[,sample(ncol(x), max.texts, replace=F)])
  }
  #name the tdms for the periods
  names(tdm.list)<-periods
  #create the output dir
  dir.create(output.dir)
  if(output.plots){
    #create the sub-folder for the tSNE plots in the output dir
    dir.create(paste(output.dir, "Scatterplot_Tsne", sep="/"))
    #create the sub-folder for the PCA plots in the output dir
    dir.create(paste(output.dir, "Scatterplot_PCA", sep="/"))
  }
  #run the kMeansStickiness function on the list of matricies - calculates the kmeans, creates the plots and returns the kmeans agreement table
  agreements<-kMeansStickiness(tdm.list, collocate.table, output.dir, k.words, output.plots=output.plots)
  #write the kmeans agreement table to the folder
  write.csv(agreements, file=paste(output.dir, "KMeans_Percent_Agreement_FullTable.csv", sep="/"), row.names=F)
  #return the kmeans agreement table
  return(agreements)
}

#function to take a period vector, import texts subselected from a metadata table, clean them, then make a tdm
makeTDM<-function(meta.table, source.folder, period.vector, word.list){
  #find the filenames of texts within the period vector
  text.filenames<-meta.table$Filename[which(meta.table$Date %in% period.vector)]
  #paste the source directory to each filename
  text.filenames<-paste(source.folder, text.filenames, sep="/")
  #import and clean all texts (returns a list of collapsed strings) - if word.list is not NULL, reduces all texts to just those words
  all.texts<-lapply(text.filenames, function(x) importText(x, word.list))
  #use the corpus function to create a corpus from the list of texts
  all.texts<-Corpus(VectorSource(unlist(all.texts)))
  #create a DTM from the corpus
  dtm<-DocumentTermMatrix(all.texts, control=list(wordLengths=c(1, Inf)))
  #transpose the DTM into a TDM
  tdm<-t(as.matrix(dtm))
  #scale the tdm based on the total number of words
  tdm<-tdm/rowSums(tdm)
  #return the TDM
  return(tdm)
}

#this code substitutes for import Text (below) - it uses the tm package to get a corpus from a folder
bulkImportTexts<-function(folder.name, meta.table, word.list){
  library(tm)
  full.corpus<-Corpus(DirSource(folder.name))
  corpus.names<-list.files(folder.name)
  full.corpus<-unlist(lapply(full.corpus, function(x) textClean(x[[1]], word.list)))
  names(full.corpus)<-corpus.names
  
  #clean up the metadata table
  meta.table<-meta.table[which(meta.table$Filename %in% corpus.names),]
  no.dates<-which(is.na(meta.table$Date))
  if(length(no.dates)>0){
    meta.table<-meta.table[-no.dates,]
    full.corpus<-full.corpus[-no.dates]
  }
  meta.table<-meta.table[order(meta.table$Filename),]
  full.corpus<-full.corpus[order(names(full.corpus))]
  
  full.corpus<-Corpus(VectorSource(unlist(full.corpus)))
  return(list(full.corpus, meta.table))
}

#this code cleans the texts for the above
#imports a text from a file, cleaning it and extracting words from a wordlist
textClean<-function(raw.text, word.list){
  #raw.text<-raw.text$Content
  #divide the texts by character
  raw.text<-unlist(strsplit(raw.text, ""))
  #make all characters lower
  raw.text<-tolower(raw.text)
  #extract only letter characters and spaces
  raw.text<-raw.text[which(raw.text %in% c(letters, " "))]
  #callapse the text by characters
  raw.text<-paste(raw.text, collapse="")
  #split the new text by spaces
  raw.text<-unlist(strsplit(raw.text, " "))
  #if wordlist is not NULL
  if(!is.null(word.list)){
    #extract the wordlist from the text
    raw.text<-raw.text[which(raw.text %in% word.list)]
  }
  #collapse the text by spaces
  raw.text<-paste(raw.text, collapse=" ")
  #return the text
  return(raw.text)
}

#this code takes a period vector and extracts those texts from a tdm
bulkTDM<-function(meta.table, bulkCorpus, period.roots, dict){
  cut.corpus<-bulkCorpus[which(meta.table$Date %in% period.roots)]
  cut.dtm<-DocumentTermMatrix(cut.corpus, control=list(wordLengths=c(1,Inf)))
  cut.dtm<-as.matrix(cut.dtm)
  cut.dtm<-cut.dtm[,which(colnames(cut.dtm) %in% dict)]
  cut.tdm<-t(cut.dtm)
  return(cut.tdm)
}

#imports a text from a file, cleaning it and extracting words from a wordlist
importText<-function(filename, word.list){
  #scan the text file, separated by newline characters
  raw.text<-scan(filename, what='character', sep="\n", quiet=T)
  #collapse the text vectors by spaces
  raw.text<-paste(raw.text, collapse=" ")
  #divide the texts by character
  raw.text<-unlist(strsplit(raw.text, ""))
  #make all characters lower
  raw.text<-tolower(raw.text)
  #extract only letter characters and spaces
  raw.text<-raw.text[which(raw.text %in% c(letters, " "))]
  #callapse the text by characters
  raw.text<-paste(raw.text, collapse="")
  #split the new text by spaces
  raw.text<-unlist(strsplit(raw.text, " "))
  #if wordlist is not NULL
  if(!is.null(word.list)){
    #extract the wordlist from the text
    raw.text<-raw.text[which(raw.text %in% word.list)]
  }
  #collapse the text by spaces
  raw.text<-paste(raw.text, collapse=" ")
  #return the text
  return(raw.text)
}

#function to find the set of collocate terms that are in every period
findTermsinAll<-function(collocate.table){
  #extract the unique period identifiers from the collocate table
  unique.periods<-unique(collocate.table$Period)
  #extract all unique terms from the collocate table
  all.terms<-unique(collocate.table$Term)
  #for every unique period
  for(period in unique.periods){
    #extract the collocates in that period
    period.table<-collocate.table[which(collocate.table$Period==period),]
    #subtract the collocates in all.terms that aren't in that period
    all.terms<-all.terms[-which(!all.terms %in% period.table$Term)]
  }
  #return the vector of terms in all periods
  return(all.terms)
}


#functions to melt collocate table to associate Targets with each other based on Term Features
#Feature is the selected descriptor of the Term (NObsScale, Obs_ExP, PValues, etc)
extractTargetFeatures<-function(col.table, feature, unique.target){
  target.index<-which(col.table$Target==unique.target)
  all.features<-col.table[target.index, feature]
  all.feature.names<-col.table$Term[target.index]
  all.features<-as.data.frame(as.list(all.features))
  colnames(all.features)<-all.feature.names
  return(all.features)
}


buildFeatureTable<-function(col.table, feature, nobs_lim=NULL, obs_exp=NULL){
  library(plyr)
  if(!is.null(nobs_lim)){
    col.table<-col.table[which(col.table$Nobs>=nobs_lim),]
  }
  if(!is.null(obs_exp)){
    col.table<-col.table[which(col.table$Obs_Exp>=obs_exp),]
  }
  unique.terms<-unique(col.table$Target)
  all.feature.sets<-lapply(unique.terms, function(x) extractTargetFeatures(col.table, feature, x))
  all.feature.sets<-do.call("rbind.fill", all.feature.sets)
  rownames(all.feature.sets)<-unique.terms
  return(all.feature.sets)
}

getRtsne<-function(data.m){
  data.rtsne<-Rtsne(data.m)
  data.rtsne<-data.rtsne$Y
  print("Rtsne")
  return(data.rtsne)
}

getPCA<-function(data.m){
  data.pca<-prcomp(data.m)
  data.pca<-data.pca$x[,c(1,2)]
  print("PCA")
  return(data.pca)
}

#wrapper to run multiple features through buildFeatureTable and bind them in a table
buildCompositeFeatureTable<-function(col.table, feature.list, nobs_lim=NULL, obs_exp=NULL){
  library(Rtsne)
  all.feature.tables<-lapply(feature.list, function(x) buildFeatureTable(col.table, x, nobs_lim, obs_exp))
  Target<-rownames(all.feature.tables[[1]])
  for(i in 1:length(all.feature.tables)){
    all.feature.tables[[i]][is.na(all.feature.tables[[i]])]<-0
  }
  #all.feature.tables<-lapply(all.feature.tables, function(x) x[is.na(x)]<-0)
  all.feature.tables<-lapply(all.feature.tables, function(x) as.matrix(x))
  all.feature.coords<-lapply(all.feature.tables, function(x) tryCatch(getRtsne(x), error=function(e){getPCA(x)}))
  all.coords.colnames<-lapply(feature.list, function(x) c(paste("X", x, sep="_"), paste("Y", x, sep="_")))
  for(i in 1:length(all.feature.coords)){
    colnames(all.feature.coords[[i]])<-all.coords.colnames[[i]]
  }
  #all.feature.coords<-mapply(function(x,y) colnames(x)<-y, all.feature.coords, all.coords.colnames, SIMPLIFY=F)
  all.feature.coords<-do.call("cbind", all.feature.coords)
  Group<-lapply(Target, function(x) col.table$Group[which(col.table$Target==x)])
  Group<-lapply(Group, function(x) x[1])
  Group<-unlist(Group)
  composite.feature.table<-data.frame(Target, all.feature.coords, Group)
  return(composite.feature.table)
}

buildDateFeatureTable<-function(date.col.table, feature.list, nobs_lim=NULL, obs_exp=NULL){
  unique.dates<-unique(date.col.table$Period)
  date.tables<-lapply(unique.dates, function(x) date.col.table[which(date.col.table$Period==x),])
  table.lengths<-lapply(date.tables, function(x) length(unique(x$Target)))
  date.columns<-mapply(function(x,y) rep(x,y), unique.dates, table.lengths)
  #library(parallel)
  #n.cores<-detectCores()-1
  #clust<-makeCluster(n.cores, type="FORK")
  #all.composite.tables<-parLapply(clust, date.tables, function(x) buildCompositeFeatureTable(x, feature.list, nobs_lim, obs_exp))
  #stopCluster(clust)
  all.composite.tables<-lapply(date.tables, function(x) buildCompositeFeatureTable(x, feature.list, nobs_lim, obs_exp))
  for(i in 1:length(all.composite.tables)){
    all.composite.tables[[i]]$Period<-date.columns[[i]]
  }
  all.composite.tables<-do.call("rbind", all.composite.tables)
  return(all.composite.tables)
}


#crap
# amfic.source<-"/users/malgeehe/dropbox/transfer/corpora/americanfiction/plaintextnew"
# amfic.meta<-read.csv("/users/malgeehe/dropbox/transfer/corpora/americanfiction/AmFicMetaCorrected.csv", header=T, stringsAsFactors=F)
# setwd("/users/malgeehe/Dropbox/Transfer/Identity/CollocateResults/StickyCols/Sticky2_0/RandomTests")
# col.table.files<-list.files()
# col.table.files<-col.table.files[1:3]
# all.col.tables<-lapply(col.table.files, function(x) read.csv(file=x, header=T, stringsAsFactors=F))
# all.col.tables[[3]]$Group<-as.character(all.col.tables[[3]]$Group)
# folder.names<-c("Profession_Cols", "Race_Cols", "Random_Cols")

matchCollocate<-function(tsne.table, col.table, period){
  print(period)
  col.sub<-col.table[which(col.table$Period==period),]
  col.sub<-col.sub[order(col.sub$Obs_Exp, decreasing=T),]
  col.sub<-col.sub[-which(duplicated(col.sub$Term)),]
  unique.terms<-unique(col.sub$Term)
  target.col<-rep(NA, nrow(tsne.table))
  for(i in 1:length(unique.terms)){
    #print(i)
    #print(unique.terms[i])
    replacement<-col.sub$Target[which(col.sub$Term==unique.terms[i])]
    #print(replacement)
    target.col[which(tsne.table$Term==unique.terms[i])]<-replacement
    #print(which(tsne.table$Term==unique.terms[i]))
  }
  tsne.table$Target<-target.col
  return(tsne.table)
}