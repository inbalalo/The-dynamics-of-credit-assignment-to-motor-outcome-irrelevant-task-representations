sum_statements<-function(name, folder, good_subj) {
  #load data
  quest<-data.frame(read.csv(paste(folder, '/', name,'.csv', sep="")))
  #clean data
  quest<-quest[good_subj,]
  #sort items
  quest<-quest[str_sort(colnames(quest), numeric = TRUE)]
  #sum scores
  if(name=='stai'){
    quest$state<-rowSums(quest[,1:20])
    quest$trait<-rowSums(quest[,21:40])
    #give participants with missing stai scores the mean score
    quest$state[is.na(quest$state)]<- mean(quest$state, 0, TRUE)
    quest$trait[is.na(quest$trait)]<- mean(quest$trait, 0, TRUE)
  }
  else{
  quest$sum<-rowSums(quest[,1:(ncol(quest)-2)])
  ##give participants with missing spq scores the mean score
  quest$sum[is.na(quest$sum)]<- mean(quest$sum, 0, TRUE)
  }
  if(name=='oci'){
    quest$hoarding<-rowSums(quest[,c(1,7,13)])
    quest$checking<-rowSums(quest[,c(2,8,14)])
    quest$ordering<-rowSums(quest[,c(3,9,15)])
    quest$counting<-rowSums(quest[,c(4,10,16)])
    quest$cleaning<-rowSums(quest[,c(5,11,17)])
    quest$obsessions<-rowSums(quest[,c(6,12,18)])
  }
  return(quest)
}

add_exported_data<-function(quest, exported_file){
  df<-merge(quest, exported_file[,c(2,7,21)])
  return(df)
}

save_data<-function(data, name, folder){
  write.csv(data, paste(folder, '/', '01_', name, '.csv', sep=""), row.names=FALSE)
  return()
}