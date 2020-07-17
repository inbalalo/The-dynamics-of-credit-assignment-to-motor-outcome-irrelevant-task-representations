rm(list=ls())
library(data.table)

sum_statements<-function(name, folder) {
  quest<-data.table(read.csv(paste(folder, '/', name,'.csv', sep="")))
  quest$sum<-rowSums(quest[,1:(ncol(quest)-2)])
  quest$sum[is.na(quest$sum)]<- mean(quest$sum, 0, TRUE)
  return(quest)
}

save_data<-function(data, name, folder){
  write.csv(data, paste(folder, '/', '01_', name, '.csv', sep=""), row.names=FALSE)
  return()
}
  