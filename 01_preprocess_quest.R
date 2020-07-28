rm(list=ls())
library(data.table)
library(stringr)
source('11_myfunc.R')

folder<-'myfolder/02_raw_data'
sub_folder<-'myfolder/03_raw_clean_data'
names<-c('bdi', 'oci', 'stai', 'spq')

#vector to clean bad participants
tab<-read.csv(paste(sub_folder, '/', '01_tab','.csv', sep=""))
good_subj<-tab$subj
good_subj<-good_subj[!duplicated(good_subj)]

#clean bad subjects+add sum columns
for (i in 1:length(names)){
  assign(names[i], sum_statements(names[i], folder, good_subj))
}

plot(hist(oci$sum))

#saving the data
for (i in 1:length(names)){
  save_data(eval(parse(text = names[i])), names[i], sub_folder)
}
