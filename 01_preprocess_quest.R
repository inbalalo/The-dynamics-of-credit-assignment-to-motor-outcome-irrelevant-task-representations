rm(list=ls())
library(data.table)
source('11_myfunc.R')

folder<-'myfolder/02_raw_data'
sub_folder<-'myfolder/03_raw_data'
names<-c('bdi', 'oci', 'stai', 'spq')

#add columns to the self reports 
for (i in 1:length(names)){
  assign(names[i], sum_statements(names[i], folder))
}

plot(hist(oci$sum))

#saving the data
for (i in 1:length(names)){
  save_data(eval(parse(text = names[i])), names[i], sub_folder)
}
