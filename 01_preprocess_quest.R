rm(list=ls())
library(data.table)
library(stringr)
source('11_myfunc.R')

folder<-'myfolder/02_raw_data'
sub_folder<-'myfolder/03_raw_clean_data'
exported_file<-data.frame(read.csv('myfolder/00_raw_exported_data/exported_all.csv'))
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

#add exported data columns (age and sex)
ids<-tab$prolific_id
exported_file<-setDT(exported_file)[participant_id %in% ids,]
names(exported_file)[names(exported_file) == "participant_id"] <- "prolific_id"
for (i in 1:length(names)){
  assign(names[i], add_exported_data(eval(parse(text = names[i])), exported_file))
}
#save a filtered exported file
write.csv(exported_file, 'myfolder/00_raw_exported_data/01_filtered_exported_data.csv', row.names=FALSE)

#save the data
for (i in 1:length(names)){
  save_data(eval(parse(text = names[i])), names[i], sub_folder)
}
