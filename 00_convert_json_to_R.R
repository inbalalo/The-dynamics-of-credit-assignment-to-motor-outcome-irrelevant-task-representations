rm(list=ls())
library("rjson")
source('99_myfunc.R')
mainfolder<-paste(getwd(),'/myfolder/01_raw_json_data',sep="")

subfolder=dir(mainfolder)
bdi<-oci<-spq<-stai<-tab<-data.frame()

print(length(subfolder))
for (i in 1:length(subfolder)){
print(subfolder[i])
print(i)
files<-dir(curnfolder<-paste(mainfolder,'/',subfolder[i],sep="", row.names=NULL))

#self reports
bdi <-con_sr_json('bdi',bdi,curnfolder,files,i)
oci <-con_sr_json('oci',oci,curnfolder,files,i)
stai<-con_sr_json('stai',stai,curnfolder,files,i)
spq <-con_sr_json('spq',spq,curnfolder,files,i)

#task
tab<-con_task_json('test',tab,curnfolder,files,i) 
}  


#sort trials
tab<-tab[order(tab$subj,tab$blk,tab$trl),]
tab$trl<-tab$trl+1
rownames(tab) <- 1:nrow(tab)

#add columns
library(data.table)
tab$stay_frc<-tab$ch==shift(tab$ch,n=1,type='lag',fill=0)
tab$stay_key<-tab$key==shift(tab$key,n=1,type='lag',fill=0)
tab$rw_pv<-shift(tab$rw,n=1,type='lag',fill=0)
tab$reoffer_ch<-(tab$frcA==shift(tab$ch,n=1,type='lag',fill=0)|tab$frcB==shift(tab$ch,n=1,type='lag',fill=0))
tab$counter<-(tab$cond[tab$blk%%2==1]=='pos' | tab$cond[tab$blk%%2==0]=='neg')*1+1

#save csv
write.csv(tab,'myfolder/02_raw_data/tab.csv', row.names=FALSE)
write.csv(bdi,'myfolder/02_raw_data/bdi.csv', row.names=FALSE)
write.csv(oci,'myfolder/02_raw_data/oci.csv', row.names=FALSE)
write.csv(stai,'myfolder/02_raw_data/stai.csv', row.names=FALSE)
write.csv(spq,'myfolder/02_raw_data/spq.csv', row.names=FALSE)