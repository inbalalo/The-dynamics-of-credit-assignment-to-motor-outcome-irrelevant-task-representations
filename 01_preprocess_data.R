rm(list=ls())
library(data.table)
tab<-data.table(read.csv('myfolder/02_raw_data/tab.csv'))

#replace -1 rt with values
tab$rt[tab$rt==-1]<-NA

#check no response probability
df<-tab[,mean(is.na(rt)==T), by=subj]
colnames(df) <- c("subject", "no_response")
plot(density(df$no_response))
sum(df$no_response>0.05)
#remove subjects with more than 5% no response trials
tab<-tab[tab$subj %in% df[(df$no_response>0.05)==FALSE]$subj]
#remove all no response trials
tab<-na.omit(tab)

#check key stay probability 
df<-tab[,mean(stay_key), by=c('subj')]
colnames(df) <- c("subject", "key_rep")
plot(density(df$key_rep))
sum(df$key_rep>=0.9)

#check long rt probability
df<-tab[,mean(rt>4000), by=subj]
colnames(df) <- c("subject", "long_rt")
plot(density(df$long_rt))
sum(df$long_rt>=0.1)

#check short rt probability
df<-tab[,mean(rt<200)>0.1, by=subj]
colnames(df) <- c("subject", "short_rt")
sum(df$short_rt)
#remove participants with more than 10% short rt trials
tab<-tab[tab$subj %in% df[df$short_rt==FALSE]$subj]
#remove all short trials
tab<-tab[rt>200]

#remove first trial of every block
tab<-tab[trl>1]

write.csv(tab, 'myfolder/03_raw_clean_data/01_tab.csv')