rm(list=ls())
library(data.table)
tab<-data.table(read.csv('myfolder/02_raw_data/tab.csv'))

#replace -1 rt with values
tab$rt[tab$rt==-1]<-NA
#take out NA
tab<-na.omit(tab)

#check key stay probability 
df<-tab[,mean(stay_key), by=c('subj')]
colnames(df) <- c("subject", "key_rep")
plot(density(df$key_rep))
sum(df$key_rep>=0.9)

#check no response probability
df<-tab[,mean(is.na(rt)==T), by=subj]
colnames(df) <- c("subject", "no_response")
plot(density(df$no_response))
sum(df$no_response>=0.05)

#check long rt probability
df<-tab[,mean(rt>4000), by=subj]
colnames(df) <- c("subject", "long_rt")
plot(density(df$long_rt))
sum(df$long_rt>=0.1)

#check short rt probability
df<-tab[,mean(rt<200), by=subj]
colnames(df) <- c("subject", "short_rt")
plot(density(df$short_rt))
sum(df$short_rt>=0.15)
#abort participants with short rt
df$abort<-df$short_rt>=0.15
df<-df[abort==FALSE]
tab<-tab[subj==c(rep(df$subject, each=200))]
tab$abort_trl<-rep(0, dim(tab)[1])
tab$abort_trl[tab$trl==1|tab$rt<200]<-1
#abort short trials
tab<-tab[abort_trl==0]
