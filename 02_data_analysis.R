rm(list=ls())
library(data.table)
library(reshape2)
library(rstatix)
library(ggplot2)

tab<-data.table(read.csv('myfolder/03_raw_clean_data/01_tab.csv'))
sub_folder<-'myfolder/04_results'

#add absolute outcome (1\0) column to tab
tab$pv_outcome<-tab$rw_pv
tab$pv_outcome[tab$cond=="neg"]<-tab$pv_outcome[tab$cond=="neg"] +1

#calculate probability to repeat a rewarded fractal\key
df<-tab[,list(mean(stay_frc[reoffer_ch]), mean(stay_key[!reoffer_ch])),by=c("subj", "cond", "pv_outcome")]
colnames(df)<-c("subject", "condition", "pv_outcome", "frc", "key")

#Another way to define stay_key mean...
#df<-tab[,list(mean(stay_frc[reoffer_ch]), mean(stay_key[!stay_frc])),by=c("subj", "cond", "pv_outcome")]
#colnames(df)<-c("subject", "condition", "pv_outcome", "frc", "key")

#calculate probability to repeat a rewarded fractal\key\fractal given a key
#df<-tab[,list(mean(stay_frc[reoffer_ch]), mean(stay_key[!reoffer_ch]), mean(stay_frc[reoffer_ch&stay_key])),by=c("subj", "cond", "pv_outcome")]


#change block names
df$condition[df$condition=="pos"]<-"win block"
df$condition[df$condition=="neg"]<-"loss block"
#convert df to long format
df1<-melt(df, id.vars = c("subject", "condition", "pv_outcome"))
colnames(df1)<-c("subject", "condition", "pv_outcome","modality", "pStay")
df1<-df1[order(df1$subject),]

#3-factor ANOVA analysis 
res.aov <- anova_test(
  data = df1,dv = pStay, wid = subject,
  within = c(modality,condition,pv_outcome),
  type=3
) 
get_anova_table(res.aov)

#summarize results
setDT(df1)
results_key<-summarySEwithin(df1[modality=="key"], measurevar="pStay", withinvars=c("pv_outcome","condition"),
                     idvar="subject", na.rm=FALSE, conf.interval=.95)

results_frc <- summarySEwithin(df1[modality=="frc"], measurevar="pStay", withinvars=c("pv_outcome","condition"),
                               idvar="subject", na.rm=FALSE, conf.interval=.95)
#plot results
pd <- position_dodge(0.1)
ggplot(results_frc, aes(x=pv_outcome, y=pStay, colour=condition)) + 
  geom_errorbar(aes(ymin=pStay-sd, ymax=pStay+sd), width=.2, position=pd) +
  geom_line() +
  geom_point(position=pd, shape=21, size=2, fill="white")

ggplot(results_key, aes(x=pv_outcome, y=pStay, colour=condition)) + 
  geom_errorbar(aes(ymin=pStay-sd, ymax=pStay+sd), width=.2, position=pd) +
  geom_line() +
  geom_point(position=pd, shape=21, size=2, fill="white")
