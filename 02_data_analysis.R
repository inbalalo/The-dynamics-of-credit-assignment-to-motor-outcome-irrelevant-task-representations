rm(list=ls())
library(data.table)
library(reshape2)
library(rstatix)
tab<-data.table(read.csv('myfolder/03_raw_clean_data/01_tab.csv'))
sub_folder<-'myfolder/04_results'

#add absolute outcome (1\0) column to tab
tab$pv_outcome<-tab$rw_pv
tab$pv_outcome[tab$cond=="neg"]<-tab$pv_outcome[tab$cond=="neg"] +1

#calculate probability to repeat a rewarded fractal\key
df<-tab[,list(mean(stay_frc[reoffer_ch]), mean(stay_key[!reoffer_ch])),by=c("subj", "cond", "pv_outcome")]
colnames(df)<-c("subject", "condition", "pv_outcome", "frc", "key")
#calculate probability to repeat a rewarded fractal\key\fractal given a key
#df<-tab[,list(mean(stay_frc[reoffer_ch]), mean(stay_key[!reoffer_ch]), mean(stay_frc[reoffer_ch&stay_key])),by=c("subj", "cond", "pv_outcome")]
#colnames(df)<-c("subject", "condition", "pv_outcome", "frc", "key", "frc:key")

#change block names
df$condition[df$condition=="pos"]<-"win block"
df$condition[df$condition=="neg"]<-"loss block"
#change df to long format
df<-melt(df, id.vars = c("subject", "condition", "pv_outcome"))
colnames(df)<-c("subject", "condition", "pv_outcome","modality", "pStay")
df<-df[order(df$subject),]

#3-factor ANOVA analysis 
res.aov <- anova_test(
  data = df,dv = pStay, wid = subject,
  within = c(modality,condition,pv_outcome),
  type=3
) 
get_anova_table(res.aov)
