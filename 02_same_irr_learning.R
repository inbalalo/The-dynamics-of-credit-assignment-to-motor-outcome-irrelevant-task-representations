rm(list = ls())
source('01_functions.R')
library(knitr)
library(kableExtra)

#generate parameters and data for N agents
N <- 1000                                   #number of agents
Nalt <- 2                                   #number of alternatives in a trial
Ncards <- 4                                 #size of a card deck
Ntrl <- c(25, 50, 100, 200, 1000)           #number of trls
conds <- c('positive', 'negative', 'positive', 'negative')
rndwlk <- read.csv('rndwlk_4frc_1000trials.csv', header = F)

####Model 2####
alpha_key_pos = runif(N, min = 0, max = 1)
w_pos = runif(N, min = 0, max = 2)
true.parms2 <-
  data.frame(
    alpha_frc = runif(N, min = 0, max = 1),
    alpha_key = cbind(alpha_key_pos, alpha_key_pos),
    beta = runif(N, min = 1, max = 8),
    w = cbind(w_pos, w_pos)
  )

model2 <-
  lapply(1:length(Ntrl), function(trl) {
    mclapply(1:N, function(s) {
      sim.task(
        Ntrl[trl],
        Nalt,
        Ncards,
        true.parms2$alpha_frc[s],
        c(true.parms2$alpha_key.alpha_key_pos[s], true.parms2$alpha_key.alpha_key_pos.1[s]),
        true.parms2$beta[s],
        c(true.parms2$w.w_pos[s], true.parms2$w.w_pos.1[s]),
        rndwlk,
        conds
      )
    })
  })

#recover param from data
rec.parms2 <-
  lapply(1:length(Ntrl), function(trl) {
    mclapply(1:N, function(s) {
      if (s %% 10 == T) {
        print(paste('Ntrl=', Ntrl[trl], ' subj=', s, sep = ''))
      }
      optim(
        par = c(
          runif(1, min = 0, max = 1),      #alpha_frc
          runif(1, min = 0, max = 1),      #alpha_key_pos
          runif(1, min = 0, max = 1),      #alpha_key_neg
          runif(1, min = 0, max = 8),      #beta
          runif(1, min = 0, max = 2),      #w_pos
          runif(1, min = 0, max = 2)       #w_neg
        ),
        fn = fit.task,
        df = model2[[trl]][[s]][['df']],
        alternatives = model2[[trl]][[s]][['alts']],
        Nalt = Nalt,
        Ncards = Ncards,
        cond = conds,
        lower = c(0, 0, 0, 0, 0, 0),
        upper = c(1, 1, 1, 8, 2, 2),
        method = "L-BFGS-B"
      )$par
    })
  })

#save data
save(model2, file = "model2.Rda")
save(true.parms2, file = "true_parms2")
save(rec.parms2, file = "rec_parms2")

#calculate cor between true and recovered params
df.tbl2   <-lapply(1:length(Ntrl), function(trl){
  data.frame(Ntrl=Ntrl[trl],
             cor.alpha_frc=cor(true.parms2$alpha_frc,(do.call(rbind,rec.parms2[[trl]])[,1])),
             cor.alpha_key_pos=cor(true.parms2$alpha_key.alpha_key_pos,(do.call(rbind,rec.parms2[[trl]])[,2])),
             cor.alpha_key_neg=cor(true.parms2$alpha_key.alpha_key_pos.1,(do.call(rbind,rec.parms2[[trl]])[,3])),
             cor.beta=cor(true.parms2$beta,(do.call(rbind,rec.parms2[[trl]])[,4])),
             cor.W_pos=cor(true.parms2$w.w_pos, (do.call(rbind,rec.parms2[[trl]])[,5])),
             cor.W_neg=cor(true.parms2$w.w_pos.1, (do.call(rbind,rec.parms2[[trl]])[,6]))
  )
})

#df.tbl2<-do.call(rbind, df.tbl2)

#plot table to file
df.tbl2 %>%
  kable() %>%
  kable_styling(bootstrap_options = c("striped", "hover", "condensed"))

#save table
save(df.tbl2, file = "tbl2.corr")