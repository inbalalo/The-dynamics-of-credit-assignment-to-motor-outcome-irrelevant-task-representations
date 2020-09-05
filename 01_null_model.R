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

####Model 1####
true.parms1 <-
  data.frame(
    alpha_frc = runif(N, min = 0, max = 1),
    alpha_key = cbind(rep(0, N), rep(0, N)),
    beta = runif(N, min = 1, max = 8),
    w = cbind(rep(0, N), rep(0, N))
  )

model1 <-
  lapply(1:length(Ntrl), function(trl) {
      mclapply(1:N, function(s) {
        sim.task(
          Ntrl[trl],
          Nalt,
          Ncards,
          true.parms1$alpha_frc[s],
          c(true.parms1$alpha_key.1[s], true.parms1$alpha_key.2[s]),
          true.parms1$beta[s],
          c(true.parms1$w.1[s], true.parms1$w.2[s]),
          rndwlk,
          conds
        )
      })
    })

#recover param from data
rec.parms1 <-
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
            runif(1, min = 0, max = 2),      #beta
            0, #w_pos
            0  #w_neg
          ),
          fn = fit.task,
          df = model1[[trl]][[s]][['df']],
          alternatives = model1[[trl]][[s]][['alts']],
          Nalt = Nalt,
          Ncards = Ncards,
          cond = conds,
          lower = c(0, 0, 0, 0, 0, 0),
          upper = c(1, 1, 1, 2, 1e-110, 1e-110),
          method = "L-BFGS-B"
        )$par
      })
    })
as.data.frame()

#save data
save(model1, file = "model1.Rda")
save(true.parms1, file = "true.parms1")
save(rec.parms1, file = "rec.parms1")

#calculate cor between true and recovered params
df.tbl1   <-lapply(1:length(Ntrl), function(trl){
    data.frame(Ntrl=Ntrl[trl],
               cor.alpha_frc=cor(true.parms1$alpha_frc,(do.call(rbind,rec.parms1[[trl]])[,1])),
               #cor.alpha_key_pos=cor(true.parms1$alpha_key[1],(do.call(rbind,rec.parms1[[trl]])[,2])),
               #cor.alpha_key_neg=cor(true.parms1$alpha_key[2],(do.call(rbind,rec.parms1[[trl]])[,3])),
               cor.beta=cor(true.parms1$beta,(do.call(rbind,rec.parms1[[trl]])[,4]))
               #cor.W_pos=cor(true.parms1$w[1], (do.call(rbind,rec.parms1[[trl]])[,5])),
               #cor.W_neg=cor(true.parms1$w[2], (do.call(rbind,rec.parms1[[trl]])[,6]))
               )
  })

#print table to file
df.tbl1 %>%
  kable() %>%
  kable_styling(bootstrap_options = c("striped", "hover", "condensed"))

#save table
save(df.tbl1, file = "tbl1.corr")
