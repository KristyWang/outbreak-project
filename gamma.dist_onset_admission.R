library(distributional)
library(epiparameter)
library(tidyverse)

of <- onset_frequency %>% arrange(-desc(onset.to.admission)) %>% dplyr::select(onset.to.admission,count) 
fill_in <- tibble(
  onset.to.admission = c(19,20, 21),
  count = c(0, 0, 0)
)


of <- bind_rows(of,fill_in) %>% arrange(-desc(onset.to.admission))


shape = seq(0.5,2,by=0.1)
rate  = seq(0.1,1,by=0.1)

res_s <- c()
res_r <- c()
res_L <- c()

for(s in shape)
  for(r in rate)
  {
    gamma_cdf  <- pgamma(seq(0.5,22.5,by=1),shape=s,rate=r) 
    gamma_disc <- gamma_cdf - c(0,gamma_cdf[1:length(gamma_cdf)-1])
    gamma_disc <- gamma_disc/sum(gamma_disc)
    
    res_s <- c(res_s,s)
    res_r <- c(res_r, r)
    res_L <- c(res_L, sum(of$count * log(gamma_disc)))
  }

result <- tibble(shape=res_s,rate=res_r,L=res_L)
