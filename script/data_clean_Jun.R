library(readxl)
library(tidyverse)
library(incidence)
library(fitdistrplus)

linelist <- read_xlsx("../data/EbolaLineList-Freetown_Practical.xlsx", sheet = 1)
colnames(linelist) <- make.names(colnames(linelist))

linelist <- linelist %>% 
  mutate(date.of.infection = as.integer(date.of.infection)) %>% 
  mutate(date.of.infection = as.Date(date.of.infection, origin = "1900-01-01")) %>% 
  mutate(date.of.outcome = as.integer(date.of.outcome)) %>% 
  mutate(date.of.outcome = as.Date(date.of.outcome, origin = "1900-01-01")) %>% 
  mutate(date.of.onset = as.Date(date.of.onset), 
         date.of.hospitalisation = as.Date(date.of.hospitalisation))

# check hospital
unique(linelist$hospital)

linelist <- linelist %>% 
  mutate(hospital = ifelse(hospital %in% c("Connaught", "Connaught Hopital"), "Connaught Hospital", hospital))

# check hospital again
unique(linelist$hospital)

# check date of outcome
unique(linelist$date.of.outcome)
sum(is.na(linelist$date.of.outcome))

# check timeline
linelist %>% 
  filter(date.of.outcome < date.of.hospitalisation)

linelist %>% 
  filter(date.of.outcome < date.of.onset)

linelist %>% 
  filter(date.of.outcome < date.of.infection)

linelist <- linelist %>% 
  mutate(date.of.outcome = ifelse(date.of.outcome < date.of.hospitalisation | date.of.outcome < date.of.onset | date.of.outcome < date.of.infection, NA, date.of.outcome))

sum(is.na(linelist$date.of.outcome))

# check date of onset
sum(is.na(linelist$`date of onset`))

# incidence plot by date of symptom onset
inc.sym.onset <- incidence(linelist$date.of.onset)
inc.sym.onset
plot(inc.sym.onset)

# plot incidence from scratch
inc.sym.onset <- linelist %>% 
  group_by(date.of.onset) %>% 
  summarise(n = n())

range(linelist$date.of.onset)
date.of.onset <- seq.Date(range(linelist$date.of.onset)[1], range(linelist$date.of.onset)[2], by = "1 day")

inc.sym.onset <- data.frame(date.of.onset) %>% 
  left_join(inc.sym.onset, by = "date.of.onset")

p1 <- ggplot(inc.sym.onset) + 
  geom_bar(aes(x = date.of.onset, y = n), stat = "identity") + 
  geom_vline(xintercept = as.Date("2014-09-23"), color = "red") + 
  labs(x = "Symptom onset", y = "Daily incidence") + 
  theme_bw()
p1

# source of infection
inc.sym.onset.exp <- linelist %>% 
  group_by(date.of.onset, source.of.infection) %>% 
  summarise(n = n())

source.of.infection <- unique(linelist$source.of.infection)
date.source <- expand.grid(date.of.onset = date.of.onset, source.of.infection = source.of.infection)

inc.sym.onset.exp <- data.frame(date.source) %>% 
  left_join(inc.sym.onset.exp, by = c("date.of.onset", "source.of.infection"))

p2 <- ggplot(inc.sym.onset.exp) + 
  geom_bar(aes(x = date.of.onset, y = n, fill = source.of.infection), stat = "identity") + 
  geom_vline(xintercept = as.Date("2014-09-23"), color = "red") + 
  labs(x = "Symptom onset", y = "Daily incidence", fill = "Source of infection") + 
  theme_bw() + 
  facet_wrap(~source.of.infection, ncol = 1)
p2

# stacked incidence plot
p3 <- ggplot(inc.sym.onset.exp) + 
  geom_bar(aes(x = date.of.onset, y = n, fill = source.of.infection), stat = "identity", position = "stack") + 
  geom_vline(xintercept = as.Date("2014-09-23"), color = "red") + 
  labs(x = "Symptom onset", y = "Daily incidence", fill = "Source of infection") + 
  theme_bw() + 
  theme(legend.position = c(0.8, 0.8))
p3

#### Compute the Case Fatality Ratio (CFR)
# basic CFR
unique(linelist$outcome)

deaths <- linelist %>% 
  filter(outcome == "Death")
 
(basic.CFR <- nrow(deaths) / nrow(linelist))
(bt <- binom.test(nrow(deaths), nrow(linelist)))

# CFR using only known outcomes
cases <- linelist %>% 
  filter(outcome %in% c("Death", "Recover"))

(CFR <- nrow(deaths) / nrow(cases))
(bt <- binom.test(nrow(deaths), nrow(cases)))

# CFR by gender
cases.gender <- linelist %>% 
  group_by(gender) %>% 
  summarize(cases = n())

deaths.gender <- deaths %>% 
  group_by(gender) %>% 
  summarize(deaths = n())

CFR.gender <- cases.gender %>% 
  left_join(deaths.gender) %>% 
  mutate(CFR = deaths / cases)

# CFR by exposure
cases.exp <- linelist %>% 
  group_by(source.of.infection) %>% 
  summarize(cases = n())

deaths.exp <- deaths %>% 
  group_by(source.of.infection) %>% 
  summarize(deaths = n())

CFR.exp <- cases.exp %>% 
  left_join(deaths.exp) %>% 
  mutate(CFR = deaths / cases)

# CFR by hospital
cases.hosp <- linelist %>% 
  group_by(hospital) %>% 
  summarize(cases = n())

deaths.hosp <- deaths %>% 
  group_by(hospital) %>% 
  summarize(deaths = n())

CFR.hosp <- cases.hosp %>% 
  left_join(deaths.hosp) %>% 
  mutate(CFR = deaths / cases)


## Day 2
### delay from onset to admission
linelist <- linelist %>% 
  mutate(delay.onset.adm = date.of.hospitalisation - date.of.onset) %>% 
  mutate(delay.onset.adm = as.integer(delay.onset.adm))

# check delay values
range(linelist$delay.onset.adm)

p4 <- ggplot(linelist) + 
  geom_histogram(aes(x = delay.onset.adm, y = ..density..), binwidth = 1, color = "white") + 
  labs(x = "Delay from onset to admission (days)", y = "Probability") + 
  theme_bw()
p4

# fit a gamma distribution
fit.gamma <- fitdist(linelist$delay.onset.adm + 0.5, distr = "gamma", method = "mle")
summary(fit.gamma)
plot(fit.gamma)

# mean delay
shape <- fit.gamma$estimate[1]
rate <- fit.gamma$estimate[2]
(mean.delay <- shape / rate - 0.5)

# add fitted gamma distribution
delay.onset.adm <- linelist %>% 
  dplyr::select(delays = delay.onset.adm) %>% 
  group_by(delays) %>% 
  summarise(n = n()) %>% 
  mutate(freq = n / sum(n))

delays <- seq(range(delay.onset.adm$delays)[1], range(delay.onset.adm$delays)[2])
delay.onset.adm <- data.frame(delays) %>% 
  left_join(delay.onset.adm, by = "delays") %>% 
  replace(is.na(.), 0)

fit.den <- dgamma(delays + 0.5, shape = shape, rate = rate)
delay.onset.adm <- cbind(delay.onset.adm, fit.den)

p5 <- ggplot(delay.onset.adm) + 
  geom_bar(aes(x = delays, y = freq), stat = "identity") + 
  geom_line(aes(x = delays, y = fit.den), color = "red") + 
  labs(x = "Delay from onset to admission (days)", y = "Probabolity") + 
  theme_bw()
p5
