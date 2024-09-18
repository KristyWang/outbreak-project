library(readxl)
library(tidyverse)
library(incidence)
library(fitdistrplus)
library(ISOweek)
library(EpiEstim)

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


#### Day 2 ####
## Describe relevant delays
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


### serial interval
serial.interval <- linelist %>% 
  filter(infector != "NA") %>% 
  dplyr::select(case.ID, infector, infectee.onset = date.of.onset) %>% 
  left_join(dplyr::select(linelist, case.ID, date.of.onset), by = c("infector" = "case.ID")) %>% 
  rename(infector.onset = date.of.onset) %>% 
  mutate(SI = infectee.onset - infector.onset) %>% 
  mutate(SI = as.integer(SI))

# check delay values
range(serial.interval$SI)

p6 <- ggplot(serial.interval) + 
  geom_histogram(aes(x = SI, y = ..density..), binwidth = 1, color = "white") + 
  labs(x = "Serial interval (days)", y = "Probability") + 
  theme_bw()
p6

# fit a gamma distribution
fit.gamma <- fitdist(serial.interval$SI + 0.5, distr = "gamma", method = "mle")
summary(fit.gamma)
plot(fit.gamma)

# mean delay
shape <- fit.gamma$estimate[1]
rate <- fit.gamma$estimate[2]
(mean.SI <- shape / rate - 0.5)
(var.SI <- shape / (rate * rate))

# add fitted gamma distribution
delay.SI <- serial.interval %>% 
  dplyr::select(delays = SI) %>% 
  group_by(delays) %>% 
  summarise(n = n()) %>% 
  mutate(freq = n / sum(n))

delays <- seq(range(delay.SI$delays)[1], range(delay.SI$delays)[2])
delay.SI <- data.frame(delays) %>% 
  left_join(delay.SI, by = "delays") %>% 
  replace(is.na(.), 0)

fit.den <- dgamma(delays + 0.5, shape = shape, rate = rate)
delay.SI <- cbind(delay.SI, fit.den)

p7 <- ggplot(delay.SI) + 
  geom_bar(aes(x = delays, y = freq), stat = "identity") + 
  geom_line(aes(x = delays, y = fit.den), color = "red") + 
  labs(x = "Serial interval (days)", y = "Probabolity") + 
  theme_bw()
p7


# stratify serial interval before and after the interventions (2014-09-23)
serial.interval <- serial.interval %>% 
  mutate(intervention = ifelse(infector.onset < as.Date("2014-09-23"), "before", "after")) %>% 
  mutate(intervention = factor(intervention, levels = c("before", "after")))

p8 <- ggplot(serial.interval) + 
  geom_histogram(aes(x = SI, y = ..density..), binwidth = 1, color = "white") + 
  labs(x = "Serial interval (days)", y = "Probability") + 
  theme_bw() + 
  facet_wrap(~ intervention)
p8

# fit a gamma distribution of serial intervals before interventions
serial.interval.before <- serial.interval %>% 
  filter(intervention == "before")

# check SI values
range(serial.interval.before$SI)

fit.gamma.before <- fitdist(serial.interval.before$SI + 0.5, distr = "gamma", method = "mle")
summary(fit.gamma.before)
plot(fit.gamma.before)

# mean delay
shape <- fit.gamma.before$estimate[1]
rate <- fit.gamma.before$estimate[2]
(mean.SI.before <- shape / rate - 0.5)

# fit a gamma distribution of serial intervals after interventions
serial.interval.after <- serial.interval %>% 
  filter(intervention == "after")

# check SI values
range(serial.interval.after$SI)

fit.gamma.after <- fitdist(serial.interval.after$SI + 0.5, distr = "gamma", method = "mle")
summary(fit.gamma.after)
plot(fit.gamma.after)

# mean delay
shape <- fit.gamma.after$estimate[1]
rate <- fit.gamma.after$estimate[2]
(mean.SI.after <- shape / rate - 0.5)


## Simple projetion
linelist <- linelist %>% 
  mutate(isoweek = date2ISOweek(date.of.onset)) %>% 
  mutate(year.week = str_sub(isoweek, start = 1, end = 8))

weekly.inc <- linelist %>% 
  group_by(year.week) %>% 
  summarise(n = n()) %>% 
  mutate(weeknum = row_number(), 
         weekending = str_c(year.week, "-7")) %>% 
  mutate(weekending = ISOweek2date(weekending)) %>% 
  mutate(intervention = ifelse(weekending < as.Date("2014-09-23"), "before", "after")) %>% 
  mutate(intervention = factor(intervention, levels = c("before", "after")))

# plot weekly incidence
p9 <- ggplot(weekly.inc) + 
  geom_bar(aes(x = weekending, y = n), stat = "identity") + 
  geom_vline(xintercept = as.Date("2014-09-23"), color = "red") + 
  labs(x = "Symptom onset", y = "Weekly incidence") + 
  theme_bw()
p9

# fit a linear regression before intervention
weekly.inc.before <- weekly.inc %>% 
  filter(intervention == "before")

fit.lm.before <- lm(log(n) ~ weeknum, data = weekly.inc.before)
summary(fit.lm.before)

# fit a linear regression after intervention
weekly.inc.after <- weekly.inc %>% 
  filter(intervention == "after")

fit.lm.after <- lm(log(n) ~ weeknum, data = weekly.inc.after)
summary(fit.lm.after)

# fitted values
fitted.before <- exp(predict(fit.lm.before, data = weeknum, level = 0.95, interval = "confidence"))
weekly.inc.before <- cbind(weekly.inc.before, fitted.before)

fitted.after <- exp(predict(fit.lm.after, data = weeknum, level = 0.95, interval = "confidence"))
weekly.inc.after <- cbind(weekly.inc.after, fitted.after)

weekly.inc.fitted <- rbind(weekly.inc.before, weekly.inc.after)

# plot weekly incidence with fitted model
p10 <- ggplot(weekly.inc.fitted) + 
  geom_bar(aes(x = weekending, y = n), stat = "identity") + 
  geom_vline(xintercept = as.Date("2014-09-23"), color = "red") + 
  geom_ribbon(aes(x = weekending, ymin = lwr, ymax = upr, fill = intervention), alpha = 0.3) + 
  geom_line(aes(x = weekending, y = fit, color = intervention)) + 
  labs(x = "Symptom onset", y = "Weekly incidence") + 
  theme_bw() + 
  theme(legend.position = c(0.8, 0.8))
p10

# daily growth rate
(r.before <- fit.lm.before$coefficients[2] / 7)
(r.after <- fit.lm.after$coefficients[2] / 7)

# doubling time or halving time
(dt.before <- log(2) / r.before)
(ht.after <- log(2) / abs(r.after))

# Reproduction number
Tinf <- 5.2
Tinc <- 9.92
Tg <- Tinf + Tinc
(R.before <- (1 + r.before * Tinf) * (1 + r.before * Tinc))
(R.after <- (1 + r.after * Tinf) * (1 + r.after * Tinc))

# forward projection
fp <- exp(predict(fit.lm.after, new = data.frame(weeknum = max(weekly.inc$weeknum) + 1:30), 
                  level = 0.95, interval = "confidence"))
fp


## Assess temporal variations in R
daily.inc <- inc.sym.onset %>% 
  rename(I = n) %>% 
  replace(is.na(.), 0)

res_parametric_si <- estimate_R(daily.inc$I, 
                                method = "parametric_si",
                                config = make_config(list(
                                  mean_si = mean.SI, 
                                  std_si = sqrt(var.SI)))
                                )
head(res_parametric_si$R)
plot(res_parametric_si, legend = FALSE)

# reproduction number before and after interventions
as.integer(as.Date("2014-09-23") - min(daily.inc$date.of.onset)) + 1
res_parametric_si <- estimate_R(daily.inc$I, 
                                method = "parametric_si",
                                config = make_config(list(
                                  mean_si = mean.SI, 
                                  std_si = sqrt(var.SI), 
                                  t_start = c(2, 170), 
                                  t_end = c(171, 389)))
)
head(res_parametric_si$R)
plot(res_parametric_si, legend = FALSE)
