#keep environment clean
rm(list = ls())

#set work directory
Setwd("C:\Users\seanm\Documents\R\R Day 3")

#load in packages
library(tidyverse)
library(haven)
library(foreign)

#load in csv data file.
dat <- read.csv("world_happiness2017.csv", header = T) %>% 
    as_tibble

#round numbers to 2 decimal places.
dat2 <- read.csv("world_happiness2017.csv") %>% 
    as_tibble %>% 
    mutate_if(is.double, round, 2)

#mutate numbers that end with A
dat2 %>% 
    mutate_at(vars(ends_with("_A")),)

regression <- lm(Happiness.Score ~ Freedom, data = dat2) %>% 
    as_tibble
summary(regression)

plot(regression)

#load in psych
library(psych)

# bptest(regression)
# dat_sdt = read.csv("SDT_data.csv")
# ?bartlett.test(Happiness.Score ~ Freedom, data = dat2)

dat3 <- read.csv("sdt_data.csv", header = T) %>% 
    as_tibble %>% 
    transform(integer)

#EFA fun!
efa_dat1 = fa(dat3 [,-c(28,29)], nfactors = 1, rotate = "oblimin", fm = "pa")
efa_dat2 = fa(dat3 [,-c(28,29)], nfactors = 2, rotate = "oblimin", fm = "pa")
efa_dat3 = fa(dat3 [,-c(28,29)], nfactors = 3, rotate = "oblimin", fm = "pa")
efa_dat4 = fa(dat3 [,-c(28,29)], nfactors = 4, rotate = "oblimin", fm = "pa")

efa_dat1$loadings
efa_dat2$loadings

fa.diagram(efa_dat2)