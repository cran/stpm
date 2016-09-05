## ----eval=FALSE----------------------------------------------------------
#  require(devtools)
#  devtools::install_github("izhbannikov/stpm")

## ----eval=FALSE----------------------------------------------------------
#  install.packages("<path to the downloaded r-package stpm>", repos=NULL, type="binary")

## ----results='hide',warning=FALSE,echo=FALSE,message=FALSE---------------
library(stpm)
# Reading longitude data:
longdat <- read.csv(system.file("data","longdat.csv",package="stpm"))
# Prepare data for optimization:
vitstat <- read.csv(system.file("data","vitstat.csv",package="stpm"))

## ----echo=FALSE----------------------------------------------------------
head(longdat)

## ----echo=FALSE----------------------------------------------------------
head(vitstat)

## ------------------------------------------------------------------------
library(stpm)
data <- simdata_discr(N=10) # simulate data for 10 individuals
head(data)

## ------------------------------------------------------------------------
library(stpm)
data <- simdata_cont(N=5) # simulate data for 5 individuals
head(data)

## ------------------------------------------------------------------------
library(stpm)
#Data simulation (200 individuals)
data <- simdata_discr(N=200)
#Estimation of parameters
pars <- spm_discrete(data)
pars

## ------------------------------------------------------------------------
library(stpm)
#Simulate some data for 100 individuals
data <- simdata_cont(N=100)
head(data)
#Estimate parameters
# a=-0.05, f1=80, Q=2e-8, f=80, b=5, mu0=2e-5, theta=0.08 are starting values for estimation procedure
pars <- spm_continuous(dat=data,a=-0.05, f1=80, Q=2e-8, f=80, b=5, mu0=2e-5, theta=0.08)
pars

## ------------------------------------------------------------------------
library(stpm)
#Data preparation:
n <- 50
data <- simdata_time_dep(N=n)
# Estimation:
opt.par <- spm_time_dep(data, 
                        start = list(a = -0.05, f1 = 80, Q = 2e-08, f = 80, b = 5, mu0 = 0.001), 
                        frm = list(at = "a", f1t = "f1", Qt = "Q", ft = "f", bt = "b", mu0t= "mu0"))
opt.par

## ------------------------------------------------------------------------
library(stpm)
data <- simdata_discr(N=10)
head(data)

## ---- eval=T-------------------------------------------------------------
library(stpm)
data <- simdata_cont(N=10)
head(data)

## ---- eval=T-------------------------------------------------------------
library(stpm)
data <- sim_pobs(N=10)
head(data)

## ---- eval=T-------------------------------------------------------------
library(stpm)
#Generating data:
data <- sim_pobs(N=10)
head(data)
#Parameters estimation:
pars <- spm_pobs(x=data)
pars

## ---- eval=T-------------------------------------------------------------
library(stpm)
data.genetic <- sim_pobs(N=10, mode='observed')
head(data.genetic)
data.nongenetic <- sim_pobs(N=50, mode='unobserved')
head(data.nongenetic)
#Parameters estimation:
pars <- spm_pobs(x=data.genetic, y = data.nongenetic, mode='combined')
pars

