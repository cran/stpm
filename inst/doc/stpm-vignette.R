## ----eval=FALSE----------------------------------------------------------
#  install.packages("stpm")

## ----eval=FALSE----------------------------------------------------------
#  require(devtools)
#  devtools::install_github("izhbannikov/stpm")

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
data <- simdata_cont(N=100, ystart = 80, a = -0.1, Q = 1e-06, mu0 = 1e-5, theta = 0.08, f1 = 80, f=80, b=1, dt=1, sd0=5)
ans <- spm_continuous(dat=data,
                      a = -0.1,
                      f1 = 80+runif(1,-5,5), 
                      Q = 1.0e-6+runif(1,-1e-7,1e-7),
                      f = 80+runif(1,-5,5),
                      b = 1,
                      mu0 = 1.0e-5+runif(1,-1e-6,1e-6),
                      theta = 0.08+runif(1,-1e-3,1e-3),
                      stopifbound = FALSE, maxeval=300,
                      lb=c(-0.12, 60, 0.6e-6, 60, 0.5, 0.6e-5, 0.06), 
                      ub=c(-0.08, 140, 1.3e-06, 140, 1.5, 1.2e-5, 0.10),
                      algorithm="NLOPT_LN_NELDERMEAD")
ans

## ------------------------------------------------------------------------
library(stpm)

data <- simdata_cont(N=100, 
                     a=matrix(c(-0.1,  0.001, 0.001, -0.1), nrow = 2, ncol = 2, byrow = T),
                     f1=t(matrix(c(100, 200), nrow = 2, ncol = 1, byrow = F)),
                     Q=matrix(c(1e-06, 1e-7, 1e-7,  1e-06), nrow = 2, ncol = 2, byrow = T),
                     f=t(matrix(c(100, 200), nrow = 2, ncol = 1, byrow = F)),
                     b=matrix(c(1, 2), nrow = 2, ncol = 1, byrow = F),
                     mu0=1e-4,
                     theta=0.08,
                     ystart = c(100,200), sd0=c(5, 10), dt=1)

a.d <- matrix(c(-0.1+runif(1,-1e-2,1e-2),  0.001+runif(1,-1e-4,1e-4), 0.001+runif(1,-1e-4,1e-4), -0.1+runif(1,-1e-2,1e-2)), nrow = 2, ncol = 2, byrow = T)
f1.d <- t(matrix(c(100+runif(1,-5,5), 200+runif(1,-5,5)), nrow = 2, ncol = 1, byrow = F))
Q.d <- matrix(c(1e-06+runif(1,-1e-7,1e-7), 1e-7+runif(1,-1e-8,1e-8), 1e-7+runif(1,-1e-8,1e-8),  1e-06+runif(1,-1e-7,1e-7)), nrow = 2, ncol = 2, byrow = T)
f.d <- t(matrix(c(100+runif(1,-5,5), 200+runif(1,-5,5)), nrow = 2, ncol = 1, byrow = F))
b.d <- matrix(c(1, 2), nrow = 2, ncol = 1, byrow = F)
mu0.d <- 1e-4 + runif(1,-1e-5,1e-5)
theta.d <- 0.08+ runif(1,-1e-4,1e-4)

ans <- spm_continuous(dat=data,
                      a = a.d, 
                      f1 = f1.d,
                      Q = Q.d,
                      f = f.d,
                      b = b.d,
                      mu0 = mu0.d,
                      theta = theta.d,
                      maxeval=150,
                      lb=c(-0.12, ifelse(a.d[2,1] > 0, a.d[2,1]-0.1*a.d[2,1], a.d[2,1]+0.1*a.d[2,1]), ifelse(a.d[1,2] > 0, a.d[1,2]-0.1*a.d[1,2], a.d[1,2]+0.1*a.d[1,2]), -0.12,  
                           95, 195, 
                           Q.d[1,1]-0.1*Q.d[1,1], ifelse(Q.d[2,1] > 0, Q.d[2,1]-0.1*Q.d[2,1], Q.d[2,1]+0.1*Q.d[2,1]), ifelse(Q.d[1,2] > 0, Q.d[1,2]-0.1*Q.d[1,2], Q.d[1,2]+0.1*Q.d[1,2]), Q.d[2,2]-0.1*Q.d[2,2],
                           95, 195,
                           0.5, 1.5,
                           0.8e-4,
                           0.06),
                      ub=c(-0.08,  0.002,  0.002, -0.08,  
                           110, 220, 
                           Q.d[1,1]+0.1*Q.d[1,1], ifelse(Q.d[2,1] > 0, Q.d[2,1]+0.1*Q.d[2,1], Q.d[2,1]-0.1*Q.d[2,1]), ifelse(Q.d[1,2] > 0, Q.d[1,2]+0.1*Q.d[1,2], Q.d[1,2]-0.1*Q.d[1,2]), Q.d[2,2]+0.1*Q.d[2,2],
                           110, 220,
                           1.5, 2.5,
                           1.2e-4,
                           0.10), algorithm = "NLOPT_LN_NELDERMEAD")
ans


## ------------------------------------------------------------------------
n <- 10
data <- simdata_time_dep(N=n)
# Estimation:
opt.par <- spm_time_dep(data, start=list(a=-0.05, f1=80, Q=2e-08, f=80, b=5, mu0=0.001), 
                        lb=c(-1, 30, 1e-8, 30, 1, 1e-6), ub=c(0, 120, 5e-8, 130, 10, 1e-2))
opt.par



## ------------------------------------------------------------------------
library(stpm)
n <- 10
data <- simdata_time_dep(N=n)
# Estimation:
opt.par <- spm_time_dep(data, frm = list(at="a", f1t="f1", Qt="Q", ft="0", bt="b", mu0t="mu0"))
opt.par

## ------------------------------------------------------------------------

n <- 10
data <- simdata_time_dep(N=n)
# Estimation:
opt.par <- spm_time_dep(data, frm = list(at="a", f1t="f1", Qt="Q", ft="0", bt="b", mu0t="mu0"), 
                        start=list(a=-0.05, f1=80, Q=2e-08, b=5, mu0=0.001), 
                        lb=c(-1, 30, 1e-8, 1, 1e-6), ub=c(0, 120, 5e-8, 10, 1e-2))
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

