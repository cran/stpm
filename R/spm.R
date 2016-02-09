#'A central function that estimates Stochastic Process Model parameters a from given dataset.
#'@references Yashin, A. et al (2007), Stochastic model for analysis of longitudinal data on aging 
#'and mortality. Mathematical Biosciences, 208(2), 538-551.
#'@references Akushevich I., Kulminski A. and Manton K. (2005). Life tables with covariates: Dynamic model 
#'for Nonlinear Analysis of Longitudinal Data. Mathematical Popu-lation Studies, 12(2), pp.: 51-80.
#'<DOI: 10.1080/08898480590932296>.
#'@references Yashin, A. et al (2007), Health decline, aging and mortality: how are they related? 
#'Biogerontology, 8(3), 291-302.<DOI:10.1007/s10522-006-9073-3>.
#'@param x A dataset: is the output from prepare_data(...) function and consists of two separate data tables:
#'(1) a data table for continuous-time model and (2) a data table for discrete-time model.
#'@param model A model type. Choices are: "discrete", "continuous" or "time-dependent".
#'@param formulas A list of parameter formulas used in the "time-dependent" model.
#'@param verbose A verbosing output indicator (FALSE by default).
#'@param tol A tolerance threshold for matrix inversion (NULL by default).
#'@return For "discrete" and "continuous" model types: 
#'(1) a list of model parameter estimates for the discrete model type described in 
#'"Life tables with covariates: Dynamic Model for Nonlinear Analysis of Longitudinal Data", 
#'Akushevich et al, 2005.<DOI:10.1080/08898480590932296>,  and  
#'(2) a list of model parameter estimates for the continuous model type described in 
#'"Stochastic model for analysis of longitudinal data on aging and mortality", 
#'Yashin et al, 2007, Math Biosci.<DOI:10.1016/j.mbs.2006.11.006>.
#'
#'For the "time-dependent" model (model parameters depend on time): a set of model parameter estimates.
#'@examples \dontrun{ 
#'library(stpm)
#'#Prepare data for optimization
#'data <- prepare_data(x=system.file("data","longdat.csv",package="stpm"), 
#'					   y=system.file("data","vitstat.csv",package="stpm"))
#'#Parameters estimation (default model: discrete-time):
#'p.discr.model <- spm(data)
#'p.discr.model
#'# Continuous-time model:
#'p.cont.model <- spm(data, model="continuous")
#'p.cont.model
#'#Model with time-dependent coefficients:
#'data <- prepare_data(x=system.file("data","longdat.csv",package="stpm"), 
#'					   y=system.file("data","vitstat.csv",package="stpm"), 
#'					   covariates="BMI")
#'p.td.model <- spm(data, model="time-dependent")
#'p.td.model
#'}
spm <- function(x, model="discrete", formulas = NULL, verbose=FALSE, tol=NULL) {
  
  # List of available models:
  models <- c("discrete", "continuous", "time-dependent")
  
  if(!(model %in% models)) {
    stop(cat(model, " - unknown model type!"))
  }
  
  # Number of variables (dimensions):
  k <- (dim(x[[1]])[2] - 4)/2
  
  
  if(model == "discrete") {
    # Estimation of starting point with discrete optimization:
    pars <- spm_discrete(dat=x[[2]],k=k, verbose = verbose, tol = tol)
    res <- list(Ak2005=list(u=pars$pars1$u, 
                            R=pars$pars1$R, 
                            b=pars$pars1$b, 
                            Q=pars$pars1$Q, 
                            Sigma=pars$pars1$Sigma,
                            mu0=pars$pars1$mu0,
                            theta=pars$pars1$theta), 
                Ya2007=list(a=pars$pars2$a, 
                            f1=pars$pars2$f1,
                            Q=pars$pars2$Q,
                            f=pars$pars2$f, 
                            b=pars$pars2$b, 
                            mu0=pars$pars2$mu0, 
                            theta=pars$pars2$theta))
    
  }
  
  
  if(model == "continuous") {
    pars <- spm_discrete(dat=x[[2]],k=k, verbose = verbose, tol = tol)
    data <- data.frame(x[[1]][,2:dim(x[[1]])[2]])
  
    if(verbose) {
      cat("Starting parameters:\n")
      print(pars)
    }
    
    if(det(pars$pars2$Q) < 0) {
      cat("Error: determinant of Q < 0\n")
      cat("Q:\n")
      print(pars$pars2$Q)
      cat("Det(Q):\n")
      print(det(pars$pars2$Q))
      
      res <- NA
    
    } else {
      spm_continuous(as.matrix(data), 
                    a=pars$pars2$a, 
                    f1=pars$pars2$f1, 
                    Q=pars$pars2$Q, 
                    f=pars$pars2$f, 
                    b=pars$pars2$b, 
                    mu0=pars$pars2$mu0, 
                    theta=pars$pars2$theta, 
                    k, 
                    verbose)
  
      res.t <- get("results",envir=.GlobalEnv)
      
      Q.c <- res.t$Q
      R.c <- res.t$a + diag(k)
      Sigma.c <- as.matrix(res.t$b)
      u.c <- (-1)*(res.t$f1 %*% res.t$a)
      b.c <- -2*res.t$f %*% res.t$Q
      mu0.c <- res.t$mu0 + res.t$f %*% res.t$Q %*% t(res.t$f)
      theta.c <- res.t$theta
      
      res <- list(Ak2005=list(u=u.c, 
                              R=R.c, 
                              b=b.c, 
                              Q=Q.c, 
                              Sigma=Sigma.c,
                              mu0=mu0.c,
                              theta=theta.c), 
                  Ya2007=list(a=res.t$a, 
                              f1=res.t$f1,
                              Q=res.t$Q,
                              f=res.t$f, 
                              b=res.t$b, 
                              mu0=res.t$mu0, 
                              theta=res.t$theta))
      
    }
  }
  
  if(model == "time-dependent") {
    data <- x[[1]][,2:dim(x[[1]])[2]]
    
    if(k > 1) {
      stop("Number of variables > 1. Model with time-dependent parameters can be used only with one variable!")
    }
    
    #if(length(formulas) != 6) {
    #  stop("It must be 6 equations for corresponding coefficients.")
    #}
    
    formulas.work = list(at="a", f1t="f1", Qt="Q", ft="f", bt="b", mu0t="mu0")
    
    if(!is.null(formulas)) {
      for(item in formulas) {
        formulas.work[[item]] <- formulas[[item]]
      }
    }
    
    pars <- spm_discrete(dat=x[[2]],k=k)
    
    res.t <- spm_time_dep(x[[1]][,2:dim(x[[1]])[2]], 
                            f = formulas.work,
                            start=list(a=pars$pars2$a, f1=pars$pars2$f1, Q=pars$pars2$Q, f=pars$pars2$f, b=pars$pars2$b, mu0=pars$pars2$mu0))
    
    res <- get("results",envir=.GlobalEnv)
  
  }
  
  invisible(res)
}