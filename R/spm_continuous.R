#'Continuous multi-dimensional optimization
#'@references Yashin, A.I. et al (2007). Stochastic model for analysis of longitudinal data on aging 
#'and mortality. Mathematical Biosciences, 208(2), 538-551.<DOI:10.1016/j.mbs.2006.11.006>.
#'@param dat A data table.
#'@param a A starting value of the rate of adaptive response to any deviation of Y from f1(t).
#'@param f1 A starting value of the average age trajectories of the variables which process is forced to follow. 
#'@param Q Starting values of the quadratic hazard term.
#'@param f A starting value of the "optimal" value of variable which corresponds to the minimum of hazard rate at a respective time.
#'@param b A starting value of a diffusion coefficient representing a strength of the random disturbance from Wiener Process.
#'@param mu0 A starting value of the baseline hazard.
#'@param theta A starting value of the parameter theta (axe displacement of Gompertz function).
#'@param verbose An indicator of verbosing output.
#'@param stopifbound Estimation stops if at least one parameter achieves lower or upper boundaries.
#'#'Check the NLopt website for a description of
#'the algorithms. Default: NLOPT_LN_NELDERMEAD
#'@param lb Lower bound of parameters under estimation.
#'@param ub Upper bound of parameters under estimation.
#'The program stops when the number of function evaluations exceeds maxeval. Default: 500.
#'@param pinv.tol A tolerance value for pseudo-inverse of matrix gamma (see Yashin, A.I. et al (2007). Stochastic model for analysis of longitudinal data on aging 
#'and mortality. Mathematical Biosciences, 208(2), 538-551.<DOI:10.1016/j.mbs.2006.11.006>.)
#'@param gomp A flag (FALSE by default). When it is set, then time-dependent exponential form of mu0 is used:
#' mu0 = mu0*exp(theta*t).
#'@param opts A list of options for \code{nloptr}.
#'Default value: \code{opt=list(algorithm="NLOPT_LN_NELDERMEAD", 
#'maxeval=100, ftol_rel=1e-8)}.
#'Please see \code{nloptr} documentation for more information.
#'@param logmu0 Natural logarith of baseline mortality.
#'Default: \code{FALSE}.
#'@return A set of estimated parameters a, f1, Q, f, b, mu0, theta and
#'additional variable \code{limit} which indicates if any parameter 
#'achieved lower or upper boundary conditions (FALSE by default).
#'@return status Optimization status (see documentation for nloptr package).
#'@return LogLik A logarithm likelihood.
#'@return objective A value of objective function (given by nloptr).
#'@return message A message given by nloptr optimization function (see documentation for nloptr package).
#'@details \code{spm_continuous} runs much slower that discrete but more precise and can handle time 
#'intervals with different lengths.
#'@export
#'@examples
#'library(stpm)
#'set.seed(123)
#'#Reading the data:
#'data <- simdata_cont(N=2)
#'head(data)
#'#Parameters estimation:
#'pars <- spm_continuous(dat=data,a=-0.05, f1=80, 
#'						           Q=2e-8, f=80, b=5, mu0=2e-5)
#'pars
#'
spm_continuous <- function(dat, 
                           a=-0.05, 
                           f1=80, 
                           Q=2e-8,
                           f=80,
                           b=5,
                           mu0=2e-5,
                           theta=0.08,
                           stopifbound=FALSE, 
                           lb=NULL, ub=NULL,
                           verbose=FALSE,
                           pinv.tol=0.01,
                           gomp=FALSE,
                           opts=list(algorithm="NLOPT_LN_NELDERMEAD", 
                                          maxeval=100, ftol_rel=1e-8),
                           logmu0=FALSE) {
  
  
  
  setlb <- function(k, params) {
    # This function sets lower and upper boundaries for optim.
    # - k - number of dimensions
    # - params - initial parameters, a vector
    #
    # Lower boundaries:
    lower_bound <- c()
    # Setting boundaries for coefficients:
    # aH
    start=1; end=k^2
    lower_bound <- c(lower_bound, unlist(lapply(start:end, function(n){ params[n] + ifelse(params[n] > 0, -0.1*params[n], 0.1*params[n]) }))) 
    # f1H
    start=end+1; end=start+k-1
    lower_bound <- c(lower_bound, unlist(lapply(start:end, function(n){ params[n] + ifelse(params[n] > 0, -0.1*params[n], 0.1*params[n]) }))) 
    # QH
    start=end+1; end=start+k^2-1
    lower_bound <- c(lower_bound, unlist(lapply(start:end, function(n){ ifelse(params[n] > 0, 0, params[n]+0.1*params[n]) })))
    # fH
    start=end+1; end=start+k-1
    lower_bound <- c(lower_bound, unlist(lapply(start:end, function(n){ params[n] + ifelse(params[n] > 0, -0.1*params[n], 0.1*params[n]) })) )
    # bH
    start=end+1; end=start+k-1
    lower_bound <- c(lower_bound, unlist(lapply(start:end, function(n){params[n] + ifelse(params[n] > 0, -0.1*params[n], 0.1*params[n]) })) )
    # mu0
    start=end+1; end=start
    lower_bound <- c( lower_bound, params[start:end] + ifelse(params[start:end] > 0, -0.1*params[start:end], 0.1*params[start:end]))
    # theta
    start=end+1; end=start
    lower_bound <- c( lower_bound, params[start:end] - 0.1*params[start:end])
    
    lower_bound
  }
  
  
  
  setub <- function(k, params) {
    # This function sets lower and upper boundaries for optim.
    # - k - number of dimensions
    # - params - initial parameters, a vector
    #
    #Upper boundaries:
    upper_bound <- c()
    # Setting boundaries for coefficients:
    # aH
    start=1; end=k^2
    upper_bound <- c(upper_bound, unlist(lapply(start:end, function(n){ifelse(params[n] > 0, params[n] + 0.1*params[n], 0*params[n]) })))
    # f1H
    start=end+1; end=start+k-1
    upper_bound <- c(upper_bound, unlist(lapply(start:end, function(n){ifelse(params[n] > 0, params[n] + 0.1*params[n], params[n]- 0.1*params[n]) })))
    # QH
    start=end+1; end=start+k^2-1
    upper_bound <- c(upper_bound, unlist(lapply(start:end, function(n) {ifelse(params[n] > 0, params[n] + 0.1*params[n], params[n]-0.1*params[n] )})))
    # fH
    start=end+1; end=start+k-1
    upper_bound <- c(upper_bound, unlist(lapply(start:end, function(n){ifelse(params[n] > 0, params[n]+0.1*params[n], params[n]-0.1*params[n]) })))
    # bH
    start=end+1; end=start+k-1
    upper_bound <- c(upper_bound, unlist(lapply(start:end, function(n){ifelse(params[n] > 0, params[n]+0.1*params[n], params[n]-0.1*params[n]) })))
    # mu0
    start=end+1; end=start
    upper_bound <- c( upper_bound, ifelse(params[start:end] > 0, params[start:end]+0.1*params[start:end], params[start:end]-0.1*params[start:end]))
    # theta
    start=end+1; end=start
    upper_bound <- c( upper_bound, params[start:end] + 0.1*params[start:end])
    
    upper_bound
  }
  
  
  ###=======For DEBUG========###
  #dat = dat
  #
  #a=matrix(c(-0.05,  0.001, 0.001, -0.05), nrow = 2, ncol = 2, byrow = T)
  #f1=t(matrix(c(100, 200), nrow = 2, ncol = 1, byrow = F))
  #Q=matrix(c(1e-06, 1e-7, 1e-7,  1e-06), nrow = 2, ncol = 2, byrow = T)
  #f=t(matrix(c(100, 200), nrow = 2, ncol = 1, byrow = F))
  #b=matrix(c(2, 5), nrow = 2, ncol = 1, byrow = F)
  #mu0=1e-4
  #theta=0.08
  #k=2
  #
  #stopifbound=FALSE
  #algorithm="NLOPT_LN_NELDERMEAD"
  #lb=NULL
  #ub=NULL
  #verbose=FALSE
  ###=========================###
  
  
  
  #avail_algorithms <- c("NLOPT_GN_DIRECT", "NLOPT_GN_DIRECT_L",
  #                      "NLOPT_GN_DIRECT_L_RAND", "NLOPT_GN_DIRECT_NOSCAL",
  #                      "NLOPT_GN_DIRECT_L_NOSCAL",
  #                      "NLOPT_GN_DIRECT_L_RAND_NOSCAL",
  #                      "NLOPT_GN_ORIG_DIRECT", "NLOPT_GN_ORIG_DIRECT_L",
  #                      "NLOPT_GD_STOGO", "NLOPT_GD_STOGO_RAND",
  #                      "NLOPT_LD_SLSQP", "NLOPT_LD_LBFGS_NOCEDAL",
  #                      "NLOPT_LD_LBFGS", "NLOPT_LN_PRAXIS", "NLOPT_LD_VAR1",
  #                      "NLOPT_LD_VAR2", "NLOPT_LD_TNEWTON",
  #                      "NLOPT_LD_TNEWTON_RESTART",
  #                      "NLOPT_LD_TNEWTON_PRECOND",
  #                      "NLOPT_LD_TNEWTON_PRECOND_RESTART",
  #                      "NLOPT_GN_CRS2_LM", "NLOPT_GN_MLSL", "NLOPT_GD_MLSL",
  #                      "NLOPT_GN_MLSL_LDS", "NLOPT_GD_MLSL_LDS",
  #                      "NLOPT_LD_MMA", "NLOPT_LN_COBYLA", "NLOPT_LN_NEWUOA",
  #                      "NLOPT_LN_NEWUOA_BOUND", "NLOPT_LN_NELDERMEAD",
  #                      "NLOPT_LN_SBPLX", "NLOPT_LN_AUGLAG", "NLOPT_LD_AUGLAG",
  #                      "NLOPT_LN_AUGLAG_EQ", "NLOPT_LD_AUGLAG_EQ",
  #                      "NLOPT_LN_BOBYQA", "NLOPT_GN_ISRES")
  #
  #if(!(algorithm %in% avail_algorithms)) {
  #  stop(cat("Provided algorithm", algorithm, "not in the list of available optimization methods."))
  #}
  
  dat <- as.matrix(dat[, 2:dim(dat)[2]])
  
  k <- dim(as.matrix(a))[1]
  
  final_res <- list()
  
  if(mu0 < 0) {mu0 <- 0}
  
  parameters <- c(t(a), f1, t(Q), f, b, mu0, theta)
  # Current results:
  #results <- list(a=NULL, f1=NULL, Q=NULL, f=NULL, b=NULL, mu0=NULL, theta=NULL)
  results_tmp <- list(a=NULL, f1=NULL, Q=NULL, f=NULL, b=NULL, mu0=NULL, theta=NULL)
  e <- new.env()
  iteration <- 0
  
  bounds <- list()
  
  if(is.null(lb)) {
    bounds$lower_bound <- setlb(k, parameters)
  } else {
    bounds$lower_bound <- lb
  }
  
  if(is.null(ub)) {
    bounds$upper_bound <- setub(k, parameters)
  } else {
    bounds$upper_bound <- ub
  }
  
  # Reading parameters:
  #start=1; end=k^2
  #a <- matrix(parameters[start:end],ncol=k, nrow=k, byrow=T)
  #results$a <- a
  ##print(results$a)
  #start=end+1; end=start+k-1
  #f1 <- matrix(parameters[start:end],ncol=1, nrow=k, byrow=T)
  #results$f1 <- f1
  ##print(results$f1)
  #start=end+1; end=start+k^2-1
  #Q <- matrix(parameters[start:end],ncol=k, nrow=k, byrow=T)
  #results$Q <- Q
  ##print(results$Q)
  #start=end+1; end=start+k-1
  #f <- matrix(parameters[start:end],ncol=1, nrow=k, byrow=F)
  #results$f <- f
  ##print(results$f)
  #start=end+1; end=start+k-1
  #b <- matrix(parameters[start:end],nrow=k, ncol=1, byrow=F)
  #results$b <- b
  ##print(results$b)
  #start=end+1; end=start
  #mu0 <- parameters[start:end]
  #results$mu0 <- mu0
  ##print(results$mu0)
  #start=end+1; end=start
  #theta <- parameters[start:end]
  #results$theta <- theta
  ##print(results$theta)
  ## End of reading parameters
  L.prev <- NA
  maxlik <- function(par) {
    
    stopflag <- F
    # Reading parameters:
    start=1; end=k^2
    a <- matrix(par[start:end],ncol=k, , nrow=k, byrow=TRUE)
    results_tmp$a <<- a
    start=end+1; end=start+k-1
    f1 <- matrix(par[start:end],ncol=1, nrow=k, byrow=FALSE)
    results_tmp$f1 <<- f1
    start=end+1; end=start+k^2-1
    Q <- matrix(par[start:end],ncol=k, nrow=k, byrow=TRUE)
    results_tmp$Q <<- Q
    start=end+1; end=start+k-1
    f <- matrix(par[start:end],ncol=1, nrow=k, byrow=FALSE)
    results_tmp$f <<- f
    start=end+1; end=start+k-1
    b <- matrix(par[start:end],nrow=k, ncol=1, byrow=FALSE)
    results_tmp$b <<- b
    start=end+1; end=start
    mu0 <- par[start:end]
    results_tmp$mu0 <<- mu0
    start=end+1; end=start
    theta <- par[start:end]
    results_tmp$theta <<- theta
    # End reading parameters
    
    if(stopifbound) {
      for(i in 1:length(results_tmp)) {
        if(length(intersect(results_tmp[[i]],c(bounds$lower_bound[i], bounds$upper_bound[i]))) >= 1) {
          cat("Parameter", names(results_tmp)[i], "achieved lower/upper bound. Process stopped.\n")
          cat(results_tmp[[i]],"\n")
          stopflag <- T
          break
        }
      }
    }
    
    if(stopflag == FALSE) {
      dims <- dim(dat)
      res <- .Call("complikMD", dat, dims[1], dims[2], a, f1, Q, b, f, mu0, theta, k, pinv.tol, gomp, logmu0)
      
      assign("results", results_tmp, envir=e)
      
      iteration <<- iteration + 1
      L.prev <<- res
      
      if(verbose) {
        cat("L = ", res,"\n")
        cat("Iteration: ", iteration,  "\nResults:\n") 
        print(results_tmp)
      }
      
    }
    
    return(as.numeric(-1*res))
  }

  # Optimization:
  if(verbose) {
    cat("Lower bound:\n")
    print(bounds$lower_bound)
    cat("Upper bound:\n")
    print(bounds$upper_bound)
  }
  #tryCatch({ans <- nloptr(x0 = parameters, 
  #               eval_f = maxlik, opts = list("algorithm"=algorithm, 
  #                                            "xtol_rel"=1.0e-1, "maxeval"=maxeval),
  #               lb = bounds$lower_bound, ub = bounds$upper_bound)
  #          
  #         },  
  #         error=function(e) {if(verbose  == TRUE) {print(e)}}, 
  #         finally=NA)
  
  #print(parameters)
  #print(bounds$lower_bound)
  #print(bounds$upper_bound)
  
  ans <- nloptr(x0 = parameters, 
  		 eval_f = maxlik, 
  		 opts = opts,
       lb = bounds$lower_bound, ub = bounds$upper_bound)
  
  final_results <- get("results",envir=e)
  #final_results <- results_tmp
  final_results[["status"]] <- ans$status
  final_results[["LogLik"]] <- L.prev
  final_results[["objective"]] <- ans$objective
  final_results[["message"]] <- ans$message
  
  # Check if any parameter achieved upper/lower limit and report it:
  limit <- FALSE
  for(i in 1:length(final_results)) {
    if(length(intersect(final_results[[i]],c(bounds$lower_bound[i], bounds$upper_bound[i]))) >= 1) {
      cat("Parameter", names(final_results)[i], "achieved lower/upper bound.\n")
      cat(final_results[[i]],"\n")
      limit <- TRUE
    }
  }
  
  if(logmu0) { 
      final_results[["mu0"]] <- exp(final_results[["mu0"]])
  }
  
  final_results$limit <- limit
  #assign("results", final_results, envir=baseenv())
  class(final_results) <- "spm.continuous"
  invisible(final_results)
}


