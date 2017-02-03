#' Fitting a 1-D genetic SPM model with constant parameters
#' 
#' @description This function implements a continuous genetic SPM model by assuming all the parameters are constants.
#' 
#' @param spm_data A dataset for the SPM model. See the STPM pacakge for more details about the format.
#' @param gene_data A two column dataset containing the genotypes for the individuals in spm_data. 
#' The first column \code{id} is the ID of the individuals in spm_data, 
#' and the second column \code{geno} is the genotype. 
#' @param a The initial value for the paramter \eqn{a}. 
#' The initial value will be predicted if not specified.
#' @param b The initial value for the paramter \eqn{b}. 
#' The initial value will be predicted if not specified.
#' @param q The initial value for the paramter \eqn{q}. 
#' The initial value will be predicted if not specified.
#' @param f The initial value for the paramter \eqn{f}. 
#' The initial value will be predicted if not specified.
#' @param f1 The initial value for the paramter \eqn{f_1}. 
#' The initial value will be predicted if not specified.
#' @param mu0 The initial value for the paramter \eqn{\mu_0} 
#' in the baseline hazard.
#' The initial value will be predicted if not specified.
#' @param theta The initial value for the paramter \eqn{\theta} 
#' in the baseline hazard. The initial value will be predicted if not specified.
#' @param lower A vector of the lower bound of the parameters.
#' @param upper A vector of the upper bound of the parameters.
#' @param effect A character vector of the parameters that are linked to genotypes. 
#' The vector can contain any combination of \code{a}, \code{b}, \code{q}, \code{f}, \code{mu0}.
#' @param control A list of the control parameters for the optimization paramters.
#' @param global A logical variable indicating whether the MLSL (TRUE) or 
#' the L-BFGS (FALSE) algorithm is used for the optimization.
#' @param verbose A logical variable indicating whether initial 
#' information is printed.
#' @param ahessian A logical variable indicating whether the 
#' approximate (FALSE) or analytical (TRUE) Hessian is returned.
#' @return est The estimates of the parameters.
#' @return hessian The Hessian matrix of the estimates.
#' @return lik The minus log-likelihood.
#' @return con A number indicating the convergence. See the 'nloptr' package for more details.
#' @return message Extra message about the convergence. See the 'nloptr' package for more details.
#' @return beta The coefficients of the genetic effect on the parameters to be linked to genotypes.
#' @references He, L., Zhbannikov, I., Arbeev, K. G., Yashin, A. I., and Kulminski, A.M., 2017. 
#' Genetic stochastic process model for detecting pleiotropic and interaction effects with longitudinal data.
#' @export
#' @examples { 
#' library(stpm) 
#' dat <- simdata_cont(N=500)
#' colnames(dat) <- c("id", "xi", "t1", "t2", "y", "y.next")
#' res <- spm_con_1d(as.data.frame(dat), a=-0.05, b=2, q=1e-8, f=80, f1=90, mu0=1e-3, theta=0.08)
#'}
spm_con_1d_g <- function(spm_data, gene_data, a = NA, b = NA, q = NA, f = NA, f1 = NA, mu0 = NA, theta = NA, effect = c('a'), lower = c(), upper = c(), control = list(xtol_rel = 1e-6), global = FALSE, verbose = TRUE, ahessian = FALSE)
{

  #a <- -0.1
  #b <- 1.2
  #q <- 0.001
  #f <- 25
  #f1 <- 26
  #mu0 <- 0.01
  #theta <- 0

  data <- spm_data
  data[which(is.na(data$y.next)),'y.next'] <- data[which(is.na(data$y.next)),'y']
  data$r0 <- 0
  aggdata <-aggregate(data[,c('id','t1','t2','xi')], by=list(data$id), FUN='max')
  tau <- aggdata$t2
  n_j <- as.data.frame(table(data$id))[,2]
  m0 <- data$y
  r0 <- data$r0
  yij <- data$y.next
  tij <- data$t2
  t0 <- data$t1
  delta <- aggdata$xi

  # gene_data, two columns (id, geno)
  index <- match(unique(data$id), gene_data$id)
  snps <- gene_data[index, 'geno']
  if(sum(is.na(snps))>0)
  {'There are individuals who have missing genotypes.'}

  if(is.na(f1))
  {f1 <- mean(m0, na.rm=TRUE)}

  if(is.na(f))
  {f0 <- f2 <- mean(m0, na.rm=TRUE)}else{
	f0 <- f2 <- f
  }
  if(is.na(a))
  {a0 <- a2 <- (-0.1)*abs(max(m0, na.rm=TRUE))/max(t0,na.rm=TRUE)}else{
	a0 <- a2 <- a
  }

  if(is.na(mu0))
  {mu00 <- mu02 <- sum(delta, na.rm=TRUE)/sum(tij-t0, na.rm=TRUE)}else{
	mu00 <- mu02 <- mu0
  }

  if(is.na(theta))
  {theta <- 0.00001}

  if(is.na(b))
  {b0 <- b2 <- sqrt(abs(mean(yij-m0, na.rm=TRUE)))/abs(mean(tij-t0,na.rm=TRUE))}else{
	b0 <- b2 <- b
  }

  if(is.na(q))
  {q0 <- q2 <- mu0/(0.25*f0^2)}else{
   q0 <- q2 <- q
  }

  if(a0>=0)
  {stop('The value for the argument \'a\' must be negative.')}
  if(b0<0)
  {stop('The value for the argument \'b\' must be positive.')}
  if(q0<0)
  {stop('The value for the argument \'q\' must be positive.')}
  if(mu0<0)
  {stop('The value for the argument \'mu0\' must be positive.')}

  param <- c(a0,a2,b0,b2,q0,q2,f0,f2,f1,mu00,mu02,theta)

  
  beta_a <- beta_b <- beta_q <- beta_f <- beta_mu <- NA

  h_index <- c()
  name_par <- c()

  if('a' %in% effect)
  {
	snps_a <- snps
	h_index <- c(h_index,c(1,2))
	name_par <- c(name_par,'a_0','a_2')
  }else{
	snps_a <- rep(0, length(snps))
	h_index <- c(h_index,c(1))
	name_par <- c(name_par,'a')
  }
  if('b' %in% effect)
  {
	snps_b <- snps
	h_index <- c(h_index,c(3,4))
	name_par <- c(name_par,'b_0','b_2')
  }else{
	snps_b <- rep(0, length(snps))
	h_index <- c(h_index,c(3))
	name_par <- c(name_par,'b')
  }
  if('q' %in% effect)
  {
	snps_q <- snps
	h_index <- c(h_index,c(5,6))
	name_par <- c(name_par,'q_0','q_2')
  }else{
	snps_q <- rep(0, length(snps))
	h_index <- c(h_index,c(5))
	name_par <- c(name_par,'q')
  }
  if('f' %in% effect)
  {
	snps_f <- snps
	h_index <- c(h_index,c(7,8))
	name_par <- c(name_par,'f_0','f_2')
  }else{
	snps_f <- rep(0, length(snps))
	h_index <- c(h_index,c(7))
	name_par <- c(name_par,'f')
  }
  
  name_par <- c(name_par, 'f1')

  h_index <- c(h_index,c(9))
  if('mu0' %in% effect)
  {
	snps_mu <- snps
	h_index <- c(h_index,c(10,11))
	name_par <- c(name_par,'mu0_0','mu0_2')
  }else{
	snps_mu <- rep(0, length(snps))
	h_index <- c(h_index,c(10))
	name_par <- c(name_par,'mu0')
  }
  h_index <- c(h_index,c(12))
  name_par <- c(name_par,'theta')
  
  if(length(lower)==0)
  {
	lower <- c(a0*5, b0*0.1, q0*0.01, f0*0.5, f1*0.5, mu00*0.1, theta)
  }else{
	if(length(lower)!=7)
	{stop('The number of lower bounds should be consistent with the number of parameters.')}
  }

  if(length(upper)==0)
  {
	upper <- c(a0*0.05, b0*5, q0*10, f0*2, f1*2, mu00*10, theta)
  }else{
	if(length(upper)!=7)
	{stop('The number of upper bounds should be consistent with the number of parameters.')}
  }
  
  lower_t <- c(lower[1],lower[1],lower[2],lower[2],lower[3],lower[3],lower[4],lower[4],lower[5],lower[6],lower[6],lower[7])
  upper_t <- c(upper[1],upper[1],upper[2],upper[2],upper[3],upper[3],upper[4],upper[4],upper[5],upper[6],upper[6],upper[7])
  
  if(verbose == TRUE)
  {
	print('Initial values:')
	print(param)
	print('Lower bounds:')
	print(lower_t)
	print('Upper bounds:')
	print(upper_t)
  }


  # re <- lik_con_1d(param, m0,r0,tau,yij,delta,tij, n_j, t0)
  # re <- c(re, gr_con_1d(param, m0,r0,tau,yij,delta,tij, n_j, t0))
  if(global == FALSE)
  {
	re <- lbfgs( x0=param,fn=lik_con_1d_g,gr=gr_con_1d_g,m0=m0,r0=r0,tau=tau,yij=yij,delta=delta,tij=tij, n_j=n_j, geno_a=snps_a, geno_b=snps_b, geno_q=snps_q, geno_f=snps_f, geno_mu=snps_mu, t0=t0,lower=lower_t,upper=upper_t, control = control)
  }else{
	re <- mlsl( x0=param,fn=lik_con_1d_g,gr=gr_con_1d_g,m0=m0,r0=r0,tau=tau,yij=yij,delta=delta,tij=tij, n_j=n_j, geno_a=snps_a, geno_b=snps_b, geno_q=snps_q, geno_f=snps_f, geno_mu=snps_mu, t0=t0,lower=lower_t,upper=upper_t, control = control)
  }

  if('a' %in% effect)
  {
	beta_a <- (re$par[2] - re$par[1])/2
  }
  if('b' %in% effect)
  {
	beta_b <- (re$par[4] - re$par[3])/2
  }
  if('q' %in% effect)
  {
	beta_q <- (re$par[6] - re$par[5])/2
  }
  if('f' %in% effect)
  {
	beta_f <- (re$par[8] - re$par[7])/2
  }
  if('mu0' %in% effect)
  {
	beta_mu <- (re$par[11] - re$par[10])/2
  }

  betas <- c(beta_a, beta_b, beta_q, beta_f, beta_mu)
  
  a_hes <- NA
  if(ahessian == TRUE)
  {
	geno_a_l <- snps_a[match(spm_data$id,gene_data[,1])]
	geno_b_l <- snps_b[match(spm_data$id,gene_data[,1])]
	geno_q_l <- snps_q[match(spm_data$id,gene_data[,1])]
	geno_f_l <- snps_f[match(spm_data$id,gene_data[,1])]
	geno_mu_l <- snps_mu[match(spm_data$id,gene_data[,1])]
	
	a_hes <- hes_loglik_g(re$par,m0,r0,tau,yij,delta,tij, n_j,t0,geno_a_l,geno_b_l,geno_q_l,geno_f_l,geno_mu_l)
	a_hes <- a_hes[h_index,h_index]
  }else{
	a_hes <- optim(re$par, lik_con_1d_g, gr_con_1d_g, m0,r0,tau,yij,delta,tij, n_j, snps_a, snps_b, snps_q, snps_f, snps_mu, t0, method = "L-BFGS-B", lower = re$par, upper = re$par, hessian=TRUE)
	a_hes <- a_hes$hessian[h_index,h_index]
  }
  par_re <- matrix(re$par[h_index], 1, length(name_par))
  colnames(par_re) <- name_par
  betas <- matrix(betas, 1, 5)
  colnames(betas) <- c('beta_a','beta_b','beta_q','beta_f','beta_mu0')
  list(est=par_re,lik=re$value, con=re$convergence, message = re$message, hessian=a_hes, beta=betas)
}
