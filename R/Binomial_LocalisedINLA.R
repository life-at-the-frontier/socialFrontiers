#' Run a Binomial Localised spatial autoregressive model using INLA
#' @author Guanpeng Dong and Nema Dean
#' @details
#' Need filling in by original authors
#' @importFrom INLA inla

binomial_localisedINLA <-
  function(formula,
           W,
           Ntrials,
           fix.rho = FALSE,
           rho = NULL,
           prior.beta.mean = 0,
           prior.beta.precision = 0.001,
           prior.precision.shape = 0.001,
           prior.precision.scale = 0.001)
  {
  ##############################################
  #### Format the arguments and check for errors
  ##############################################
  #### Overall formula object
  frame <- try(suppressWarnings(model.frame(formula, na.action=na.pass)), silent=TRUE)
  if(class(frame)=="try-error") stop("the formula inputted contains an error, e.g the variables may be different lengths.", call.=FALSE)

  #### Design matrix
  ## Create the matrix
  X <- try(suppressWarnings(model.matrix(object=attr(frame, "terms"), data=frame)), silent=TRUE)
  if(class(X)=="try-error") stop("the covariate matrix contains inappropriate values.", call.=FALSE)
  if(sum(is.na(X))>0) stop("the covariate matrix contains missing 'NA' values.", call.=FALSE)

  n <- nrow(X)
  p <- ncol(X)

  #### Response variable
  ## Create the response
  Y <- model.response(frame)

  #### W matrix
  if(!is.matrix(W)) stop("W is not a matrix.", call.=FALSE)
  if(nrow(W)!= n) stop("W has the wrong number of rows.", call.=FALSE)
  if(ncol(W)!= n) stop("W has the wrong number of columns.", call.=FALSE)
  if(sum(is.na(W))>0) stop("W has missing 'NA' values.", call.=FALSE)
  if(!is.numeric(W)) stop("W has non-numeric values.", call.=FALSE)
  if(!sum(names(table(W))==c(0,1))==2) stop("W has non-binary (zero and one) values.", call.=FALSE)

  #### Global correlation parameter rho
  if(is.null(rho) & fix.rho==TRUE) stop("rho is fixed but no value has been provided", call.=FALSE)
  if(is.null(rho)) rho <- runif(1)
  if(length(rho)!= 1) stop("rho is the wrong length.", call.=FALSE)
  if(sum(is.na(rho))>0) stop("rho has missing 'NA' values.", call.=FALSE)
  if(!is.numeric(rho)) stop("rho has non-numeric values.", call.=FALSE)
  if(rho < 0 | rho >= 1) stop("rho is outside the interval [0,1).", call.=FALSE)
  if(fix.rho!=TRUE & fix.rho!=FALSE) stop("fix.rho has a non TRUE/FALSE value.", call.=FALSE)

  #### Create a data frame object
  data.temp <- data.frame(Y=Y, X=X, region=1:n)

  ###############################################
  #### Run an initial model assuming independence
  ###############################################
  #### Run the model
  form <- Y ~ -1 + X +  f(region, model="iid", constr=TRUE, hyper=list(theta=list(prior="loggamma", param=c(prior.precision.shape, prior.precision.scale))))
  model  =  inla(form, family="binomial", data=data.temp, Ntrials=Ntrials, control.results=list(return.marginals.predictor=TRUE), control.fixed=list(mean=prior.beta.mean, mean.intercept=prior.beta.mean, prec=prior.beta.precision, prec.intercept=prior.beta.precision), control.compute=list(dic=TRUE, mlik=TRUE), control.predictor=list(compute=TRUE))


  #### Compute Morans I
  fitted <- model$summary.fitted.values[ ,4]
  residuals <- data.temp$Y - fitted
  difference <- residuals - mean(residuals)
  denom <- sum(W) * sum(difference^2)
  num <- n * sum(difference %*% t(difference) * W)
  MoransI <- num/denom

  #### Compute the initial W matrix
  RE.all <- cbind(model$summary.random[[1]]$"0.025quant", model$summary.random[[1]]$"0.975quant")
  W.current <- array(0, c(n,n))

  for(i in 1:n)
  {
    for(j in 1:n)
    {
      if(i>j & W[i,j]==1)
      {
        RE.temp <- RE.all[c(i, j), ]
        value <- 1 - as.numeric(RE.temp[1,2]<RE.temp[2,1] | RE.temp[2,2]<RE.temp[1,1])
        W.current[i,j] <- value
        W.current[j,i] <- value
      }else
      {
      }
    }
  }

  #### Store this initial spatial weights matrix---W
  W.store <- array(NA, c(1, n*n))
  W.store[1, ] <- as.numeric(W.current)
  iteration <- 0
  difference.current <- 10
  difference.minimum <- 10

  #######################################################################
  #### Run the model and save the results
  #### Two possible cases depending on whether rho is fixed or estimated
  #######################################################################

  #######################################################################
  #### Run the model and save the results
  #### Two possible cases depending on whether rho is fixed or estimated
  #######################################################################
  if(fix.rho)
  {
    #################
    #### rho is fixed
    #################

    #####################################################################################
    #### Iterate the estimation of W and Theta until the termination condition is reached
    #####################################################################################
    while(difference.current > 0 & difference.minimum >0)
    {
      #### Compute the current precision matrix
      I.n <- diag(rep(1,n))
      W.temp <- - W.current
      diag(W.temp) <- apply(W.current,1,sum)
      Q <- rho * W.temp + (1-rho) * I.n


      #### Fit the model with the current W matrix
      form <- Y ~  -1 + X + f(region, model="generic0", Cmatrix = Q, constr=TRUE, hyper=list(theta=list(prior="loggamma", param=c(prior.precision.shape, prior.precision.scale))))
      model  =  inla(form, family="binomial", data=data.temp, Ntrials=Ntrials,
#                     verbose = T,
                     control.results=list(return.marginals.predictor=TRUE), control.fixed=list(mean=prior.beta.mean, mean.intercept=prior.beta.mean, prec=prior.beta.precision, prec.intercept=prior.beta.precision), control.compute=list(dic=TRUE, mlik=TRUE), control.predictor=list(compute=TRUE))


      #### Compute Moran's I
      fitted <- model$summary.fitted.values[ ,4]
      residuals <- data.temp$Y - fitted
      difference <- residuals - mean(residuals)
      denom <- sum(W) * sum(difference^2)
      num <- n * sum(difference %*% t(difference) * W)
      MoransI <- c(MoransI, num/denom)


      #### Compute the new W matrix
      RE.all <- cbind(model$summary.random[[1]]$"0.025quant", model$summary.random[[1]]$"0.975quant")
      W.new <- array(0, c(n,n))

      for(i in 1:n)
      {
        for(j in 1:n)
        {
          if(i>j & W[i,j]==1)
          {
            RE.temp <- RE.all[c(i, j), ]
            value <- 1 - as.numeric(RE.temp[1,2]<RE.temp[2,1] | RE.temp[2,2]<RE.temp[1,1])
            W.new[i,j] <- value
            W.new[j,i] <- value
          }else
          {
          }
        }
      }


      #### Determine whether to terminate and save the W matrix
      W.store <- rbind(W.store, as.numeric(W.new))
      W.current <- W.new
      iteration <- iteration + 1
      difference.all <- rep(NA, iteration)

      for(k in 1:iteration)
      {
        difference.all[k] <- sum(abs(W.store[k, ] - W.store[(iteration+1), ])) / 2
      }

      difference.current <- difference.all[iteration]
      difference.minimum <- min(difference.all)
      # the progress
      cat("This is", iteration, "iteration\n")
    }



    ########################################
    #### Determine W and fit the final model
    ########################################
    if(difference.current==0)
    {
      #### If the W matrix has converged
      ## Compute Q
      W.final <- W.current
      termination.condition <- "Converge"
      I.n <- diag(rep(1,n))
      W.temp <- - W.final
      diag(W.temp) <- apply(W.final,1,sum)
      Q <- rho * W.temp + (1-rho) * I.n


      ## Run the final model
      form <- Y ~  -1 + X  +  f(region, model="generic0", Cmatrix = Q, constr=TRUE, hyper=list(theta=list(prior="loggamma", param=c(prior.precision.shape, prior.precision.scale))))
      model  =  inla(form, family="binomial", data=data.temp, Ntrials=Ntrials, control.results=list(return.marginals.predictor=TRUE), control.fixed=list(mean=prior.beta.mean, mean.intercept=prior.beta.mean, prec=prior.beta.precision, prec.intercept=prior.beta.precision), control.compute=list(dic=TRUE, mlik=TRUE), control.predictor=list(compute=TRUE))


      ## Save the results
      fixed <- model$summary.fixed[ ,c(4,3,5)]
      phi <- model$summary.random$region[ , c(5,4,6)]
      colnames(phi) <- c("Median", "LCI", "UCI")
      fitted <- model$summary.fitted.values[ ,c(4,3,5)]
      colnames(fitted) <- c("Median", "LCI", "UCI")
      residuals <- data.temp$Y - fitted[ ,1]
      DIC <- c(model$dic$dic, model$dic$p.eff)
      names(DIC) <- c("DIC", "p.d")
      hyperparameters <- model$summary.hyperpar[c(4,3,5)]
      results <- list(beta=fixed, phi=phi, fittedvalues=fitted, DIC=DIC, residuals=residuals, hyperparameters=hyperparameters, W.estimated=W.final, iteration=iteration, termination.condition=termination.condition, n.cycle=NA)
    }else
    {
      #### The model has cycled
      ## Choose W by minimising Moran's I
      start.cycle <- which(difference.all==0)[1] + 1
      cycle <- start.cycle:(iteration+1)
      which.moransI <- which(min(abs(MoransI[cycle]))==abs(MoransI[cycle]))
      W.index <- cycle[which.moransI]
      W.final <- matrix(W.store[W.index, ], byrow=TRUE, nrow=n, ncol=n)


      ## Compute Q
      termination.condition <- "Cycle"
      n.cycle <- length(cycle)
      I.n <- diag(rep(1,n))
      W.temp <- - W.final
      diag(W.temp) <- apply(W.final,1,sum)
      Q <- rho * W.temp + (1-rho) * I.n


      ## Run the final model
      form <- Y ~  -1 + X  +  f(region, model="generic0", Cmatrix = Q, constr=TRUE, hyper=list(theta=list(prior="loggamma", param=c(prior.precision.shape, prior.precision.scale))))
      model  =  inla(form, family="binomial", data=data.temp, Ntrials=Ntrials, control.results=list(return.marginals.predictor=TRUE), control.fixed=list(mean=prior.beta.mean, mean.intercept=prior.beta.mean, prec=prior.beta.precision, prec.intercept=prior.beta.precision), control.compute=list(dic=TRUE, mlik=TRUE), control.predictor=list(compute=TRUE))


      ## Save the results
      fixed <- model$summary.fixed[ ,c(4,3,5)]
      phi <- model$summary.random$region[ , c(5,4,6)]
      colnames(phi) <- c("Median", "LCI", "UCI")
      fitted <- model$summary.fitted.values[ ,c(4,3,5)]
      colnames(fitted) <- c("Median", "LCI", "UCI")
      residuals <- data.temp$Y - fitted[ ,1]
      DIC <- c(model$dic$dic, model$dic$p.eff)
      names(DIC) <- c("DIC", "p.d")
      hyperparameters <- model$summary.hyperpar[c(4,3,5)]
      results <- list(beta=fixed, phi=phi, fittedvalues=fitted, DIC=DIC, residuals=residuals, hyperparameters=hyperparameters, W.estimated=W.final, iteration=iteration, termination.condition=termination.condition, n.cycle=n.cycle)
    }
  }else
  {
    ##########################################
    #### rho is estimated as part of the model
    ##########################################

    #####################################################################################
    #### Iterate the estimation of W and Theta until the termination condition is reached
    #####################################################################################
    while(difference.current > 0 & difference.minimum >0)
    {
      #### Compute the current precision matrix
      W.temp <- W.current
      diag(W.temp) <- rep(1,n) - apply(W.current,1,sum)
      Q <- W.temp


      #### Fit the model with the current W matrix
      form <- Y ~  -1 + X +  f(region, model="generic1", Cmatrix = Q, constr=TRUE, hyper=list(theta1=list(prior="loggamma", param=c(prior.precision.shape, prior.precision.scale)), theta2=list(prior="gaussian", param=c(0, 0.01))))
      model  =  inla(form, family="binomial", data=data.temp, Ntrials=Ntrials,control.results=list(return.marginals.predictor=TRUE), control.fixed=list(mean=prior.beta.mean, mean.intercept=prior.beta.mean, prec=prior.beta.precision, prec.intercept=prior.beta.precision), control.compute=list(dic=TRUE, mlik=TRUE), control.predictor=list(compute=TRUE))


      #### Compute Moran's I
      fitted <- model$summary.fitted.values[ ,4]
      residuals <- data.temp$Y - fitted
      difference <- residuals - mean(residuals)
      denom <- sum(W) * sum(difference^2)
      num <- n * sum(difference %*% t(difference) * W)
      MoransI <- c(MoransI, num/denom)


      #### Compute the new W matrix
      RE.all <- cbind(model$summary.random[[1]]$"0.025quant", model$summary.random[[1]]$"0.975quant")
      W.new <- array(0, c(n,n))

      for(i in 1:n)
      {
        for(j in 1:n)
        {
          if(i>j & W[i,j]==1)
          {
            RE.temp <- RE.all[c(i, j), ]
            value <- 1 - as.numeric(RE.temp[1,2]<RE.temp[2,1] | RE.temp[2,2]<RE.temp[1,1])
            W.new[i,j] <- value
            W.new[j,i] <- value
          }else
          {
          }
        }
      }


      #### Determine whether to terminate and save the W matrix
      W.store <- rbind(W.store, as.numeric(W.new))
      W.current <- W.new
      iteration <- iteration + 1
      difference.all <- rep(NA, iteration)

      for(k in 1:iteration)
      {
        difference.all[k] <- sum(abs(W.store[k, ] - W.store[(iteration+1), ])) / 2
      }

      difference.current <- difference.all[iteration]
      difference.minimum <- min(difference.all)

      # the progress
      cat("This is", iteration, "iteration\n")

    }



    ########################################
    #### Determine W and fit the final model
    ########################################
    if(difference.current==0)
    {
      #### If the W matrix has converged
      ## Compute Q
      W.final <- W.current
      termination.condition <- "Converge"
      W.temp <- W.final
      diag(W.temp) <- rep(1,n) - apply(W.final,1,sum)
      Q <- W.temp


      ## Run the final model
      form <- Y ~  -1 + X + f(region, model="generic1", Cmatrix = Q, constr=TRUE, hyper=list(theta1=list(prior="loggamma", param=c(prior.precision.shape, prior.precision.scale)), theta2=list(prior="gaussian", param=c(0, 0.01))))
      model  =  inla(form, family="binomial", data=data.temp, Ntrials=Ntrials,control.results=list(return.marginals.predictor=TRUE), control.fixed=list(mean=prior.beta.mean, mean.intercept=prior.beta.mean, prec=prior.beta.precision, prec.intercept=prior.beta.precision), control.compute=list(dic=TRUE, mlik=TRUE), control.predictor=list(compute=TRUE))


      ## Save the results
      fixed <- model$summary.fixed[ ,c(4,3,5)]
      phi <- model$summary.random$region[ , c(5,4,6)]
      colnames(phi) <- c("Median", "LCI", "UCI")
      fitted <- model$summary.fitted.values[ ,c(4,3,5)]
      colnames(fitted) <- c("Median", "LCI", "UCI")
      residuals <- data.temp$Y - fitted[ ,1]
      DIC <- c(model$dic$dic, model$dic$p.eff)
      names(DIC) <- c("DIC", "p.d")
      hyperparameters <- model$summary.hyperpar[ , c(4,3,5)]
      results <- list(beta=fixed, phi=phi, fittedvalues=fitted, DIC=DIC, residuals=residuals, hyperparameters=hyperparameters, W.estimated=W.final, iteration=iteration, termination.condition=termination.condition, n.cycle=NA)
    }else
    {
      #### The model has cycled
      ## Choose W by minimising Moran's I
      start.cycle <- which(difference.all==0)[1] + 1
      cycle <- start.cycle:(iteration+1)
      which.moransI <- which(min(abs(MoransI[cycle]))==abs(MoransI[cycle]))
      W.index <- cycle[which.moransI]
      W.final <- matrix(W.store[W.index, ], byrow=TRUE, nrow=n, ncol=n)


      ## Compute Q
      termination.condition <- "Cycle"
      n.cycle <- length(cycle)
      W.temp <- W.final
      diag(W.temp) <- rep(1,n) - apply(W.final,1,sum)
      Q <- W.temp


      ## Run the final model
      form <- Y ~  -1 + X  +  f(region, model="generic1", Cmatrix = Q, constr=TRUE, hyper=list(theta1=list(prior="loggamma", param=c(prior.precision.shape, prior.precision.scale)), theta2=list(prior="gaussian", param=c(0, 0.01))))
      model  =  inla(form, family="binomial", data=data.temp, Ntrials=Ntrials,control.results=list(return.marginals.predictor=TRUE), control.fixed=list(mean=prior.beta.mean, mean.intercept=prior.beta.mean, prec=prior.beta.precision, prec.intercept=prior.beta.precision), control.compute=list(dic=TRUE, mlik=TRUE), control.predictor=list(compute=TRUE))


      ## Save the results
      fixed <- model$summary.fixed[ ,c(4,3,5)]
      phi <- model$summary.random$region[ , c(5,4,6)]
      colnames(phi) <- c("Median", "LCI", "UCI")
      fitted <- model$summary.fitted.values[ ,c(4,3,5)]
      colnames(fitted) <- c("Median", "LCI", "UCI")
      residuals <- data.temp$Y - fitted[ ,1]
      DIC <- c(model$dic$dic, model$dic$p.eff)
      names(DIC) <- c("DIC", "p.d")
      hyperparameters <- model$summary.hyperpar[ ,c(4,3,5)]
      results <- list(beta=fixed, phi=phi, fittedvalues=fitted, DIC=DIC, residuals=residuals, hyperparameters=hyperparameters, W.estimated=W.final, iteration=iteration, termination.condition=termination.condition, n.cycle=n.cycle)
    }
  }


  #### Return the results
  return(results)
}
