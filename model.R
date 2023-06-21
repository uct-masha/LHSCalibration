# Example runModel function
runModel <- function(parms) {
  # NOTE: this is for illustration and makes no actual sense.
  log(prod(parms[parms!=0]))
}

# Example getGOF function
getGOF <- function(parms) {
  # You should calculate some goodness of fit value in here.
  # Typically there will be many data points which are compared to model outputs.
  # In this case we will just use one single point:
  data <- 13  # must be an integer for poisson distribution
  modelOutput <- runModel(parms) # you probably want to call runModel here
  LL <- dpois(x = data, lambda = modelOutput, log = T)
  NLL <- -1*LL
  NLL
}