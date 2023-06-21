library(lhs)
library(tidyverse)
library(triangle)
library(plotly)

lhs.sample <- function(tbParmsBase, nSamples) {
  #tbParmsBase=parms.dynamic; nSamples=numparms # testing
  tbDists <- tribble(~dist, ~func,
                     # Given a percent v between 0 and 1
                     # and m=expected value, l=low, h=high,
                     # return the quantile function value of the corresponding distribution at v
                     "tri",   function(v,m,l,h){qtriangle(p=v,a=l,b=h,c=m)},
                     "unif",  function(v,m,l,h){qunif(p=v,min=l,max=h)},
                     "norm",  function(v,m,l,h){qnorm(p=v,mean=m,sd=(h-l)/4)},
                     "const", function(v,m,l,h){m}  # TODO: Add beta and log-normal distributions
  )
  
  # Sometimes we want to consider a distribution on the days but the parameter is a rate
  # For example a parameter called incD=365.25/7 representing 7 days incubation period
  # If we have a triangular distribution between 5 and 8 days with peak at 7 days then
  # we want to sample the distribution as a rate, ie from 365.25/8 to 365.25/5.
  # In this case, we take distribution=trirate to mean that we should sample the days
  # with a triangular distribution and return the result as 365.25/days. Specifically
  # then the values in the mean/high/low columns are expressed in days with the understanding
  # that the resulting value will be in the form 365.25/days.
  tbRateDists <- tbDists %>% 
    mutate(dist=paste0(dist, "rate"),
           func=sapply(func, function(f){function(v,m,l,h){365.25/f(1-v,m,l,h)}}))
  
  tbParms <- tbParmsBase %>%
    # filter(dist %in% c("trirate", "unif", "norm")) %>%
    left_join(tbDists %>% bind_rows(tbRateDists), by='dist') %>%
    # This is so we can, if mean/hi/lo are char expressions, evaluate them
    rowwise() %>%
    mutate(mean=mean %>% as.character %>% parse(text=.) %>% eval(),
           lo=lo %>% as.character %>% parse(text=.) %>% eval(),
           hi=hi %>% as.character %>% parse(text=.) %>% eval()) %>%
    mutate(LO=min(lo,hi),
           HI=max(lo,hi)) %>%
    mutate(lo=LO,hi=HI) %>% select(!LO)%>% select(!HI)
  
  nParams <- nrow(tbParms)
  matSamples <- randomLHS(n = nSamples, k = nParams)
  colnames(matSamples) <- tbParms$parm
  tbSamples <- matSamples %>%
    as_tibble() %>%
    rowid_to_column(var = "sampleID") %>%
    pivot_longer(!sampleID, names_to = 'parm') %>%
    left_join(tbParms, by='parm') %>%
    rowwise() %>%
    # summarise(sampleID, parm=parm, valueBase=value, valueTrans=func(value, mean, lo, hi)) %>%
    # pivot_wider(names_from = parm, values_from = c(valueBase, valueTrans))
    summarise(sampleID, parm=parm, value=func(value, mean, lo, hi)) %>%  # if value should be a rate 365.25/days then take 365.25/func(1-value,...)
    pivot_wider(names_from = parm, values_from = "value")
  
  return(tbSamples)
}
