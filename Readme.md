# README

This repository illustrates how you can do model calibration in R.
Usually we have some parameter sheet with parameter values and we can include to that information about their underlying distributions. An example of such a parameter sheet can be found in `ParametersExample.xlsx`

```{r}
readxl::read_excel('ParametersExample.xlsx', sheet='universal')
```
Here I have recorded each parameter as it is used in my model together with information on what value it should take. In the case that the parameter is not constant, we include low and high values as well as the distribution to sample from. Another thing I do in this repo is allow for one to add "rate" to the distribution. For example if you have a parameter for incubation period, you might want to specify this in days and also sample with your distribution in days also. The resulting parameter will take your $x$ days and convert it to a rate $365.25/x$. In the example parameters sheet we ask for the incubation period `nu` to be sampled from a triangle distribution in days. Here is a histogram of what gets sampled:
```{r}
source("LHS.R")
nuValues <- lhs.sample(tibble(parm='nu', dist='trirate',mean=28,lo=15,hi=50), 1000)
nu <- nuValues$nu
hist(365.25/nu, breaks = 50)
```
Of course the actual value of `nu` used in your model will take on a different distribution:
```{r}
hist(nu, breaks = 50)
```

## How to use this repo
First you will want to ensure you are using a parameter sheet in the same format as `ParameterExample.xlsx` and that these parameters are sufficient to pull into your model. One code snippet useful for this is:
```{r}
parms <- readxl::read_excel("ParametersExample.xlsx", sheet="universal") %>%
  # parameters with distribution ending with "rate" should be converted
  mutate(mean=ifelse(str_ends(Distribution,'rate'),365.25/mean,mean)) %>% 
  # Make this into a list of values
  pull(mean, name=Name) %>% 
  as.list()
```

This list can then get passed to your model and you can access the parameter names directly if you use `with` like so:
```{r}
rates <- function(parms) {
  print("The value of nu outside `with` must be accessed like this:")
  print(parms$nu)
  with(parms, {
    print("The value of nu inside `with` can be accessed like this:")
    print(nu)
  })
}
rates(parms)
```

Once you're satisfied that your model runs and the parameter sheet looks good you can make some parameters using the `makeLHS.R` script. You will need to specify how many parameter sets you want as well as modify the call to `read_excel` according to your needs. Once that's done you run the rest of the script to save the dynamic parameters you created to `parms.set.lhs.rds` and the static parameters (constant values) to `parms.static.rds`.

```{r}
source('makeLHS.R')
```

Now that you have some parameters sampled using LHS you need to run your model with each of these. The `calibrate.R` script is designed to be called with a parameter. To call an R script with a parameter you can use the `Rscript` program in the terminal. For example if I call `Rscript calibrate.R 1` from the terminal in the working directory then inside that R script the `commandArgs(T)` will be '1'.

The idea behind the `calibrate.R` script is that it will be run with different inputs and each will process a different chunk of the parameters. On my machine, I have 12 cores, so I would run `calibrate.R` 12 times with different inputs and the first input would process the first 1/12th of the parameters. I can either run `seq 1 12 | parallel --eta Rscript calibrate.R` to have the GNU parallel library do this for me or use this snippet:
```{r}
library(parallel)
library(doParallel)
library(foreach)
library(callr)
ncpu <- detectCores()
cl <- makeCluster(ncpu)
registerDoParallel(cl)
result <- foreach(i=1:ncpu) %dopar% {
  callr::rscript(script='calibrate.R', cmdargs = i, show=F)
}
```

This will place results in the `calibrationResults` directory. Then open `exploreCalibrationResults.R` and see how we can start to explore the results.
```{r}
tbCalibration <- dir('calibrationResults', full.names = T) %>%
  lapply(readRDS) %>%
  bind_rows()

# Plot each variable against the GOF
tbCalibration %>%
  pivot_longer(!c(sampleID, GOF), names_to = 'parm') %>%
  ggplot() +
    aes(x = value, y = GOF) +
    geom_point(shape = "circle", size = 1.5, colour = "#112446") +
    labs(
      x = "parameter value",
      title = "Basic Comparison of Dynamic Parameters"
    ) +
    theme_minimal() +
    facet_wrap(vars(parm), scales = "free_x")
```

Of course for this example code the outputs don't correspond to the sort of thing you should see in your model. 
