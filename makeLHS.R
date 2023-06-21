pacman::p_load(tidyverse, here, readxl, glue)
source("LHS.R")

# Choose how many samples you want
nSamples <- 1024
# Get your parameters into the format this script expects:
parms <- read_excel("ParametersExample.xlsx", sheet="universal") %>% 
  transmute(parm=Name,
            dist=Distribution, # Each dist should be defined in tbDists in LHS.R, if it ends with rate we convert it as 365.25/x and sample on the transformed variables
            mean=mean, # The mean/expected value for the distributions
            lo=low,  # Make sure you call the lower number "lo"
            hi=high) # As above, call this "hi"

parms.split = parms %>%
  mutate(mean=ifelse(dist=='constrate', 365.25/mean, mean)) %>% 
  # if it starts with const, it's not a dynamic parameter
  group_by(is.static=dist %>% str_starts("const")) %>% 
  group_split()

parms.dynamic = parms.split[[1]]
parms.static = parms.split[[2]]

# Make a set of parameters
parms.set.lhs <- lhs.sample(tbParmsBase = parms.dynamic, nSamples = nSamples)
parms.set.lhs

# Save them to disk
saveRDS(parms.set.lhs, 'parms.set.lhs.rds')
# You probably also want to save the static variables so you can combine parameters later
parms.static %>%
  select(parm,mean) %>%
  pivot_wider(names_from=parm, values_from = mean) %>% 
  saveRDS('parms.static.rds')
