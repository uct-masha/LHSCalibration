library(tidyverse)
library(glue)

# Get the results from calibration.R
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

# Another nice way to look at results is with esquisser:
# tbCalibration %>%
#   pivot_longer(nu:cov3, names_to = 'parm') %>% 
#   esquisse::esquisser()
