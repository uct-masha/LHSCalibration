library(tidyverse)
library(glue)

source("model.R")  # runModel(parms) and getGOF(parms)
source("getChunkSeq.R")  # getChunkSeq(chunkID, numJobs, numChunks)

parms.dynamic <- readRDS('parms.set.lhs.rds')
parms.static <- readRDS('parms.static.rds')

numChunks <- parallel::detectCores()  # maybe you want to tweak this.
thisChunk <- as.integer(tail(commandArgs(T),1))  # when we call `Rscript calibrate.R 3` this will be 3
# I recommend calling this script with gnu parallel:
if(F) {
  print(glue("seq 1 {numChunks} | parallel --eta --workdir={getwd()} Rscript calibrate.R"))
}
# Or you could do something like this in your R Console to run this script for each chunk:
if (F) {
  library(parallel)
  library(doParallel)
  library(foreach)
  library(callr)
  ncpu <- detectCores()
  cl <- makeCluster(ncpu)
  registerDoParallel(cl)
  result <- foreach(i=1:ncpu) %dopar% {
    rscript(script='calibrate.R', cmdargs = i, show=F)
  }
}

# Get the parms to be processed by this chunk/worker
parms.dynamic.chunk <- parms.dynamic %>% 
  slice(getChunkSeq(thisChunk, nrow(parms.dynamic), numChunks))

# Run model for each of these parameters and get some modelOut output:
modelOut <- parms.dynamic.chunk %>%
  rowwise() %>% 
  group_map(~.x %>% mutate(GOF=getGOF(parms=cbind(.x, parms.static)))) %>% 
  bind_rows()

# Save to disk
calibrationResultsDirname <- 'calibrationResults'
if (!dir.exists(calibrationResultsDirname)) dir.create(calibrationResultsDirname)
saveRDS(modelOut, glue("{calibrationResultsDirname}/modelOut.{thisChunk}.rds"))
#modelOut %>% arrange(desc(GOF)) %>% view()
