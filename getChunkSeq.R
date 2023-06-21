# Gets the sequence of job indices belonging to a given chunkID
getChunkSeq <- function(chunkID, numJobs, numChunks) {
  minChunkSize <- floor(numJobs/numChunks)
  numBiggerChunks <- numJobs %% numChunks
  if (chunkID<=numBiggerChunks) {
    chunkStart <- (minChunkSize+1)*(chunkID-1)+1
  } else {
    chunkStart <- (minChunkSize+1)*numBiggerChunks + minChunkSize*(chunkID-1-numBiggerChunks)+1
  }
  chunkEnd <- chunkStart+minChunkSize+as.numeric(chunkID<=numBiggerChunks)-1
  chunkStart:chunkEnd
}