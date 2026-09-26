xi <- c(3, 4, 8, 10)
ni <- c(3, 15, 2, 4)
N <- sum(ni)

meanFreqAgroupData <- function(xi, ni, N){
  return((sum(xi * ni)) / N)
}

print(meanFreqAgroupData(xi, ni, N))