data <- c(21.8, 40.8, 25.1, 34.6, 39.1, 37.8, 38.4, 36.0, 34.4, 30.1,
          25.1, 37.4, 38.0, 25.1, 37.8, 29.4, 34.3, 27.2, 23.8, 29.9,
          26.8, 36.8, 25.0, 36.1, 24.9, 38.5, 23.8, 36.8, 31.7, 20.6,
          29.8, 24.3, 36.5, 39.0, 40.9, 27.4, 22.2, 28.2, 31.1, 31.5,
          38.9, 24.6, 22.3, 39.1, 25.9, 21.6, 24.1, 29.4, 22.1, 28.9,
          42.4, 38.5, 43.8, 25.9, 26.5, 33.8, 20.8, 37.5, 37.6, 38.4,
          27.4, 28.5, 31.5, 28.5, 25.1, 31.3, 22.3, 25.3, 22.6, 22.3,
          20.4, 31.1, 21.2, 28.7, 24.9, 23.2, 26.4, 20.6, 27.8, 26.9,
          30.2, 20.5, 31.1, 30.0, 26.7, 25.4, 25.1, 30.3, 28.8, 23.6,
          25.3, 26.6, 23.8, 28.5, 27.0, 23.9, 21.9, 31.9, 26.0, 23.5)


media <- mean(data)
print(media)

median <- function(x){
  x_ordered <- sort(x[!is.na(x)])
  n <- length(x_ordered)
  if(n %% 2 == 1){
    return (x_ordered[(n+1) / 2])
  } else{
    return((x_ordered[n/2] + x_ordered[(n/2) + 1])/2)
  }
}

median <- median(data)
print(median)

quartil <- function(x, p){
  x_ordered <- sort(x[!is.na(x)])
  n <- length(x_ordered)   #number of values
  pos <- p * (n+1)         #Calculates the theoretical position of the quartil
  k <- floor(pos)          #obtains the integer part of the position
  d <- pos - k             #obtains the decimal part of the position
  
  if(k < 1) return (x_ordered[1])    #if the integer part is less than 1, returns the first element
  if(k>=n) return(x_ordered[n])      #if the integer part is greater than the number of elements, returns the element n
  
  return(x_ordered[k] + d * (x_ordered[k+1] - x_ordered[k]))
}

interquartileRange <- function(q3, q1){
  return (q3 - q1)
}


iqr <- interquartileRange(quartil(data, 0.75), quartil(data, 0.25))
print(iqr)


standardDeviation <- function(x){
  x_clear <- x[!is.na(x)]
  m <- mean(x_clear)
  n <- length(x_clear)
  return(sqrt(sum((x_clear - m)^2) / n))
}

sd <- standardDeviation(data)
print(sd)


coefficientOfVariation <- function(standard_deviation, mean){
  return(standard_deviation / mean)
}

cv <- coefficientOfVariation(sd, media)
print(cv)


centralMoment <- function(x, k){
  x_clear <- x[!is.na(x)]
  n <- length(x_clear)
  m <- mean(x_clear)
  
  return(sum((x_clear - m)^k)/n)
}


asymmetry <- function(x){
  m3 <- centralMoment(x, 3)
  sigma <- standardDeviation(x)
  return(m3 / (sigma^3))
}

skewness <- asymmetry(data)
print(skewness)


kurtosis <- function(x){
  m4 <- centralMoment(x, 4)
  sd <- standardDeviation(x)
  
  return((m4/(sd^4)) - 3)
}

k <- kurtosis(data)
print(k)